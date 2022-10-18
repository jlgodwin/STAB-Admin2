################################################################
#########   Load libraries
################################################################

## Download most recent version of SUMMER from Github
library(devtools)
# devtools::install_github("bryandmartin/SUMMER",
#                          build_vignettes = F, force = T)
# 

rm(list = ls())
library(SUMMER)
library(classInt)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(rgdal)
library(scales)
library(INLA)
INLA:::inla.dynload.workaround()
library(survey)
library(ggplot2)
library(gridExtra)
library(parallel)
library(spdep)
library(geosphere)


# Set working directory to the Main folder.
# This needs to be replaced by the directory in each local system.
home.dir <- '~/Dropbox/DHS-SAR/'

# Set directories 
data.dir <- paste0(home.dir,'/Data/',sep='')
res.dir <- paste0(home.dir,'/Results/',sep='')

setwd(data.dir)

################################################################
#########   set parameters
################################################################

#### Files info ####
country <- "Bangladesh"
gadm.abbrev <- "BGD"
poly.path <- paste0(country,"/shapeFiles_gadm")

# dhsStata, which contains survey data
dhsStata.file<-"BDBR7RDT/BDBR7RFL.DTA"

# survey GPS
dhsFlat.file<-'BDGE7RFL'


#### Analysis info ####
beg.year = 2009
end.year = 2018
type.st =  4
survey_year<-2018

################################################################
#########   load polygon files
################################################################
poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                         '0', sep = "_")
poly.layer.adm1 <- paste('gadm36', gadm.abbrev,
                         '1', sep = "_")
poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                         '2', sep = "_")

poly.adm0 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm0)) 
# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1))

if(sum(grepl(paste('gadm36', gadm.abbrev,
                   '2', sep = "_"), list.files(poly.path))) != 0){
  poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character(poly.layer.adm2))}

if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

#### LOAD GEO DATA ####
poly.adm1.sf <- poly.adm1 %>% 
  st_as_sf() 
poly.adm2.sf <- poly.adm2 %>% 
  st_as_sf() 
################################################################
######### pop density processing
################################################################
in_folder <- "~/Dropbox/AfricaAdmin2Estimates/Data/Population/Bangladesh/"
dir.create(in_folder, showWarnings = FALSE)
setwd(in_folder)
out_folder <- "~/Dropbox/AfricaAdmin2Estimates/Data/Population/Bangladesh/"
dir.create(out_folder, showWarnings = FALSE)

for (yr in 2000:2020) {
  print(yr)
  f0 <- raster(paste0("bgd_f_0_", yr, ".tif"))
  f1 <- raster(paste0("bgd_f_1_", yr, ".tif"))
  m0 <- raster(paste0("bgd_m_0_", yr, ".tif"))
  m1 <- raster(paste0("bgd_m_1_", yr, ".tif"))
  u5 <- calc(brick(f0, f1, m0, m1), sum)
  saveRDS(readAll(u5), paste0(out_folder, "u5_pop_", yr, ".rds"))
}


################################################################
######### EVI processing on 1km grid?
################################################################
in_folder <- "~/Dropbox/DHS-SAR/Data/Bangladesh/covariates/raw/evi/"
dir.create(in_folder, showWarnings = FALSE)
out_folder <- "~/Dropbox/DHS-SAR/Data/Bangladesh/covariates/cleaned/evi/"
dir.create(out_folder, showWarnings = FALSE)
pop_folder <- "~/Dropbox/AfricaAdmin2Estimates/Data/Population/Bangladesh/"

#temp <- read_stars(paste0(in_folder, "MOD13Q1.006__250m_16_days_EVI_doy2001001_aid0001.tif"))
adm1.tbl.list <- list()
adm2.tbl.list <- list()
for (yr in 2001:2020) {
  u5 <- readRDS(paste0(pop_folder, "u5_pop_", yr, ".rds")) %>% aggregate(fact = 10, fun = sum, na.rm = T)
  evi <- raster(paste0(in_folder, "MOD13Q1.006__250m_16_days_EVI_doy", yr, "001_aid0001.tif"))
  evi <- projectRaster(evi, u5)
  cov.sf <- st_as_stars(stack(u5, evi)) %>% st_as_sf(as_points = T)
  colnames(cov.sf)[1:2] <- c("u5.pd", "evi")
  # ASSIGN PIXELS TO AREAS
  admin1.key <- do.call(c, lapply(sf::st_intersects(cov.sf, poly.adm1.sf),
                                  function(x) ifelse(length(x) > 0, x[1], NA)))
  # for any NA values, choose nearest region
  admin1.key[is.na(admin1.key)] <-
    do.call(c, lapply(sf::st_nearest_feature(cov.sf[is.na(admin1.key), ], poly.adm1.sf),
                      function(x) ifelse(length(x) > 0, x[1], NA)))
  admin2.key <- do.call(c, lapply(sf::st_intersects(cov.sf, poly.adm2.sf),
                                     function(x) ifelse(length(x) > 0, x[1], NA)))
  # for any NA values, choose nearest region
  admin2.key[is.na(admin2.key)] <-
    do.call(c, lapply(sf::st_nearest_feature(cov.sf[is.na(admin2.key), ], poly.adm2.sf),
                      function(x) ifelse(length(x) > 0, x[1], NA)))
  cov.dt <- cov.sf %>%
    #st_set_geometry(NULL) %>%
    mutate(admin1 = admin1.key,
           admin1.char = paste0("admin1_", admin1.key),
           admin1.name = as.character(poly.adm1.sf$NAME_1)[admin1.key],
           admin2 = admin2.key,
           admin2.char = paste0("admin2_", admin2.key),
           admin2.name = as.character(poly.adm2.sf$NAME_2)[admin2.key],
           pixel.id = 1:n())
  adm1.pop <- aggregate(cov.dt$u5.pd, list(admin1.name = cov.dt$admin1.name), sum, na.rm = T)
  adm1.evi <- data.frame(years = yr,
                         admin1.name = adm1.pop[,1],
                         u5.pop = adm1.pop[,2],
                         evi = aggregate(cov.dt$evi * cov.dt$u5.pd, list(admin1.name = cov.dt$admin1.name), sum, na.rm = T)[, 2] / adm1.pop[,2])
  adm1.tbl.list <- c(adm1.tbl.list, list(adm1.evi))
  adm2.pop <- aggregate(cov.dt$u5.pd, list(admin2.name = cov.dt$admin2.name), sum, na.rm = T)
  adm2.evi <- data.frame(years = yr,
                         admin2.name = adm2.pop[,1],
                         u5.pop = adm2.pop[,2],
                         evi = aggregate(cov.dt$evi * cov.dt$u5.pd, list(admin2.name = cov.dt$admin2.name), sum, na.rm = T)[, 2] / adm2.pop[,2])
  adm2.tbl.list <- c(adm2.tbl.list, list(adm2.evi))
}
adm1.tbl <- bind_rows(adm1.tbl.list)
adm2.tbl <- bind_rows(adm2.tbl.list)
saveRDS(adm1.tbl, paste0(out_folder, "adm1_tbl.rds"))
saveRDS(adm2.tbl, paste0(out_folder, "adm2_tbl.rds"))
