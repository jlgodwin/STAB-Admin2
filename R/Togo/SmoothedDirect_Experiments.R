# Smoothed Direct Experiments
# doBenchmark = F for now

#### Libraries ####
library(SUMMER)
library(classInt)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(rgdal)
library(scales)
library(INLA)
library(survey)
library(ggplot2)
library(gridExtra)


#### Source fitINLA.R ####

# start with just SUMMER to test type.st=4
source("/Users/mirandafix/Desktop/fitINLA.R")

#### Parameters ####

country <- "Malawi"
beg.year <- 1990
end.year <- 2019
doBenchmark <- F

beg.years <- seq(1990,2015,5)
end.years <- beg.years + 4
periods <- paste(beg.years, end.years, sep = "-")


data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '../../Analysis/R'
ihme.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '../HIV/'
setwd(data.dir)

source(paste0(code.dir.rel,'/LoadCommandCenter.R'))
CountryList <- sheets_read(sheet_key, sheet = "CountryList")
folder.name <- CountryList$folderName[CountryList$Country == country]
hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
                       gsub(" ", "", folder.name))
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
n.survey <- CountryList$nSurvey[CountryList$Country == country]

## Get Survey years #### 

SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]

## Do HIV Adjustment? ####

HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                unique(HIV.country$`UNAIDS data?`) == "Y")


#### Load polygon data ####
poly.file <- shapes.sub.dir
poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                         '0', sep = "_")
poly.layer.adm1 <- paste('gadm36', gadm.abbrev,
                         '1', sep = "_")
poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                         '2', sep = "_")

poly.path <- paste0(folder.name, poly.file)
poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0))
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1))
if(sum(grepl(paste('gadm36', gadm.abbrev,
                   '2', sep = "_"), list.files(paste0(folder.name, poly.file)))) != 0){
  poly.adm2 <- readOGR(dsn = poly.path,
                       layer = as.character(poly.layer.adm2))
}

if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat.rda'))
load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat_Names.rda'))

#### Load data ####

files.list <- list.files(paste0('./', folder.name))

if(useHIVAdj){
  if(sum(grep("HIV_", files.list)) == 0){
    warning("No HIV adjusted files founds.")
    load(paste0(folder.name, '/', country, '_direct_natl_yearly.rda'))
    load(paste0(folder.name, '/', country, '_direct_natl.rda'))
    load(paste0(folder.name, '/', country, '_direct_admin1.rda'))
    load(paste0(folder.name, '/', country, '_direct_admin2.rda'))
  }else{
    load(paste0(folder.name, '/', country, '_directHIV_natl_yearly.rda'))
    load(paste0(folder.name, '/', country, '_directHIV_natl.rda'))
    load(paste0(folder.name, '/', country, '_directHIV_admin1.rda'))
    load(paste0(folder.name, '/', country, '_directHIV_admin2.rda'))
  }
}else{
  load(paste0(folder.name, '/', country, '_direct_natl_yearly.rda'))
  load(paste0(folder.name, '/', country, '_direct_natl.rda'))
  load(paste0(folder.name, '/', country, '_direct_admin1.rda'))
  load(paste0(folder.name, '/', country, '_direct_admin2.rda'))
}

#### Load IGME data ####

file.list <- list.files(igme.dir.rel)
igme.file <- file.list[grepl("Results.csv", file.list)]
igme.ests <- read.csv(paste0(igme.dir.rel,'/',igme.file),
                      header = T)
# igme.ests <- igme.ests[igme.ests$INDICATOR == "Under-five mortality rate" &
#                          igme.ests$SEX == "Total" &
#                          igme.ests$SERIES_YEAR == "2019" &
#                          igme.ests$SERIES_NAME == "UN IGME estimate 2019",]
# igme.ests$year <- igme.ests$REF_DATE - 0.5
# if(country == "Cote dIvoire"){
#   igme.ests <- igme.ests[igme.ests$REF_AREA == levels(igme.ests$REF_AREA)[45],]  
# }else{
#   igme.ests <- igme.ests[igme.ests$REF_AREA == country, ]
# }
# igme.ests <- igme.ests[order(igme.ests$year),]
# igme.ests <- igme.ests[igme.ests$year %in% beg.year:end.year,]


igme.ests <- igme.ests[igme.ests$Indicator== "Under-five Mortality Rate" &
                         igme.ests$Subgroup == "Total",]
names(igme.ests) <- gsub("X", "", names(igme.ests))

igme.ests <- igme.ests[igme.ests$Country.Name == country,]
a <- reshape(igme.ests,idvar = c("Country.Name", "Quantile"), 
             varying = list((1:dim(igme.ests)[2])[-c(1:5)]),
             v.names = "OBS_VALUE" ,direction = "long", 
             times = names(igme.ests)[-c(1:5)])
igme.ests <- reshape(a, idvar = c("time"),
                     v.names = "OBS_VALUE", 
                     timevar = "Quantile", direction = "wide")

names(igme.ests)[grepl(".Lower", names(igme.ests))] <- "LOWER_BOUND"
names(igme.ests)[grepl(".Upper", names(igme.ests))] <- "UPPER_BOUND"
names(igme.ests)[grepl(".Median", names(igme.ests))] <- "OBS_VALUE"
names(igme.ests)[grepl("time", names(igme.ests))] <- "REF_DATE"
igme.ests$year <- as.numeric(as.character(igme.ests$REF_DATE)) - 0.5
igme.ests <- igme.ests[order(igme.ests$year),]
igme.ests <- igme.ests[igme.ests$year %in% beg.year:end.year,]

#### Load IHME data ####
file.list <- list.files(ihme.dir.rel)
ihme.files <- file.list[grepl("IHME", file.list)]

ihme.ests <- list()
ihme.ests[['adm0']] <- read.csv( paste0(ihme.dir.rel, '/',
                                        ihme.files[grepl("ADM0_Y2019M10D16.CSV", ihme.files)]),
                                 header = T)
ihme.ests[['adm1']] <- read.csv( paste0(ihme.dir.rel, '/',
                                        ihme.files[grepl("ADM1_Y2019M10D16.CSV", ihme.files)]),
                                 header = T)

ihme.ests[['adm2']] <- read.csv( paste0(ihme.dir.rel,'/',
                                        ihme.files[grepl("ADM2_Y2019M10D16.CSV", ihme.files)]),
                                 header = T)

ihme.ests <- lapply(ihme.ests, function(x){
  if(!(country %in% x$ADM0_NAME)){
    message('\n Country name not found in one of the IHME files.\n')
  }
  if(country != "Cote dIvoire"){
    x[x$ADM0_NAME == country,]
  }else{
    x[x$ADM0_NAME %in% c(levels(ihme.ests[[1]]$ADM0_NAME)[20],
                         levels(ihme.ests[[2]]$ADM0_NAME)[6],
                         levels(ihme.ests[[3]]$ADM0_NAME)[6]),]
  }
})

doAdmin2 <- TRUE
if(country == "Malawi"){
  ihme.ests$adm1 <- ihme.ests$adm2
  ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
  doAdmin2 <- FALSE
}

source(paste0(hand.dir.rel,
              '/IHMEHand_', country, '.R'))


#### Aggregate surveys ####

data.natl <- aggregateSurvey(direct.natl)
data.natl.yearly <- aggregateSurvey(direct.natl.yearly)
data.admin1 <- aggregateSurvey(direct.admin1)
data.admin2 <- aggregateSurvey(direct.admin2)

#### National Model ####

proj.per <- paste(end.year+1, end.year+5, sep = "-")
fit.natl <- fitINLA(data.natl, geo = NULL, Amat = NULL,
                    year_label = c(periods, proj.per),
                    year_range = c(beg.year, end.year + 5), is.yearly = F)
res.natl <- getSmoothed(fit.natl, year_range = c(beg.year, end.year+5),
                        year_label = c(periods, proj.per))
res.natl$years.num <- seq(beg.year+2, end.year+5, 5)
res.natl$region.gadm <- country
head(res.natl)
tail(res.natl)
# file.out <- paste0(country, "_res_natl_SmoothedDirect.rda")
# save(res.natl, file = paste0(folder.name, '/', file.out))

fit.natl.yearly <- fitINLA(data.natl.yearly, geo = NULL, Amat = NULL,
                           year_label = as.character(beg.year:(end.year + 5)),
                           year_range = c(beg.year, end.year + 5), is.yearly = F)
res.natl.yearly <- getSmoothed(fit.natl.yearly, year_range = c(beg.year, end.year + 5),
                               year_label = as.character(beg.year:(end.year + 5)))
res.natl.yearly$years.num <- beg.year:(end.year + 5)
res.natl.yearly$region.gadm <- country
head(res.natl.yearly)
tail(res.natl.yearly)
# file.out <- paste0(country, "_res_natl_yearly_SmoothedDirect.rda")
# save(res.natl.yearly, file = paste0(folder.name, '/', file.out))


#### Admin1 Model ####

# SUMMER::fitINLA
# fit.admin1 <- fitINLA(data.admin1, geo = poly.adm1, Amat = admin1.mat,
#                       year_label = c(periods, "2020-2024"),
#                       year_range = c(1990, 2024), is.yearly = F,
#                       type.st = 4, rw = 2, ar = 0)

# customized fitINLA
fit.admin1 <- fitINLA(data.admin1, geo = poly.adm1, Amat = admin1.mat,
                      year_label = c(periods, "2020-2024"),
                      year_range = c(1990, 2024), is.yearly = F,
                      type.st = 4, rw = 2, ar = 0, 
                      working.directory = "/Users/mirandafix/Desktop/testing")

res.admin1 <- getSmoothed(fit.admin1, Amat = admin1.mat,
                          year_range = c(1990, 2024),
                          year_label = c(periods, "2020-2024"))
res.admin1$years.num <- seq(beg.year+2, end.year+5, 5)[match(res.admin1$years, c(periods, "2020-2024"))]
res.admin1$region.gadm <- admin1.names$GADM[match(res.admin1$region, admin1.names$Internal)]
head(res.admin1)
tail(res.admin1)

# modify file.out as needed
# file.out <- paste0(country, "_res_admin1_SmoothedDirect_rw2_u001.rda")
# save(res.admin1, file = paste0(folder.name, '/', file.out))

#### Spaghetti Plot ####

# modify plot name as needed
pdf(paste0(folder.name,"/Plots/SmoothedDirect/", country,
           '_admin1_SmoothedDirect_rw2_u100.pdf'))
tmp.res <- res.admin1
tmp.res$region <- tmp.res$region.gadm
plot(tmp.res, is.yearly = F, is.subnational = T) 
dev.off()

#### Admin2 Model ####

# SUMMER::fitINLA
fit.admin2 <- fitINLA(data.admin2, geo = poly.adm2, Amat = admin2.mat,
                      year_label = c(periods, "2020-2024"),
                      year_range = c(1990, 2024), is.yearly = F,
                      type.st = 4, rw = 2, ar = 0)

# customized fitINLA
# fit.admin1 <- fitINLA(data.admin1, geo = poly.adm1, Amat = admin1.mat,
#                       year_label = c(periods, "2020-2024"),
#                       year_range = c(1990, 2024), is.yearly = F,
#                       type.st = 4, rw = 2, ar = 0, my.interaction.u = 0.01)

res.admin1 <- getSmoothed(fit.admin1, Amat = admin1.mat,
                          year_range = c(1990, 2024),
                          year_label = c(periods, "2020-2024"))
res.admin1$years.num <- seq(beg.year+2, end.year+5, 5)[match(res.admin1$years, c(periods, "2020-2024"))]
res.admin1$region.gadm <- admin1.names$GADM[match(res.admin1$region, admin1.names$Internal)]
head(res.admin1)
tail(res.admin1)

# modify file.out as needed
file.out <- paste0(country, "_res_admin1_SmoothedDirect_rw2_u001.rda")
save(res.admin1, file = paste0(folder.name, '/', file.out))

#### Spaghetti Plot ####

# modify plot name as needed
pdf(paste0(folder.name,"/Plots/SmoothedDirect/", country,
           '_admin1_SmoothedDirect_rw2_u001.pdf'))
tmp.res <- res.admin1
tmp.res$region <- tmp.res$region.gadm
plot(tmp.res, is.yearly = F, is.subnational = T) 
dev.off()