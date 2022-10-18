
#  getHIVAdjReg_Cote dIvoire.R
#  author: Jessica Godwin
#  


rm(list = ls())
setwd('~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/')
devtools::install_github("bryandmartin/SUMMER",
                         build_vignettes = F, force = T)

#### Libraries ####
library(SUMMER)
library(dplyr)
library(tidyr)
library(rgdal)
library(spdep)
library(geosphere)
library(rgeos)

help(package = "SUMMER", help_type = "html")
utils::browseVignettes(package = "SUMMER")

#### Parameters ####

country <- "Cote dIvoire"
folder.name <- "CotedIvoire"
beg.year <- 1990
end.year <- 2019

load(paste0(folder.name, '/', country, 
            '_cluster_dat.rda'))
load(paste0(folder.name, '/shapeFiles_gadm/', country,
            '_Amat_Names.rda'))


dhs.poly <- readOGR(dsn = 'CotedIvoire/shapeFiles_CI2012DHS/shps',
                    layer = 'sdr_subnational_boundaries')
dhs.poly@data$REGNAME
levels(dhs.poly@data$DHSREGFR)[9] <- "Sud"
levels(dhs.poly@data$DHSREGFR)[11] <- "Abidjan"

points.frame <- SpatialPoints(unique(mod.dat[,c("LONGNUM", "LATNUM")]))

poly.over.dhs<- SpatialPolygons(dhs.poly@polygons)
proj4string(points.frame) <- proj4string(poly.over.dhs)
dhs.key <- over(points.frame, poly.over.dhs)
miss.frame.dhs <- unique(points.frame@coords[which(is.na(dhs.key)),])

if(dim(miss.frame.dhs)[1] != 0){
  miss.poly.dhs <- dist2Line(miss.frame.dhs, poly.over.dhs)
  
  for(i in 1:dim(miss.poly.dhs)[1]){
    long.ids <- which(points.frame@coords[,c("LONGNUM")] %in% miss.frame.dhs[i,1])
    lat.ids <- which(points.frame@coords[,c("LATNUM")] %in% miss.frame.dhs[i,2])
    ids <- intersect(long.ids, lat.ids)
    
    # ids[(length(ids)/2 + 1):length(ids)] <- ids[(length(ids)/2 + 1):length(ids)] - dim(points.frame@coords)[1]
    # ids <- unique(ids)
    dhs.key[ids] <- rep(miss.poly.dhs[i, 'ID'], length(ids))
  }
}

HIVnames.key <- data.frame(LONGNUM = points.frame$LONGNUM,
                           LATNUM = points.frame$LATNUM,
                           dhs.name = dhs.poly@data$DHSREGFR[dhs.key],
                           admin1.Internal = NA,
                           admin2.Internal = NA)

for(i in 1:length(dhs.key)){
  HIVnames.key$admin1.Internal[i] <- unique(mod.dat$admin1.char[mod.dat$LONGNUM == 
                                              points.frame$LONGNUM[i]])
  HIVnames.key$admin2.Internal[i] <- unique(mod.dat$admin2.char[mod.dat$LONGNUM == 
                                              points.frame$LONGNUM[i]])
}


save(HIVnames.key, file = paste0(folder.name, '/',
                                 country, "_HIVnames.key.rda"))

