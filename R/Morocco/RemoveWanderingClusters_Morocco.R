#  RemoveWanderingClusters.R
#  author: Jessica Godwin
#  
#  sources: LoadCommandCenter.R
#           


rm(list = ls())
setwd('~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/')
devtools::install_github("bryandmartin/SUMMER",
                         build_vignettes = F, force = T)


## Libraries ####
library(SUMMER)
help(package = "SUMMER", help_type = "html")
#utils::browseVignettes(package = "SUMMER")
library(classInt)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(rgdal)
library(scales)
library(survey)
library(geosphere)

source('../../Analysis/R/LoadCommandCenter.R')


## Parameters ####

country <- "Mali"
survey.to.fix <- 2012
beg.year <- 1990
end.year <- 2019

CountryList <- gs_read(sheet_key, ws = "CountryList")
folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
n.survey <- CountryList$nSurvey[CountryList$Country == country]

## Get Survey years #### 

SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]
#### Get GPS filenames ####
message(cat(country, " has ", length(surveys), "surveys with GPS in years ", surveys,".\n
            If any of these are NA check to see that you have filled out
            whether each survey has a GPS dataset with a 'Y' or 'N'."))


SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
points.file <- points.layer <- 
  SurveyInfo$GPSFile[SurveyInfo$Country == country &
                       SurveyInfo$`GPS?` == "Y" &
                       SurveyInfo$`Survey Year` == survey.to.fix]
points.file #Check the points files


## Load polygon files ####

poly.file <- "/shapeFiles_gadm"
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
poly.adm2 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm2))
proj4string(poly.adm0) <- proj4string(poly.adm1) <- proj4string(poly.adm2)
load(paste0(folder.name,'/shapeFiles_gadm/', country, '_Amat.rda'))
load(paste0(folder.name, '/shapeFiles_gadm/', country, '_Amat_Names.rda'))

#### Load Data ####
load(paste0(folder.name, '/', country, '_cluster_dat.rda'))

points.path <- paste0(folder.name, "/dhsFlat/", points.file)
points <- readOGR(dsn = path.expand(points.path),
                  layer = as.character(points.layer))

plot(poly.adm0)
poly.adm0@bbox
points@bbox

poly.over <- SpatialPolygons(poly.adm0@polygons)
proj4string(poly.over) <- proj4string(points)
over <- over(points, poly.over)
missing <- points@coords[which(is.na(over)),]
dists <- dist2Line(missing,poly.over)
dists

## Distances are in meteres 
## points 1-4 are clearly the outlying clusters.
## Setting cutoff for Morocco to 400m
cutoff <- 200
remove.id <- which(dists[,'distance'] > cutoff )

remove.clus.idx <- which(is.na(over))[remove.id]
points@data[remove.clus.idx,]
clus.no <- points@data$DHSCLUST[remove.clus.idx]

## Check by hand. Is Admin1 assignment 
## same as points@data$ADM1FIPSNA[points@data$DHSCLUST == clus]

for(clus in clus.no){
message("Cluster no. ", clus)
message("Assigned: ",unique(mod.dat$admin1.name[mod.dat$survey == survey.to.fix &
                             mod.dat$cluster == clus]))
message("points: ", points@data$ADM1FIPSNA[points@data$DHSCLUST == clus])
cat("\n", sum(mod.dat$Y[mod.dat$cluster == clus]), " deaths and ",
    sum(mod.dat$total[mod.dat$cluster == clus]), " agemonths.\n")
}


## If you want to remove
remove_name<-paste("RemoveWanderingClusters", country, sep="_")
wandering.folder.name<-paste(folder.name, remove_name, sep="/")

mod.dat <- mod.dat[!(mod.dat$cluster %in% clus.no),]

save(mod.dat, file = paste0(folder.name, '/', "RemoveWanderingClusters_", country, '.rda'))


