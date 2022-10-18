#  DirectEstimates.R
#  author: Jessica Godwin
#  
#  sources: LoadCommandCenter.R
#           IHMEHand_CountryName.R


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

source('../../Analysis/R/LoadCommandCenter.R')


## Parameters ####

country <- "Nigeria"
beg.year <- 1992
end.year <- 2013

#CountryList <- gs_read(sheet_key, ws = "CountryList")
CountryList <- sheets_read(sheet_key, sheet = "CountryList")
folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
n.survey <- CountryList$nSurvey[CountryList$Country == country]

## Get Survey years #### 

#SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]


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

levels(poly.adm1$NAME_1)[15] <- "Abuja"

writeOGR(poly.adm1, dsn = poly.path,
         layer = as.character(poly.layer.adm1), driver = "ESRI Shapefile",
         overwrite_layer = T)
