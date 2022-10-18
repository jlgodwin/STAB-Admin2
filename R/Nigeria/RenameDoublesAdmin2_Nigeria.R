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

poly.adm2@data$NAME_2 <- as.character(poly.adm2@data$NAME_2)
poly.adm2@data[poly.adm2$NAME_2 == "Bassa",]
poly.adm2@data$NAME_2[472] <- "Bassa, Kogi"
poly.adm2@data$NAME_2[666] <- "Bassa, Plateau"

poly.adm2@data[poly.adm2$NAME_2 %in% c("Nasarawa", "Nassaraw", "Nassarawa Egon"),]

poly.adm2@data[poly.adm2$NAME_2 == "Surulere",]
poly.adm2@data$NAME_2[525] <- "Surulere, Lagos"
poly.adm2@data$NAME_2[664] <- "Surulere, Oyo"

poly.adm2@data[poly.adm2$NAME_2 == "Mainland",]
poly.adm2@data[poly.adm2$NAME_2 == "Ifelodun",]
poly.adm2@data$NAME_2[494] <- "Ifelodun, Kwara"
poly.adm2@data$NAME_2[617] <- "Ifelodun, Osun"

poly.adm2@data[poly.adm2$NAME_2 == "Irepodun",]
poly.adm2@data$NAME_2[498] <- "Irepodun, Kwara"
poly.adm2@data$NAME_2[621] <- "Irepodun, Osun"

poly.adm2@data[poly.adm2$NAME_2 == "Obi",]
poly.adm2@data$NAME_2[131] <- "Makurdi"
poly.adm2@data$NAME_2[132] <- "Obi, Benue"
poly.adm2@data$NAME_2[536] <- "Obi, Nassarawa"


writeOGR(poly.adm2, dsn = poly.path,
         layer = as.character(poly.layer.adm2), driver = "ESRI Shapefile",
         overwrite_layer = T)

