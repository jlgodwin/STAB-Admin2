# Austin Schumacher
# 5/16/2020
# Create files with Azad Jammu and Kashmir (Azad Kashmir) and Gilgit Ballistan (Northern Areas) deleted

rm(list = ls())

#### Libraries ####
# devtools::install_github("bryandmartin/SUMMER",
#                        build_vignettes = F, force = T)
library(SUMMER)
#help(package = "SUMMER", help_type = "html")
#utils::browseVignettes(package = "SUMMER")
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
library(parallel)
library(cartography)
library(rgeos)

#### Parameters ####
country <- "Pakistan"
cluster <- FALSE
message("If have the same subfolder structure as 
        AfricaAdmin2Estimates/Data/countryDataFolders/. Do nothing!\n
        Otherwise, edit the following paths as needed.\n")

# data.dir <- './toCluster'
# code.dir.rel <- '../../Analysis/R'
# igme.dir.rel <- '..'
# ihme.dir.rel <- '..'
# shapes.sub.dir <- '/shapeFiles_gadm'
# hiv.dir.rel <- '..'

data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '../../Analysis/R'
ihme.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '../HIV/'

setwd(data.dir)

if(!exists("sheet_key", envir = .GlobalEnv)){
    source(paste0(code.dir.rel,'/LoadCommandCenter.R'))
}
CountryList <- sheets_read(sheet_key, sheet = "CountryList")
#CountryList <- read.csv("CountryList.csv", header = T)

folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]

message('Where is IHMEHand_CountryName.rda?\n')
hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
                       gsub(" ", "", folder.name))
#hand.dir.rel <- paste0(gsub(" ", "", folder.name))


#### More Params ####
disputed_regions <- c("Azad Kashmir", "Northern Areas")

beg.year <- 1990
end.year <- 2020
# time.mod <- "rw2"
# time.mod <- "ar1"
# time.mod <- "rw2_rw1"
time.mod <- "rw2main_randomSlopes_rw1xICAR"
# time.mod <- "rw2main_randomSlopes_ar1xICAR"

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

# load neighbor matrices
load(paste0(folder.name, 
            shapes.sub.dir,
            '/', country, '_Amat.rda'))
load(paste0(folder.name, 
            shapes.sub.dir,
            '/', country, '_Amat_Names.rda'))

#### delete disputed regions from admin2 polygon files and neighbor matrices, resave, and remake maps ####

# delete regions from polygon files
poly.adm1_excluding_disputed <- poly.adm1[!(poly.adm1$NAME_1 %in% disputed_regions),]
## uncomment this if you need to resave
# writeOGR(poly.adm1_excluding_disputed,
#          dsn = poly.path,
#          layer = paste0(poly.layer.adm1, "_excluding_disputed"),
#          driver = "ESRI Shapefile")
poly.adm2_excluding_disputed <- poly.adm2[!(poly.adm2$NAME_2 %in% disputed_regions),]
## uncomment this if you need to resave
# writeOGR(poly.adm2_excluding_disputed,
#          dsn = poly.path,
#          layer = paste0(poly.layer.adm2, "_excluding_disputed"),
#          driver = "ESRI Shapefile")

# delete regions from neighbor matrices
admin1.names_excluding_disputed <- admin1.names[!(admin1.names$GADM %in% disputed_regions),]
admin1.mat_excluding_disputed <- admin1.mat[which(rownames(admin1.mat) %in% 
                                                      as.character(admin1.names_excluding_disputed$Internal)),
                                            which(rownames(admin1.mat) %in% 
                                                      as.character(admin1.names_excluding_disputed$Internal))]

admin2.names_excluding_disputed <- admin2.names[!(admin2.names$GADM %in% disputed_regions),]
admin2.mat_excluding_disputed <- admin2.mat[which(rownames(admin2.mat) %in% 
                                                      as.character(admin2.names_excluding_disputed$Internal)),
                                            which(rownames(admin2.mat) %in% 
                                                      as.character(admin2.names_excluding_disputed$Internal))]

# get internal names for disputed regions
disputed_regions_internal_admin1 <- admin1.names$Internal[admin1.names$GADM %in% disputed_regions]
disputed_regions_internal_admin2 <- admin2.names$Internal[admin2.names$GADM %in% disputed_regions]

# resave maps
pdf(paste0(folder.name, '/Plots/ShapeCheck/', country, '_adm1_neighb_excluding_disputed.pdf'))
cent <- getSpPPolygonsLabptSlots(poly.adm1_excluding_disputed)
cols <- rainbow(10)
plot(poly.adm1_excluding_disputed, col = cols, border = F, axes = F,)
for(i in 1:dim(cent)[1]){
    neighbs <- which(admin1.mat_excluding_disputed[i,] != 0)
    if(length(neighbs) != 0){
        for(j in 1:length(neighbs)){
            ends <- cent[neighbs,]
            segments(x0 = cent[i, 1], y0 = cent[i, 2],
                     x1 = cent[neighbs[j], 1], y1 = cent[neighbs[j], 2], col = 'black')
        }
    }
}
dev.off()

pdf(paste0(folder.name, '/Plots/ShapeCheck/', country, '_adm2_neighb_excluding_disputed.pdf'))
cent <- getSpPPolygonsLabptSlots(poly.adm2_excluding_disputed)
cols <- rainbow(10)
plot(poly.adm2_excluding_disputed, col = cols, border = F, axes = F,)
for(i in 1:dim(cent)[1]){
    neighbs <- which(admin2.mat_excluding_disputed[i,] != 0)
    if(length(neighbs) != 0){
        for(j in 1:length(neighbs)){
            ends <- cent[neighbs,]
            segments(x0 = cent[i, 1], y0 = cent[i, 2],
                     x1 = cent[neighbs[j], 1], y1 = cent[neighbs[j], 2], col = 'black')
        }
    }
}
dev.off()

centroids <- gCentroid(poly.adm1_excluding_disputed, byid = TRUE,
                       id = poly.adm1_excluding_disputed@data$GID_1)
pdf(paste0(folder.name, '/Plots/ShapeCheck/',
           country,'_Admin1Names_excluding_disputed.pdf'))
par(lend=1)
plot(poly.adm1_excluding_disputed,
     xlim = poly.adm1_excluding_disputed@bbox['x',],
     ylim = poly.adm1_excluding_disputed@bbox['y',],
     axes = F)
text(centroids$x, centroids$y,
     labels = poly.adm1_excluding_disputed@data$NAME_1,
     cex = 0.45)
dev.off()

centroids <- gCentroid(poly.adm2_excluding_disputed, byid = TRUE,
                       id = poly.adm2_excluding_disputed@data$GID_2)
pdf(paste0(folder.name, '/Plots/ShapeCheck/',
           country,'_Admin2Names_excluding_disputed.pdf'))
par(lend=1)
plot(poly.adm2_excluding_disputed,
     xlim = poly.adm2_excluding_disputed@bbox['x',],
     ylim = poly.adm2_excluding_disputed@bbox['y',],
     axes = F)
text(centroids$x, centroids$y,
     labels = poly.adm2_excluding_disputed@data$NAME_2,
     cex = 0.45)
dev.off()

#### Get Survey years #### 

#SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
#SurveyInfo <- read.csv("SurveyInfo.csv", header = T)
#surveys <- SurveyInfo$Survey.Year[SurveyInfo$Country == country &
#                                    SurveyInfo$`GPS.` == "Y"]

#SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                        SurveyInfo$`GPS?` == "Y"]
survey.legends <- SurveyInfo$`OfficialSurveyName`[SurveyInfo$Country == country &
                                                      SurveyInfo$`GPS?` == "Y"]

# delete disputed areas from files in External folder
setwd("../../../Admin2ResultsUN/External/Pakistan")

dat <- read.csv("Pakistan_direct_admin1.csv", header = TRUE)
dat <- dat[!(dat$region %in% disputed_regions_internal_admin1),]
write.csv(dat, "excluding_AJK_GB/Pakistan_direct_admin1_excluding_AJK_GB.csv")

dat <- read.csv("Pakistan_direct_admin2.csv", header = TRUE)
dat <- dat[!(dat$region %in% disputed_regions_internal_admin2),]
write.csv(dat, "excluding_AJK_GB/Pakistan_direct_admin2_excluding_AJK_GB.csv")

dat <- read.csv("Pakistan_res_rw2main_randomSlopes_rw1xICAR_admin1Bench.csv", header = TRUE)
dat <- dat[!(dat$region %in% disputed_regions_internal_admin1),]
write.csv(dat, "excluding_AJK_GB/Pakistan_res_rw2main_randomSlopes_rw1xICAR_admin1Bench_excluding_AJK_GB.csv")

dat <- read.csv("Pakistan_res_rw2main_randomSlopes_rw1xICAR_admin2Bench.csv", header = TRUE)
dat <- dat[!(dat$region %in% disputed_regions_internal_admin2),]
write.csv(dat, "excluding_AJK_GB/Pakistan_res_rw2main_randomSlopes_rw1xICAR_admin2Bench_excluding_AJK_GB.csv")

