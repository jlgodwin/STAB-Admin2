#  DataProcessing.R
#  author: Jessica Godwin
#  
#  sources: LoadCommandCenter.R
#  reads:   SurveyNum.csv
#           SurveyList_BR_GE.csv


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

source('../../Analysis/R/LoadCommandCenter.R')
#source('../../Analysis/R/FilenameLookup.R')

help(package = "SUMMER", help_type = "html")
utils::browseVignettes(package = "SUMMER")

#### Parameters ####

country <- "South Africa"
beg.year <- 1990
end.year <- 2019

#CountryList <- gs_read(sheet_key, ws = "CountryList")
CountryList <- sheets_read(sheet_key, sheet = "CountryList")
folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
n.survey <- CountryList$nSurvey[CountryList$Country == country]

SurveyNum <- read.csv('../../Analysis/R/SurveyNum.csv')
SurveyIds <- SurveyNum$SurveyId[SurveyNum$CountryName == country]
SurveyIds <- as.character(SurveyIds[grepl("DHS", SurveyIds)])
SurveyMeta <- read.csv('../../Analysis/R/SurveyList_BR_GE.csv')

#### Get Births recodes filenames #### 

SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
#SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country]
n.survey <- length(surveys)
message(cat(country, " has ", length(surveys), "surveys in years ", surveys,".\n"))

births.files <- SvyIds.idx <- rep(NA, n.survey)

for(i in 1:n.survey){
  SvyIds.idx[i] <- grep(surveys[i], SurveyIds)
  br.row.idx <- which(SurveyMeta$SurveyId == SurveyIds[SvyIds.idx[i]] &
                        SurveyMeta$FileType == "Births Recode" &
                        SurveyMeta$SurveyType == "DHS" &
                        SurveyMeta$FileFormat == "Stata dataset (.dta)")
  
  br.name.zip <- as.character(SurveyMeta$FileName[br.row.idx])
  br.name.root <- gsub("DT.ZIP", "", ignore.case = T, br.name.zip)
  br.name.root <- toupper(br.name.root)
  births.files[i] <- paste0(br.name.root, "DT/", br.name.root, "FL.dta")
  
  # survey.row.idx <- which(SurveyInfo$Country == country &
  #                           SurveyInfo$`Survey Year` == surveys[i])
  # anchor.col <- LETTERS[which(names(SurveyInfo) == "BirthsFile")]
  # gs_edit_cells(ss = sheet_key, ws = "SurveyInfo",
  #               input = births.files[i],
  #               anchor = paste0(anchor.col, survey.row.idx + 1))
  # 
  
  
} 
names(births.files) <- surveys
births.files

#SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
filenames <- SurveyInfo$BirthsFile[SurveyInfo$Country == country
                                   & SurveyInfo$`GPS?`== "Y"]

filenames #Check to see they updated in the spreadsheet

surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]
n.survey <- length(surveys)
gps.files <- SvyIds.idx <- rep(NA, n.survey)

for(i in 1:n.survey){
  SvyIds.idx[i] <- grep(surveys[i], SurveyIds)  
  
  survey.row.idx <- which(SurveyInfo$Country == country &
                            SurveyInfo$`Survey Year` == surveys[i])
  gps.row.idx <- which(SurveyMeta$SurveyId == SurveyIds[SvyIds.idx[i]] &
                         SurveyMeta$FileType == "Geographic Data" &
                         SurveyMeta$SurveyType == "DHS")
  
  gps.name.zip <- as.character(SurveyMeta$FileName[gps.row.idx])
  gps.name.root <- gsub(".ZIP", "", ignore.case = T, gps.name.zip)
  gps.files[i] <- toupper(gps.name.root)
  # anchor.col <- LETTERS[which(names(SurveyInfo) == "GPSFile")]
  # gs_edit_cells(ss = sheet_key, ws = "SurveyInfo",
  #               input = gps.files[i],
  #               anchor = paste0(anchor.col, survey.row.idx + 1))
}

names(gps.files) <- surveys
gps.files

#### Get GPS filenames ####

surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]
message(cat(country, " has ", length(surveys), "surveys with GPS in years ", surveys,".\n
            If any of these are NA check to see that you have filled out
            whether each survey has a GPS dataset with a 'Y' or 'N'."))


# ## Fix the problem surveys
# ## Must be checked and done by hand. See DataProcessing.pdf
# list.files(path = paste0(folder.name, "/dhsFlat"),
#            pattern = "GE")
# idx <- match(2011, surveys)
# survey.row.idx <- which(SurveyInfo$`Survey Year` == surveys[idx] &
#                           SurveyInfo$Country == country)
# anchor.col <- LETTERS[which(names(SurveyInfo) == "GPSFile")]
# gs_edit_cells(ss = sheet_key, ws = "SurveyInfo",
#               input = list.files(path = paste0(folder.name, "/dhsFlat"),
#                                  pattern = "GE")[3],
#               anchor = paste0(anchor.col, survey.row.idx+ 1))

#SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
points.files <- points.layer <- 
  SurveyInfo$GPSFile[SurveyInfo$Country == country &
                       SurveyInfo$`GPS?` == "Y"]
points.files #Check the points files

#### Load polygon files ####

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

#### Check if Polygon files contain water ####
summary(poly.adm1$TYPE_1)
summary(poly.adm2$TYPE_2)

message("This might be different for everyone!")
# poly.adm2 <- poly.adm2[poly.adm2@data$TYPE_2 != "Water body",]
# summary(poly.adm2$TYPE_2)
# writeOGR(poly.adm2, dsn = poly.path,
#          layer = as.character(poly.layer.adm2), driver = "ESRI Shapefile",
#          overwrite_layer = T)

#### Create adjacency matrices ####

#if(!file.exists(paste0(poly.path,'/', country, '_Amat.rda'))){
adm1.ind <- exists("poly.adm1")
adm2.ind <- exists("poly.adm2")
if(adm1.ind){
  admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
  admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
  colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
  admin1.names <- data.frame(GADM = poly.adm1@data$NAME_1,
                             Internal = rownames(admin1.mat))
}else{
  message("There is no Admin1 polygon file.")
}
if(adm2.ind){
  admin2.mat <- poly2nb(SpatialPolygons(poly.adm2@polygons))
  admin2.mat <- nb2mat(admin2.mat, zero.policy = TRUE)
  colnames(admin2.mat) <- rownames(admin2.mat) <- paste0("admin2_", 1:dim(admin2.mat)[1])
  admin2.names <- data.frame(GADM = poly.adm2@data$NAME_2,
                             Internal = rownames(admin2.mat))
}else{
  message("There is no Admin2 polygon file.")
}

if(adm1.ind & adm2.ind){
  save(admin1.mat, admin2.mat, file = paste0(poly.path,'/', country, '_Amat.rda'))
  save(admin1.names, admin2.names, file = paste0(poly.path, '/', country, '_Amat_Names.rda'))
}else if(adm1.ind){
  save(admin1.mat, file = paste0(poly.path,'/', country, '_Amat.rda'))
  save(admin1.names, file = paste0(poly.path, '/', country, '_Amat_Names.rda'))
}else{
  message("No polygon files. No adjacency matrix created.")
}
#}else{
#  message(paste0("Adjacency matrices already exist for ", country))
#}

#### Polygon Plots ####
pdf(paste0(folder.name, '/Plots/ShapeCheck/', country, '_adm1_neighb.pdf'))
cent <- getSpPPolygonsLabptSlots(poly.adm1)
cols <- rainbow(10)
plot(poly.adm1, col = cols, border = F, axes = F,)
for(i in 1:dim(cent)[1]){
  neighbs <- which(admin1.mat[i,] != 0)
  if(length(neighbs) != 0){
    for(j in 1:length(neighbs)){
      ends <- cent[neighbs,]
      segments(x0 = cent[i, 1], y0 = cent[i, 2],
               x1 = cent[neighbs[j], 1], y1 = cent[neighbs[j], 2], col = 'black')
    }
  }
}
dev.off()

if(exists("admin2.mat")){
  pdf(paste0(folder.name, '/Plots/ShapeCheck/', country, '_adm2_neighb.pdf'))
  cent <- getSpPPolygonsLabptSlots(poly.adm2)
  cols <- rainbow(10)
  plot(poly.adm2, col = cols, border = F, axes = F,)
  for(i in 1:dim(cent)[1]){
    neighbs <- which(admin2.mat[i,] != 0)
    if(length(neighbs) != 0){
      for(j in 1:length(neighbs)){
        ends <- cent[neighbs,]
        segments(x0 = cent[i, 1], y0 = cent[i, 2],
                 x1 = cent[neighbs[j], 1], y1 = cent[neighbs[j], 2], col = 'black')
      }
    }
  }
  dev.off()
}

#### Load Data ####


births.list <- list()

svy.idx <- 0
for(survey in surveys){
  svy.idx <- svy.idx + 1
  dat.tmp <- getBirths(filepath = paste0(folder.name,'/dhsStata/',filenames[svy.idx]),
                       surveyyear = survey,
                       year.cut = seq(beg.year, end.year + 1, 1),
                       strata = c("v024", "v025"), compact = T)
  dat.tmp <- dat.tmp[ ,c("v001", "v024", "time", "total",
                         "age", "v005", "v025", "strata", "died")]
  
  # 
  # dat.tmp <- dat.tmp %>% 
  #   group_by(v001, age, time) %>%
  #   summarise(total = n(),
  #             Y = sum(died),
  #             strata = unique(strata),
  #             v005 = unique(v005),
  #             v025 = unique(v025),
  #             v024 = unique(v024))
  
  cat("\n Made cluster x age x time frame for survey", svy.idx,"\n")
  
  #### Assign Lat & Long ####
  
  points.path <- paste0(folder.name, "/dhsFlat/", points.files[[svy.idx]])
  points <- readOGR(dsn = path.expand(points.path),
                    layer = as.character(points.layer[[svy.idx]]))
  
  
  wrong.points <- which(points@data$LATNUM == 0.0 & points@data$LONGNUM == 0.0)
  plot(points[-wrong.points,])
  
  dat.tmp <- dat.tmp[!(dat.tmp$v001 %in% points@data$DHSCLUST[wrong.points]),]
  points@data$DHSCLUST[wrong.points] %in% unique(dat.tmp$v001)
  
  dat.tmp$LONGNUM <- dat.tmp$LATNUM <- NA
  for(i in 1:dim(points)[1]){
    dat.tmp$LATNUM[dat.tmp$v001 == points@data$DHSCLUST[i]] <- points@data$LATNUM[i]
    dat.tmp$LONGNUM[dat.tmp$v001 == points@data$DHSCLUST[i]] <- points@data$LONGNUM[i]
  }
  
  miss <- which(dat.tmp$LATNUM == 0 & dat.tmp$LONGNUM == 0)
  if(length(miss != 0)){
    dat.tmp <- dat.tmp[-miss,]
  }
  
  cat("\n Assigned LAT & LONG for survey ", svy.idx)
  
  #### Assign to Admin 2 ####
  adm1.ind <- exists("poly.adm1")
  adm2.ind <- exists("poly.adm2")
  
  points.frame <- as.data.frame(dat.tmp[,c("LONGNUM", "LATNUM")])
  points.frame <- SpatialPoints(points.frame)
  if(adm2.ind){
    poly.over.adm2 <- SpatialPolygons(poly.adm2@polygons)
    proj4string(points.frame) <- proj4string(poly.over.adm2) <- 
      proj4string(poly.adm2)  <- 
      proj4string(poly.adm1)  
    admin2.key <- over(points.frame, poly.over.adm2)
    miss.frame.adm2 <- unique(points.frame@coords[which(is.na(admin2.key)),])
    
    if(dim(miss.frame.adm2)[1] != 0){
      miss.poly.adm2 <- dist2Line( miss.frame.adm2, poly.over.adm2)
      
      for(i in 1:dim(miss.poly.adm2)[1]){
        long.ids <- which(points.frame@coords[,c("LONGNUM")] %in% miss.frame.adm2[i,1])
        lat.ids <- which(points.frame@coords[,c("LATNUM")] %in% miss.frame.adm2[i,2])
        ids <- intersect(long.ids, lat.ids)
        #ids[(length(ids)/2 + 1):length(ids)] <- ids[(length(ids)/2 + 1):length(ids)] - dim(points.frame@coords)[1]
        #ids <- unique(ids)
        admin2.key[ids] <- rep(miss.poly.adm2[i, 'ID'], length(ids))
      }
    }
    
    dat.tmp$admin2 <- admin2.key
    dat.tmp$admin2.char <- paste0("admin2_", admin2.key)
    dat.tmp$admin2.name <- as.character(poly.adm2@data$NAME_2)[admin2.key]
  }else{
    dat.tmp$admin2 <- dat.tmp$admin2.name <- NA
    message("There is no Admin2 polygon to assign points to.")
  }
  
  if(adm1.ind){
    poly.over.adm1 <- SpatialPolygons(poly.adm1@polygons)
    proj4string(points.frame) <- proj4string(poly.over.adm1) <- 
      proj4string(poly.adm1) 
    admin1.key <- over(points.frame, poly.over.adm1)
    miss.frame.adm1 <- unique(points.frame@coords[which(is.na(admin1.key)),])
    
    if(dim(miss.frame.adm1)[1] != 0){
      miss.poly.adm1 <- dist2Line( miss.frame.adm1, poly.over.adm1)
      
      for(i in 1:dim(miss.poly.adm1)[1]){
        long.ids <- which(points.frame@coords[,c("LONGNUM")] %in% miss.frame.adm1[i,1])
        lat.ids <- which(points.frame@coords[,c("LATNUM")] %in% miss.frame.adm1[i,2])
        ids <- intersect(long.ids, lat.ids)
        
        # ids[(length(ids)/2 + 1):length(ids)] <- ids[(length(ids)/2 + 1):length(ids)] - dim(points.frame@coords)[1]
        # ids <- unique(ids)
        admin1.key[ids] <- rep(miss.poly.adm1[i, 'ID'], length(ids))
      }
    }
    
    dat.tmp$admin1 <- admin1.key
    dat.tmp$admin1.char <- paste0("admin1_", admin1.key)
    dat.tmp$admin1.name <- as.character(poly.adm1@data$NAME_1)[admin1.key]
  }else{
    dat.tmp$admin2 <- dat.tmp$admin2.name <- NA
    message("There is no Admin1 polygon to assign points to.")
  }  
  
  
  
  
  #### Plots ####
  if(!dir.exists(paste0(folder.name,'/Plots/'))){
    dir.create(paste0(folder.name,'/Plots/'))
  }
  
  if(!dir.exists(paste0(folder.name,'/Plots/ShapeCheck'))){
    dir.create(paste0(folder.name,'/Plots/ShapeCheck'))
  }
  
  png(paste0(folder.name,'/Plots/ShapeCheck/Points_', survey, '_GE.png'))
  plot(points, pch = 19, main = country, col = 'blue', axes = F)
  plot(poly.adm0, add  = T)
  dev.off()
  
  cat("\n Made Admin1 and Admin2 assignments for survey ", svy.idx)
  png(paste0(folder.name,'/Plots/ShapeCheck/Points_', survey, '_BR.png'))
  plot(unique(dat.tmp[,c('LONGNUM','LATNUM')]),
       pch = 19, main = country, col = 'blue',
       xlab = "", ylab = "", axes = F)
  plot(poly.adm0, add  = T)
  dev.off()
  
  births.list[[svy.idx]] <- dat.tmp
}

names(births.list) <- surveys

births.list <- lapply(births.list, function(x){
  tmp <- x[,c("v001", "age", "time", "total", "died", "v005", 
              "strata", "v025", "LONGNUM", "LATNUM",
              "admin1", "admin2", "admin1.char", "admin2.char", "admin1.name", "admin2.name")]
  colnames(tmp) <- c("cluster", "age", "years", "total",
                     "Y", "v005", "strata", "urban", "LONGNUM", "LATNUM",
                     "admin1", "admin2", "admin1.char", "admin2.char", "admin1.name", "admin2.name")
  return(tmp)
})


#### Check each list element ####

message('Check each list element for consistencies/errors.')
head(births.list[[1]])]

#### Check survey year is assigned to correct file name ####
message("Check survey year is assigned to correct file name.\n")

sort(unique(births.list[[1]]$years))
sum(as.numeric(unique(as.character(births.list[[1]]$years))) > 
      as.numeric(names(births.list)[1]))

#### Combine survey frames ####

svy.idx <- 0
for(survey in surveys){
  svy.idx <- svy.idx + 1
  births.list[[svy.idx]]$survey <- survey
  births.list[[svy.idx]]$survey.id <- svy.idx
  
  if(svy.idx != 1){
    mod.dat <- rbind.data.frame(mod.dat, births.list[[svy.idx]])
  }else{
    mod.dat <- births.list[[svy.idx]]
  }
}


#### Check frame ####

head(mod.dat)

message("Check time and also amount of data in each year.
        Do we need to remove most recent year because of no data?\n")
aggregate(cbind(Y, total) ~ years, data = mod.dat,
          FUN = sum)
summary(mod.dat[mod.dat$years == max(as.numeric(as.character(mod.dat$years))), ])
message('If there are no deaths and 
        an extremely small amount of person months, remove the most recent year.\n')
#mod.dat <- mod.dat[mod.dat$years != max(as.numeric(as.character(mod.dat$years))),]

#### Save frame ####

save(mod.dat, file = paste0(folder.name, '/', country, '_cluster_dat.rda'))
