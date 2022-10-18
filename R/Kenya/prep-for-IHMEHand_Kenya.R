
country <- "Kenya"

data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
ihme.dir.rel <- '../../Analysis/R'

setwd(data.dir)

folder.name <- country

load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat_Names.rda'))

#### Load IHME data ####
ihme.ests <- list()
ihme.ests[['adm0']] <- read.csv(paste0(ihme.dir.rel,'/',
              'IHME_LMICS_U5M_2000_2017_Q_UNDER5_ADM0_Y2019M10D16.CSV'),
                                 header = T)
ihme.ests[['adm1']] <- read.csv( paste0(ihme.dir.rel,'/',
              'IHME_LMICS_U5M_2000_2017_Q_UNDER5_ADM1_Y2019M10D16.CSV'),
                                 header = T)
ihme.ests[['adm2']] <- read.csv( paste0(ihme.dir.rel,'/',
              'IHME_LMICS_U5M_2000_2017_Q_UNDER5_ADM2_Y2019M10D16.CSV'),
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


if(sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME)) != 0){
  message("IHME Admin 1 names do not match GADM Admin 1 names.\n")
}

if(sum(!(admin1.names$GADM %in% ihme.ests$adm2$ADM2_NAME)) == 0){
  ihme.ests$adm1 <- ihme.ests$adm2
  ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
  doAdmin2 <- FALSE
  message("IHME Admin 2 names match GADM Admin 1 names.\n Changing Admin 1 to Admin 2.\n")
}else if(sum(!(admin1.names$GADM %in% ihme.ests$adm2$ADM2_NAME)) != 0){
  message("IHME Admin 2 names don't match GADM Admin 1 names either.\n")
}

#### Load polygon files ####

library(rgdal)
gadm.abbrev <- "KEN"

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

#### Looking at duplicates ####

summary(poly.adm2@data) # NAME_2
idx.1 <- which(poly.adm2@data$NAME_2 == "Igembe South")
idx.2 <- which(poly.adm2@data$NAME_2 == "Lugari")
mycol <- rep("#FFFFFF",dim(admin2.names)[1])
mycol[idx.1] <- "#FF0000"
mycol[idx.2] <- "#0000FF"

pdf(paste0(folder.name,"/Plots/", country, '_admin2_', 
           'duplicateGADM.pdf'))
par(mar=c(1,1,1.5,1))
plot(poly.adm2, col=mycol, main=country)
legend("topright",c("Igembe South","Lugari"),
       col=c("#FF0000","#0000FF"),pch=c(15,15))
dev.off()

poly.adm2@data[idx.1,]
poly.sub1 <- poly.adm2[poly.adm2@data$NAME_1=="Meru" | poly.adm2@data$NAME_1=="Tharaka-Nithi", ]
col.sub1 <- rep("#FFFFFF",dim(poly.sub1@data)[1])
col.sub1[which(poly.sub1@data$NAME_2 == "Igembe South")] <- "#FF0000"

pdf(paste0(folder.name,"/Plots/", country, '_admin2_', 
           'duplicateGADM_sub1.pdf'))
par(mar=c(1,1,1.5,1))
plot(poly.sub1, col=col.sub1)
legend("topright",c("Igembe South"),
       col=c("#FF0000"),pch=c(15))
dev.off()

poly.adm2@data[idx.2,]
poly.sub2 <- poly.adm2[poly.adm2@data$NAME_1=="Bungoma" | poly.adm2@data$NAME_1=="Kakamega", ]
col.sub2 <- rep("#FFFFFF",dim(poly.sub2@data)[1])
col.sub2[which(poly.sub2@data$NAME_2 == "Lugari")] <- "#0000FF"

pdf(paste0(folder.name,"/Plots/", country, '_admin2_', 
           'duplicateGADM_sub2.pdf'))
par(mar=c(1,1,1.5,1))
plot(poly.sub2, col=col.sub2)
legend("topright",c("Lugari"),
       col=c("#0000FF"),pch=c(15))
dev.off()
