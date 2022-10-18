
country <- "Zambia"

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
