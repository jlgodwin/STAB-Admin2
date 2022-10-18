
country <- "Senegal"

data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
ihme.dir.rel <- '../../Analysis/R'

setwd(data.dir)

folder.name <- country

load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat_Names.rda'))

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
    x[x$ADM0_NAME == country,]
})


if(sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME)) != 0){
  message("IHME Admin 1 names do not match GADM Admin 1 names.\n")
}
