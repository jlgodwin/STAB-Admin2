setwd('~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/Kenya')

library(rgdal)

hiv.shape <- readOGR(dsn = "shapeFiles_KE2014DHS/shps",
                     layer = "sdr_subnational_boundaries2")
hiv.name.key <- hiv.shape@data[,c("DHSREGEN", "OTHREGNA")]
hiv.name.key$DHSREGEN[5] <- gsub(" ", "-", hiv.name.key$DHSREGEN[5]) # make Elgeyo Marakwet, Elgeyo-Marakwet
hiv.name.key$DHSREGEN[42] <- gsub("-", " ", hiv.name.key$DHSREGEN[42]) # make Trans-Nzoia, Trans Nzoia
save(hiv.name.key,
     file = 'Kenya_HIVNameKey.rda')
