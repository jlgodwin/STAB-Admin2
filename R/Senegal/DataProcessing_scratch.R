library(rgdal)
dhs2017bnd <- readOGR(dsn = '/Users/mirandafix/Desktop/sdr_subnational_boundaries_2020-09-30/shps', 
                      layer = 'sdr_subnational_boundaries')
dhs2017bnd@data$DHSREGEN


# do something like (this was if only have numbers)
# data.tmp$admin1.name <- admin1.names$GADM[data.tmp$v024]

#-------------------------------------------------------------#

svy.idx <- 1
gps_ind[svy.idx]
dat.test <- getBirths(filepath = paste0(folder.name,'/dhsStata/',filenames_all[svy.idx]),
                     surveyyear = surveys_all[svy.idx],
                     year.cut = seq(beg.year, end.year + 1, 1),
                     strata = c("v024", "v025"), compact = T)
dat.test <- dat.test[ ,c("v001", "v024", "time", "total",
                       "age", "v005", "v025", "strata", "died")]

dat.test$LONGNUM <- dat.test$LATNUM <- NA
dat.test$admin2 <- NA
dat.test$admin2.char <- NA
dat.test$admin2.name <- NA

v024_reorder <- factor(dat.test$v024, levels=sort(levels(dat.test$v024)))
dat.test$admin1 <- as.numeric(v024_reorder)
dat.test$admin1.char <- paste0('admin1_',dat.test$admin1)
dat.test$admin1.name <- admin1.names$GADM[dat.test$admin1]


