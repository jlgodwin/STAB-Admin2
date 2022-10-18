library(rgdal)
library(spdep)
library(maptools)
library(raster)

SIM.mal <- readOGR( dsn = '~/Dropbox/Godwin-Wakefield/CountryData/Malawi/Shapes/MWDHS2015/shps/',
                    layer = 'sdr_subnational_boundaries2')
GADM.mal <- readOGR(dsn = '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/Malawi/shapeFiles_gadm',
                    layer = 'gadm36_MWI_1')
JP.mal <- readOGR(dsn = '~/Dropbox/Godwin-Wakefield/Admin2Methods/JPShapefiles',
                  layer = 'mwi_admbnda_adm2_nso_20181016')

proj4string(JP.mal) <- proj4string(GADM.mal) <- proj4string(SIM.mal)
plot(SIM.mal, axes = F)
plot(GADM.mal, border = 'blue', add = T)
plot(JP.mal, border = 'red', add = T)

JP.union <- unionSpatialPolygons(SpatialPolygons(JP.mal@polygons),rep(1, dim(JP.mal)[1]))


proj4string(JP.union) <- proj4string(JP.mal)


SIM.mal.clip <- intersect(JP.union, SIM.mal)
plot(SIM.mal)
plot(SIM.mal.clip, border = 'red', add = T)
writeOGR(SIM.mal.clip, dsn = '~/Dropbox/Godwin-Wakefield/CountryData/Malawi/Shapes/MWDHS2015/shps/',
         layer = 'sdr_subnational_boundaries2', driver = "ESRI Shapefile",
         overwrite_layer = T)

GADM.mal.clip <- intersect(JP.union, GADM.mal)
plot(GADM.mal)
plot(GADM.mal.clip, border = 'blue', add = T)

writeOGR(GADM.mal.clip, dsn = '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/Malawi/shapeFiles_gadm',
         layer = 'gadm36_MWI_1', driver = "ESRI Shapefile",
         overwrite_layer = T)

GADM.mal <- readOGR(dsn = '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/Malawi/shapeFiles_gadm',
                    layer = 'gadm36_MWI_0')
GADM.mal.clip <- intersect(JP.union, GADM.mal)
plot(GADM.mal)
plot(GADM.mal.clip, border = 'blue', add = T)

writeOGR(GADM.mal.clip, dsn = '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/Malawi/shapeFiles_gadm',
         layer = 'gadm36_MWI_0', driver = "ESRI Shapefile",
         overwrite_layer = T)

GADM.mal <- readOGR(dsn = '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/Malawi/shapeFiles_gadm',
                    layer = 'gadm36_MWI_2')
GADM.mal.clip <- intersect(JP.union, GADM.mal)
plot(GADM.mal)
plot(GADM.mal.clip, border = 'blue', add = T)
