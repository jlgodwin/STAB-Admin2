levels(poly.adm1@data$NAME_1)

library(rgeos)
dhs.poly <- readOGR(dsn = 'CotedIvoire/shapeFiles_CI2012DHS/shps',
                    layer = 'sdr_subnational_boundaries')
dhs.poly@data$REGNAME

over <- over(SpatialPolygons(poly.adm1@polygons),
             SpatialPolygons(dhs.poly@polygons))

writeOGR(poly.adm2, dsn = poly.path,
         layer = poly.layer.adm2, driver = "ESRI Shapefile",
         overwrite_layer = T)

