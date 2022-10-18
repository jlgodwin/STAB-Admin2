levels(poly.adm1@data$NAME_1)
levels(poly.adm1@data$NAME_1)[levels(poly.adm1@data$NAME_1) == "Nassa"] <- "Niassa"
levels(poly.adm1@data$NAME_1)
writeOGR(poly.adm1, dsn = poly.path,
         layer = poly.layer.adm1, driver = "ESRI Shapefile",
         overwrite_layer = T)

levels(poly.adm2@data$NAME_1)
levels(poly.adm2@data$NAME_1)[levels(poly.adm2@data$NAME_1) == "Nassa"] <- "Niassa"
levels(poly.adm2@data$NAME_1)
writeOGR(poly.adm2, dsn = poly.path,
         layer = "test", driver = "ESRI Shapefile",
         overwrite_layer = T)
test <- readOGR(dsn = poly.path,
                layer = "test")
levels(poly.adm2@data$NAME_1)

writeOGR(poly.adm2, dsn = poly.path,
         layer = poly.layer.adm2, driver = "ESRI Shapefile",
         overwrite_layer = T)

