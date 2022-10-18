# #  RemoveWanderingClusters.R
# #  author: Jessica Godwin
# #  
# #  sources: LoadCommandCenter.R
# #           
# 
# points.file <- points.layer <- 
#   SurveyInfo$GPSFile[SurveyInfo$Country == country &
#                        SurveyInfo$`GPS?` == "Y" &
#                        SurveyInfo$`Survey Year` == 2012]
# points.file #Check the points files
# 
# 
# 
# #### Load Data ####
# 
# points.path <- paste0(folder.name, "/dhsFlat/", points.file)
# points <- readOGR(dsn = path.expand(points.path),
#                   layer = as.character(points.layer))
# 
# plot(poly.adm0)
# poly.adm0@bbox
# points@bbox
# adm0.box <- poly.adm0@bbox
# points.box <- points@bbox
# 
# poly.over <- SpatialPolygons(poly.adm0@polygons)
# proj4string(poly.over) <- proj4string(points)
# over <- over(points, poly.over)
# missing <- points@coords[which(is.na(over)),]
# dists <- dist2Line(missing,poly.over)
# dists
# 
# ## Distances are in meteres
# ## points 1-4 are clearly the outlying clusters.
# ## Setting cutoff for Morocco to 400m
# cutoff <- 400
# remove.id <- which(dists[,'distance'] > cutoff )
# 
# remove.clus.idx <- which(is.na(over))[remove.id]
# points@data[remove.clus.idx,]
# plot(points, col = 'white')
# plot(points[remove.clus.idx,], add = T,pch = 19, col = 'blue')
# plot(poly.adm0, add = T)
# clus.no <- points@data$DHSCLUST[remove.clus.idx]
# 
# ## Check by hand. Is Admin1 assignment 
# ## same as points@data$ADM1FIPSNA[points@data$DHSCLUST == clus]
# 
# for(clus in clus.no){
# message("Cluster no. ", clus)
# message("Assigned to Admin 1: ",unique(mod.dat$admin1.name[mod.dat$survey == 2012 &
#                              mod.dat$cluster == clus]),"\n")
# message("Assigned to Admin 2:: ",unique(mod.dat$admin2.name[mod.dat$survey == 2012 &
#                                                   mod.dat$cluster == clus]))
# message("points: ", points@data$ADM1FIPSNA[points@data$DHSCLUST == clus],"\n")
# cat("\n", sum(mod.dat$Y[mod.dat$cluster == clus]), " deaths and ",
#     sum(mod.dat$total[mod.dat$cluster == clus]), " agemonths.\n")
# }
# 
# ## Plot to understand INLA error in SmoothedDirect.R
# 
# ## If you want to remove)
# 
# mod.dat <- mod.dat[!(mod.dat$cluster %in% clus.no),]
# 
# 
