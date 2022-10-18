#### Admin1 ####
#Admin 1 match

#### Admin2 ####
# new IHME admin2 matches GADM admin2

n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
                                    rep(NA, n - n.ihme)))
adm2.compare

mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
                          as.character(adm2.compare[,2])))
adm2.compare[mismatch.idx,]

#-----OLD-----#
# ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
# 
# ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Bulilima", "Bulilima (North)", as.character(ihme.ests$adm2$ADM2_NAME))
# #3 GADM admin 2s didn't have IHME matches
# 
# 
# sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))