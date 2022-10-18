
#### Admin1 ####
# new IHME admin1 matches GADM admin1

n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
n <- max(n.gadm, n.ihme)
adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
                                    rep(NA, n - n.ihme)))
mismatch.idx <- which(!(as.character(adm1.compare[,1]) %in%
                          as.character(adm1.compare[,2])))
adm1.compare[mismatch.idx,]
adm1.compare

#### Admin2 ####
# new IHME Admin2 matches GADM Admin2

n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
                                    rep(NA, n - n.ihme)))
mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
                          as.character(adm2.compare[,2])))
adm2.compare[mismatch.idx,]


#---------OLD---------#
# ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)
# 
# for(idx in mismatch.idx){
#   ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME ==
#                              as.character(adm1.compare$IHME[idx])] <- as.character(adm1.compare$GADM[idx])
# }
# 
# sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME))
# n.gadm <- dim(admin1.names)[1]
# n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
# n <- max(n.gadm, n.ihme)
# adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
#                                     rep(NA, n-n.gadm)),
#                            IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
#                                     rep(NA, n - n.ihme)))
# mismatch.idx <- which(!(as.character(adm1.compare[,1]) %in%
#                           as.character(adm1.compare[,2])))
# 
# adm1.compare

# (Admin 2)

# names.poss <- adm2.compare$IHME
# 
# adm2.compare$IHME[14:24] <- c(NA, as.character(names.poss)[14:23])
# adm2.compare$IHME[25:35] <- c(as.character(names.poss)[24:31], NA, as.character(names.poss)[32:33])
# adm2.compare$IHME[36:45] <- names.poss[34:43]
# 
# adm2.compare
# 
# mismatch.idx <- which(!(as.character(adm2.compare[,1]) ==
#                           as.character(adm2.compare[,2])))
# 
# ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
# 
# for(idx in mismatch.idx){
#   if(!is.na(adm2.compare$GADM[idx])){
#     ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
#                                as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM[idx])
#   }
# }
# sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))
# 
