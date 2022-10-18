# #### Admin1 ####
# n.gadm <- dim(admin1.names)[1]
# n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
# n <- max(n.gadm, n.ihme)
# adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
#                                     rep(NA, n-n.gadm)),
#                            IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
#                                     rep(NA, n - n.ihme)))
# mismatch.idx <- which(!(as.character(adm1.compare[,1]) ==
#                           as.character(adm1.compare[,2])))
# adm1.compare[mismatch.idx,]
# adm1.compare
# 
# adm1.compare$IHME[6:7] <- as.character(adm1.compare$IHME[7:6])
# 
# mismatch.idx <- which(!(as.character(adm1.compare[,1]) ==
#                           as.character(adm1.compare[,2])))
# adm1.compare[mismatch.idx,]
# adm1.compare
# 
# ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)
# 
# for(idx in mismatch.idx){
#   if(!is.na(adm1.compare$GADM[idx])){
#     ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME ==
#                                as.character(adm1.compare$IHME[idx])] <- as.character(adm1.compare$GADM[idx])
#   }
# }
# 
# #### Admin2 ####
# n.gadm <- dim(admin2.names)[1]
# n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
# n <- max(n.gadm, n.ihme)
# adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
#                                     rep(NA, n-n.gadm)),
#                            IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
#                                     rep(NA, n - n.ihme)))
# mismatch.idx <- which(!(as.character(adm2.compare[,1]) ==
#                           as.character(adm2.compare[,2])))
# adm2.compare[mismatch.idx,]
# ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
# 
# names.poss <- adm2.compare$GADM
# 
# adm2.compare$GADM[21:29] <- names.poss[c(22,24,25,26,27,28,29,30,31)]
# adm2.compare$GADM[30:40] <- names.poss[32:42]
# adm2.compare$GADM[41:51] <- c(NA, NA, as.character(names.poss)[43:45],
#                               NA, as.character(names.poss)[c(46:47,49,48,50)])
# adm2.compare$GADM[52:62] <- names.poss[51:61]
# adm2.compare$GADM[63:73] <- names.poss[62:72]
# adm2.compare$GADM[74:77] <- names.poss[73:76]
# 
# 
# names.still.poss <- names.poss[!(names.poss %in% adm2.compare$GADM)]
# mismatch.still.idx <- which(is.na(adm2.compare$GADM))
# adm2.compare[mismatch.still.idx,]
# names.still.poss
# adm2.compare$GADM[41:42] <- names.still.poss[1:2]
# 
# adm2.compare
# mismatch.idx <- which(!(as.character(adm2.compare[,1]) ==
#                           as.character(adm2.compare[,2])))
# for(idx in mismatch.idx){
#   if(!is.na(adm2.compare$GADM[idx])){
#   ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
#                              as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM[idx])
#   }
# }
# sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))
# 
