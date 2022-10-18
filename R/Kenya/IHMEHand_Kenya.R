#### Admin1 ####
# new IHME admin1 names match GADM

doAdmin2 <- FALSE
n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
n <- max(n.gadm, n.ihme)
adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
                                    rep(NA, n - n.ihme)))

adm1.compare


#### Admin2 ####
# GADM has two instances of "Igembe South", "Lugari
# IHME doesn't have "unknown 3" or "unknown 8"

# n.gadm <- dim(admin2.names)[1]
# n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
# n <- max(n.gadm, n.ihme)
# adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
#                                     rep(NA, n-n.gadm)),
#                            IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
#                                     rep(NA, n - n.ihme)))
# adm2.compare
# mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
#                           as.character(adm2.compare[,2])))
# adm2.compare[mismatch.idx, ]
# mismatch.idx <- which(!(as.character(adm2.compare[,2]) %in%
#                           as.character(adm2.compare[,1])))
# adm2.compare[mismatch.idx, ]


#-------OLD-------#
# n.gadm <- dim(admin1.names)[1]
# ihme.ests$adm1 <- ihme.ests$adm2
# ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
# 
# doAdmin2 <- FALSE
# 
# n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
# n <- max(n.gadm, n.ihme)
# adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
#                                     rep(NA, n-n.gadm)),
#                            IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
#                                     rep(NA, n - n.ihme)))
# 
# adm1.compare
# 
# adm1.compare$IHME <- as.character(adm1.compare$IHME)
# names.poss <- as.character(adm1.compare$GADM[!is.na(adm1.compare$GADM)])
# adm1.compare$GADM[1:10] <- names.poss[c(1:2,NA,3,NA,4,NA,16,6:7)]
# adm1.compare$GADM[11:20] <- names.poss[c(NA, 8, NA, NA, 9:11, NA, 12:13)]
# adm1.compare$GADM[21:30] <- names.poss[c(14:15,17:18,NA,NA,19:21,NA)]
# adm1.compare$GADM[31:40] <- names.poss[c(22:23, NA, 24, NA,5,25,NA, 26, 26)]
# adm1.compare$GADM[41:50] <- names.poss[c(26:28,NA,NA,29,NA,30:32)]
# adm1.compare$GADM[51:60] <- names.poss[c(32:35, NA, 36, NA, 37:38, NA)]
# adm1.compare$GADM[61:70] <- names.poss[c(39:40,NA, 41, NA, NA, 42:45)]
# adm1.compare$GADM[71:72] <- names.poss[46:47]
# 
# mismatch.idx <- which(!(as.character(adm1.compare[,1]) %in%
#                           as.character(adm1.compare[,2])))
# adm1.compare[mismatch.idx,]
# 
# for(idx in mismatch.idx){
#   ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME ==
#                              as.character(adm1.compare$IHME[idx])] <- as.character(adm1.compare$GADM[idx])
# }
# sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME))



