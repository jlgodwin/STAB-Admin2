#### Admin1 ####
# The new IHME admin1 names match GADM

#-------OLD-------#
n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
n <- max(n.gadm, n.ihme)
adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
                                    rep(NA, n - n.ihme)))

adm1.compare
# adm1.compare$IHME[c(9,10)] <- adm1.compare$IHME[c(10,9)]
# 
# ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)
# mismatch.idx <- which(!(as.character(adm1.compare[,1]) %in%
#                           as.character(adm1.compare[,2])))
# adm1.compare[mismatch.idx,]
# 
# for(idx in mismatch.idx){
#   ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME ==
#                              as.character(adm1.compare$IHME[idx])] <- as.character(adm1.compare$GADM[idx])
# }
# sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME))

#### Admin2 ####
# The new IHME admin2 names match GADM admin2
# TO DO: deal with there being two regions named "North Shewa"
# !!! rename to
# !!! North Shewa (Amhara)
# !!! North Shewa (Oromia)

n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
                                    rep(NA, n - n.ihme)))
adm2.compare
# mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
#                           as.character(adm2.compare[,2])))
# adm2.compare[mismatch.idx, ]

#-------OLD-------#
# move.idx <- grep("Zone",adm2.compare$IHME)
# adm2.compare$IHME <- c(NA, as.character(adm2.compare$IHME[move.idx]), 
#                        as.character(adm2.compare$IHME[-move.idx])[1:70],
#                        NA, NA, NA)
# adm2.compare$IHME <- c(as.character(adm2.compare$IHME)[1:7],
#                        as.character(adm2.compare$IHME)[12],
#                        as.character(adm2.compare$IHME)[c(8:11,13:79)])
# adm2.compare$IHME <- c(as.character(adm2.compare$IHME)[1:10],
#                        NA, NA, NA,
#                        as.character(adm2.compare$IHME)[11:76])
# names.list <- adm2.compare$IHME
# adm2.compare$IHME[16] <- NA
# adm2.compare$IHME[17:20] <- as.character(names.list[16:19])
# adm2.compare$IHME[c(21, 22, 29)] <- c(NA, as.character(names.list[21:22]))
# adm2.compare$IHME[23:27] <- as.character(names.list[c(65,67,66,68,69)])
# adm2.compare$IHME[c(28:34)] <- c(NA, as.character(names.list)[c(22,23,29,30:32)])
# adm2.compare$IHME[c(35:41)] <- as.character(names.list)[33:39]
# adm2.compare$IHME[42:47] <- c(as.character(names.list)[c(40:42, 45)],
#                               NA, as.character(names.list)[43])
# adm2.compare$IHME[48:53] <- c(as.character(names.list)[c(44,46,47)],
#                               NA, as.character(names.list)[c(48,77)])
# adm2.compare$IHME[54:58] <- as.character(names.list)[c(72, 73, 74,75, 76)]
# adm2.compare$IHME[59:64] <- as.character(names.list)[c(24,25,26,27,28,50)]
# adm2.compare$IHME[65:71] <- as.character(names.list)[c(52:53, 56, 57,51,55,54)]
# adm2.compare$IHME[72:77] <- as.character(names.list)[c(61,62,63,60,64,71)]
# 
# mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
#                           as.character(adm2.compare[,2])))
# adm2.compare[mismatch.idx,]
# ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
# 
# for(idx in mismatch.idx){
#   if(!is.na(as.character(adm2.compare$IHME[idx]))){
#   ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
#                              as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM[idx])
#   }
# }
# sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))