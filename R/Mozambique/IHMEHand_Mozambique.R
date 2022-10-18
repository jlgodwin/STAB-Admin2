#### Admin1 ####
n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
n <- max(n.gadm, n.ihme)
adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
                                    rep(NA, n - n.ihme)))
mismatch.idx <- which(!(as.character(adm1.compare[,1]) ==
                          as.character(adm1.compare[,2])))
adm1.compare[mismatch.idx,]
adm1.compare



#### Admin2 ####
n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
                                    rep(NA, n - n.ihme)))
mismatch.idx <- which(!(as.character(adm2.compare[,1]) ==
                          as.character(adm2.compare[,2])))
adm2.compare[mismatch.idx,]
ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)

names.poss <- adm2.compare$GADM
adm2.compare$GADM[9:16] <- names.poss[10:17]
adm2.compare$GADM[17:27] <- names.poss[18:28]
adm2.compare$GADM[24:34] <- NA
adm2.compare$GADM[35:45] <- names.poss[25:35]
adm2.compare$GADM[c(46,48,50:54,56)] <- names.poss[c(36:43)]
adm2.compare$GADM[c(47,49,55)] <-NA
adm2.compare$GADM[c(57,59:67)] <- names.poss[c(44:53)]
adm2.compare$GADM[58] <- NA
adm2.compare$GADM[68:75] <- c(as.character(names.poss[54:61]))
adm2.compare$GADM[76:87] <- c(NA,as.character(names.poss[64:73]), NA)
adm2.compare$GADM[88:98] <- names.poss[74:84]
adm2.compare$GADM[99:109] <- names.poss[c(85:86, 90,
                                          87:89,91:95)]
adm2.compare$GADM[110:120] <- c(as.character(names.poss[96:103]),
                                NA, as.character(names.poss[105]), NA)
adm2.compare$GADM[121:129] <- NA
adm2.compare$GADM[130:132] <- c(as.character(names.poss[115:117]))
adm2.compare$GADM[133:143] <- names.poss[118:128]
adm2.compare$GADM[144] <- names.poss[129]

names.still.poss <- names.poss[!(names.poss %in% adm2.compare$GADM)]
mismatch.still.idx <- which(is.na(adm2.compare$GADM))
adm2.compare[mismatch.still.idx,]
adm2.compare$GADM[c(121:129)] <- names.still.poss[c(4:5,7:10,3,11,12)]

names.still.poss <- names.poss[!(names.poss %in% adm2.compare$GADM)]
mismatch.still.idx <- which(is.na(adm2.compare$GADM))
adm2.compare[mismatch.still.idx,]
adm2.compare$GADM[76] <- names.still.poss[2]
adm2.compare$GADM[c(28:31,34)] <- c("Lichinga", "Maputo", "Nampula",
                                    "Pemba", "Xai-Xai")
adm2.compare
mismatch.idx <- which(!(as.character(adm2.compare[,1]) ==
                          as.character(adm2.compare[,2])))
for(idx in mismatch.idx){
  if(!is.na(adm2.compare$GADM[idx])){
  ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
                             as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM[idx])
  }
}
sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))
admin2.names[!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME),]
