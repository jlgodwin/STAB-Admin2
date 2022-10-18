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
names.list <- as.character(adm1.compare$IHME)
adm1.compare$IHME[1:5] <- names.list[c(4,1,2,3,6)]
adm1.compare$IHME[6:12] <- names.list[7:13]
adm1.compare$IHME[13] <- names.list[5]
mismatch.idx <- which(!(as.character(adm1.compare[,1]) ==
                          as.character(adm1.compare[,2])))
adm1.compare[mismatch.idx,]
adm1.compare


ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)

for(idx in mismatch.idx){
  ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME ==
                             as.character(adm1.compare$IHME[idx])] <- as.character(adm1.compare$GADM[idx])
}
sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME))


#### Admin2 ####
n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
                                    rep(NA, n - n.ihme)))

mismatch.idx <- which(as.character(adm2.compare[,1]) !=
                          as.character(adm2.compare[,2]))
adm2.compare[mismatch.idx, ]

names.list <- as.character(adm2.compare$IHME)

adm2.compare$IHME[10:12] <- names.list[c(11,12,10)]
adm2.compare$IHME[21:25] <- c(names.list[c(23,21,22)], NA, names.list[24])
adm2.compare$IHME[26:30] <- names.list[c(25:29)]
adm2.compare$IHME[31:33] <- names.list[30:32]

ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
mismatch.idx <- which(as.character(adm2.compare[,1]) !=
                        as.character(adm2.compare[,2]))
adm2.compare[mismatch.idx, ]

for(idx in mismatch.idx){
  ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
                             as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM[idx])
}
sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))
