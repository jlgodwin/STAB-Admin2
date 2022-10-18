#### Admin1 ####
n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(as.character(ihme.ests$adm1$ADM1_NAME)))

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
ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)

for(idx in mismatch.idx){
  ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
                             as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM[idx])
}
sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))
