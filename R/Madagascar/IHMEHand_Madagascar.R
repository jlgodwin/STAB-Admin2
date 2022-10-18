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

dim(poly.adm1)
dim(poly.adm2)

ihme.ests$adm2 <- ihme.ests$adm1
ihme.ests$adm2$ADM2_NAME <- ihme.ests$adm2$ADM1_NAME


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

ihme.ests$adm1[,c('mean', 'uci','lci')] <- NA
