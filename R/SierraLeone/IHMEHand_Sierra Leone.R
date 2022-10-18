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

ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)

adm1.compare$GADM <- c("Eastern Province",
                       "Northern Province",
                       "Southern Province",
                       "Western Area",
                       "Northwestern Province")

for(i in 1:nrow(adm1.compare)){
  if(!is.na(adm1.compare$IHME[i])){
    ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME == adm1.compare$IHME[i]] <- adm1.compare$GADM[i]
  }
}

#### Admin2 ####
ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
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
adm2.compare

ihme.names <- unique(adm2.compare$IHME)

adm2.compare$IHME[4] <- NA
adm2.compare$IHME[5:6] <- ihme.names[4:5]
adm2.compare$IHME[7] <- NA
adm2.compare$IHME[8:16] <- ihme.names[6:14]



for(i in 1:nrow(adm2.compare)){
  if(!is.na(adm2.compare$IHME[i])){
    ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == adm2.compare$IHME[i]] <- as.character(adm2.compare$GADM[i])
  }
}
