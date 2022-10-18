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
# new IHME admin2 matches GADM admin2

n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(tolower(as.character(admin2.names$GADM))),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(tolower(unique(as.character(ihme.ests$adm2$ADM2_NAME)))),
                                    rep(NA, n - n.ihme)))

names.poss <- adm2.compare$GADM
adm2.compare$GADM[1:5] <- names.poss[c(4,6,9,10,15)]
adm2.compare$GADM[6:10] <- names.poss[c(17:21)]
adm2.compare$GADM[11:15] <- names.poss[c(25,28:29,31,33)]
adm2.compare$GADM[16:21] <- names.poss[c(34:39)]
adm2.compare$GADM[22:nrow(adm2.compare)] <- NA

for(area.idx in 1:nrow(adm2.compare)){
  if(!is.na(adm2.compare$GADM[area.idx])){
    area.name <- toupper(adm2.compare$GADM[area.idx])
    
    ihme.ests$adm2$ADM2_NAME[tolower(ihme.ests$adm2$ADM2_NAME) ==
                               adm2.compare$IHME[area.idx]] <- area.name
  }
}
