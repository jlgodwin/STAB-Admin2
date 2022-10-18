library(tm)
#### Admin1 ####
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
ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)

for(idx in mismatch.idx){
  ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME ==
                             as.character(adm1.compare$IHME[idx])] <- as.character(adm1.compare$GADM[idx])
}
sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME))

#### Admin2 ####
n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
Encoding(ihme.ests$adm2$ADM2_NAME)<-"latin1"
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
                                    rep(NA, n - n.ihme)))
mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
                          as.character(adm2.compare[,2])))
adm2.compare[mismatch.idx,]

ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Djéma", "Djemah", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Gambo", "Gambo-Ouango", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Mbaïki", "M'Baïki", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Ouanda-Djallé", "Ouanda Djallé", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Rafai", "Rafaï", as.character(ihme.ests$adm2$ADM2_NAME))

sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))