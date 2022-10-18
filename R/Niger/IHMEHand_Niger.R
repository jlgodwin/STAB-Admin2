#### Admin1 ####
n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
n <- max(n.gadm, n.ihme)
adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
                                    rep(NA, n - n.ihme)))
adm1.compare
mismatch.idx <- which(!(as.character(adm1.compare[,1]) ==
                          as.character(adm1.compare[,2])))
adm1.compare[mismatch.idx,]

ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)

for(idx in mismatch.idx){
  ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME ==
                             as.character(adm1.compare$IHME[idx])] <- as.character(adm1.compare$GADM[idx])
}
sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME))

# unique(ihme.ests$adm1$ADM1_NAME)

#### Admin2 ####
ihme.ests$adm2$ADM2_NAME<-as.character(ihme.ests$adm2$ADM2_NAME)
n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
                                    rep(NA, n - n.ihme)))
adm2.compare
# IHME is missing Niamey and has extra Commune 1,2,3

ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Aguie", "Aguié", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Birni N'gaoure", "Boboye", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Birni N'konni", "Bkonni", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Dogondoutchi", "Dogon-Doutchi", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Filingue", "Filingué", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Goure", "Gouré", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Guidan Roumji", "Groumdji", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Illela", "Illéla", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Maine-soroa", "Maïné-Soroa", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Matamey", "Matameye", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Miria", "Mirriah", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="N'guigmi", "N'Guigmi", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Tchin Tabaradene", "Tchin-Tabarade", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Tera", "Téra", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Tillaberi", "Tillabéry", as.character(ihme.ests$adm2$ADM2_NAME))

# unique(ihme.ests$adm2$ADM2_NAME)
