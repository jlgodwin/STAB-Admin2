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

ihme.ests$adm1$ADM1_NAME<-as.character(ihme.ests$adm1$ADM1_NAME)

for(idx in mismatch.idx){
  ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME ==
                             as.character(adm1.compare$IHME[idx])] <- as.character(adm1.compare$GADM[idx])
}

sum(!(as.character(admin1.names$GADM) %in% as.character(ihme.ests$adm1$ADM1_NAME)))

#### Admin2 ####
ihme.ests$adm2$ADM2_NAME<-as.character(ihme.ests$adm2$ADM2_NAME)
Encoding(ihme.ests$adm2$ADM2_NAME)<-"latin1"
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

ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Abanga-Bigne", "Abanga-Bigné", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Basse-Banio", "Basse Banio", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Bendje", "Bendjé", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Douigni", "Douigny", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Lolo-Bouenguidi", "Lolo Bouenguidi", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Ogooue et Lacs", "Ogooué et des Lacs", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Sebe-Brikolo", "Sébé-Brikolo", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Tsamba-Magotsi", "Tsamba Mangotsi", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Zadie", "Zadié", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Lebombi-Leyou", "Léboumbi-Leyou", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Lekoko", "Lékoko", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Boumi-Louetsi", "Boumi-lowetsi", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Douya-Onoye", "Douya Onoye", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Lope", "Lopé", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Mouloundou", "Mouloudnou", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Etimboue", "Étimboué", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Haut-Komo", "Haut-Como", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Boumi-Louetsi", "Boumi-lowetsi", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Douya-Onoye", "Douya Onoye", as.character(ihme.ests$adm2$ADM2_NAME))
ihme.ests$adm2$ADM2_NAME<-ifelse(ihme.ests$adm2$ADM2_NAME=="Etimboue", "Étimboué", as.character(ihme.ests$adm2$ADM2_NAME))


sum(!(as.character(admin2.names$GADM) %in% as.character(ihme.ests$adm2$ADM2_NAME)))#Couldn't match Léconi-Djoué and Mpassa    
miss<- which(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))
admin2.names$GADM[miss]