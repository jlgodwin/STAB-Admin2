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

adm1.compare$GADM[16] <- "Rutana"
adm1.compare$GADM[17] <- "Ruyigi"
adm1.compare$GADM[18] <- NA

ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)

for(i in 1:nrow(adm1.compare)){
  ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME == adm1.compare$IHME[i]] <- as.character(adm1.compare$GADM[i])
}

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

gadm.names <- sort(as.character(adm2.compare$GADM))
ihme.names <- as.character(adm2.compare$IHME)

adm2.compare$GADM[21] <- NA
adm2.compare$GADM[22] <- gadm.names[21]
adm2.compare$GADM[23] <- gadm.names[22]
adm2.compare$GADM[24] <- gadm.names[23]
adm2.compare$GADM[25] <- NA
adm2.compare$GADM[26] <- gadm.names[24]
adm2.compare$GADM[27] <- gadm.names[25]
adm2.compare$GADM[28] <- NA
adm2.compare$GADM[29:30] <- gadm.names[26:27]
adm2.compare$GADM[31] <- NA
adm2.compare$GADM[32:33] <- gadm.names[28:29]
adm2.compare$GADM[34:38] <- gadm.names[30:34]
adm2.compare$GADM[39:43] <- gadm.names[c(35:36, NA, 37:38)]
adm2.compare$GADM[44:48] <- gadm.names[39:43]
adm2.compare$GADM[49:53] <- gadm.names[44:48]
adm2.compare$GADM[54:58] <- gadm.names[c(49,NA, 50, NA, 51)]
adm2.compare$GADM[59:63] <- gadm.names[c(52:56)]
adm2.compare$GADM[64:68] <- gadm.names[c(NA, NA, 57:59)]
adm2.compare$GADM[69:73] <- gadm.names[c(NA, 60:63)]
adm2.compare$GADM[74:78] <- gadm.names[c(64:68)]
adm2.compare$GADM[79:83] <- gadm.names[c(69:73)]
adm2.compare$GADM[84:88] <- gadm.names[c(74,76:77, 79:80)]
adm2.compare$GADM[89:93] <- gadm.names[c(81:82, NA, 83:84)]
adm2.compare$GADM[94:98] <- gadm.names[c(85:89)]
adm2.compare$GADM[99:103] <- gadm.names[c(90:92, NA,  93)]
adm2.compare$GADM[104:108] <- gadm.names[c(95:99)]
adm2.compare$GADM[109:113] <- gadm.names[c(NA,100:103)]
adm2.compare$GADM[114:118] <- gadm.names[c(NA, 104:107)]
adm2.compare$GADM[119] <- gadm.names[108]
adm2.compare$GADM[120] <- gadm.names[109]
adm2.compare$GADM[121:125] <- gadm.names[110:114]
adm2.compare$GADM[126:130] <- gadm.names[115:119]
ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)


for(idx in mismatch.idx){
  ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
                             as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM)[idx]
}
sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))
