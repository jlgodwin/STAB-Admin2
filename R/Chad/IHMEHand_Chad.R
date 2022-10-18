#### Admin1 ####
n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
n <- max(n.gadm, n.ihme)
adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
                                    rep(NA, n - n.ihme)), stringsAsFactors = F)
mismatch.idx <- which(!(as.character(adm1.compare[,1]) %in%
                          as.character(adm1.compare[,2])))
adm1.compare[mismatch.idx,]
adm1.compare

# Taylor edited
adm1.compare <- data.frame(GADM = sort(as.character(admin1.names$GADM)),
                           IHME = adm1.compare$IHME[c(4,NA,8,NA,10,10,11,NA,14,15,17,18,19,NA,NA,NA,23,24,25,26,28,NA,NA)])


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

adm2.compare

#taylor edited
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM))),
           IHME = as.character(ihme.ests$adm2$ADM2_NAME %>% unique() %>% sort())[c(1,2,4,NA,9,10,11,12, 13, 14, 15, 18,NA, NA, NA, 19, 20, 21, 21, 22, 23, 24, NA, 25, 26,27,28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 45, 46, NA, 47, 48, 49,50,51, 52, NA, 53)]
)









