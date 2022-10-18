
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
# except IHME has a bunch of Lakes

ihme.names <- unique(as.character(ihme.ests$adm2$ADM2_NAME))
ihme.nolakes <- ihme.names[!grepl("Lake", ihme.names)]

n.gadm <- dim(admin2.names)[1]
n.ihme <- length(ihme.nolakes)
n <- max(n.gadm, n.ihme)

adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(ihme.nolakes),
                                    rep(NA, n - n.ihme)))
mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
                          as.character(adm2.compare[,2])))
adm2.compare[mismatch.idx,]


#--------OLD--------#
# library(stringr)
# #### Admin1 ####
# n.gadm <- dim(admin1.names)[1]
# n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
# n <- max(n.gadm, n.ihme)
# #adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
# #                                    rep(NA, n-n.gadm)),
# #                           IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
# #                                    rep(NA, n - n.ihme)))
# #mismatch.idx <- which(!(as.character(adm1.compare[,1]) %in%
# #                          as.character(adm1.compare[,2])))
# #adm1.compare[mismatch.idx,]
# #adm1.compare
# 
# ## OA
# adm1.compare <- data.frame(GADM = c(as.character(admin1.names$GADM), rep(NA, n-n.gadm)),
#                            IHME = rep("", n))
# adm1.compare$IHME <- as.character(adm1.compare$IHME)
# ihme.names <- unique(as.character(ihme.ests$adm1$ADM1_NAME))
# for(i in row.names(adm1.compare)) {
#   xmatch <- agrep(adm1.compare[i, ]$GADM, ihme.names, value = T, ignore.case = TRUE, max.distance = 1)
#   #print(xmatch)
#   if(length(xmatch) > 0) {
#     adm1.compare[i,]$IHME<- as.character(ifelse(toupper(str_sub(xmatch[1], start = 1, end = 1)) == toupper(str_sub(adm1.compare[i, ]$GADM, start = 1, end = 1)) , 
#                                      as.character(xmatch[1]), 
#                                      as.character(adm1.compare[i, ]$IHME)))
#     
#     # & (str_sub(xmatch, start = 1, end = 1) 
#   }
# }
# adm1.compare[adm1.compare$GADM == "Dar es Salaam",]$IHME <- "Dar-es-salaam"
# adm1.compare[adm1.compare$GADM == "Tanga",]$IHME <- "Tanga"
# adm1.compare[adm1.compare$GADM == "Pemba North",]$IHME <- as.character(ihme.names[7])
# adm1.compare[adm1.compare$GADM == "Pemba South",]$IHME <- as.character(ihme.names[12])
# adm1.compare
# 
# ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)
# adm1.compare <- subset(adm1.compare, IHME != "")
# mismatch.idx <- which(!(as.character(adm1.compare[,1]) ==
#                           as.character(adm1.compare[,2])))
# adm1.compare[mismatch.idx,]
# 
# for(idx in mismatch.idx){
#   ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME ==
#                              as.character(adm1.compare$IHME[idx])] <- as.character(adm1.compare$GADM[idx])
# }
# sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME))
# 
# #### Admin 2 ####
# 
# 
# n.gadm <- length(unique(admin2.names$GADM))     ## Boane is repeated
# n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
# n <- max(n.gadm, n.ihme)
# 
# ## OA
# adm2.compare <- data.frame(GADM = c(sort(unique(as.character(admin2.names$GADM))), rep(NA, n-n.gadm)),
#                            IHME = rep(NA, n))
# ihme.names <- unique(as.character(ihme.ests$adm2$ADM2_NAME))
# ok <- rep(F, length(ihme.names))
# # row.names(adm2.compare)
# ## Almost Excact match
# for(i in 1:dim(adm2.compare)[1]) {
#   if(is.na(adm2.compare[i, ]$GADM)) {
#     next()
#   }
#   # pos <- agrep(adm2.compare[i, ]$GADM, ihme.names[!ok], value = F, ignore.case = TRUE,  max.distance = 0)
#   # xmatch <- agrep(adm2.compare[i, ]$GADM, ihme.names[!ok], value = T, ignore.case = TRUE, max.distance = 0)
#   xmatch <- agrep(adm2.compare[i, ]$GADM, ihme.names[!ok], value = T, ignore.case = TRUE, max.distance = 0)
#   #xmatch <- ihme.names[pos]
#   #ok[i] <- F
#   # print(paste0(adm2.compare[i, ]$GADM, " - ", length(xmatch)))
#   if(length(xmatch) > 0) {
#     #print(xmatch)
#     pos <- which(xmatch %in% ihme.names)[1]
#     adm2.compare[i, ]$IHME <- xmatch[1]
#     #ok[pos] <- T
#   }
# }
# 
# adm2.compare[is.na(adm2.compare$IHME),]
# 
# adm2.compare$IHME[107] <- ihme.names[96]
# adm2.compare$IHME[44:45] <- ihme.names[38:39]
# adm2.compare$IHME[53] <- ihme.names[48]
# adm2.compare$IHME[109:110] <- ihme.names[52:53]
# adm2.compare$IHME[119:120] <- ihme.names[76:77]
# adm2.compare$IHME[150] <- ihme.names[152]
# mismatch.idx <- which(!(as.character(adm2.compare[,1]) ==
#                           as.character(adm2.compare[,2])))
# adm2.compare[mismatch.idx,]
# 
# ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
# 
# 
# for(idx in mismatch.idx){
#   ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
#                              as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM[idx])
# }
# sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))
# 
