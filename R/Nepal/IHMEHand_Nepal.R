## Edited: Austin Schumacher

# #### Admin 1 ####
# ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)
# 
# 
# #### Admin2 ####
# ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
# 
# hand edits

# n.gadm <- dim(admin2.names)[1]
# n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
# n <- max(n.gadm, n.ihme)
# adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
#                                     rep(NA, n-n.gadm)),
#                            IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
#                                     rep(NA, n - n.ihme)))
# mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
#                           as.character(adm2.compare[,2])) & !is.na(adm2.compare[,1]))
# mismatch.idx2 <- which(!(as.character(adm2.compare[,2]) %in%
#                             as.character(adm2.compare[,1])))
# as.character(adm2.compare[mismatch.idx,1])
# as.character(adm2.compare[mismatch.idx2,2])
# 
# sum(!(as.character(admin2.names$GADM) %in% ihme.ests$adm2$ADM2_NAME))
# sum(!(unique(ihme.ests$adm2$ADM2_NAME) %in% as.character(admin2.names$GADM)))

# Note:
message("Neither IHME admin 1 nor admin 2 match with the new UN provided regions")
# message("IHME admin2 had Dhaualagiri while GADM did not. Not sure if this has anything to do with it, but according to wikipedia, Dhaulagiri was divided into 4 districts; since 2015, these districts have been redesignated as parts of Gandaki Pradesh.")
