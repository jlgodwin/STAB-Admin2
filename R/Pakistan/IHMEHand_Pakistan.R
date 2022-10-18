## Edited: Austin Schumacher

# #### Admin1 ####
ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm1$ADM1_NAME)

# hand edits
ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME == "Khyber Pakhtunkhwa"] <- "N.W.F.P."

#### Admin2 ####
ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)

# hand edits
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "F.A.T.A. 1"] <- "F.A.T.A."
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "F.A.T.A. 2"] <- "F.A.T.A."

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

message("IHME admin1 had Khyber Pakhtunkhwa which is the merger of N.W.F.P. and F.A.T.A., but these are separate in GADM.")
message("Renamed Khyber Pakhtunkhwa as N.W.F.P. in IHME and F.A.T.A. has no IHME corrolary")
message("GADM has two admin2 regions named F.A.T.A., while IHME had F.A.T.A. 1 and F.A.T.A. 2. I assigned these both to F.A.T.A.")
