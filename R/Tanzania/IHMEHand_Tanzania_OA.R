library(stringr)
#### Admin1 ####
n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
n <- max(n.gadm, n.ihme)
#adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
#                                    rep(NA, n-n.gadm)),
#                           IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
#                                    rep(NA, n - n.ihme)))
#mismatch.idx <- which(!(as.character(adm1.compare[,1]) %in%
#                          as.character(adm1.compare[,2])))
#adm1.compare[mismatch.idx,]
#adm1.compare

## OA
adm1.compare <- data.frame(GADM = c(as.character(admin1.names$GADM), rep(NA, n-n.gadm)),
                           IHME = rep("", n))
ihme.names <- unique(as.character(ihme.ests$adm1$ADM1_NAME))
for(i in row.names(adm1.compare)) {
  xmatch <- agrep(adm1.compare[i, ]$GADM, ihme.names, value = T, ignore.case = TRUE, max.distance = 1)
  #print(xmatch)
  if(length(xmatch) > 0) {
    adm1.compare[i, ]$IHME <- ifelse(toupper(str_sub(xmatch[1], start = 1, end = 1)) == toupper(str_sub(adm1.compare[i, ]$GADM, start = 1, end = 1)) , 
                                     xmatch[1], 
                                     adm1.compare[i, ]$IHME)
    
    # & (str_sub(xmatch, start = 1, end = 1) 
  }
}
adm1.compare[adm1.compare$GADM == "Dar es Salaam",]$IHME <- "Dar-es-salaam"
adm1.compare[adm1.compare$GADM == "Tanga",]$IHME <- "Tanga"
adm1.compare
adm1.compare <- subset(adm1.compare, IHME != "")
##

#### Admin2 ####
n.gadm <- length(unique(admin2.names$GADM))     ## Boane is repeated
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)

## OA
adm2.compare <- data.frame(GADM = c(sort(unique(as.character(admin2.names$GADM))), rep(NA, n-n.gadm)),
                           IHME = rep(NA, n))
ihme.names <- unique(as.character(ihme.ests$adm2$ADM2_NAME))
ok <- rep(F, length(ihme.names))
# row.names(adm2.compare)
## Almost Excact match
for(i in 1:dim(adm2.compare)[1]) {
  if(is.na(adm2.compare[i, ]$GADM)) {
    next()
  }
  # pos <- agrep(adm2.compare[i, ]$GADM, ihme.names[!ok], value = F, ignore.case = TRUE,  max.distance = 0)
  # xmatch <- agrep(adm2.compare[i, ]$GADM, ihme.names[!ok], value = T, ignore.case = TRUE, max.distance = 0)
  xmatch <- agrep(adm2.compare[i, ]$GADM, ihme.names[!ok], value = T, ignore.case = TRUE, max.distance = 0)
  #xmatch <- ihme.names[pos]
  #ok[i] <- F
  # print(paste0(adm2.compare[i, ]$GADM, " - ", length(xmatch)))
  if(length(xmatch) > 0) {
    #print(xmatch)
    pos <- which(xmatch %in% ihme.names)[1]
    adm2.compare[i, ]$IHME <- xmatch[1]
    #ok[pos] <- T
  }
}

#cbind(adm2.compare, ok)
# adm2.compare
#adm2.compare$IHME[adm2.compare$GADM == "Ile"] <- "Ile"
#adm2.compare$IHME[adm2.compare$GADM == "Sanga"] <- "Sanga"
# ok[adm2.compare$GADM == "Ile"] <- F
adm2.compare <- subset(adm2.compare, !(is.na(GADM) & is.na(IHME)))

subset(adm2.compare, is.na(IHME))
adm2.compare

adm2.compare <- subset(adm2.compare, ! is.na(IHME))





























