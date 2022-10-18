## Edited: Austin Schumacher

# #### Admin1 ####
# For Uganda, GADM has 56 admin 1 (which is the number that existed in 2002).
# IHME has 112 admin 1 (which is the number that existed in 2010).
# Currently (Dec 2019), Wikipedia says there are 134 admin 1 districts, which is also expanding.

#### Admin2 ####
if(FALSE){
ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)

# hand edits
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Soroti Municipality"] <- "Soroti  (Municipality)"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Tororo Municipality"] <- "Tororo (Municipality)"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Buyikwe"] <- "Buikwe"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Busiro"] <- "Busiiro"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Entebbe Municipality"] <- "Entebbe"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Fort Portal Municipality"] <- "Fort Portal"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Gulu Municipality"] <- "Gulu"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Moroto Municipality"] <- "Moroto"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Jinja Municipality"] <- "Jinja"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Kabale Municipality"] <- "Kabale"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Kisoko (west Budama)"] <- "Kisoko (West Budama)"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Kadani (chekwii)"] <- "Chekwii"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Lira Municipality"] <- "Lira"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Madi-okollo"] <- "Madi-Okollo"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Masaka Municipality"] <- "Masaka"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Mbale Municipality"] <- "Mbale"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Mbarara Municipality"] <- "Mbarara"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Samia-bugwe"] <- "Samia-Bugwe"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Wabusana (bamunanika)"] <- "Bamunanika"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Adjumani"] <- "East Moyo"

n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
                                    rep(NA, n - n.ihme)))
mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
                          as.character(adm2.compare[,2])) & !is.na(adm2.compare[,1]))
mismatch.idx2 <- which(!(as.character(adm2.compare[,2]) %in%
                            as.character(adm2.compare[,1])))
as.character(adm2.compare[mismatch.idx,1])
as.character(adm2.compare[mismatch.idx2,2])

sum(!(as.character(admin2.names$GADM) %in% ihme.ests$adm2$ADM2_NAME))
sum(!(unique(ihme.ests$adm2$ADM2_NAME) %in% as.character(admin2.names$GADM)))

# Note:
#   In IHME names, the following are disaggregations of Kampala:
#       Central Kampala, Kawempe, Makindye, Nakawa, and Rubaga

message("there are still 4 GADM admin2 that don't match IHME, and there are still 10 IHME admin2 that don't match GADM")
message("5 of the IHME ones that aren't in GADM are disaggregated subdivisions of Kampala")
}