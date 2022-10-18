# #### Admin1 ####
# n.gadm <- dim(admin1.names)[1]
# n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
# n.ihme <- length(unique(ihme.ests$adm0$ADM0_NAME))
# n <- max(n.gadm, n.ihme)
# adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
#                                     rep(NA, n-n.gadm)),
#                            IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
#                                     rep(NA, n - n.ihme)))
# mismatch.idx <- which(!(as.character(adm1.compare[,1]) %in%
#                           as.character(adm1.compare[,2])))
# adm1.compare[mismatch.idx,]
# adm1.compare
# 
# #### Admin2 ####
# n.gadm <- dim(admin2.names)[1]
# n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
# n <- max(n.gadm, n.ihme)
# adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
#                                     rep(NA, n-n.gadm)),
#                            IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
#                                     rep(NA, n - n.ihme)))
# mismatch.idx <- which(!(as.character(adm2.compare[,1]) %in%
#                           as.character(adm2.compare[,2])))
# adm2.compare[mismatch.idx,]
# ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
# 
# for(idx in mismatch.idx){
#   ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
#                              as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM[idx])
# }
# sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))

#### Admin1 ####
# check with the differences in the names are
unique(ihme.ests$adm1$ADM1_NAME)
admin1.names$GADM

# ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME == "Bie"] = factor("Bié")
# ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME == "Kuanza Norte"] = "Cuanza Norte"
# ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME == "Uige"] = "Uíge"
# ihme.ests$adm1$ADM1_NAME[ihme.ests$adm1$ADM1_NAME == "Huila"] = "Huíla"

# save the indices of each unique name. These will later be replaced by a new factor
uniqueNames = unique(ihme.ests$adm1$ADM1_NAME)
uniqueIs = list()
for(i in 1:length(uniqueNames)) {
  uniqueIs = c(uniqueIs, list(ihme.ests$adm1$ADM1_NAME == uniqueNames[i]))
}

# correct the spellings
newUniqueNames = as.character(uniqueNames)
newUniqueNames[newUniqueNames == "Bie"] = "Bié"
newUniqueNames[newUniqueNames == "Kuanza Norte"] = "Cuanza Norte"
newUniqueNames[newUniqueNames == "Uige"] = "Uíge"
newUniqueNames[newUniqueNames == "Huila"] = "Huíla"

# make a new factor with new levels containing the name updates at the corresponding indices of the IHME data frame
newNames = factor(as.character(ihme.ests$adm1$ADM1_NAME), levels=levels(admin1.names$GADM))

for(i in 1:length(uniqueNames)) {
  newNames[uniqueIs[[i]]] = newUniqueNames[i]
}

# check to make sure the new names match the old ones except for fixed spelling
print(data.frame(ihme.ests$adm1$ADM1_NAME, newNames))

# replace the old names
ihme.ests$adm1$ADM1_NAME = newNames

#### Admin2 ####
# check with the differences in the names are
sort(as.character(unique(ihme.ests$adm2$ADM2_NAME))) # 159 in total
sort(as.character(admin2.names$GADM)) # 163 in total

# save the indices of each unique name. These will later be replaced by a new factor
uniqueNames = unique(ihme.ests$adm2$ADM2_NAME)
uniqueIs = list()
for(i in 1:length(uniqueNames)) {
  uniqueIs = c(uniqueIs, list(ihme.ests$adm2$ADM2_NAME == uniqueNames[i]))
}

# correct the IHME spellings
newUniqueNames = as.character(uniqueNames)
newUniqueNames[newUniqueNames == "Baia Farta"] = "Baía Farta"
newUniqueNames[newUniqueNames == "Buco-zau"] = "Buco Zau"
newUniqueNames[newUniqueNames == "Caala"] = "Caála"
newUniqueNames[newUniqueNames == "Cacongo (landana)"] = "Landana"
newUniqueNames[newUniqueNames == "Cacuso"] = "Cacuzo"
newUniqueNames[newUniqueNames == "Caimbambo"] = "Caiambambo"
newUniqueNames[newUniqueNames == "Cambundi-catembo"] = "Cambundi-Catembo"
newUniqueNames[newUniqueNames == "Capenda Camulemba"] = "Capenda"
newUniqueNames[newUniqueNames == "Cazengo (ndalatando)"] = "Cazengo"
newUniqueNames[newUniqueNames == "Gonguembo"] = "Ngonguembo"
newUniqueNames[newUniqueNames == "Icolo E Bengo"] = "Icolo e Bengo"
newUniqueNames[newUniqueNames == "Kameia Lumege"] = "Cameia"
newUniqueNames[newUniqueNames == "Katchiungo"] = "Catchiungo"
newUniqueNames[newUniqueNames == "Kiwaba N'zogi"] = "Cuaba Nzogo" # ?????
newUniqueNames[newUniqueNames == "Kunda Dia-baze"] = "Cunda-dia-Baza"
newUniqueNames[newUniqueNames == "Leua"] = "Léua"
newUniqueNames[newUniqueNames == "Londuimbali"] = "Londuimbale"
newUniqueNames[newUniqueNames == "Luacano"] = "Lucano"
# newUniqueNames[newUniqueNames == "Luanda"] = "Luanda" # ????? This is the capital city
newUniqueNames[newUniqueNames == "Luena (moxico)"] = "Moxico"
newUniqueNames[newUniqueNames == "Lumbala N'guimbo"] = "Lumbala-Nguimbo"
newUniqueNames[newUniqueNames == "M'banza Congo"] = "M'Banza Congo"
newUniqueNames[newUniqueNames == "Maquela Do Zombo"] = "Maquela do Zombo"
newUniqueNames[newUniqueNames == "N'harea"] = "Nharea"
newUniqueNames[newUniqueNames == "N'zeto"] = "N'Zeto"
newUniqueNames[newUniqueNames == "Namacunde"] = "Namakunde"
newUniqueNames[newUniqueNames == "Nankova"] = "Nancova"
newUniqueNames[newUniqueNames == "Pango Aluquem"] = "Pango Aluquém"
# newUniqueNames[newUniqueNames == "Quipungo"] = "Quipungo" # ????? next to Gambos
# newUniqueNames[newUniqueNames == "Quissama"] = "Quissama" # ????? one of municipalities of Luanda province
newUniqueNames[newUniqueNames == "Samba Caju"] = "Samba Cajú"
newUniqueNames[newUniqueNames == "Santa Cruz"] = "Milunga"
newUniqueNames[newUniqueNames == "Songo/mucaba"] = "Mucaba" # or Songo...
newUniqueNames[newUniqueNames == "Tchikala-tcholo."] = "Tchicala-Tcholoanga"
newUniqueNames[newUniqueNames == "Tombua"] = "Tombwa"
newUniqueNames[newUniqueNames == "Uige"] = "Uíge"
newUniqueNames[newUniqueNames == "Waku Kungu"] = "Waku Kungo"
newUniqueNames[newUniqueNames == "Xa-muteba"] = "Xá Muteba"

# municipalities of Luanda province:
# Belas (possibly found as Kilamba Kiaxi)
# Cacuaco (found)
# Cazenga (found)
# Icolo e Bengo (found)
# Luanda (city) (possibly found as Sambizanga or Rangel or Malanga or Ingombota)
# Quicama (found as Qissama)
# Viana (found)

sort(newUniqueNames)
sort(as.character(admin2.names$GADM))

# make a new factor with new levels containing the name updates at the corresponding indices of the IHME data frame
newNames = as.character(ihme.ests$adm2$ADM2_NAME)

for(i in 1:length(uniqueNames)) {
  newNames[uniqueIs[[i]]] = newUniqueNames[i]
}
tempNewNames = factor(newNames, levels=levels(admin2.names$GADM))

# add back in levels without matches
noMatches = is.na(tempNewNames)
noMatchNames = sort(unique(newNames[noMatches]))
print("IHME Admin2 names without GADM match:")
print(noMatchNames)
factor(newNames, levels=c(levels(admin2.names$GADM), noMatchNames))

# check to make sure the new names match the old ones except for fixed spelling
print(data.frame(ihme.ests$adm2$ADM2_NAME, newNames))

# replace the old names
ihme.ests$adm2$ADM2_NAME = newNames
