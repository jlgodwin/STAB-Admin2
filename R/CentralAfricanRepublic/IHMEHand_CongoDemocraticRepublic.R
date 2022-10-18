#### Admin1 ####
#IHME is using pre-2010 provinces (n=11), while GADM is using post-2010 provinces (n=26). Even where names match, geographies probably don't.
n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))

#### Admin2 ####
#IHME is using pre-2010 provinces (n=48), while GADM is using post-2010 provinces (n=240). Even where names match, geographies probably don't.
n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))