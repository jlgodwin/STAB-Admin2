rm(list = ls())

# Libraries ####
#devtools::install_github("bryandmartin/SUMMER",
#                        build_vignettes = F, force = T)
library(SUMMER)
#help(package = "SUMMER", help_type = "html")
#utils::browseVignettes(package = "SUMMER")
library(classInt)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(rgdal)
library(scales)
library(INLA)
library(survey)
library(ggplot2)
library(gridExtra)
library(parallel)
library(cartography)
library(rgeos)
library(stringr)

# Parameters ####
country <- "Myanmar"
beg.year <- 1990
end.year <- 2019
time.mod <- "rw2main_randomSlopes_rw1xICAR"
# time.mod <- "rw2main_randomSlopes_ar1xICAR"
usingGoogleSheets <- TRUE
doBenchmark <- TRUE


message("If have the same subfolder structure as 
        AfricaAdmin2Estimates/Data/countryDataFolders/. Do nothing!\n
        Otherwise, edit the following paths as needed.\n")


## Set relative paths ####

data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '../../Analysis/R'
ihme.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '../HIV/'
hand.dir.rel <- '../../Analysis/countryAnalysisFolders/'
ed.sub.dir <- "/excessDeathAdjustment/"
setwd(data.dir)

if(usingGoogleSheets){
  source(paste0(code.dir.rel,
                '/LoadCommandCenter.R'))
  CountryList <- range_read(sheet_key, sheet = "CountryList")
  SurveyInfo <- range_read(sheet_key, sheet = "SurveyInfo")
  HIV.sheet <- range_read(sheet_key, sheet = "HIV")
}else{
  
  ## If using please visit
  ## https://docs.google.com/spreadsheets/d/1GgrysoVHM2bO6DUZx8Cmj7WICKZ5KpTay0GOT72zK24/edit#gid=0
  ## and download most recent version of the .csv's
  CountryList <- read.csv('CountryList.csv')
  SurveyInfo <- read.csv('SurveyInfo.csv')
  HIV.sheet <- read.csv('HIV.csv')
}

folder.name <- CountryList$folderName[CountryList$Country == country]

hand.dir.rel <- paste0(hand.dir.rel,
                       gsub(" ", "", folder.name))

gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
n.survey <- CountryList$nSurvey[CountryList$Country == country]

## Use HIV Adjusted data? ####

HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])

if(usingGoogleSheets){
  useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                  unique(HIV.country$`UNAIDS data?`) == "Y")
}else{
  useHIVAdj <- (unique(HIV.country$MM.Adj.by.IGME) == "Y" & 
                  unique(HIV.country$UNAIDS.data.) == "Y")
}

## Get Survey years #### 
if(usingGoogleSheets){
  surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                        SurveyInfo$`GPS?` == "Y"]
  survey.legends <- SurveyInfo$`OfficialSurveyName`[SurveyInfo$Country == country &
                                                      SurveyInfo$`GPS?` == "Y"]
  
  frames <- SurveyInfo[SurveyInfo$Country == country, 
                       c("Survey Year", "Frame", "PropFrame?")]
  names(frames)[match("Survey Year", names(frames))] <- "Survey.Year"
  
  useGADM <- unique(SurveyInfo$useGADM[SurveyInfo$Country == country &
                                         SurveyInfo$`GPS?` == "Y"])
  if(useGADM == "N"){
    admin1poly.alt <- unique(SurveyInfo$Admin1Alt[SurveyInfo$Country == country &
                                                    SurveyInfo$`GPS?` == "Y"])
    admin2poly.alt <- unique(SurveyInfo$Admin2Alt[SurveyInfo$Country == country &
                                                    SurveyInfo$`GPS?` == "Y"])
  }
}else{
  surveys <- SurveyInfo$Survey.Year[SurveyInfo$Country == country &
                                      SurveyInfo$GPS. == "Y"]
  survey.legends <- SurveyInfo$OfficialSurveyName[SurveyInfo$Country == country &
                                                    SurveyInfo$GPS. == "Y"]
  
  frames <- SurveyInfo[SurveyInfo$Country == country, 
                       c("Survey.Year", "Frame","PropFrame.")]
  
  useGADM <- unique(SurveyInfo$useGADM[SurveyInfo$Country == country &
                                         SurveyInfo$GPS. == "Y"])
  
  if(useGADM == "N"){
    admin1poly.alt <- unique(SurveyInfo$Admin1Alt[SurveyInfo$Country == country &
                                                    SurveyInfo$GPS. == "Y"])
    admin2poly.alt <- unique(SurveyInfo$Admin2Alt[SurveyInfo$Country == country &
                                                    SurveyInfo$GPS. == "Y"])
  }
}


frames <- frames[!is.na(frames$Frame),]

## Universal plot parameters ####

cols <- rainbow(8+1+1+1)
cols <- cols[c(1,3,7,2,4,11,9,5,6,8,10)]

# Load polygon data ####

poly.file <- shapes.sub.dir

if(useGADM == "Y"){
  poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                           '0', sep = "_")
  poly.layer.adm1 <- paste('gadm36', gadm.abbrev,
                           '1', sep = "_")
  poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                           '2', sep = "_")
}else if(useGADM == "N"){
  poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                           '0', sep = "_")
  poly.layer.adm1 <- admin1poly.alt
  poly.layer.adm2 <- admin2poly.alt
}else{
  message("Fill out useGADM column in SurveyInfo.\n")
}


poly.path <- paste0(folder.name, poly.file)
poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0))
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1))
if(sum(grepl(paste('gadm36', gadm.abbrev,
                   '2', sep = "_"), 
             list.files(paste0(folder.name, poly.file)))) != 0){
  poly.adm2 <- readOGR(dsn = poly.path,
                       layer = as.character(poly.layer.adm2))
}

if(exists("poly.adm2")){
  proj4string(poly.adm0) <- 
    proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(folder.name, 
            shapes.sub.dir,
            '/',
            country, '_Amat.rda'),
     envir = .GlobalEnv)

load(paste0(folder.name, 
            shapes.sub.dir,
            '/',
            country, '_Amat_Names.rda'),
     envir = .GlobalEnv)

# Load model data ####

load(paste0(folder.name, '/',
            country,
            '_cluster_dat.rda'),
     envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
dat.years <- sort(unique(mod.dat$years))
beg.years <- seq(beg.year,
                 end.year, 5)
end.years <- beg.years + 4
periods <- paste(beg.years,
                 end.years,
                 sep = "-")
mod.dat$period <- as.character(cut(mod.dat$years,
                                   breaks = c(beg.years,
                                              beg.years[length(beg.years)]+5),
                                   include.lowest = T,
                                   right = F,
                                   labels = periods))
mod.dat$strata.orig <- mod.dat$strata
mod.dat$strata <- mod.dat$urban
mod.dat$country <- as.character(country)

## Universal plot parameters ####

ages <- levels(mod.dat$age)
age.cols <- rainbow(length(ages))
age.cols[2] <- "orange"

plot.years <- beg.year:end.year
obs.idx <- which(beg.year:end.year %in% mod.dat$years)
pred.idx <- which(!(beg.year:end.year %in% mod.dat$years))

pane.years <- seq(beg.year + 2, 
                  end.year - 2, 5)
est.idx <- which(seq(beg.year, 
                     end.year - 5, 5) < max(mod.dat$years))

# Load IGME ####

file.list <- list.files(igme.dir.rel)
igme.file <- file.list[grepl("Results.csv", file.list)]
igme.ests <- read.csv(paste0(igme.dir.rel,'/',igme.file),
                      header = T)
igme.ests <- igme.ests[igme.ests$Indicator== "Under-five Mortality Rate" &
                         igme.ests$Subgroup == "Total",]
names(igme.ests) <- gsub("X", "", names(igme.ests))

igme.ests <- igme.ests[igme.ests$Country.Name == country,]
a <- reshape(igme.ests,idvar = c("Country.Name", "Quantile"), 
             varying = list((1:dim(igme.ests)[2])[-c(1:5)]),
             v.names = "OBS_VALUE" ,direction = "long", 
             times = names(igme.ests)[-c(1:5)])
igme.ests <- reshape(a, idvar = c("time"),
                     v.names = "OBS_VALUE", 
                     timevar = "Quantile", direction = "wide")

names(igme.ests)[grepl(".Lower", names(igme.ests))] <- "LOWER_BOUND"
names(igme.ests)[grepl(".Upper", names(igme.ests))] <- "UPPER_BOUND"
names(igme.ests)[grepl(".Median", names(igme.ests))] <- "OBS_VALUE"
names(igme.ests)[grepl("time", names(igme.ests))] <- "REF_DATE"
igme.ests$year <- as.numeric(as.character(igme.ests$REF_DATE)) - 0.5
igme.ests <- igme.ests[order(igme.ests$year),]
igme.ests <- igme.ests[igme.ests$year %in% beg.year:end.year,]

# Load IHME ####

file.list <- list.files(ihme.dir.rel)
ihme.files <- file.list[grepl("IHME", file.list)]

ihme.ests <- list()
ihme.ests[['adm0']] <- read.csv( paste0(ihme.dir.rel, '/',
                                        ihme.files[grepl("ADM0_Y2019M10D16.CSV", ihme.files)]),
                                 header = T)
ihme.ests[['adm1']] <- read.csv( paste0(ihme.dir.rel, '/',
                                        ihme.files[grepl("ADM1_Y2019M10D16.CSV", ihme.files)]),
                                 header = T)

ihme.ests[['adm2']] <- read.csv( paste0(ihme.dir.rel,'/',
                                        ihme.files[grepl("ADM2_Y2019M10D16.CSV", ihme.files)]),
                                 header = T)

ihme.ests <- lapply(ihme.ests, function(x){
  if(!(country %in% x$ADM0_NAME)){
    message('\n Country name not found in one of the IHME files.\n')
  }
  if(country != "Cote dIvoire"){
    x[x$ADM0_NAME == country,]
  }else{
    x[x$ADM0_NAME %in% c(levels(ihme.ests[[1]]$ADM0_NAME)[20],
                         levels(ihme.ests[[2]]$ADM0_NAME)[6],
                         levels(ihme.ests[[3]]$ADM0_NAME)[6]),]
  }
})

doAdmin2 <- TRUE

## NB: these are handwritten files to compare
##     IHME to our estimates. Let me know if you
##     want these files

source(paste0(hand.dir.rel,
              '/IHMEHand_', country, '.R'))


if(!dir.exists(paths = paste0(folder.name,
                              '/Plots/',
                              'Betabinomial'))){
  dir.create(path = paste0(folder.name, 
                           '/Plots/',
                           'Betabinomial'))
  dir.create(path = paste0(folder.name, 
                           '/Plots/',
                           'Betabinomial/National'))
  dir.create(path = paste0(folder.name, 
                           '/Plots/',
                           'Betabinomial/Admin1'))
  if(exists("poly.adm2")){
    dir.create(path = paste0(folder.name, 
                             '/Plots/',
                             'Betabinomial/Admin2'))
    
  }
}


# Load Population data ####

# affected admin 2 indices

# affected_admin_2_names <- 
#   read.csv(paste0(ed_data_dir, "Myanmar_admin2_U1_5.csv"))$names[affected_admin_2_ind] %>%
#   as.character()
affected.adm2.idx<- c(1:5, 60:63)
pop.1to5.admin2 <- read.csv(paste0(folder.name,
                                   ed.sub.dir, 
                                   country,
                                   "_admin2_U1_5.csv"))
pop.1to5.admin2 <- pop.1to5.admin2[,-1]
names(pop.1to5.admin2) <- gsub("X", "", names(pop.1to5.admin2))
pop.1to5.admin2$region <- admin2.names$Internal[match(pop.1to5.admin2$names,
                                                      admin2.names$GADM)]
pop.1to5.admin2 <- pop.1to5.admin2 %>%
  mutate(adm2.idx = 1:nrow(pop.1to5.admin2)) %>%
  mutate(crisis = ifelse(adm2.idx %in% affected.adm2.idx,
                         1, 0)) %>%
  pivot_longer(cols = -c(names, region, crisis),
               names_to = "year", 
               values_to = "pop_1_5")


pop.0to1.admin2 <- read.csv(paste0(folder.name,
                                   ed.sub.dir, 
                                   country,
                                   "_admin2_U1.csv")) %>%
  dplyr::select(-X)

names(pop.0to1.admin2) <- gsub("X", "", names(pop.0to1.admin2))
pop.0to1.admin2$region <- admin2.names$Internal[match(pop.0to1.admin2$names,
                                                      admin2.names$GADM)]

pop.0to1.admin2 <- pop.0to1.admin2 %>%
  pivot_longer(names_to = "year", 
               values_to = "pop_0_1",
               -c(names,region))

# excess death mortality rate table -- to be computed!
pop <- pop.1to5.admin2 %>%
  filter(year == "2008") %>% 
  left_join(pop.0to1.admin2,
            by = c("names", "region", "year")) %>%
  mutate(pop_0_5 = pop_1_5 + pop_0_1)

igme.crisis <- read.csv(paste0(folder.name,
                               ed.sub.dir,
                               "Crisis_Under5_deaths2020.csv"))

natl_u5_ed <-  igme.crisis$Crisis.d0.5[igme.crisis$Year == 2008.5 &
                                         igme.crisis$ISO3Code == "MMR"]

# KEY STEP:
# compute excess death mortality rates from 2008 cyclone
edmr_tbl <- pop %>%
  mutate(ed_0_1 = (0.2) * natl_u5_ed * pop_0_1 * crisis / sum(pop_0_1 * crisis),
         ed_1_5 = (0.8) * natl_u5_ed * pop_1_5 * crisis / sum(pop_1_5 * crisis)) %>%
  mutate(ed_1m0 = ed_0_1 / pop_0_1,
         ed_4m1 = ed_1_5 / pop_1_5) %>%
  mutate(ed_1q0 = ed_1m0 / (1 + (1 - (0.3)) * ed_1m0),
         ed_4q1 = 4 * ed_4m1 / (1 + (4 - 4 * (0.4)) * ed_4m1)) %>%
  mutate(ed_5q0 = 1 - (1 - ed_1q0) * (1 - ed_4q1)) %>%
  rename(Admin2 = names) %>%
  mutate(Country = country)
edmr_tbl$Admin1 <- poly.adm2@data$NAME_1[match(edmr_tbl$Admin2,
                                               poly.adm2@data$NAME_2)]


## Load results
load(paste0(folder.name,'/',
            country,
            '_res_rw2_natlEDBench.rda'))
load(paste0(folder.name,'/',
            country,
            '_res_',
            time.mod,
            '_admin1EDBench.rda'))
load(paste0(folder.name,'/',
            country,
            '_res_',
            time.mod,
            '_admin2EDBench.rda'))


# Plots ####

centroids.adm1 <- gCentroid(poly.adm1,
                            byid = TRUE,
                            id = poly.adm1@data$NAME_1)

centroids.adm2 <- gCentroid(poly.adm2,
                            byid = TRUE,
                            id = poly.adm2@data$NAME_2)

## Affected Areas ####
poly.adm2@data$crisis <- edmr_tbl$crisis[match(poly.adm2@data$NAME_2,
                                               edmr_tbl$Admin2)]
poly.adm1@data$crisis <- ifelse(poly.adm1@data$NAME_1 %in%
                                  c("Ayeyarwady", "Yangon"),
                                1, 0)
pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_NargisAreas.pdf'))
par(lend=1, pin = c(1,3), mar = rep(0.05, 4))
plot(poly.adm1,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'grey80',
     col = 'grey80')
plot(poly.adm2[poly.adm2@data$NAME_1 == "Yangon",],
     border = 'white',
     col = alpha('firebrick', 0.8),
     lwd = .25, add = TRUE)
plot(poly.adm2[poly.adm2@data$NAME_1 == "Ayeyarwady",],
     border = 'white',
     col = alpha('navy', 0.8),
     lwd = .25, add = TRUE)
# plot(poly.adm1[poly.adm1@data$crisis == 1,],
#      border = 'navy', lwd = 0.5, add = TRUE)
# text(centroids.adm1$x, centroids.adm1$y,
#      labels = row.names(centroids.adm1),cex = 0.45)
legend('bottomleft',
       bty = 'n', cex = 0.85,
       fill = alpha(c("firebrick",
                      "navy"), 0.8),
       border = FALSE,
       legend = c("Yangon",
                  "Ayeyarwady"))
dev.off()


edmr_tbl_adm1 <- aggregate(cbind(pop_0_1, ed_0_1,
                             pop_1_5, ed_1_5) ~ Admin1,
                           data = edmr_tbl,
                           FUN = sum)
edmr_tbl_adm1$pop_0_5 <- edmr_tbl_adm1$pop_0_1 +
  edmr_tbl_adm1$pop_1_5

edmr_tbl_adm1$ed_0_5 <- edmr_tbl_adm1$ed_0_1 +
  edmr_tbl_adm1$ed_1_5

## Population ####
pop.pal <- brewer.pal(n = 9, name = "PuBu")
pop.int <- classIntervals(round(c(edmr_tbl_adm1$pop_0_5,
                                edmr_tbl$pop_0_5), 0),
                          n = 9,
                          style = 'jenks')
pop.col <- findColours(pop.int, pop.pal)

pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_Pop_AdminsSameScale.pdf'))
par(lend=1, pin = c(1,3),
    mar = rep(0.05, 4),
    mfrow = c(1,2))
plot(poly.adm1,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = pop.col[1:nrow(poly.adm1@data)])
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Yangon",],
#      border = 'white',
#      col = alpha('firebrick', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Ayeyarwady",],
#      border = 'white',
#      col = alpha('navy', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm1[poly.adm1@data$crisis == 1,],
#      border = 'navy', lwd = 0.5, add = TRUE)
# text(centroids.adm1$x, centroids.adm1$y,
#      labels = row.names(centroids.adm1),cex = 0.45)

plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = pop.col[-c(1:nrow(poly.adm1@data))])
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Yangon",],
#      border = 'white',
#      col = alpha('firebrick', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Ayeyarwady",],
#      border = 'white',
#      col = alpha('navy', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm1[poly.adm1@data$crisis == 1,],
#      border = 'navy', lwd = 0.5, add = TRUE)
# text(centroids.adm1$x, centroids.adm1$y,
#      labels = row.names(centroids.adm1),cex = 0.45)
legend('bottomleft',
       fill = pop.pal,
       legend = names(attr(pop.col, 'table')),
       border = FALSE,
       bty = 'n', 
       cex = 0.75)
dev.off()

### Admin2 ####
pop.pal <- brewer.pal(n = 9, name = "PuBu")
breaks <- seq(range(edmr_tbl$pop_0_5/1000)[1],
              range(edmr_tbl$pop_0_5/1000)[2],
              length.out = 10)
breaks[1] <- breaks[1] - .1
pop.int <- classIntervals(edmr_tbl$pop_0_5/1000,
                          n = 9,
                          style = 'fixed',
                          fixedBreaks = signif(breaks,2),
                          intervalClosure = "left")
pop.col <- findColours(pop.int, pop.pal)

pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_Pop_Admin2.pdf'))
par(lend=1, pin = c(1,3),
    mar = rep(0.05, 4),
    mfrow = c(1,1))
plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = pop.col)
legend('bottomleft',
       fill = pop.pal,
       legend = names(attr(pop.col, 'table')),
       border = FALSE,
       bty = 'n', 
       cex = 0.75)
dev.off()


## Population Density ####
edmr_tbl$dens_0_5 <- edmr_tbl$pop_0_5/sum(edmr_tbl$pop_0_5)
edmr_tbl_adm1$dens_0_5 <- edmr_tbl_adm1$pop_0_5/sum(edmr_tbl_adm1$pop_0_5)
dens.pal <- brewer.pal(n = 9, name = "PuBu")
dens.int <- classIntervals(round(c(edmr_tbl_adm1$dens_0_5,
                                edmr_tbl$dens_0_5), 2),
                          n = 9,
                          style = 'jenks')
dens.col <- findColours(dens.int, dens.pal)

pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_Dens_AdminsSameScale.pdf'))
par(lend=1, pin = c(1,3),
    mar = rep(0.05, 4),
    mfrow = c(1,2))
plot(poly.adm1,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = dens.col[1:nrow(poly.adm1@data)])
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Yangon",],
#      border = 'white',
#      col = alpha('firebrick', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Ayeyarwady",],
#      border = 'white',
#      col = alpha('navy', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm1[poly.adm1@data$crisis == 1,],
#      border = 'navy', lwd = 0.5, add = TRUE)
# text(centroids.adm1$x, centroids.adm1$y,
#      labels = row.names(centroids.adm1),cex = 0.45)

plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = dens.col[-c(1:nrow(poly.adm1@data))])
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Yangon",],
#      border = 'white',
#      col = alpha('firebrick', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Ayeyarwady",],
#      border = 'white',
#      col = alpha('navy', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm1[poly.adm1@data$crisis == 1,],
#      border = 'navy', lwd = 0.5, add = TRUE)
# text(centroids.adm1$x, centroids.adm1$y,
#      labels = row.names(centroids.adm1),cex = 0.45)
legend('bottomleft',
       fill = dens.pal,
       legend = names(attr(dens.col, 'table')),
       border = FALSE,
       bty = 'n', 
       cex = 0.75)
dev.off()

### Admin2 ####
dens.pal <- brewer.pal(n = 5, name = "PuBu")
breaks <- seq(range(edmr_tbl$dens_0_5)[1],
              range(edmr_tbl$dens_0_5)[2],
              length.out = 6)
breaks[c(1,6)] <- c(0, 0.05)
dens.int <- classIntervals(edmr_tbl$dens_0_5,
                          n = 5,
                          style = 'fixed',
                          fixedBreaks = round(breaks,2),
                          intervalClosure = "left")
dens.col <- findColours(dens.int, dens.pal)

pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_Dens_Admin2.pdf'))
par(lend=1, pin = c(1,3),
    mar = rep(0.05, 4),
    mfrow = c(1,1))
plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = dens.col)
legend('bottomleft',
       fill = dens.pal,
       legend = names(attr(dens.col, 'table')),
       border = FALSE,
       bty = 'n', 
       cex = 0.75)
dev.off()



## Excess deaths ####
edmr_tbl$ed_0_5 <- edmr_tbl$ed_0_1 + edmr_tbl$ed_1_5
ed.pal <- brewer.pal(n = 7, name = "PuBu")
ed.int <- classIntervals(round(c(edmr_tbl_adm1$ed_0_5,
                               edmr_tbl$ed_0_5), 0),
                          n = 7,
                          style = 'jenks')
ed.col <- findColours(ed.int, ed.pal)

pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_ED_AdminsSameScale.pdf'))
par(lend=1, pin = c(1,3),
    mar = rep(0.05, 4),
    mfrow = c(1,2))
plot(poly.adm1,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = ed.col[1:nrow(poly.adm1@data)])
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Yangon",],
#      border = 'white',
#      col = alpha('firebrick', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Ayeyarwady",],
#      border = 'white',
#      col = alpha('navy', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm1[poly.adm1@data$crisis == 1,],
#      border = 'navy', lwd = 0.5, add = TRUE)
# text(centroids.adm1$x, centroids.adm1$y,
#      labels = row.names(centroids.adm1),cex = 0.45)

plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = ed.col[-c(1:nrow(poly.adm1@data))])
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Yangon",],
#      border = 'white',
#      col = alpha('firebrick', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm2[poly.adm2@data$NAME_1 == "Ayeyarwady",],
#      border = 'white',
#      col = alpha('navy', 0.8),
#      lwd = .25, add = TRUE)
# plot(poly.adm1[poly.adm1@data$crisis == 1,],
#      border = 'navy', lwd = 0.5, add = TRUE)
# text(centroids.adm1$x, centroids.adm1$y,
#      labels = row.names(centroids.adm1),cex = 0.45)
legend('bottomleft',
       fill = ed.pal,
       legend = names(attr(ed.col, 'table')),
       border = FALSE,
       bty = 'n', 
       cex = 0.75)
dev.off()

### Admin2 ####

breaks <- seq(range(edmr_tbl$ed_0_5)[1],
              range(edmr_tbl$ed_0_5)[2],
              length.out = 8)
breaks[2] <- 0

ed.int <- classIntervals(edmr_tbl$ed_0_5,
                          n = 7,
                          style = 'fixed',
                          fixedBreaks = signif(breaks, 2),
                          intervalClosure = "right")
ed.col <- findColours(ed.int, ed.pal)

pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_ED_Admin2.pdf'))
par(lend=1, pin = c(1,3),
    mar = rep(0.05, 4),
    mfrow = c(1,1))
plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = ed.col)
legend('bottomleft',
       fill = ed.pal,
       legend = names(attr(ed.col, 'table')),
       border = FALSE,
       bty = 'n', 
       cex = 0.75)
dev.off()

## Estimates ####


res.natl$overall$region.name <- country
res.natl$overall$level <- "National"
res.natl$overall$IHME <- ihme.ests$adm0$mean[ihme.ests$adm0$year == 2008]
res.admin1$overall$region.name <- admin1.names$GADM[match(res.admin1$overall$region,
                                                          admin1.names$Internal)]
res.admin1$overall$level <- "Admin1"
res.admin1$overall$IHME <- ihme.ests$adm1$mean[match(res.admin1$overall$region.name,
                                                     ihme.ests$adm1$ADM1_NAME)]

res.admin2$overall <- res.admin2$overall[, -grep("na.rm", names(res.admin2$overall))]
res.admin2$overall$region.name <- admin2.names$GADM[match(res.admin2$overall$region,
                                                          admin2.names$Internal)]
res.admin2$overall$level <- "Admin2"
res.admin2$overall$IHME <- ihme.ests$adm2$mean[match(res.admin2$overall$region.name,
                                                     ihme.ests$adm2$ADM2_NAME)]
plot.data <- rbind.data.frame(res.natl$overall,
                              res.admin1$overall,
                              res.admin2$overall)
plot.data <- plot.data %>%
  filter(years.num == 2008)

est.pal <- brewer.pal(n = 9, "YlGnBu")
est.int <- classIntervals(round(c(plot.data$median,
                            plot.data$IHME)*1000, 1),
                          n = 9,
                          intervalClosure = "left",
                          style = 'fixed',
                          fixedBreaks = round(seq(35,175,
                                                  length.out = 10),1))
est.col <- findColours(est.int, est.pal)

plot.data$est.col <- est.col[1:nrow(plot.data)]
plot.data$IHME.col <- est.col[-c(1:nrow(plot.data))]


pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_IHMECompare_National.pdf'))
par(lend=1, pin = c(1,3),
    mar = c(0.05, 0.05, 0.1, 0.05),
    mfrow = c(1,2))
plot(poly.adm0,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = plot.data$est.col[plot.data$level == "National"])
plot(poly.adm0,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = plot.data$IHME.col[plot.data$level == "National"])
legend('bottomleft',
       bty = 'n', cex = .75,
       fill = est.pal, border = FALSE,
       legend = names(attr(est.col, 'table')))
dev.off()

pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_IHMECompare_Admin1.pdf'))
par(lend=1, pin = c(1,3),
    mar = c(0.05, 0.05, 0.1, 0.05),
    mfrow = c(1,2))
plot(poly.adm1,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = plot.data$est.col[plot.data$level == "Admin1"])
plot(poly.adm1,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = plot.data$IHME.col[plot.data$level == "Admin1"])
legend('bottomleft',
       bty = 'n', cex = .75,
       fill = est.pal, border = FALSE,
       legend = names(attr(est.col, 'table')))
dev.off()


pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_IHMECompare_Admin2.pdf'))
par(lend=1, pin = c(1,3),
    mar = c(0.05, 0.05, 0.1, 0.05),
    mfrow = c(1,2))
plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = plot.data$est.col[plot.data$level == "Admin2"])
plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = plot.data$IHME.col[plot.data$level == "Admin2"])
legend('bottomleft',
       bty = 'n', cex = .75,
       fill = est.pal, border = FALSE,
       legend = names(attr(est.col, 'table')))
dev.off()

## Deaths ####
for(level in c("National", "Admin1", "Admin2")){
  if(level == "National"){
    pop.vec <- sum(edmr_tbl$pop_0_5)
  }else if(level == "Admin1"){
    pop.vec <- edmr_tbl_adm1$pop_0_5
  }else{
    pop.vec <- edmr_tbl$pop_0_5
  }
  plot.data$deaths[plot.data$level == level] <-
    plot.data$median[plot.data$level == level]*pop.vec
  
  plot.data$IHME.deaths[plot.data$level == level] <-
    plot.data$IHME[plot.data$level == level]*pop.vec
}

deaths.pal <- brewer.pal(n = 9, "PuBu")
breaks.end <- range(plot.data[plot.data$level %in% c("Admin2"),
                              c("deaths", "IHME.deaths")]/1000)
breaks <- seq(breaks.end[1], breaks.end[2], length.out = 10)
deaths.int <- classIntervals(round(c(plot.data$deaths[plot.data$level == "Admin2"],
                                  plot.data$IHME.deaths[plot.data$level == "Admin2"])/1000, 1),
                          n = 9,
                          intervalClosure = "left",
                          style = 'fixed',
                          fixedBreaks = round(breaks,1))
deaths.col <- findColours(deaths.int, deaths.pal)


pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_IHMEDeathsCompare_Admin2.pdf'))
par(lend=1, pin = c(1,3),
    mar = c(0.05, 0.05, 0.1, 0.05),
    mfrow = c(1,2))
plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = deaths.col[1:nrow(poly.adm2@data)])
plot(poly.adm2,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = deaths.col[-c(1:nrow(poly.adm2@data))])
legend('bottomleft',
       bty = 'n', cex = .75,
       fill = deaths.pal, border = FALSE,
       legend = names(attr(deaths.col, 'table')))
dev.off()


breaks.end <- range(plot.data[plot.data$level %in% c("Admin1"),
                              c("deaths", "IHME.deaths")]/1000)
breaks <- seq(breaks.end[1], breaks.end[2], length.out = 10)

deaths.int <- classIntervals(round(c(plot.data$deaths[plot.data$level == "Admin1"],
                                     plot.data$IHME.deaths[plot.data$level == "Admin1"])/1000, 1),
                             n = 9,
                             intervalClosure = "left",
                             style = 'fixed',
                             fixedBreaks = round(breaks,1))
deaths.col <- findColours(deaths.int, deaths.pal)


pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_IHMEDeathsCompare_Admin1.pdf'))
par(lend=1, pin = c(1,3),
    mar = c(0.05, 0.05, 0.1, 0.05),
    mfrow = c(1,2))
plot(poly.adm1,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = deaths.col[1:nrow(poly.adm1@data)])
plot(poly.adm1,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F, lwd = 0.25,
     border = 'white',
     col = deaths.col[-c(1:nrow(poly.adm1@data))])
legend('bottomleft',
       bty = 'n', cex = .75,
       fill = deaths.pal, border = FALSE,
       legend = names(attr(deaths.col, 'table')))
dev.off()
