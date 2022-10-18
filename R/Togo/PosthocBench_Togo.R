#  DirectEstimates.R
#  author: Jessica Godwin
#  
#  sources: LoadCommandCenter.R
#           IHMEHand_CountryName.R
#
#' loads: all files in /shapeFiles_gadm 
#'        CountryName_Amat_Names.rda
#'        CountryName_Amat.rda
#'        CountryName_cluster_dat.rda
#'        Data/HIV/HIVAdjustments.rda
#'        Analysis/R/Results.csv (IGME estimates)
#'        Analysis/R/ADM0_Y2019M10D16.CSV (IHME estimates)
#'        Analysis/R/ADM1_Y2019M10D16.CSV
#'        Analysis/R/ADM2_Y2019M10D16.CSV



rm(list = ls())
# devtools::install_github("bryandmartin/SUMMER",
#                          build_vignettes = F, force = T)


## Libraries ####
library(SUMMER)
#help(package = "SUMMER", help_type = "html")
#utils::browseVignettes(package = "SUMMER")
library(classInt)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(rgdal)
library(scales)
library(survey)

## Parameters ####

country <- "Togo"
beg.year <- 1990
end.year <- 2019
usingGoogleSheets <- TRUE

## NB: Relative file paths defined

data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '../../Analysis/R'
ihme.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '../HIV/'
hand.dir.rel <- '../../Analysis/countryAnalysisFolders/'

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

#### Get Survey years #### 
if(usingGoogleSheets){
  surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                        SurveyInfo$`GPS?` == "Y"]
  
  frames <- SurveyInfo[SurveyInfo$Country == country, 
                       c("Survey Year", "Frame", "PropFrame?")]
  names(frames)[match("Survey Year", names(frames))] <- "Survey.Year"
}else{
  surveys <- SurveyInfo$Survey.Year[SurveyInfo$Country == country &
                                      SurveyInfo$GPS. == "Y"]
  frames <- SurveyInfo[SurveyInfo$Country == country, 
                       c("Survey.Year", "Frame","PropFrame.")]
}

frames <- frames[!is.na(frames$Frame),]

#### Do HIV Adjustment? ####

HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])

if(usingGoogleSheets){
  doHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                 unique(HIV.country$`UNAIDS data?`) == "Y")
}else{
  doHIVAdj <- (unique(HIV.country$MM.Adj.by.IGME) == "Y" & 
                 unique(HIV.country$UNAIDS.data.) == "Y")
}

## Load polygon data ####
poly.file <- shapes.sub.dir
poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                         '0', sep = "_")
poly.layer.adm1 <- paste('gadm36', gadm.abbrev,
                         '1', sep = "_")
poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                         '2', sep = "_")

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
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(folder.name, 
            shapes.sub.dir, '/',
            country, '_Amat.rda'))
load(paste0(folder.name,
            shapes.sub.dir, '/',
            country, '_Amat_Names.rda'))

## Load data ####

load(paste0(folder.name,'/',
            country,'_cluster_dat.rda'))
mod.dat$years <- as.numeric(as.character(mod.dat$years))
dat.years <- sort(unique(mod.dat$years))
beg.years <- seq(1990,2015,5)
end.years <- beg.years + 4
periods <- paste(beg.years, end.years, sep = "-")
mod.dat$period <- as.character(cut(mod.dat$years,
                                   breaks = c(beg.years, beg.years[length(beg.years)]+5),
                                   include.lowest = T,
                                   right = F, labels = periods))

### Load IGME ####
{
  file.list <- list.files(igme.dir.rel)
  igme.file <- file.list[grepl("Results.csv", file.list)]
  igme.ests <- read.csv(paste0(igme.dir.rel,'/',igme.file),
                        header = T)
  # igme.ests <- igme.ests[igme.ests$INDICATOR == "Under-five mortality rate" &
  #                          igme.ests$SEX == "Total" &
  #                          igme.ests$SERIES_YEAR == "2019" &
  #                          igme.ests$SERIES_NAME == "UN IGME estimate 2019",]
  # igme.ests$year <- igme.ests$REF_DATE - 0.5
  # if(country == "Cote dIvoire"){
  #   igme.ests <- igme.ests[igme.ests$REF_AREA == levels(igme.ests$REF_AREA)[45],]  
  # }else{
  #   igme.ests <- igme.ests[igme.ests$REF_AREA == country, ]
  # }
  # igme.ests <- igme.ests[order(igme.ests$year),]
  # igme.ests <- igme.ests[igme.ests$year %in% beg.year:end.year,]
  
  
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
  
}

### Load IHME####
{
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
  if(country == "Malawi"){
    ihme.ests$adm1 <- ihme.ests$adm2
    ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
    doAdmin2 <- FALSE
  }
  
  ## NB: these are handwritten files to compare
  ##     IHME to our estimates. Let me know if you
  ##     want these files
  
  source(paste0(hand.dir.rel,
                '/IHMEHand_', country, '.R'))
  
}  

### Load BB8 ####

load(paste0(folder.name, '/',
            country,
            '_res_rw2main_randomSlopes_rw1xICAR_admin1_CI90.rda'))

load(paste0(folder.name, '/',
            country,
            '_res_rw2main_randomSlopes_rw1xICAR_admin2_CI90.rda'))
load(paste0(folder.name, '/',
            country, '_res_rw2_natl.rda'))

benchmark <- data.frame(year = beg.year:end.year,
                        est = NA,
                        igme = NA,
                        ratio = NA)
# Benchmarking ####
yr.idx <- 0
for(year in beg.year:end.year){
  yr.idx <- yr.idx + 1
  benchmark$est[yr.idx] <- res.natl$overall$median[match(year,
                                                         res.natl$overall$years.num)]
  benchmark$igme[yr.idx] <- igme.ests$OBS_VALUE[igme.ests$Country.Name == country &
                                                  igme.ests$year == year]/1000
}

benchmark$ratio <- benchmark$est/benchmark$igme


res.admin1$overall[,c('median', 'mean', 'upper', 'lower')] <-
  res.admin1$overall[,c('median', 'mean', 'upper', 'lower')]/benchmark$ratio[match(res.admin1$overall$years.num,
                                                                                   benchmark$year)]
res.admin2$overall[,c('median', 'mean', 'upper', 'lower')] <-
  res.admin2$overall[,c('median', 'mean', 'upper', 'lower')]/benchmark$ratio[match(res.admin2$overall$years.num,
                                                                                   benchmark$year)]

res.admin1$overall$variance <-
  res.admin1$overall$variance/(benchmark$ratio[match(res.admin1$overall$years.num,
                                                     benchmark$year)]^2)
res.admin2$overall$variance <-
  res.admin2$overall$variance/(benchmark$ratio[match(res.admin2$overall$years.num,
                                                     benchmark$year)]^2)                                                                                   

save(res.admin1,
     file = paste0(folder.name, '/',
                   country,
                   '_res_rw2main_randomSlopes_rw1xICAR_admin1posthocBench_CI90.rda'))
write.csv(res.admin1$overall, row.names = FALSE,
          file = paste0(folder.name, '/',
                        country,
                        '_res_rw2main_randomSlopes_rw1xICAR_admin1posthocBench_CI90.csv'))
save(res.admin2,
     file = paste0(folder.name, '/',
                   country,
                   '_res_rw2main_randomSlopes_rw1xICAR_admin2posthocBench_CI90.rda'))
write.csv(res.admin2$overall, row.names = FALSE,
          file =  paste0(folder.name, '/',
                         country,
                         '_res_rw2main_randomSlopes_rw1xICAR_admin2posthocBench_CI90.csv'))

## Plots ####
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country,
           '_posthocBenchcheck_admin1.pdf'),
    height = 6, width = 6)

plot.lims <- range(res.admin1$overall$median - 
                     benchmark$igme[match(res.admin1$overall$years.num,
                                          benchmark$year)]) + c(-0.01, 0.01)
plot.lims <- plot.lims*1000

plot(NA,
     xlim = c(beg.year, end.year),
     ylim = plot.lims,
     xlab = "Year",
     ylab = "Deviation from IGME")
abline(h = 0)
cols <- rainbow(nrow(admin1.names))
for(area in 1:nrow(admin1.names)){
  points(beg.year:end.year,
         1000*res.admin1$overall$median[res.admin1$overall$region == 
                                          admin1.names$Internal[area]] -
           1000*benchmark$igme,
         pch = 19, col = alpha(cols[area], 0.5))
}

legend('topright',
       bty = 'n',
       ncol = 2,
       cex = 0.6,
       pch = rep(19, nrow(admin1.names)),
       col = alpha(cols,0.5),
       legend = c(admin1.names$GADM))
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country,
           '_posthocBenchcheck_admin2.pdf'),
    height = 6, width = 6)

plot.lims <- range(res.admin2$overall$median - 
                     benchmark$igme[match(res.admin2$overall$years.num,
                                          benchmark$year)]) + c(-0.01, 0.01)
plot.lims <- plot.lims*1000

plot(NA,
     xlim = c(beg.year, end.year),
     ylim = plot.lims,
     xlab = "Year",
     ylab = "Deviation from IGME")
abline(h = 0)
cols <- rainbow(nrow(admin2.names))
for(area in 1:nrow(admin2.names)){
  points(beg.year:end.year,
         1000*res.admin2$overall$median[res.admin2$overall$region == 
                                          admin2.names$Internal[area]] -
           1000*benchmark$igme,
         pch = 19, col = alpha(cols[area], 0.5))
}
dev.off()

### Load old bench ####

load(paste0(folder.name, '/',
            country,
            '_res_rw2main_randomSlopes_rw1xICAR_admin1Bench.rda'))

load(paste0(folder.name, '/',
            country,
            '_res_rw2main_randomSlopes_rw1xICAR_admin2Bench.rda'))


pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country,
           '_Benchcheck_admin1.pdf'),
    height = 6, width = 6)

plot.lims <- range(res.admin1$overall$median - 
                     benchmark$igme[match(res.admin1$overall$years.num,
                                          benchmark$year)]) + c(-0.01, 0.01)
plot.lims <- plot.lims*1000

plot(NA,
     xlim = c(beg.year, end.year),
     ylim = plot.lims,
     xlab = "Year",
     ylab = "Deviation from IGME")
abline(h = 0)
cols <- rainbow(nrow(admin1.names))
for(area in 1:nrow(admin1.names)){
  points(beg.year:end.year,
         1000*res.admin1$overall$median[res.admin1$overall$region == 
                                          admin1.names$Internal[area]] -
           1000*benchmark$igme,
         pch = 19, col = alpha(cols[area], 0.5))
}

legend('topright',
       bty = 'n',
       ncol = 2,
       cex = 0.6,
       pch = rep(19, nrow(admin1.names)),
       col = alpha(cols,0.5),
       legend = c(admin1.names$GADM))
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country,
           '_Benchcheck_admin2.pdf'),
    height = 6, width = 6)

plot.lims <- range(res.admin2$overall$median - 
                     benchmark$igme[match(res.admin2$overall$years.num,
                                          benchmark$year)]) + c(-0.01, 0.01)
plot.lims <- plot.lims*1000

plot(NA,
     xlim = c(beg.year, end.year),
     ylim = plot.lims,
     xlab = "Year",
     ylab = "Deviation from IGME")
abline(h = 0)
cols <- rainbow(nrow(admin2.names))
for(area in 1:nrow(admin2.names)){
  points(beg.year:end.year,
         1000*res.admin2$overall$median[res.admin2$overall$region == 
                                          admin2.names$Internal[area]] -
           1000*benchmark$igme,
         pch = 19, col = alpha(cols[area], 0.5))
}
dev.off()

