#  SmoothedDirect.R
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

## Libraries ####
# devtools::install_github("bryandmartin/SUMMER",
#                          build_vignettes = F, force = T)
library(SUMMER)
# help(package = "SUMMER", help_type = "html")
# utils::browseVignettes(package = "SUMMER")
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


## Parameters ####

country <- "Togo"
beg.year <- 1990
end.year <- 2019
usingGoogleSheets <- TRUE
doBenchmark <- TRUE

### Set relative paths ####

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

### Get Survey years #### 
if(usingGoogleSheets){
  surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
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


### Use HIV Adjusted data? ####

HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])

if(usingGoogleSheets){
  useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                  unique(HIV.country$`UNAIDS data?`) == "Y")
}else{
  useHIVAdj <- (unique(HIV.country$MM.Adj.by.IGME) == "Y" & 
                  unique(HIV.country$UNAIDS.data.) == "Y")
}

## Load polygon data ####
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
beg.years <- seq(beg.year,
                 end.year,5)
end.years <- beg.years + 4
periods <- paste(beg.years,
                 end.years, sep = "-")

mod.dat$period <- as.character(cut(mod.dat$years,
                                   breaks = c(beg.years, 
                                              beg.years[length(beg.years)]+5),
                                   include.lowest = T,
                                   right = F, labels = periods))


### Load direct estimates ####

files.list <- list.files(paste0('./', folder.name))

if(useHIVAdj){
  if(sum(grep("HIV_", files.list)) == 0){
    warning("No HIV adjusted files founds.")
    load(paste0(folder.name, '/', country, '_direct_natl_yearly.rda'))
    load(paste0(folder.name, '/', country, '_direct_natl.rda'))
    load(paste0(folder.name, '/', country, '_direct_admin1.rda'))
  }else{
    load(paste0(folder.name, '/', country, '_directHIV_natl_yearly.rda'))
    load(paste0(folder.name, '/', country, '_directHIV_natl.rda'))
    load(paste0(folder.name, '/', country, '_directHIV_admin1.rda'))
  }
}else{
  load(paste0(folder.name, '/', country, '_direct_natl_yearly.rda'))
  load(paste0(folder.name, '/', country, '_direct_natl.rda'))
  load(paste0(folder.name, '/', country, '_direct_admin1.rda'))
}

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


## Aggregate surveys ####
data.natl <- aggregateSurvey(direct.natl)
data.natl.yearly <- aggregateSurvey(direct.natl.yearly)
data.admin1 <- aggregateSurvey(direct.admin1)


## Fit smoothing model ####

proj.per <- paste(end.year+1, end.year+5, sep = "-")

### National ####
fit.natl <- smoothDirect(data.natl, Amat = NULL,
                         type.st = 4,
                         year_label = c(periods, proj.per),
                         year_range = c(beg.year, end.year + 5),
                         is.yearly = FALSE)
res.natl <- getSmoothed(fit.natl, 
                        year_range = c(beg.year, end.year+5),
                        year_label = c(periods, proj.per))
res.natl$years.num <- seq(beg.year+2,
                          end.year+5,
                          5)
res.natl$region.gadm <- country
head(res.natl)
tail(res.natl)

file.out <- paste0(country,
                   "_res_natl_SmoothedDirect.rda")
save(res.natl,
     file = paste0(folder.name,
                   '/', file.out))

fit.natl.yearly <- smoothDirect(data.natl.yearly,
                                Amat = NULL,
                                year_label = as.character(beg.year:(end.year + 5)),
                                year_range = c(beg.year, end.year + 5),
                                is.yearly = F)
res.natl.yearly <- getSmoothed(fit.natl.yearly,
                               year_range = c(beg.year, end.year + 5),
                               year_label = as.character(beg.year:(end.year + 5)))
res.natl.yearly$years.num <- beg.year:(end.year + 5)
res.natl.yearly$region.gadm <- country
head(res.natl.yearly)
tail(res.natl.yearly)

file.out <- paste0(country,
                   "_res_natl_yearly_SmoothedDirect.rda")
save(res.natl.yearly,
     file = paste0(folder.name,
                   '/', file.out))


### Admin1 ####

fit.admin1 <- smoothDirect(data.admin1,
                           Amat = admin1.mat,
                           type.st = 4,
                           year_label = c(periods, "2020-2024"),
                           year_range = c(1990, 2024),
                           is.yearly = F)
res.admin1 <- getSmoothed(fit.admin1,
                          Amat = admin1.mat,
                          year_range = c(1990, 2024),
                          year_label = c(periods, "2020-2024"))
res.admin1$years.num <- seq(beg.year+2, end.year+5,
                            5)[match(res.admin1$years,
                                     c(periods, "2020-2024"))]
res.admin1$region.gadm <- admin1.names$GADM[match(res.admin1$region,
                                                  admin1.names$Internal)]
head(res.admin1)
tail(res.admin1)

file.out <- paste0(country,
                   "_res_admin1_SmoothedDirect.rda")
save(res.admin1,
     file = paste0(folder.name, 
                   '/', file.out))

## Benchmarking ####

if(doBenchmark){
  
  benchmark <- igme.period <- 
    data.frame(period = periods,
               median = NA)
  names(benchmark) <- c("years", "ratio")
  
  for (i in 1:length(periods)) {
    igme.period$median[i] <- median(igme.ests$OBS_VALUE[which(igme.ests$year %in% 
                                                                (beg.year + c(((i-1)*5):(5*i - 1))))]/1000)
  }
  
  benchmark$ratio <- data.natl$mean/igme.period$median
  
  ### National ####
  data.natl <- getAdjusted(data.natl,
                           ratio = benchmark,
                           logit.lower = NULL,
                           logit.upper = NULL,
                           prob.upper = "upper",
                           prob.lower = "lower")
  fit.natl <- smoothDirect(data.natl, 
                           Amat = NULL,
                           type.st = 4,
                           year_label = c(periods,
                                          proj.per),
                           year_range = c(beg.year,
                                          end.year + 5),
                           is.yearly = F)
  res.natl <- getSmoothed(fit.natl, 
                          year_label = c(periods,
                                         proj.per),
                          year_range = c(beg.year,
                                         end.year + 5))
  res.natl$years.num <- seq(beg.year+2,
                            end.year+5, 5)
  res.natl$region.gadm <- country
  head(res.natl)
  tail(res.natl)
  
  
  file.out <- paste0(country,
                     "_res_natlBench_SmoothedDirect.rda")
  
  save(res.natl, 
       file = paste0(folder.name,
                     '/', file.out))
  ## Admin 1 ####
  data.admin1 <- getAdjusted(data.admin1,
                             ratio = benchmark,
                             logit.lower = NULL,
                             logit.upper = NULL,
                             prob.upper = "upper",
                             prob.lower = "lower")
  
  fit.admin1 <- smoothDirect(data.admin1,
                             Amat = admin1.mat,
                             type.st = 4,
                             year_label = c(periods,
                                            proj.per),
                             year_range = c(beg.year,
                                            end.year + 5),
                             is.yearly = F)
  
  res.admin1 <- getSmoothed(fit.admin1,
                            Amat = admin1.mat,
                            year_label = c(periods,
                                           proj.per),
                            year_range = c(beg.year,
                                           end.year + 5))
  res.admin1$years.num <- seq(beg.year+2, end.year+5,
                              5)[match(res.admin1$years,
                                       c(periods, proj.per))]
  res.admin1$region.gadm <- admin1.names$GADM[match(res.admin1$region,
                                                    admin1.names$Internal)]
  head(res.admin1)
  tail(res.admin1)
  
  file.out <- paste0(country,
                     "_res_admin1Bench_SmoothedDirect.rda")
  
  save(res.admin1,
       file = paste0(folder.name,
                     '/', file.out))
  
  ## National Yearly ####
  benchmark <- data.frame(years = beg.year:end.year,
                          ratio = NA)
  benchmark$ratio <- data.natl.yearly$mean/(igme.ests$OBS_VALUE/1000)[match(beg.year:end.year,
                                                                            igme.ests$year)]
  data.natl.yearly <- getAdjusted(data.natl.yearly,
                                  ratio = benchmark,
                                  logit.lower = NULL,
                                  logit.upper = NULL,
                                  prob.upper = "upper",
                                  prob.lower = "lower")
  fit.natl.yearly <- smoothDirect(data.natl.yearly,
                                  Amat = NULL,
                                  type.st = 4,
                                  year_label = as.character(beg.year:(end.year + 5)),
                                  year_range = c(beg.year,
                                                 (end.year + 5)),
                                  is.yearly = F)
  res.natl.yearly<- getSmoothed(fit.natl.yearly, 
                                year_label = as.character(beg.year:(end.year + 5)),
                                year_range = c(beg.year, (end.year + 5)))
  res.natl.yearly$years.num <- beg.year:(end.year+5)
  res.natl.yearly$region.gadm <- country
  head(res.natl.yearly)
  tail(res.natl.yearly)
  
  file.out <- paste0(country,
                     "_res_natlBench_yearly_SmoothedDirect.rda")
  save(res.natl.yearly,
       file = paste0(folder.name,
                     '/', file.out))
  
}

## SUMMER plots ####
if(!dir.exists(paths = paste0(folder.name,
                              '/Plots/',
                              'SmoothedDirect'))){
  dir.create(path = paste0(folder.name, 
                           '/Plots/',
                           'SmoothedDirect'))
}

### National ####
pdf(paste0(folder.name,
           "/Plots/SmoothedDirect/",
           country,
           '_natl_SmoothedDirect.pdf'),
    height = 6, width = 6)
plot(res.natl,
     is.yearly = F,
     is.subnational = F)
dev.off()

### Admin 1 ####
pdf(paste0(folder.name,
           "/Plots/SmoothedDirect/",
           country,
           '_admin1_SmoothedDirect.pdf'),
    height = 6, width = 6)
tmp.res <- res.admin1
tmp.res$region <- tmp.res$region.gadm
plot(tmp.res,
     is.yearly = F,
     is.subnational = T) 
dev.off()

## Spaghetti Plots ####

### National ####
cols <- rainbow(n.survey)
plot.years <- seq(beg.year + 2,
                  end.year, 5)

pdf(paste0(folder.name,
           "/Plots/SmoothedDirect/",
           country, 
           '_natl_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow = c(1,1))
  
  direct.natl$width <- direct.natl$upper - direct.natl$lower
  direct.natl$cex2 <- median(direct.natl$width, na.rm = T)/direct.natl$width
  direct.natl$cex2[direct.natl$cex2 > 6] <- 6
  
  if(dim(direct.natl)[1] != 0 &
     !(sum(is.na(direct.natl$mean)) == nrow(direct.natl))){
    plot.max <- max(direct.natl$upper+.025, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  if(nrow(direct.natl) > 0 &
     sum(is.na(direct.natl$mean)) == nrow(direct.natl)){
    plot(NA,
         xlab = "Year",
         ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year + 5),
         type = 'l',
         col = cols[svy.idx],
         lwd = 2,
         main = country)
    
    igme.years <- jitter(beg.year:max(igme.ests$year))
    lines(igme.years,
          igme.ests$OBS_VALUE/1000,
          lwd = 2, col  = 'grey37')
    lines(igme.years,
          igme.ests$UPPER_BOUND/1000,
          lwd = 1, lty = 2,
          col  = 'grey37')
    lines(igme.years, 
          igme.ests$LOWER_BOUND/1000,
          lwd = 1, lty = 2,
          col  = 'grey37')
    
    ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
    lines(ihme.years,
          ihme.ests[[1]]$mean,
          lwd = 2,
          col  = 'darkgrey')
    lines(ihme.years,
          ihme.ests[[1]]$lower,
          lty = 2,
          col  = 'darkgrey')
    lines(ihme.years,
          ihme.ests[[1]]$upper,
          lty = 2,
          col  = 'darkgrey')
    
    
    legend('topright',
           bty = 'n',
           col = c(cols,
                   'grey37',
                   'darkgrey',
                   'black'),
           lwd = 2, lty = 1,
           legend = c(surveys,
                      "UN IGME",
                      "IHME",
                      "Smoothed"))
    
  }
  else {
    for(survey in surveys){
      tmp <- direct.natl[direct.natl$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      pane.years <- jitter(plot.years)
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year",
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year + 5),
               type = 'l',
               col = cols[svy.idx],
               lwd = 2,
               main = country)
          
          lines(pane.years, tmp$mean,
                cex = tmp$cex2,
                type = 'l',
                col = cols[svy.idx],
                lwd = 2)
          
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          
          
          igme.years <- jitter(beg.year:max(igme.ests$year))
          lines(igme.years,
                igme.ests$OBS_VALUE/1000,
                lwd = 2, col  = 'grey37')
          lines(igme.years,
                igme.ests$UPPER_BOUND/1000,
                lty = 2, col  = 'grey37')
          lines(igme.years,
                igme.ests$LOWER_BOUND/1000,
                lty = 2, col  = 'grey37')
          
          ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
          lines(ihme.years,
                ihme.ests[[1]]$mean,
                lwd = 2, col  = 'darkgrey')
          lines(ihme.years,
                ihme.ests[[1]]$lower,
                lty = 2, col  = 'darkgrey')
          lines(ihme.years,
                ihme.ests[[1]]$upper, 
                lty = 2, col  = 'darkgrey')
          
        }else{
          plot(NA,
               xlab = "Year",
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l',
               col = cols[svy.idx],
               lwd = 2,
               main = country)
        }
      }else{
        if(dim(tmp)[1] != 0){
          lines(pane.years, tmp$mean,
                cex = tmp$cex2,
                type = 'l',
                col = cols[svy.idx],
                lwd = 2)
          points(pane.years, tmp$mean,
                 pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
        } 
      }
      
      
    }
  }
  
  lines(res.natl$years.num,
        res.natl$median,
        col = 'black', lwd = 2)
  lines(res.natl$years.num,
        res.natl$upper,
        col = 'black', lty = 2)
  lines(res.natl$years.num,
        res.natl$lower, 
        col = 'black', lty = 2)
  legend('topright',
         bty = 'n',
         col = c(cols,
                 'grey37',
                 'darkgrey',
                 'black'),
         lwd = 2, 
         legend = c(surveys,
                    "UN IGME",
                    "IHME",
                    "Smoothed"))
  
}
dev.off()


pdf(paste0(folder.name,
           "/Plots/SmoothedDirect/",
           country,
           '_natl_yearly_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  direct.natl.yearly$width <- direct.natl.yearly$upper - 
    direct.natl.yearly$lower
  direct.natl.yearly$cex2 <- median(direct.natl.yearly$width,
                                    na.rm = T)/direct.natl.yearly$width
  direct.natl.yearly$cex2[direct.natl.yearly$cex2 > 6] <- 6
  
  if(dim(direct.natl.yearly)[1] != 0 & 
     !(sum(is.na(direct.natl.yearly$mean)) == nrow(direct.natl.yearly))){
    plot.max <- max(direct.natl.yearly$upper+.025, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  if (nrow(direct.natl.yearly) > 0 &
      sum(is.na(direct.natl.yearly$mean)) == nrow(direct.natl.yearly)) {
    plot(NA,
         xlab = "Year",
         ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year + 5),
         type = 'l', lwd = 2,
         col = cols[svy.idx], 
         main = country)
    
    igme.years <- jitter(beg.year:max(igme.ests$year))
    lines(igme.years,
          igme.ests$OBS_VALUE/1000,
          lwd = 2, col  = 'grey37')
    lines(igme.years,
          igme.ests$UPPER_BOUND/1000,
          lty = 2, col  = 'grey37')
    lines(igme.years,
          igme.ests$LOWER_BOUND/1000,
          lty = 2, col  = 'grey37')
    
    ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
    lines(ihme.years,
          ihme.ests[[1]]$mean,
          lwd = 2, col  = 'darkgrey')
    lines(ihme.years,
          ihme.ests[[1]]$lower,
          lty = 2, col  = 'darkgrey')
    lines(ihme.years, 
          ihme.ests[[1]]$upper,
          lty = 2, col  = 'darkgrey')
    
    
    legend('topright',
           bty = 'n',
           col = c(cols,
                   'grey37',
                   'darkgrey',
                   'black'),
           lwd = 2,
           legend = c(surveys,
                      "UN IGME",
                      "IHME",
                      "Smoothed"))
    
  } else {
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      pane.years <- jitter(as.numeric(tmp$years))
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year",
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year + 5),
               type = 'l',
               col = cols[svy.idx],
               lwd = 2,
               main = country)
          
          lines(pane.years, tmp$mean,
                cex = tmp$cex2,
                type = 'l',
                col = cols[svy.idx],
                lwd = 2)
          
          points(pane.years, tmp$mean,
                 pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          
          
          igme.years <- jitter(beg.year:max(igme.ests$year))
          lines(igme.years,
                igme.ests$OBS_VALUE/1000,
                lwd = 2, col  = 'grey37')
          lines(igme.years,
                igme.ests$UPPER_BOUND/1000,
                lty = 2, col  = 'grey37')
          lines(igme.years,
                igme.ests$LOWER_BOUND/1000, 
                lty = 2, col  = 'grey37')
          
          ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
          lines(ihme.years,
                ihme.ests[[1]]$mean,
                lwd = 2, col  = 'darkgrey')
          lines(ihme.years,
                ihme.ests[[1]]$lower, 
                lty = 2, col  = 'darkgrey')
          lines(ihme.years,
                ihme.ests[[1]]$upper, 
                lty = 2, col  = 'darkgrey')
        }else{
          plot(NA,
               xlab = "Year",
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l',
               col = cols[svy.idx],
               lwd = 2,
               main = country)
        }
      }else{
        if(dim(tmp)[1] != 0){
          lines(pane.years, tmp$mean,
                cex = tmp$cex2,
                type = 'l',
                col = cols[svy.idx],
                lwd = 2)
          points(pane.years, tmp$mean,
                 pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
        } 
      }
      
      
    }
    
    legend('topright',
           bty = 'n',
           col = c(cols,
                   'grey37',
                   'darkgrey',
                   'black'),
           lwd = 2,
           legend = c(surveys,
                      "UN IGME",
                      "IHME",
                      "Smoothed"))
  }
  
  lines(res.natl.yearly$years.num,
        res.natl.yearly$median,
        col = 'black', lwd = 2)
  lines(res.natl.yearly$years.num,
        res.natl.yearly$upper,
        col = 'black', lty = 2)
  lines(res.natl.yearly$years.num,
        res.natl.yearly$lower,
        col = 'black', lty = 2)
}
dev.off()

### Admin 1 ####

pdf(paste0(folder.name,
           "/Plots/SmoothedDirect/",
           country,
           '_admin1_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow = c(1,1),
      lend = 1)
  for(area in 1:dim(poly.adm1)[1]){
    tmp.area <- direct.admin1[direct.admin1$region == 
                                as.character(admin1.names$Internal[area]),]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == 
                                 as.character(admin1.names$GADM[area]),]
    
    res.area <- res.admin1[res.admin1$region == 
                             as.character(admin1.names$Internal[area]),]
    
    if(dim(tmp.area)[1] != 0 &
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(tmp.area$upper+.025, na.rm = T)
    }else{
      plot.max <- 0.25
    }
    
    if(nrow(tmp.area) > 0 &
       sum(is.na(tmp.area$mean)) == nrow(tmp.area)){
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 5),
           type = 'l',
           col = cols[svy.idx],
           lwd = 2,
           main = admin1.names$GADM[area])
      legend('topright',
             bty = 'n',
             col = c(cols,
                     'black'),
             lwd = 2, 
             legend = c(surveys, 
                        "Smoothed"))
      
    } else {
      for(survey in surveys){
        tmp <- tmp.area[tmp.area$surveyYears == survey,]
        svy.idx <- match(survey, surveys) 
        pane.years <- jitter(plot.years)
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year + 5),
                 type = 'l',
                 col = cols[svy.idx],
                 lwd = 2,
                 main = admin1.names$GADM[area])
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  type = 'l',
                  col = cols[svy.idx],
                  lwd = 2)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            ihme.years <- jitter(tmp.ihme$year)
            lines(ihme.years, tmp.ihme$mean,
                  lwd = 2, col  = 'darkgrey')
            lines(ihme.years, tmp.ihme$lci, 
                  lty = 2, col  = 'darkgrey')
            lines(ihme.years, tmp.ihme$uci, 
                  lty = 2, col  = 'darkgrey')
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l',
                 col = cols[svy.idx],
                 lwd = 2,
                 main = admin1.names$GADM[area])
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  type = 'l',
                  col = cols[svy.idx],
                  main = surveys[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
    }
    
    lines(res.area$years.num,
          res.area$median,
          col = 'black', lwd = 2)
    lines(res.area$years.num,
          res.area$upper,
          col = 'black', lty = 2)
    lines(res.area$years.num,
          res.area$lower,
          col = 'black', lty = 2)
    
    legend('topright',
           bty = 'n',
           col = c(cols,
                   'darkgrey',
                   'black'),
           lwd = 2, 
           legend = c(surveys,
                      'IHME',
                      "Smoothed"))
    
  }
}
dev.off()




pdf(paste0(folder.name,
           "/Plots/SmoothedDirect/",
           country,
           '_admin1_SmoothedDirect_spaghetti2.pdf'),
    height = 4, width = 8)
{
  par(mfrow = c(2,4),
      lend = 1)
  for(area in 1:dim(poly.adm1)[1]){
    tmp.area <- direct.admin1[direct.admin1$region == 
                                as.character(admin1.names$Internal[area]),]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == 
                                 as.character(admin1.names$GADM[area]),]
    
    
    res.area <- res.admin1[res.admin1$region == 
                             as.character(admin1.names$Internal[area]),]
    
    if(dim(tmp.area)[1] != 0 &
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(tmp.area$upper+.025, na.rm = T)
    }else{
      plot.max <- 0.25
    }
    
    if (nrow(tmp.area) > 0 &
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 5),
           type = 'l',
           col = cols[svy.idx], 
           lwd = 2,
           main = admin1.names$GADM[area])
      legend('topright',
             bty = 'n',
             col = c(cols,
                     'black'),
             lwd = 2,
             legend = c(surveys,
                        "Smoothed"))
      
    } else {
      for(survey in surveys){
        tmp <- tmp.area[tmp.area$surveyYears == survey,]
        svy.idx <- match(survey, surveys) 
        pane.years <- jitter(plot.years)
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year + 5),
                 type = 'l', 
                 col = cols[svy.idx],
                 lwd = 2,
                 main = admin1.names$GADM[area])
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  type = 'l',
                  col = cols[svy.idx],
                  main = surveys[svy.idx], 
                  lwd = 2)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            ihme.years <- jitter(tmp.ihme$year)
            lines(ihme.years, tmp.ihme$mean,
                  lwd = 2, col  = 'darkgrey')
            lines(ihme.years, tmp.ihme$lci,
                  lty = 2, col  = 'darkgrey')
            lines(ihme.years, tmp.ihme$uci,
                  lty = 2, col  = 'darkgrey')
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l',
                 col = cols[svy.idx],
                 lwd = 2,
                 main = admin1.names$GADM[area])
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  type = 'l',
                  col = cols[svy.idx],
                  main = surveys[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
      }
      
    }
    
    lines(res.area$years.num,
          res.area$median,
          col = 'black', lwd = 2)
    lines(res.area$years.num,
          res.area$upper,
          col = 'black', lty = 2)
    lines(res.area$years.num,
          res.area$lower,
          col = 'black', lty = 2)
    
    legend('topright',
           bty = 'n',
           col = c(cols,
                   'grey37',
                   'black'),
           lwd = 2, 
           legend = c(surveys,
                      "IHME",
                      "Smoothed"),
           cex = .5)
  }
}
dev.off()

## Polygon plots ####
### Admin 1 #### 
med.palette <- brewer.pal(n = 7, name = "Purples")
med.int <- classIntervals(round(res.admin1$median, 3),
                          n = 7, style = 'jenks')
med.col <- findColours(med.int, med.palette)

pdf(paste0(folder.name,
           "/Plots/SmoothedDirect/",
           country,
           '_admin1_', 
           'SmoothedDirect_poly.pdf'),
    height = 4, width = 8)
{
  par(mfrow = c(2,4),
      lend = 1)
  for(year in periods){
    idx <- which(res.admin1$years == year) 
    plot(poly.adm1, border = F, col = med.col[idx],
         axes = F, main = year)
  }
  plot(NA, xlim = c(0,1),
       ylim = c(0,1), 
       axes = F, xlab = "", ylab = "")
  legend(x = "center",
         inset = 0,
         legend = names(attr(med.col, 'table')),
         fill = med.palette,
         cex= .75, horiz = FALSE, 
         bty = 'n')
}
dev.off()

