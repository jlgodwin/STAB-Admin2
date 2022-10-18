#' filename: postProcess.R
#' @author Jessica Godwin
#'  
#' last edit: 20210131
#'   
#' sources: LoadCommandCenter.R
#'          IHMEHand_CountryName.R
#'
#' loads: all files in /shapeFiles_gadm 
#'        CountryName_Amat_Names.rda
#'        CountryName_Amat.rda
#'        CountryName_cluster_dat.rda
#'        Data/HIV/HIVAdjustments.rda
#'        CountryName_HIVnames.key.rda (if exists)
#'        Results.csv
#'  
#' @param country character, country name
#' @param beg.year integer/numeric beginning of period of estimation
#' @param end.year integer/numeric end of period of estimation
#' @param usingGoogleSheets logical TRUE if using googlesheets interface
#'                                  FALSE if loading .csv's
#' @param cluster logical TRUE if running on cluster (set's filepaths)
#'                        FALSE if on home machine, define your own relative paths
#' @param type.st integer/numeric can take values 1, 2, 3, 4, represents which
#'                        spacetime interaction model will be used
#'                        defined by Knorr-Held (2000)
#' @param doBenchmark logical if TRUE, benchmark to UN IGME
#' @param doRandomSlopesRW1 logical if TRUE & type.st %in% c(3,4)
#'                                  spacetime interaction with
#'                                  RW1 temporal model is fit
#' @param doRandomSlopesAR1 logical if TRUE & type.st %in% c(3,4)
#'                                  spacetime interaction with
#'                                  AR1 temporal model is fit  
#' @param doNatl logical if TRUE fit national model (RW2)
#' @param doAdmin1 logical if TRUE fit subnational model to Admin1
#' @param doAdmin2 logcal if TRUE fit subnational model to Admin2 
#' @param refit logical if TRUE, refit the unbenchmarked model
#' @param refitBench logical if TRUE, refit benchmarking model
#' @param loadSamples logical if TRUE, load posterior samples objects
#'                             (useful if you need to refit the 
#'                             benchmarking model and do not want to
#'                             refit the unbenchmarked model.) 
#' @param data.dir character filepath of the directory which contains
#'                           folders for each country each of which contains
#'                           cluster level data, i.e.
#'                           paste0(data.dir,
#'                           country, "/CountryName_cluster_dat.rda")
#' @param code.dir.rel character relative filepath from data.dir to 
#'                               the directory containing
#'                              LoadCommandCenter.R     
#' @param igme.dir.rel character relative filepath from data.dir to 
#'                               the directory IGME estimates
#'                               Results.csv
#' @param shapes.sub.dir character subdirectory of data.dir
#'                                 containing
#'                                 CountryName_Amat.rda
#'                                 CountryName_Amat_Names.rda
#'                                 i.e, paste0(data.dir,
#'                                             country, shapes.sub.dir,
#'                                             '/CountryName_Amat.rda') 
#' @param hiv.dir.rel character relative path from data.dir to directory 
#'                              containing 
#'                              HIVAdjustments.rda
#' @param hand.dir.rel character relative path from data.dir to directory
#'                               containing 
#'                               IHMEHand_CountryName.R                                             

## NB: which time model did you fit for 
##     Admin1 or Admin2 areas in Betabinomial.R

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

# Parameters ####
country <- "Pakistan"
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
                           '1_excluding_disputed', sep = "_")
  poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                           '2_excluding_disputed', sep = "_")
  
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

# National ####

### Load Betabinomial ####
load(paste0(folder.name, '/',
            country,
            '_rw2_natl.rda'),
     envir = .GlobalEnv)
load(paste0(folder.name, '/',
            country,
            '_res_rw2_natl.rda'),
     envir = .GlobalEnv)

### Load Direct ####

if(useHIVAdj){
  load(paste0(folder.name, '/',
              country,
              '_directHIV_natl_yearly.rda'),
       envir = .GlobalEnv)
}else{
  load(paste0(folder.name, '/',
              country, 
              '_direct_natl_yearly.rda'), 
       envir = .GlobalEnv)
}

### Load SmoothedDirect ####
load(paste0(folder.name, '/',
            country,
            '_res_natl_yearly_SmoothedDirect.rda'),
     envir = .GlobalEnv)

## Convert to U5MR (# of children per 1000)
res.natl.yearly[,c("median", "lower", "upper")] <-
  res.natl.yearly[,c("median", "lower", "upper")]*1000
direct.natl.yearly[,c("mean", "upper", "lower")] <-
  direct.natl.yearly[,c("mean", "upper", "lower")]*1000

## Save smaller model objects ####
hyperpar.table <- fit.natl$fit$summary.hyperpar
save(hyperpar.table,
     file = paste0(folder.name, '/', 
                   country, '_rw2',
                   '_natl_noStrata_hyperpar.rda'))

fixed.eff <- fit.natl$fit$summary.fixed
save(fixed.eff,
     file = paste0(folder.name, '/', 
                   country, '_rw2',
                   '_natl_noStrata_fixed.rda'))

## Spaghetti Plot ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/National/',
           country,
           '_rw2_natl_noStrata_spaghetti.pdf'), 
    height = 8, width = 8)
{
  tmp.area <- res.natl$overall
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  tmp.ihme <- ihme.ests[[1]]
  tmp.ihme[,c("mean", "lower","upper")] <- 
    tmp.ihme[,c("mean", "lower","upper")]*1000

  tmp.area$median <- tmp.area$median*1000
  tmp.area$upper <- tmp.area$upper*1000
  tmp.area$lower <- tmp.area$lower*1000
  
  
  par(mfrow = c(2,2),
      lend=1)
  
  ## Set plot dimensions y-axis
  if(dim(tmp.area)[1] != 0 &
     !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(direct.natl.yearly$mean + 25, na.rm = T)
  }else{
    plot.max <- 25
  }
  
  
  ### Noodles ####  
  #### Plot Direct estimates ####
  for(survey in surveys){
    
    tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
    svy.idx <- match(survey, surveys) 
    
    if(svy.idx== 1){
      if(dim(tmp)[1] != 0){
        plot(NA,
             xlab = "Year",
             ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year),
             main = "")
        
        lines(plot.years,
              tmp$mean[1:length(plot.years)], 
              cex = tmp$cex2,
              col = cols[svy.idx])
        
        points(plot.years,
               tmp$mean[1:length(plot.years)], 
               pch = 19,
               col = alpha(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        
      }else{
        plot(NA,
             xlab = "Year",
             ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year),
             main =  country)
      }
    }else{
      if(dim(tmp)[1] != 0){
        lines(plot.years,
              tmp$mean[1:length(plot.years)], 
              cex = tmp$cex2,
              col = cols[svy.idx])
        points(plot.years,
               tmp$mean[1:length(plot.years)], 
               pch = 19,
               col = alpha(cols[svy.idx], 0.35),
               cex = tmp$cex2)
      } 
    }
    
  }
  
  #### Add IHME ####
  
  ihme.years <- (tmp.ihme$year)
  lines(ihme.years, tmp.ihme$mean,
        lwd = 2, col = cols[9])
  
  #### Add IGME ####
  igme.years <- (igme.ests$year)
  lines(igme.years, igme.ests$OBS_VALUE,
        lty = 1, lwd = 2, col = cols[10])
  
  #### Add results ####
  
  ## Smooth Direct
  lines(plot.years[obs.idx],
        res.natl.yearly$median[obs.idx], 
        col = cols[11],
        lwd = 2)
  lines(plot.years[pred.idx], 
        res.natl.yearly$median[pred.idx], 
        col = cols[11], 
        lwd = 2, lty = 2)
  
  ## Betabinomial RW2
  lines(plot.years[obs.idx],
        tmp.area$median[obs.idx],
        col = 'black',
        lwd = 2)
  lines(plot.years[pred.idx], 
        tmp.area$median[pred.idx], 
        col = 'black', 
        lwd = 2, lty = 2)
  
  ### Direct Uncertainty ####
  #### Add Direct Estimates ####
  for(survey in surveys){
    tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
    tmp.est.idx <- which(!is.na(tmp$mean))
    svy.idx <- match(survey, surveys) 
    
    if(svy.idx == 1){
      if(dim(tmp)[1] != 0){
        plot(NA,
             xlab = "Year", 
             ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year),
             main = "")
        
        polygon(x = c(plot.years[tmp.est.idx],
                      rev(plot.years[tmp.est.idx])),
                y = c(tmp$upper[tmp.est.idx],
                      rev(tmp$lower[tmp.est.idx])),
                col = alpha(cols[svy.idx], 0.25),
                border = FALSE)
        
      }else{
        plot(NA,
             xlab = "Year", 
             ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year),
             main = country)
      }
    }else{
      polygon(x = c(plot.years[tmp.est.idx],
                    rev(plot.years[tmp.est.idx])),
              y = c(tmp$upper[tmp.est.idx],
                    rev(tmp$lower[tmp.est.idx])),
              col = alpha(cols[svy.idx], 0.25),
              border = FALSE)
    }
    
  }  
  
  #### Add Betabinomial ####
  polygon(x = c(tmp.area$years.num,
                rev(tmp.area$years.num)),
          y = c(tmp.area$upper,
                rev(tmp.area$lower)),
          col = alpha('black', 0.25),
          border = FALSE)
  
  ### Smoothed Uncertainty ####  
  
  plot(NA,
       xlab = "Year",
       ylab = "U5MR",
       ylim = c(0, plot.max),
       xlim = c(beg.year, end.year),
       main = "")
  
  #### Add IHME ####
  ihme.years <- (tmp.ihme$year)
  polygon(x = c(ihme.years, rev(ihme.years)),
          y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
          col = alpha(cols[9], 0.25),
          border = FALSE)
  
  
  #### Add IGME ####
  igme.years <- (igme.ests$year)
  polygon(x = c(igme.years, rev(igme.years)),
          y = c(igme.ests$UPPER_BOUND,
                rev(igme.ests$LOWER_BOUND)),
          col = alpha(cols[10], 0.25),
          border = FALSE)
  
  #### Add results ####
  ## Smoothed direct
  polygon(x = c(plot.years,
                rev(plot.years)),
          y = c(res.natl.yearly$upper[1:length(plot.years)],
                rev(res.natl.yearly$lower[1:length(plot.years)])),
          col = alpha(cols[11], 0.25),
          border = FALSE)
  
  ## Betabinomial
  polygon(x = c(plot.years,
                rev(plot.years)),
          y = c(tmp.area$upper,
                rev(tmp.area$lower)),
          col = alpha('black', 0.25),
          border = FALSE)
  lines(tmp.area$years.num[obs.idx], 
        tmp.area$median[obs.idx],
        col = 'black',
        lwd = 2, lty = 1)
  lines(tmp.area$years.num[pred.idx], 
        tmp.area$median[pred.idx], 
        col = 'black', 
        lwd = 2, lty = 2)
  
  ### Legend ####
  plot(NA, 
       xlim = c(0,1),
       ylim = c(0,1),
       axes = F,
       xlab = "", ylab = "")
  legend('center',
         bty = 'n',
         pch = c(rep(19, length(surveys)),
                 rep(NA, 4)),
         lty = c(rep(1, length(surveys)),
                 rep(NA, 4)),
         col = c(alpha(cols[1:length(surveys)], .25),
                 rep(NA, 4)),
         fill = c(rep(NA, length(surveys)),
                  alpha(c(cols[(9:11)],
                          'black'), .25)),
         border = c(rep(NA, length(surveys)),
                    cols[9:11],
                    'black'), cex = 1,
         legend = c(survey.legends,
                    'IHME', 
                    'IGME',
                    'Smoothed Direct', 
                    'Betabinomial'))
}
dev.off() 


## Hazards over time ####

temporals <- getDiag(fit.natl,
                     field = "time",
                     year_label = plot.years)

pdf(paste0(folder.name,
           '/Plots/Betabinomial/National/',
           country,
           '_rw2_natl_temporal.pdf'), 
    height = 5, width = 7.5)
{
  
  for(age in ages){
    age.idx <- grepl(paste0(age),
                     temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), 
                       row.names(fit.natl$fit$summary.fixed))
    fixed.eff <- fit.natl$fit$summary.fixed$`0.5quant`[fixed.idx]
    tmp <- temporals[age.idx,]
    
    if(match(age, ages) == 1){
      plot.min <- min(outer(temporals$median,
                            fit.natl$fit$summary.fixed$`0.5quant`,
                            FUN="+")) - 0.25
      plot.max <- max(outer(temporals$median, 
                            fit.natl$fit$summary.fixed$`0.5quant`, 
                            FUN="+")) + 0.25
      
      par(mfrow =c(1,1),
          lend=1)
      plot(NA,
           xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           yaxt = 'n',
           xlab = "Year",
           ylab = "Monthly hazard",
           main = "")
      labs <- round(expit(seq(plot.min, plot.max, 1))*1000,1)
      axis(2, at = seq(plot.min, plot.max, 1),
           labels = labs)
    }
    
    lines(plot.years,
          tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], 
          lwd = 2)
    
  }
  
  legend('bottomleft', 
         cex = 0.65,
         lwd = 2,
         col = c(age.cols),
         legend = c(ages), 
         bty = 'n')
}
dev.off()

## Fixed Effects ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/National/',
           country,
           '_rw2_natl_coefs.pdf'),
    height = 5, width = 5)

{
  plot.idx <- 0
  for(age in ages){
    plot.idx <- plot.idx + 1
    age.idx <- grepl(paste0(age),
                     temporals$group)
    fixed.idx <- grepl(paste0(age,"$"),
                       row.names(fit.natl$fit$summary.fixed))
    fixed.eff <- fit.natl$fit$summary.fixed[fixed.idx,]
    tmp <- temporals[age.idx,]
    
    if(plot.idx == 1){
      plot.min <- min(fit.natl$fit$summary.fixed$`0.025quant`) - 0.025
      plot.max <- max(fit.natl$fit$summary.fixed$`0.975quant`) + 0.025
      
      par(mfrow =c(1,1), 
          lend=1)
      plot(NA,
           xlim = c(0, length(ages) + 1),
           xaxt = 'n',
           yaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Age",
           ylab = "",
           main = "")
      axis(1, at = 1:length(ages),
           labels = ages, cex.axis = 0.7)
      axis(2, at = seq(plot.min, plot.max, 1),
           labels = round(expit(seq(plot.min, plot.max, 1))*1000,
                          digits = 1))
    }
    
    
    points(plot.idx, 
           fixed.eff$`0.5quant`,
           pch = 19, col = age.cols[plot.idx])
    segments(plot.idx,
             fixed.eff$`0.025quant`,
             plot.idx,
             fixed.eff$`0.975quant`,
             col = age.cols[plot.idx])
    
  }
  
}
dev.off()


# National Benchmark ####

### Load Betabinomial #### 
load(paste0(folder.name, '/',
            country,
            '_rw2_natlBench.rda'))
load(paste0(folder.name,'/',
            country,
            '_res_rw2_natlBench.rda'))
load(paste0(folder.name, '/',
            country, 
            '_rw2_natlBenchmarks.rda'))

### Load Smoothed Direct ####
load(paste0(folder.name, '/',
            country, 
            '_res_natlBench_yearly_SmoothedDirect.rda'))
res.natl.yearly[,c("median", "lower", "upper")] <-
  res.natl.yearly[,c("median", "lower", "upper")]*1000

## Save smaller model objects ####

hyperpar.table <- fit.natl$fit$summary.hyperpar
save(hyperpar.table,
     file = paste0(folder.name, '/', 
                   country,
                   '_rw2',
                   '_natlBench_noStrata_hyperpar.rda'))

fixed.eff <- fit.natl$fit$summary.fixed
save(fixed.eff,
     file = paste0(folder.name, '/', 
                   country,
                   '_rw2',
                   '_natlBench_noStrata_fixed.rda'))

## Benchmarks ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/National/',
           country, 
           '_rw2_natlBenchmarks.pdf'), 
    height = 4, width = 4)
{ 
  par(lend=1)
  plot(NA,
       xlim = c(beg.year, end.year),
       ylim = c(min(bench.adj$ratio) - 0.025,
                max(bench.adj$ratio) + 0.025),
       xlab = "Year",
       ylab = "Offset",
       main = "")
  abline(h = 1)
  
  bench.tmp <- bench.adj$ratio
  bench.tmp[bench.tmp == 1] <- NA
  lines(plot.years,
        bench.tmp,
        lwd = 2, 
        col = 'red')
}
dev.off()

## Spaghetti Plot ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/National/',
           country,
           '_rw2_natlBench_noStrata_spaghetti.pdf'),
    height = 8, width = 8)
{
  tmp.area <- res.natl$overall
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  tmp.ihme <- ihme.ests[[1]]
  tmp.ihme[,c("mean", "lower","upper")] <- 
    tmp.ihme[,c("mean", "lower","upper")]*1000
  
  tmp.area$median <- tmp.area$median*1000
  tmp.area$upper <- tmp.area$upper*1000
  tmp.area$lower <- tmp.area$lower*1000
  
  if(dim(tmp.area)[1] != 0 &
     !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(direct.natl.yearly$mean + 25, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  par(mfrow = c(2,2),
      lend=1)
  if (nrow(tmp.area) > 0 & 
      sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
    plot(NA,
         xlab = "Year",
         ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year),
         main = "")
  } else {
    
    ### Noodles ####
    #### Add direct estimates ####
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year",
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               main = "")
          
          lines(plot.years, tmp$mean, 
                cex = tmp$cex2,
                col = cols[svy.idx],
                main = surveys[svy.idx])
          
          points(plot.years, tmp$mean,
                 pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          
        }else{
          plot(NA,
               xlab = "Year",
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               main =  country)
        }
      }else{
        if(dim(tmp)[1] != 0){
          lines(plot.years, tmp$mean,
                cex = tmp$cex2,
                col = cols[svy.idx])
          points(plot.years, tmp$mean,
                 pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
        } 
      }
      
    }
    
    #### Add IHME ####
    ihme.years <- (tmp.ihme$year)
    lines(ihme.years, tmp.ihme$mean,
          lwd = 2, col = cols[9])
    
    #### Add IGME ####
    igme.years <- (igme.ests$year)
    lines(igme.years, igme.ests$OBS_VALUE,
          lwd = 2, col = cols[10])
    
    #### Add results ####

    lines(plot.years[obs.idx],
          res.natl.yearly$median[obs.idx],
          col = cols[11],
          lwd = 2)
    lines(plot.years[pred.idx],
          res.natl.yearly$median[pred.idx],
          col = cols[11], 
          lwd = 2, lty = 2)
    lines(tmp.area$years.num[obs.idx], 
          tmp.area$median[obs.idx],
          col = 'black',
          lwd = 2)
    lines(tmp.area$years.num[pred.idx],
          tmp.area$median[pred.idx],
          col = 'black', 
          lwd = 2, lty = 2)
    
    ### Direct Uncertainty ####
    #### Add Direct Estimates ####
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      tmp.est.idx <- which(!is.na(tmp$mean))
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx == 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", 
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               main = "")
          
          polygon(x = c(plot.years[tmp.est.idx],
                        rev(plot.years[tmp.est.idx])),
                  y = c(tmp$upper[tmp.est.idx],
                        rev(tmp$lower[tmp.est.idx])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
          
        }else{
          plot(NA,
               xlab = "Year", 
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               main = country)
          
          lines(plot.years, tmp$mean, 
                cex = tmp$cex2,
                col = cols[svy.idx],
                lwd = 2)
          points(plot.years, tmp$mean,
                 pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
        }
      }else{
        polygon(x = c(plot.years[tmp.est.idx],
                      rev(plot.years[tmp.est.idx])),
                y = c(tmp$upper[tmp.est.idx],
                      rev(tmp$lower[tmp.est.idx])),
                col = alpha(cols[svy.idx], 0.25),
                border = FALSE)
      }
      
    }  
    
    #### Add Betabinomial ####
    polygon(x = c(tmp.area$years.num,
                  rev(tmp.area$years.num)),
            y = c(tmp.area$upper,
                  rev(tmp.area$lower)),
            col = alpha('black', 0.25),
            border = FALSE)
    
    
    ### Smoothed Uncertainty ####  
    
    plot(NA,
         xlab = "Year",
         ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year),
         main = "")
    
    #### Add IHME ####
    ihme.years <- (tmp.ihme$year)
    polygon(x = c(ihme.years, rev(ihme.years)),
            y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
            col = alpha(cols[9], 0.25),
            border = FALSE)
    
    
    #### Add IGME ####
    igme.years <- (igme.ests$year)
    polygon(x = c(igme.years, rev(igme.years)),
            y = c(igme.ests$UPPER_BOUND,
                  rev(igme.ests$LOWER_BOUND)),
            col = alpha(cols[10], 0.25),
            border = FALSE)
    #### Add results ####
    ## Smoothed direct
    polygon(x = c(plot.years,
                  rev(plot.years)),
            y = c(res.natl.yearly$upper[1:length(plot.years)],
                  rev(res.natl.yearly$lower[1:length(plot.years)])),
            col = alpha(cols[11], 0.25),
            border = FALSE)
    
    ## Betabinomial
    polygon(x = c(plot.years, rev(plot.years)),
            y = c(tmp.area$upper, rev(tmp.area$lower)),
            col = alpha('black', 0.25),
            border = FALSE)
    lines(tmp.area$years.num[obs.idx], 
          tmp.area$median[obs.idx],
          col = 'black',
          lwd = 2, lty = 1)
    lines(tmp.area$years.num[pred.idx], 
          tmp.area$median[pred.idx], 
          col = 'black', 
          lwd = 2, lty = 2)
    
    ### Legend ####
    plot(NA, 
         xlim = c(0,1),
         ylim = c(0,1), axes = F,
         xlab = "", ylab = "")
    legend('center',
           bty = 'n',
           pch = c(rep(19, length(surveys)),
                   rep(NA, 4)),
           col = c(alpha(cols[1:length(surveys)], .25),
                   rep(NA, 4)),
           fill = c(rep(NA, length(surveys)),
                    alpha(c(cols[9:11],
                            'black'), .25)),
           border = c(rep(NA, length(surveys)),
                      cols[9:11],
                      'black'), cex = 1,
           legend = c(survey.legends,
                      'IHME', 'IGME',
                      'Smoothed Direct', 'Betabinomial'))
  }
  
}
dev.off()

temporals <- getDiag(fit.natl,
                     field = "time",
                     year_label = plot.years)

## Hazards over time ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/National/',
           country, 
           '_rw2_natlBench_temporal.pdf'),
    height = 5, width = 7.5)
{
  
  for(age in ages){
    age.idx <- grepl(paste0(age),
                     temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), 
                       row.names(fit.natl$fit$summary.fixed))
    fixed.eff <- fit.natl$fit$summary.fixed$`0.5quant`[fixed.idx]
    tmp <- temporals[age.idx,]
    
    if(match(age, ages) == 1){
      plot.min <- min(outer(temporals$median,
                            fit.natl$fit$summary.fixed$`0.5quant`,
                            FUN="+")) - 0.25
      plot.max <- max(outer(temporals$median, 
                            fit.natl$fit$summary.fixed$`0.5quant`, 
                            FUN="+")) + 0.25
      
      par(mfrow =c(1,1),
          lend=1)
      plot(NA,
           xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           yaxt = 'n',
           xlab = "Year",
           ylab = "Monthly hazard",
           main = "")
      labs <- round(expit(seq(plot.min, plot.max, 1))*1000,1)
      axis(2, at = seq(plot.min, plot.max, 1),
           labels = labs)
    }
    
    lines(plot.years,
          tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], 
          lwd = 2)
    
  }
  
  legend('bottomleft', 
         cex = 0.65,
         lwd = 2,
         col = c(age.cols),
         legend = c(ages), 
         bty = 'n')
}
dev.off()

## Fixed Effects ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/National/',
           country,
           '_rw2_natlBench_coefs.pdf'), 
    height = 5, width = 5)
{
  
  plot.idx <- 0
  for(age in ages){
    plot.idx <- plot.idx + 1
    age.idx <- grepl(paste0(age),
                     temporals$group)
    fixed.idx <- grepl(paste0(age,"$"),
                       row.names(fit.natl$fit$summary.fixed))
    fixed.eff <- fit.natl$fit$summary.fixed[fixed.idx,]
    tmp <- temporals[age.idx,]
    
    if(plot.idx == 1){
      plot.min <- min(fit.natl$fit$summary.fixed$`0.025quant`) - 0.025
      plot.max <- max(fit.natl$fit$summary.fixed$`0.975quant`) + 0.025
      
      par(mfrow =c(1,1), 
          lend=1)
      plot(NA,
           xlim = c(0, length(ages) + 1),
           xaxt = 'n',
           yaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Age",
           ylab = "",
           main = "")
      axis(1, at = 1:length(ages),
           labels = ages, cex.axis = 0.7)
      axis(2, at = seq(plot.min, plot.max, 1),
           labels = round(expit(seq(plot.min, plot.max, 1))*1000,
                          digits = 1))
    }
    
    
    points(plot.idx, 
           fixed.eff$`0.5quant`,
           pch = 19, col = age.cols[plot.idx])
    segments(plot.idx,
             fixed.eff$`0.025quant`,
             plot.idx,
             fixed.eff$`0.975quant`,
             col = age.cols[plot.idx])
    
  }
}
dev.off()


# Admin 1 ####

## Load Direct ####
if(useHIVAdj){
  load(paste0(folder.name, '/',
              country,
              '_direct_admin1.rda'),
       envir = .GlobalEnv)
  write.csv(direct.admin1,
            file = paste0(folder.name, '/',
                          country,
                          '_direct_admin1.csv'),
            row.names = FALSE)
  load(paste0(folder.name, '/',
              country,
              '_directHIV_admin1.rda'),
       envir = .GlobalEnv)
  write.csv(direct.admin1,
            file = paste0(folder.name, '/',
                          country,
                          '_directHIV_admin1.csv'),
            row.names = FALSE)
}else{
  load(paste0(folder.name, '/',
              country,
              '_direct_admin1.rda'), 
       envir = .GlobalEnv)
  write.csv(direct.admin1,
            file = paste0(folder.name, '/',
                          country,
                          '_direct_admin1.csv'),
            row.names = FALSE)
}

direct.admin1[,c("mean", "lower", "upper")] <- 
  direct.admin1[,c("mean", "lower", "upper")]*1000

## Load Smoothed Direct ####

load(paste0(folder.name, '/',
            country,
            '_res_admin1_SmoothedDirect.rda'))

res.smoothdir <- res.admin1
rm(res.admin1)

## Load Betabinomial ####
load(paste0(folder.name,'/',
            country,
            '_res_', 
            time.mod,
            '_admin1.rda'))
write.csv(res.admin1$overall,
          file = paste0(folder.name, '/',
                        country,
                        '_res_', 
                        time.mod,
                        '_admin1.csv'),
          row.names = FALSE)
load(paste0(folder.name,'/',
            country, '_', 
            time.mod, 
            '_admin1Benchmarks.rda'))
load(paste0(folder.name, '/',
            country, '_',
            time.mod,
            '_admin1_noStrata_temporals.rda'))
load(paste0(folder.name, '/',
            country, '_', 
            time.mod,
            '_admin1_noStrata_spatials.rda'))
load(paste0(folder.name, '/',
            country, 
            '_',
            time.mod,
            '_admin1_noStrata_fixedeff.rda'))
load(paste0(folder.name, '/',
            country,'_',
            time.mod,
            '_admin1_noStrata_PosteriorInteractions.rda'))

if (grepl("randomSlopes",time.mod)) {
  load(paste0(folder.name, '/',
              country,'_',
              time.mod,
              '_admin1_noStrata_posteriorRandomSlopes.rda'))
}


## Spaghetti Plot ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_',
           time.mod,
           '_admin1_noStrata_spaghetti_6per.pdf'),
    height = 5, width = 7.5)
{
  par(mfrow = c(2,3),
      lend=1)
  
  area.idx <- 0
  for(area in admin1.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == 
                                 as.character(admin1.names$GADM[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    
    res.tmp <- res.smoothdir[res.smoothdir$region == 
                               as.character(admin1.names$Internal[area.idx]),]
    res.tmp[,c("median", "lower", "upper")] <-
      res.tmp[,c("median", "lower", "upper")]*1000
    
   if(dim(tmp.area)[1] != 0 &
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin1$upper[direct.admin1$region == as.character(area)]),
                      na.rm = T) + 25
    }else{
      plot.max <- max(direct.adm1$upper, na.rm = TRUE)
    }
    
    if (nrow(tmp.area) > 0 &
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = admin1.names$GADM[area.idx])
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        "Betabinomial"))
      
    } else {
      ### Noodles ####
      #### Add Direct Estimates ####
      for(survey in surveys){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = admin1.names$GADM[area.idx])
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean, 
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main =  paste0(admin1.names$GADM[area.idx]))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean, 
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      #### Add IHME ####
      ihme.years <- (tmp.ihme$year)
      lines(ihme.years, tmp.ihme$mean, 
            lwd = 2, col  = cols[9])
      
      #### Add Smoothed Direct ####
      lines(pane.years[est.idx],
            res.tmp$median[est.idx], 
            col = cols[11],
            lwd = 2, lty = 1)
      lines(pane.years[max(est.idx):length(pane.years)], 
            res.tmp$median[max(est.idx):length(pane.years)], 
            col = cols[11],
            lwd = 2, lty = 2)
      
      #### Add Betabinomial ###
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx],
            tmp.area$median[pred.idx],
            col = 'black', 
            lwd = 2, lty = 2)
      
      #### Legend ####
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     cols[9],
                     cols[11],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        'IHME',
                        'Smoothed Direct',
                        'Betabinomial'),
             cex = 0.6)
      
      ### Direct Uncertainty ####
      #### Add Direct Estimates ####
      for(survey in surveys){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == 
                               as.character(admin1.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        tmp.est.idx <- which(!is.na(tmp$mean))
        
        if(svy.idx == 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", 
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = admin1.names$GADM[area.idx])
            
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha(cols[svy.idx], 0.25),
                    border = FALSE)
            
          }else{
            
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main =  paste0(admin1.names$GADM[area.idx]))
          }
          
        }else{
          
          polygon(x = c(pane.years[tmp.est.idx],
                        rev(pane.years[tmp.est.idx])),
                  y = c(tmp$upper[tmp.est.idx],
                        rev(tmp$lower[tmp.est.idx])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
        }
        
      }  
      
      #### Add Betabinomial ####
      
      polygon(x = c(plot.years,
                    end.year:beg.year),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      #### Legends ####
      legend('topright',
             bty = 'n',
             fill = alpha(c(cols[1:length(surveys)],
                            'black'), 0.25),
             border = c(cols[1:length(surveys)],
                        'black'),
             legend = c(survey.legends,
                        'Betabinomial'),
             cex = 0.6)
      
      ### Smoothed Uncertainty ####
      
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year),
           main = admin1.names$GADM[area.idx])
      
      #### Add IHME ####
      
      ihme.years <- tmp.ihme$year
      polygon(x = c(ihme.years, rev(ihme.years)),
              y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
              col = alpha(cols[9], 0.25),
              border = FALSE)
      
      #### Add Smoothed Direct ####
      
      polygon(x = c(pane.years, rev(pane.years)),
              y = c(res.tmp$upper[1:length(pane.years)],
                    rev(res.tmp$lower[1:length(pane.years)])),
              col = alpha(cols[11], 0.25),
              border = FALSE)
      
      #### Add Betabinomial ####
      
      polygon(x = c(tmp.area$years.num,
                    rev(tmp.area$years.num)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25), 
              border = FALSE)
      
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx],
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      
      legend('topright',
             bty = 'n',
             fill = alpha(c(cols[c(9,11)],
                            'black'), .25),
             border = c(cols[c(9,11)],
                        'black'),
             legend = c('IHME',
                        'Smoothed Direct',
                        'Betabinomial'),
             cex = 0.75)
    }
    
    
  }
}
dev.off()

## Hazards over time ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_',
           time.mod,
           '_admin1_temporal.pdf'),
    height = 5, width = 7.5)
{
  par(mfrow =c(1,1),
      lend=1)
  
  for(age in ages){
    fixed.idx <- grepl(paste0(age,"$"),
                       row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table$`0.5quant`[fixed.idx]
    
    age.idx <- grepl(age,
                     temporals$group)
    tmp <- temporals[age.idx,]
    
    if(match(age, ages) == 1){
      plot.min <- min(temporals$median +
                        min(fixed.eff.table$`0.5quant`)) - 0.25
      plot.max <- max(temporals$median +
                        max(fixed.eff.table$`0.5quant`)) + 0.25
      
      plot(NA,
           xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           yaxt = 'n',
           xlab = "Year",
           ylab = "Monthly hazard",
           main = "")
      axis(2, at = seq(plot.min,
                       plot.max, 1),
           labels = round(expit(seq(plot.min,
                                    plot.max, 1))*1000, 
                          digits = 1))
    }
    
    lines(plot.years,
          tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], 
          lwd = 2)
  }
  
  legend('bottomleft',
         cex = 0.7,
         lwd = 2,
         col = c(age.cols),
         legend = c(ages),
         bty = 'n')
}
dev.off()

## Fixed Effects ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_',
           time.mod, 
           '_admin1_coefs.pdf'), 
    height = 5, width = 5)
{
  par(mfrow =c(1,1),
      lend=1)
  
  for(age in ages){
    
    fixed.idx <- grepl(paste0(age,"$"), 
                       row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table[fixed.idx,]
    
    tmp <- temporals[age.idx,]
    age.idx <- grepl(paste0(age),
                     temporals$group)
    
    if(match(age, ages) == 1 ){
      plot.min <- min(fixed.eff.table$`0.025quant`) - 0.025
      plot.max <- max(fixed.eff.table$`0.975quant`) + 0.025
      
      plot(NA,
           xlim = c(0, length(ages) + 1),
           xaxt = 'n',
           yaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Age",
           ylab = "",
           main = "")
      axis(1, at = 1:length(ages),
           labels = ages, cex.axis = 0.7)
      axis(2, at = seq(plot.min, plot.max, 1),
           labels = round(expit(seq(plot.min, plot.max, 1))*1000,
                          digits = 1))
    }
    
    points(match(age,ages),
           fixed.eff$`0.5quant`,
           pch = 19,
           col = age.cols[match(age, ages)])
    segments(match(age,ages),
             fixed.eff$`0.025quant`,
             match(age,ages),
             fixed.eff$`0.975quant`,
             col = age.cols[match(age, ages)])
    
  }
  
  legend('bottomleft', cex = 0.7, lty = 1,
         col = c(age.cols), pch =19,
         legend = c(ages), bty = 'n')
}
dev.off()

## Random Slopes ####

if (grepl("randomSlopes",time.mod)) {
  pdf(paste0(folder.name,
             '/Plots/Betabinomial/Admin1/',
             country, '_',
             time.mod,
             '_admin1_randomSlopes.pdf'),
      height = 5, width = 5)
  years.std <- ((plot.years)-mean(plot.years))/sd(plot.years)
  plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                        years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
  par(lend=1)
  plot(NA,
       xlim = range(years.std),
       ylim = plot.range,
       ylab = "effect",
       xlab="year (standardized)")
  abline(h = 0, lty = 2)
  region.cols <- viridis_pal(option = "A")(nrow(posteriorRandomSlopes))
  for (i in 1:nrow(posteriorRandomSlopes)) {
    abline(0, posteriorRandomSlopes$`0.5quant`[i],
           col=region.cols[i])
  }
  legend("topleft",
         legend = admin1.names[,1],
         col = region.cols,
         lty = 1,
         ncol= 2,
         cex = 0.5)
  dev.off()
}

## Admin1 Names Plots  ####
centroids <- gCentroid(poly.adm1,
                       byid = TRUE,
                       id = poly.adm1@data$NAME_1)
pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_Admin1Names.pdf'))
par(lend=1)
plot(poly.adm1,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F)
text(centroids$x, centroids$y,
     labels = row.names(centroids),cex = 0.45)
dev.off()


# Admin 1 Benchmark ####

## Load Direct Estimates ####
if(useHIVAdj){
  load(paste0(folder.name, '/',
              country,
              '_directHIV_admin1.rda'), 
       envir = .GlobalEnv)
}else{
  load(paste0(folder.name, '/',
              country,
              '_direct_admin1.rda'), 
       envir = .GlobalEnv)
}

direct.admin1[,c("mean", "lower", "upper")] <- 
  direct.admin1[,c("mean", "lower", "upper")]*1000

## Load Smoothed Direct ####
load(paste0(folder.name,
            '/',
            country,
            '_res_admin1Bench_SmoothedDirect.rda'))
res.smoothdir <- res.admin1

## Load Betabinomial ####
load(paste0(folder.name,'/',
            country,
            '_res_',
            time.mod,
            '_admin1Bench.rda'))
write.csv(res.admin1$overall,
          file = paste0(folder.name, '/',
                        country,
                        '_res_',
                        time.mod,
                        '_admin1Bench.csv'),
          row.names = FALSE)
load(paste0(folder.name,'/', 
            country, '_', 
            time.mod, 
            '_admin1Benchmarks.rda'))
load(paste0(folder.name, '/', 
            country, '_', 
            time.mod,
            '_admin1Bench_noStrata_temporals.rda'))
load(paste0(folder.name, '/',
            country, '_',
            time.mod,
            '_admin1Bench_noStrata_spatials.rda'))
load(paste0(folder.name, '/',
            country, '_',
            time.mod,
            '_admin1Bench_noStrata_fixedeff.rda'))

if (grepl("randomSlopes",time.mod)) {
  load(paste0(folder.name, '/',
              country,'_',
              time.mod,
              '_admin1Bench_noStrata_posteriorRandomSlopes.rda'))
}

## Benchmarks ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_',
           time.mod, '_admin1Benchmarks.pdf'),
    height = 4, width = 4)
{
  par(lend=1)
  plot(NA,
       xlim = c(beg.year, end.year),
       ylim = c(min(bench.adj$ratio) - 0.025,
                max(bench.adj$ratio) + 0.025),
       xlab = "Year",
       ylab = "Offset",
       main = "")
  abline(h = 1)
  
  bench.tmp <- bench.adj$ratio
  bench.tmp[bench.tmp == 1] <- NA
  lines(plot.years,
        bench.tmp,
        lwd = 2,
        col = 'red')
  
}
dev.off()

## Spaghetti Plot ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_',
           time.mod,
           '_admin1Bench_noStrata_spaghetti_6per.pdf'),
    height = 5, width = 7.5)
{
  par(mfrow = c(2,3),
      lend=1)
  
  area.idx <- 0
  for(area in admin1.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == 
                                 as.character(admin1.names$GADM[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    
    res.tmp <- res.smoothdir[res.smoothdir$region == 
                               as.character(admin1.names$Internal[area.idx]),]
    res.tmp[,c("median", "lower", "upper")] <-
      res.tmp[,c("median", "lower", "upper")]*1000
    
    if(dim(tmp.area)[1] != 0 &
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin1$upper[direct.admin1$region == as.character(area)]),
                      na.rm = T) + 25
    }else{
      plot.max <- max(direct.adm1$upper, na.rm = TRUE)
    }
    
    if (nrow(tmp.area) > 0 &
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = admin1.names$GADM[area.idx])
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        "Betabinomial"))
      
    } else {
      ### Noodles ####
      #### Add Direct Estimates ####
      for(survey in surveys){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = admin1.names$GADM[area.idx])
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean, 
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main =  paste0(admin1.names$GADM[area.idx]))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean, 
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      #### Add IHME ####
      ihme.years <- (tmp.ihme$year)
      lines(ihme.years, tmp.ihme$mean, 
            lwd = 2, col  = cols[9])
      
      #### Add Smoothed Direct ####
      lines(pane.years[est.idx],
            res.tmp$median[est.idx], 
            col = cols[11],
            lwd = 2, lty = 1)
      lines(pane.years[max(est.idx):length(pane.years)], 
            res.tmp$median[max(est.idx):length(pane.years)], 
            col = cols[11],
            lwd = 2, lty = 2)
      
      #### Add Betabinomial ###
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx],
            tmp.area$median[pred.idx],
            col = 'black', 
            lwd = 2, lty = 2)
      
      #### Legend ####
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     cols[9],
                     cols[11],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        'IHME',
                        'Smoothed Direct',
                        'Betabinomial'),
             cex = 0.6)
      
      ### Direct Uncertainty ####
      #### Add Direct Estimates ####
      for(survey in surveys){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == 
                               as.character(admin1.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        tmp.est.idx <- which(!is.na(tmp$mean))
        
        if(svy.idx == 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", 
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = admin1.names$GADM[area.idx])
            
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha(cols[svy.idx], 0.25),
                    border = FALSE)
            
          }else{
            
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main =  paste0(admin1.names$GADM[area.idx]))
          }
          
        }else{
          
          polygon(x = c(pane.years[tmp.est.idx],
                        rev(pane.years[tmp.est.idx])),
                  y = c(tmp$upper[tmp.est.idx],
                        rev(tmp$lower[tmp.est.idx])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
        }
        
      }  
      
      #### Add Betabinomial ####
      polygon(x = c(plot.years,
                    end.year:beg.year),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      #### Legends ####
      legend('topright',
             bty = 'n',
             fill = alpha(c(cols[1:length(surveys)],
                            'black'), 0.25),
             border = c(cols[1:length(surveys)],
                        'black'),
             legend = c(survey.legends,
                        'Betabinomial'),
             cex = 0.6)
      
      ### Smoothed Uncertainty ####
      
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year),
           main = admin1.names$GADM[area.idx])
      
      #### Add IHME ####
      
      ihme.years <- tmp.ihme$year
      polygon(x = c(ihme.years, rev(ihme.years)),
              y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
              col = alpha(cols[9], 0.25),
              border = FALSE)
      
      #### Add Smoothed Direct ####
      
      polygon(x = c(pane.years, rev(pane.years)),
              y = c(res.tmp$upper[1:length(pane.years)],
                    rev(res.tmp$lower[1:length(pane.years)])),
              col = alpha(cols[11], 0.25),
              border = FALSE)
      
      #### Add Betabinomial ####
      
      polygon(x = c(tmp.area$years.num,
                    rev(tmp.area$years.num)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25), 
              border = FALSE)
      
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx],
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      
      legend('topright',
             bty = 'n',
             fill = alpha(c(cols[c(9,11)],
                            'black'), .25),
             border = c(cols[c(9,11)],
                        'black'),
             legend = c('IHME',
                        'Smoothed Direct',
                        'Betabinomial'),
             cex = 0.75)
    }
    
    
  }
}
dev.off()

## Spaghetti Plot no IHME ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_', 
           time.mod,
           '_admin1Bench_noStrata_noIHME_spaghettiSingle_6per.pdf'),
    height = 9, width = 6)
{
  par(mfrow = c(3,2),
      lend=1)
  area.idx <- 0
  for(area in admin1.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    if(dim(tmp.area)[1] != 0 & 
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin1$upper[direct.admin1$region == 
                                              as.character(area)]),
                      na.rm = T) + 25
    }else{
      plot.max <- 25
    }
    
    
    if (nrow(tmp.area) > 0 & 
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = admin1.names$GADM[area.idx])
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2,
             legend = c(survey.legends,
                        'Smoothed'),
             cex = 0.6)
      
    } else {
      
      #### Add Direct ####
      
      for(survey in surveys){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == 
                               as.character(admin1.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = admin1.names$GADM[area.idx])
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main =  admin1.names$GADM[area.idx])
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      #### Add Betabinomial ####
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(plot.years,
                    rev(plot.years)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      
      #### Legend #####
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2,
             legend = c(survey.legends,
                        'Smoothed'),
             cex = 0.6)
      
    }
    
    
  }
}
dev.off()


## Spaghetti Singles ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_', 
           time.mod,
           '_admin1Bench_noStrata_spaghettiSingle.pdf'),
    height = 5, width = 5)
{
  par(mfrow = c(1,1),
      lend=1)
  area.idx <- 0
  for(area in admin1.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    
    tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    
    res.tmp <- res.smoothdir[res.smoothdir$region == 
                               as.character(admin1.names$Internal[area.idx]),]
    res.tmp[,c("median", "lower", "upper")] <-
      res.tmp[,c("median", "lower", "upper")]*1000
    
  
    if(dim(tmp.area)[1] != 0 & 
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin1$upper[direct.admin1$region == 
                                              as.character(area)]),
                      na.rm = T) + 25
    }else{
      plot.max <- 25
    }
    
    
    if (nrow(tmp.area) > 0 & 
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = admin1.names$GADM[area.idx])
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        "Betabinomial"))
      
    } else {
      
      #### Add Direct ####
      
      for(survey in surveys){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == 
                               as.character(admin1.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = admin1.names$GADM[area.idx])
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main =  admin1.names$GADM[area.idx])
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      #### Add IHME ####
      lines(tmp.ihme$year, 
            tmp.ihme$mean,
            lwd = 2, 
            col  = cols[9])
      polygon(c(tmp.ihme$year,
                rev(tmp.ihme$year)),
              c(tmp.ihme$upper,
                rev(tmp.ihme$lower)),
              col = alpha(cols[9], 0.25),
              border = FALSE)
      
      #### Add Smoothed Direct ####
      lines(pane.years[est.idx],
            res.tmp$median[est.idx], 
            col = cols[11],
            lwd = 2)
      lines(pane.years[max(est.idx):length(pane.years)], 
            res.tmp$median[max(est.idx):length(pane.years)], 
            col = cols[11], 
            lwd = 2, lty = 2)
      polygon(x = c(pane.years,
                    rev(pane.years)),
              y = c(res.tmp$upper[match(pane.years,
                                        res.tmp$years.num)],
                    rev(res.tmp$lower[match(pane.years,
                                            res.tmp$years.num)])),
              col = alpha(cols[11], 0.25),
              border = FALSE)
      
      #### Add Betabinomial ####
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(plot.years,
                    rev(plot.years)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      
      #### Legend #####
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     cols[9],
                     cols[11],
                     'black'),
             lwd = 2,
             legend = c(survey.legends,
                        'IHME',
                        'Smoothed Direct',
                        'Betabinomial'),
             cex = 0.6)
      
    }
    
    
  }
}
dev.off()

## Spaghetti Singles no IHME ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_', 
           time.mod,
           '_admin1Bench_noStrata_noIHME_spaghettiSingle.pdf'),
    height = 5, width = 5)
{
  par(mfrow = c(1,1),
      lend=1)
  area.idx <- 0
  for(area in admin1.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000

    if(dim(tmp.area)[1] != 0 & 
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin1$upper[direct.admin1$region == 
                                              as.character(area)]),
                      na.rm = T) + 25
    }else{
      plot.max <- 25
    }
    
    
    if (nrow(tmp.area) > 0 & 
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = admin1.names$GADM[area.idx])
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2,
             legend = c(survey.legends,
                        'Smoothed'),
             cex = 0.6)
      
    } else {
      
      #### Add Direct ####
      
      for(survey in surveys){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == 
                               as.character(admin1.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = admin1.names$GADM[area.idx])
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main =  admin1.names$GADM[area.idx])
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      #### Add Betabinomial ####
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(plot.years,
                    rev(plot.years)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      
      #### Legend #####
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2,
             legend = c(survey.legends,
                        'Smoothed'),
             cex = 0.6)
      
    }
    
    
  }
}
dev.off()

## Hazards over time ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_',
           time.mod,
           '_admin1Bench_temporal.pdf'),
    height = 5, width = 7.5)
{
  par(mfrow =c(1,1),
      lend=1)
  
  for(age in ages){
    fixed.idx <- grepl(paste0(age,"$"),
                       row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table$`0.5quant`[fixed.idx]
    
    age.idx <- grepl(age,
                     temporals$group)
    tmp <- temporals[age.idx,]
    
    if(match(age, ages) == 1){
      plot.min <- min(temporals$median +
                        min(fixed.eff.table$`0.5quant`)) - 0.25
      plot.max <- max(temporals$median +
                        max(fixed.eff.table$`0.5quant`)) + 0.25
      
      plot(NA,
           xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           yaxt = 'n',
           xlab = "Year",
           ylab = "Monthly hazard",
           main = "")
      axis(2, at = seq(plot.min,
                       plot.max, 1),
           labels = round(expit(seq(plot.min,
                                    plot.max, 1))*1000, 
                          digits = 1))
    }
    
    lines(plot.years,
          tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], 
          lwd = 2)
  }
  
  legend('bottomleft',
         cex = 0.7,
         lwd = 2,
         col = c(age.cols),
         legend = c(ages),
         bty = 'n')
}
dev.off()

## Fixed Effects ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_',
           time.mod, 
           '_admin1Bench_coefs.pdf'), 
    height = 5, width = 5)
{
  par(mfrow =c(1,1),
      lend=1)
  
  for(age in ages){
    
    fixed.idx <- grepl(paste0(age,"$"), 
                       row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table[fixed.idx,]
    
    tmp <- temporals[age.idx,]
    age.idx <- grepl(paste0(age),
                     temporals$group)
    
    if(match(age, ages) == 1 ){
      plot.min <- min(fixed.eff.table$`0.025quant`) - 0.025
      plot.max <- max(fixed.eff.table$`0.975quant`) + 0.025
      
      plot(NA,
           xlim = c(0, length(ages) + 1),
           xaxt = 'n',
           yaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Age",
           ylab = "",
           main = "")
      axis(1, at = 1:length(ages),
           labels = ages, cex.axis = 0.7)
      axis(2, at = seq(plot.min, plot.max, 1),
           labels = round(expit(seq(plot.min, plot.max, 1))*1000,
                          digits = 1))
    }
    
    points(match(age,ages),
           fixed.eff$`0.5quant`,
           pch = 19,
           col = age.cols[match(age, ages)])
    segments(match(age,ages),
             fixed.eff$`0.025quant`,
             match(age,ages),
             fixed.eff$`0.975quant`,
             col = age.cols[match(age, ages)])
    
  }
  
  legend('bottomleft',
         cex = 0.7,
         col = age.cols,
         pch =19,
         legend = ages,
         bty = 'n')
}
dev.off()

## Random Slopes ####

if (grepl("randomSlopes",time.mod)) {
  pdf(paste0(folder.name,
             '/Plots/Betabinomial/Admin1/',
             country, '_',
             time.mod,
             '_admin1Bench_randomSlopes.pdf'),
      height = 5, width = 5)
  years.std <- ((plot.years)-mean(plot.years))/sd(plot.years)
  plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                        years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
  par(lend=1)
  plot(NA,
       xlim = range(years.std),
       ylim = plot.range,
       ylab = "effect",
       xlab="year (standardized)")
  abline(h = 0, lty = 2)
  region.cols <- viridis_pal(option = "A")(nrow(posteriorRandomSlopes))
  for (i in 1:nrow(posteriorRandomSlopes)) {
    abline(0, posteriorRandomSlopes$`0.5quant`[i],
           col=region.cols[i])
  }
  legend("topleft",
         legend = admin1.names[,1],
         col = region.cols,
         lty = 1,
         ncol= 2,
         cex = 0.5)
  dev.off()
}

## Spatial Effects ####
med.palette <- rev(brewer.pal(n = 5,
                              name = "RdBu"))
med.int <- classIntervals(round(spaces$median[spaces$label == "Total"], 3),
                          n = 5,
                          style = 'fixed',
                          fixedBreaks = quantile(round(spaces$median[spaces$label == "Total"], 
                                                       3),
                                                 c(0,.25,.5,.75,1)))
med.col <- findColours(med.int,
                       med.palette)

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_',
           time.mod,
           '_admin1Bench_spatial.pdf'))
par(mar = c(0.15,0.15,0.15,0.15),
    mai = c(0.1,0.1,0.1,0.1),
    lend=1)
plot(poly.adm1,
     col = med.col,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F)
legend('bottomleft',
       fill = med.palette,
       legend = names(attr(med.col, 'table')),
       bty = 'n', 
       cex = 0.75)
dev.off()

## Spaghetti All ####
# change the region names from Internal to GADM for legend labels
res.admin1$overall$region.orig <- res.admin1$overall$region
for (i in 1:nrow(admin1.names)) {
  res.admin1$overall$region[as.character(res.admin1$overall$region) == 
                              as.character(admin1.names$Internal[i])] <- 
    as.character(admin1.names$GADM[i])
}



numberAreasTotal <- nrow(admin1.names)

# order data by median magnitude in end.year
res.admin1.order <- res.admin1$overall %>% 
  filter(years.num == end.year) %>%
  arrange(median)
areaOrder <- res.admin1.order$region.orig

#make plots

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin1/',
           country, '_',
           time.mod, 
           '_admin1Bench_spaghetti_allByMedian.pdf'),
    height = 6, width = 6)
par(lend=1)

tmp <- res.admin1$overall
tmp$region.plot <- factor(tmp$region.orig,
                          levels = areaOrder,
                          labels = admin1.names$GADM[match(areaOrder,
                                                           admin1.names$Internal)])


{
  
  print(ggplot(tmp, aes(x = years.num,
                        y = median*1000,
                        col = region.plot)) +
          geom_line() +
          geom_point() +
          theme_light() +
          xlab("Year") +
          ylab("U5MR: deaths per 1000 live births") +
          ggtitle(country) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 6)) +
          guides(col = guide_legend(ncol=3,
                                    title = "")) +
          ylim(c(0, max(tmp$median)*1000)))
  
}

dev.off()


# Admin 2 ####

## Load Direct Estimates ####
if(useHIVAdj){
  load(paste0(folder.name, '/',
              country,
              '_direct_admin2.rda'),
       envir = .GlobalEnv)
  write.csv(direct.admin2,
            file = paste0(folder.name, '/',
                          country, 
                          '_direct_admin2.csv'),
            row.names = FALSE)
  load(paste0(folder.name, '/',
              country,
              '_directHIV_admin2.rda'),
       envir = .GlobalEnv)
  write.csv(direct.admin2,
            file = paste0(folder.name, '/',
                          country,
                          '_directHIV_admin2.csv'),
            row.names = FALSE)
}else{
  load(paste0(folder.name, '/',
              country,
              '_direct_admin2.rda'),
       envir = .GlobalEnv)
  write.csv(direct.admin2,
            file = paste0(folder.name, '/',
                          country,
                          '_direct_admin2.csv'),
            row.names = FALSE)
}

direct.admin2[,c("mean", "lower", "upper")] <- 
  direct.admin2[,c("mean", "lower", "upper")]*1000

## Load Betabinomial ####
load(paste0(folder.name,'/',
            country,
            '_res_',
            time.mod,
            '_admin2.rda'))
write.csv(res.admin2$overall,
          file = paste0(folder.name, '/',
                        country, 
                        '_res_', 
                        time.mod,
                        '_admin2.csv'),
          row.names = FALSE)
load(paste0(folder.name,'/',
            country, '_', 
            time.mod, 
            '_admin2Benchmarks.rda'))
load(paste0(folder.name, '/', 
            country, '_',
            time.mod,
            '_admin2_noStrata_temporals.rda'))
load(paste0(folder.name, '/',
            country, '_', 
            time.mod,
            '_admin2_noStrata_spatials.rda'))
load(paste0(folder.name, '/',
            country,'_',
            time.mod,
            '_admin2_noStrata_fixedeff.rda'))

if (grepl("randomSlopes",time.mod)) {
  load(paste0(folder.name, '/',
              country,'_',
              time.mod,
              '_admin2_noStrata_posteriorRandomSlopes.rda'))
}

## Spaghetti Plot ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod,
           '_admin2_noStrata_spaghetti_6per.pdf'),
    height = 5, width = 7.5)
{
  par(mfrow = c(2,3),
      lend=1)
  
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.area[,c("median", "lower", "upper")] <- 
      tmp.area[,c("median", "lower", "upper")]*1000
    
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == 
                                 as.character(admin2.names$GADM[area.idx]) & 
                                 ihme.ests[[3]]$ADM1_NAME == 
                                 as.character(poly.adm2$NAME_1[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <-
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
  
    if(dim(tmp.area)[1] != 0 &
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper, 
                        direct.admin2$mean[direct.admin2$region == area]),
                      na.rm = T) + 25
    }else{
      plot.max <- 25
    }
    
    if (nrow(tmp.area) > 0 &
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = paste(admin2.names$GADM[area.idx],
                        poly.adm2$NAME_1[area.idx],
                        sep = ", "),
           cex.main = 0.8)
      legend('topright',
             bty = 'n', 
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        "Betabinomial"))
      
    } else {
      ### Noodles ####
      #### Add direct estimates ####
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == 
                               as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], sep = ", "),
                 cex.main = 0.8)
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  main = surveys[svy.idx],
                  lwd = 2)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], sep = ", "),
                 cex.main = 0.8)
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
      }
      
      #### Add IHME ####
      ihme.years <- (tmp.ihme$year)
      lines(ihme.years, tmp.ihme$mean,
            lwd = 2, col  = cols[9])
      
      #### Add Betabinomial ####
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx], col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      #### Legend ####
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     cols[9],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends, 
                        'IHME', 
                        'Betabinomial'),
             cex = 0.6)
      
      ### Direct Uncertainty ####
      #### Add Direct Estimates ####
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == 
                               as.character(admin2.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx == 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = "")
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha(cols[svy.idx], 0.25),
                    border = FALSE)
            
            
          }else{
            
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = "")
          }
          
        }else{
          
          polygon(x = c(pane.years[tmp.est.idx],
                        rev(pane.years[tmp.est.idx])),
                  y = c(tmp$upper[tmp.est.idx],
                        rev(tmp$lower[tmp.est.idx])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
        }
      }  
      
      #### Add Betabinomial ####
      
      polygon(x = c(plot.years,
                    end.year:beg.year),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      
      #### Legend ####
      legend('topright',
             bty = 'n',
             fill = alpha(c(cols[1:length(surveys)],
                            'black'), 0.25),
             border = c(cols[1:length(surveys)],
                        'black'),
             legend = c(survey.legends,
                        "Betabinomial"),
             cex = 0.6)
      
      ### Smoothed Uncertainty ####
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year),
           main = "")
      
      #### Add IHME ####
      
      ihme.years <- tmp.ihme$year
      polygon(x = c(ihme.years,
                    rev(ihme.years)),
              y = c(tmp.ihme$upper,
                    rev(tmp.ihme$lower)),
              col = alpha(cols[9], 0.25),
              border = FALSE)
      #### Add Betabinomial ####
      
      polygon(x = c(plot.years,
                    end.year:beg.year),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx], 
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      #### Legend ####
      legend('topright',
             bty = 'n',
             fill = alpha(c(cols[9],
                            'black'), .25),
             border = c(cols[9],
                        'black'),
             legend = c('IHME', 
                        'Betabinomial'),
             cex = 0.75)
    }
    
    
  }
}
dev.off()


## Hazards over time ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod,
           '_admin2_temporal.pdf'),
    height = 5, width = 7.5)
{
  par(mfrow =c(1,1),
      lend=1)
  
  for(age in ages){
    fixed.idx <- grepl(paste0(age,"$"),
                       row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table$`0.5quant`[fixed.idx]
    
    age.idx <- grepl(age,
                     temporals$group)
    tmp <- temporals[age.idx,]
    
    if(match(age, ages) == 1){
      plot.min <- min(temporals$median +
                        min(fixed.eff.table$`0.5quant`)) - 0.25
      plot.max <- max(temporals$median +
                        max(fixed.eff.table$`0.5quant`)) + 0.25
      
      plot(NA,
           xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           yaxt = 'n',
           xlab = "Year",
           ylab = "Monthly hazard",
           main = "")
      axis(2, at = seq(plot.min,
                       plot.max, 1),
           labels = round(expit(seq(plot.min,
                                    plot.max, 1))*1000, 
                          digits = 1))
    }
    
    lines(plot.years,
          tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], 
          lwd = 2)
  }
  
  legend('bottomleft',
         cex = 0.7,
         lwd = 2,
         col = c(age.cols),
         legend = c(ages),
         bty = 'n')
}
dev.off()
## Fixed Effects ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod, '_admin2_coefs.pdf'), 
    height = 5, width = 5)
{
  par(mfrow =c(1,1),
      lend=1)
  
  for(age in ages){
    
    fixed.idx <- grepl(paste0(age,"$"), 
                       row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table[fixed.idx,]
    
    tmp <- temporals[age.idx,]
    age.idx <- grepl(paste0(age),
                     temporals$group)
    
    if(match(age, ages) == 1 ){
      plot.min <- min(fixed.eff.table$`0.025quant`) - 0.025
      plot.max <- max(fixed.eff.table$`0.975quant`) + 0.025
      
      plot(NA,
           xlim = c(0, length(ages) + 1),
           xaxt = 'n',
           yaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Age",
           ylab = "",
           main = "")
      axis(1, at = 1:length(ages),
           labels = ages, cex.axis = 0.7)
      axis(2, at = seq(plot.min, plot.max, 1),
           labels = round(expit(seq(plot.min, plot.max, 1))*1000,
                          digits = 1))
    }
    
    points(match(age,ages),
           fixed.eff$`0.5quant`,
           pch = 19,
           col = age.cols[match(age, ages)])
    segments(match(age,ages),
             fixed.eff$`0.025quant`,
             match(age,ages),
             fixed.eff$`0.975quant`,
             col = age.cols[match(age, ages)])
    
  }
  
  legend('bottomleft',
         cex = 0.7,
         col = age.cols,
         pch =19,
         legend = ages,
         bty = 'n')
}
dev.off()

## Random Slopes ####

if (grepl("randomSlopes",time.mod)) {
  pdf(paste0(folder.name,
             '/Plots/Betabinomial/Admin2/',
             country, '_',
             time.mod,
             '_admin2_randomSlopes.pdf'),
      height = 5, width = 5)
  years.std <- ((plot.years)-mean(plot.years))/sd(plot.years)
  plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                        years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
  par(lend=1)
  plot(NA,
       xlim = range(years.std),
       ylim = plot.range,
       ylab = "effect",
       xlab="year (standardized)")
  abline(h = 0, lty = 2)
  region.cols <- viridis_pal(option = "A")(nrow(posteriorRandomSlopes))
  for (i in 1:nrow(posteriorRandomSlopes)) {
    abline(0, posteriorRandomSlopes$`0.5quant`[i],
           col=region.cols[i])
  }
  legend("topleft",
         legend = admin2.names[,1],
         col = region.cols,
         lty = 1,
         ncol= 3,
         cex = 0.5)
  dev.off()
}


## Admin2 Names Plots ####

centroids <- gCentroid(poly.adm2, byid = TRUE,
                       id = poly.adm2@data$GID_2)
pdf(paste0(folder.name,
           '/Plots/ShapeCheck/',
           country,
           '_Admin2Names.pdf'))
par(lend=1)
plot(poly.adm2,
     xlim = poly.adm2@bbox['x',],
     ylim = poly.adm2@bbox['y',],
     axes = F)
text(centroids$x, centroids$y,
     labels = poly.adm2@data$NAME_2,
     cex = 0.45)
dev.off()


# Admin 2 benchmark ####
## Load Direct Estimates ####
if(useHIVAdj){
  load(paste0(folder.name, '/',
              country,
              '_direct_admin2.rda'),
       envir = .GlobalEnv)
  write.csv(direct.admin2,
            file = paste0(folder.name, '/',
                          country, 
                          '_direct_admin2.csv'),
            row.names = FALSE)
  load(paste0(folder.name, '/',
              country,
              '_directHIV_admin2.rda'),
       envir = .GlobalEnv)
  write.csv(direct.admin2,
            file = paste0(folder.name, '/',
                          country,
                          '_directHIV_admin2.csv'),
            row.names = FALSE)
}else{
  load(paste0(folder.name, '/',
              country,
              '_direct_admin2.rda'),
       envir = .GlobalEnv)
  write.csv(direct.admin2,
            file = paste0(folder.name, '/',
                          country,
                          '_direct_admin2.csv'),
            row.names = FALSE)
}

direct.admin2[,c("mean", "lower", "upper")] <- 
  direct.admin2[,c("mean", "lower", "upper")]*1000

## Load Betabinomial ####
load(paste0(folder.name, '/',
            country,
            '_res_',
            time.mod,
            '_admin2Bench.rda'))
write.csv(res.admin2$overall,
          file = paste0(folder.name, '/',
                        country,
                        '_res_',
                        time.mod,
                        '_admin2Bench.csv'),
          row.names = FALSE)
load(paste0(folder.name,'/',
            country, '_', 
            time.mod,
            '_admin2Benchmarks.rda'))
load(paste0(folder.name, '/', 
            country, '_',
            time.mod,
            '_admin2Bench_noStrata_temporals.rda'))
load(paste0(folder.name, '/',
            country, '_',
            time.mod,
            '_admin2Bench_noStrata_spatials.rda'))
load(paste0(folder.name, '/',
            country, '_',
            time.mod, 
            '_admin2Bench_noStrata_fixedeff.rda'))

if (grepl("randomSlopes",time.mod)) {
  load(paste0(folder.name, '/',
              country,'_',
              time.mod,
              '_admin2Bench_noStrata_posteriorRandomSlopes.rda'))
}

## Benchmarks ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod,
           '_admin2Benchmarks.pdf'), 
    height = 4, width = 4)
{
  par(mfrow = c(1,1),
      lend=1)
  plot(NA,
       xlim = c(beg.year, end.year),
       ylim = c(min(bench.adj$ratio) - 0.025,
                max(bench.adj$ratio) + 0.025),
       xlab = "Year",
       ylab = "Offset",
       main = "")
  abline(h = 1)

  bench.tmp <- bench.adj$ratio
  bench.tmp[bench.tmp == 1] <- NA
  lines(plot.years,
        bench.tmp,
        lwd = 2, col = 'red')
}
dev.off()


## Spaghetti Plot ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod,
           '_admin2Bench_noStrata_spaghetti_6per.pdf'),
    height = 5, width = 7.5)
{
  par(mfrow = c(2,3),
      lend=1)
  
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.area[,c("median", "lower", "upper")] <- 
      tmp.area[,c("median", "lower", "upper")]*1000
    
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == 
                                 as.character(admin2.names$GADM[area.idx]) & 
                                 ihme.ests[[3]]$ADM1_NAME == 
                                 as.character(poly.adm2$NAME_1[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <-
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    
    if(dim(tmp.area)[1] != 0 &
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper, 
                        direct.admin2$mean[direct.admin2$region == area]),
                      na.rm = T) + 25
    }else{
      plot.max <- 25
    }
    
    if (nrow(tmp.area) > 0 &
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = paste(admin2.names$GADM[area.idx],
                        poly.adm2$NAME_1[area.idx],
                        sep = ", "),
           cex.main = 0.8)
      legend('topright',
             bty = 'n', 
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        "Betabinomial"))
      
    } else {
      ### Noodles ####
      #### Add direct estimates ####
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == 
                               as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], sep = ", "),
                 cex.main = 0.8)
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  main = surveys[svy.idx],
                  lwd = 2)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], sep = ", "),
                 cex.main = 0.8)
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
      }
      
      #### Add IHME ####
      ihme.years <- (tmp.ihme$year)
      lines(ihme.years, tmp.ihme$mean,
            lwd = 2, col  = cols[9])
      
      #### Add Betabinomial ####
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx], col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      #### Legend ####
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     cols[9],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends, 
                        'IHME', 
                        'Betabinomial'),
             cex = 0.6)
      
      ### Direct Uncertainty ####
      #### Add Direct Estimates ####
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == 
                               as.character(admin2.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx == 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = "")
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha(cols[svy.idx], 0.25),
                    border = FALSE)
            
            
          }else{
            
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = "")
          }
          
        }else{
          
          polygon(x = c(pane.years[tmp.est.idx],
                        rev(pane.years[tmp.est.idx])),
                  y = c(tmp$upper[tmp.est.idx],
                        rev(tmp$lower[tmp.est.idx])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
        }
      }  
      
      #### Add Betabinomial ####
      
      polygon(x = c(plot.years,
                    end.year:beg.year),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      
      #### Legend ####
      legend('topright',
             bty = 'n',
             fill = alpha(c(cols[1:length(surveys)],
                            'black'), 0.25),
             border = c(cols[1:length(surveys)],
                        'black'),
             legend = c(survey.legends,
                        "Betabinomial"),
             cex = 0.6)
      
      ### Smoothed Uncertainty ####
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year),
           main = "")
      
      #### Add IHME ####
      
      ihme.years <- tmp.ihme$year
      polygon(x = c(ihme.years,
                    rev(ihme.years)),
              y = c(tmp.ihme$upper,
                    rev(tmp.ihme$lower)),
              col = alpha(cols[9], 0.25),
              border = FALSE)
      #### Add Betabinomial ####
      
      polygon(x = c(plot.years,
                    end.year:beg.year),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx], 
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      #### Legend ####
      legend('topright',
             bty = 'n',
             fill = alpha(c(cols[9],
                            'black'), .25),
             border = c(cols[9],
                        'black'),
             legend = c('IHME', 
                        'Betabinomial'),
             cex = 0.75)
    }

  }
}
dev.off()

## Spaghetti Singles ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod,
           '_admin2Bench_noStrata_spaghettiSingle.pdf'),
    height = 5, width = 5)
{
  par(mfrow = c(1,1),
      lend=1)
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == 
                                 as.character(admin2.names$GADM[area.idx]) & 
                                 ihme.ests[[3]]$ADM1_NAME == 
                                 as.character(poly.adm2@data$NAME_1[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    
    
    if(dim(tmp.area)[1] != 0 &
       !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin2$upper[direct.admin2$region ==
                                              as.character(area)]),
                      na.rm = T) + 25
    }else{
      plot.max <- 25
    }
    
    if (nrow(tmp.area) > 0 &
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year",
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = paste(admin2.names$GADM[area.idx],
                        poly.adm2$NAME_1[area.idx], 
                        sep = ", "))
      legend('topright',
             bty = 'n',
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        "Betabinomial"))
      
    } else {
      ### Add Direct ####
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == 
                               as.character(admin2.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], 
                              sep = ", "))
            lines(pane.years, tmp$mean, 
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  main = surveys[svy.idx],
                  lwd = 2)
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year,
                          end.year),
                 main =  paste(admin2.names$GADM[area.idx],
                               poly.adm2$NAME_1[area.idx], 
                               sep = ", "))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      #### Add IHME ####
      lines(tmp.ihme$year, 
            tmp.ihme$mean, lwd = 2,
            lty = 1, 
            col  = cols[9])
      polygon(c(tmp.ihme$year,
                rev(tmp.ihme$year)),
              c(tmp.ihme$upper,
                rev(tmp.ihme$lower)),
              col = alpha(cols[9], 0.25),
              border = FALSE)
      
      #### Add Betabinomial ####
      
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx],
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(plot.years,
                    rev(plot.years)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      
      #### Legend ####
      legend('topright',
             bty = 'n',
             col = c(cols[1:(length(surveys))],
                     cols[9],
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        'IHME',
                        'Betabinomial'),
             cex = 0.6)
    }
  }
}
dev.off()

## Spaghetti Singles no IHME ####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod,
           '_admin2Bench_noStrata_spaghettiSingle_noIHME.pdf'),
    height = 5, width = 5)
{
  par(mfrow = c(1,1),
      lend=1)
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000

    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin2$upper[direct.admin2$region ==
                                              as.character(area)]),
                      na.rm = T) + 25
    }
    
    if (nrow(tmp.area) > 0 &
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", 
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = paste(admin2.names$GADM[area.idx],
                        poly.adm2$NAME_1[area.idx], 
                        sep = ", "))
      legend('topright',
             bty = 'n', 
             col = c(cols[1:length(surveys)], 
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        "Betabinomial"))
      
    } else {
      
      #### Add Direct ####
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = admin2.names$GADM[area.idx])
                lines(pane.years, tmp$mean,
                      cex = tmp$cex2,
                      col = cols[svy.idx],
                      main = surveys[svy.idx],
                      lwd = 2)
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, 
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], 
                              sep = ", "))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
      }
      
      #### Add Betabinomial ####
      lines(plot.years[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(plot.years[pred.idx],
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(plot.years,
                    rev(plot.years)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      
      #### Legend ####
      legend('topright', bty = 'n', 
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2, lty = c(rep(1, length(surveys))),
             legend = c(survey.legends,
                        "Betabinomial"),
             cex = 0.6)

    }
  }
}
dev.off()

## Spaghetti Singles no IHME 6 per####

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod,
           '_admin2Bench_noStrata_spaghettiSingle_noIHME_6per.pdf'),
    height = 9, width = 6)
{
  par(mfrow = c(3,2),
      lend=1)
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin2$upper[direct.admin2$region ==
                                              as.character(area)]),
                      na.rm = T) + 25
    }
    
    if (nrow(tmp.area) > 0 &
        sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", 
           ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           main = paste(admin2.names$GADM[area.idx],
                        poly.adm2$NAME_1[area.idx], 
                        sep = ", "))
      legend('topright',
             bty = 'n', 
             col = c(cols[1:length(surveys)], 
                     'black'),
             lwd = 2, 
             legend = c(survey.legends,
                        "Betabinomial"))
      
    } else {
      
      #### Add Direct ####
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = admin2.names$GADM[area.idx])
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  main = surveys[svy.idx],
                  lwd = 2)
            
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, 
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], 
                              sep = ", "))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[tmp.est.idx],
                          rev(pane.years[tmp.est.idx])),
                    y = c(tmp$upper[tmp.est.idx],
                          rev(tmp$lower[tmp.est.idx])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
      }
      
      #### Add Betabinomial ####
      lines(plot.years[obs.idx], 
            tmp.area$median[obs.idx],
            col = 'black',
            lwd = 2)
      lines(plot.years[pred.idx],
            tmp.area$median[pred.idx], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(plot.years,
                    rev(plot.years)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      
      #### Legend ####
      legend('topright', bty = 'n', 
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2, lty = c(rep(1, length(surveys))),
             legend = c(survey.legends,
                        "Betabinomial"),
             cex = 0.6)
      
    }
  }
}
dev.off()


## Hazards over time ####


pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod,
           '_admin2Bench_temporal.pdf'),
    height = 5, width = 7.5)
{
  par(mfrow =c(1,1),
      lend=1)
  
  for(age in ages){
    fixed.idx <- grepl(paste0(age,"$"),
                       row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table$`0.5quant`[fixed.idx]
    
    age.idx <- grepl(age,
                     temporals$group)
    tmp <- temporals[age.idx,]
    
    if(match(age, ages) == 1){
      plot.min <- min(temporals$median +
                        min(fixed.eff.table$`0.5quant`)) - 0.25
      plot.max <- max(temporals$median +
                        max(fixed.eff.table$`0.5quant`)) + 0.25
      
      plot(NA,
           xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           yaxt = 'n',
           xlab = "Year",
           ylab = "Monthly hazard",
           main = "")
      axis(2, at = seq(plot.min,
                       plot.max, 1),
           labels = round(expit(seq(plot.min,
                                    plot.max, 1))*1000, 
                          digits = 1))
    }
    
    lines(plot.years,
          tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], 
          lwd = 2)
  }
  
  legend('bottomleft',
         cex = 0.7,
         lwd = 2,
         col = c(age.cols),
         legend = c(ages),
         bty = 'n')
}
dev.off()

## Fixed Effects ####
pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod, 
           '_admin2Bench_coefs.pdf'), 
    height = 5, width = 5)
{
  par(mfrow =c(1,1),
      lend=1)
  
  for(age in ages){
    
    fixed.idx <- grepl(paste0(age,"$"), 
                       row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table[fixed.idx,]
    
    tmp <- temporals[age.idx,]
    age.idx <- grepl(paste0(age),
                     temporals$group)
    
    if(match(age, ages) == 1 ){
      plot.min <- min(fixed.eff.table$`0.025quant`) - 0.025
      plot.max <- max(fixed.eff.table$`0.975quant`) + 0.025
      
      plot(NA,
           xlim = c(0, length(ages) + 1),
           xaxt = 'n',
           yaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Age",
           ylab = "",
           main = "")
      axis(1, at = 1:length(ages),
           labels = ages, cex.axis = 0.7)
      axis(2, at = seq(plot.min, plot.max, 1),
           labels = round(expit(seq(plot.min, plot.max, 1))*1000,
                          digits = 1))
    }
    
    points(match(age,ages),
           fixed.eff$`0.5quant`,
           pch = 19,
           col = age.cols[match(age, ages)])
    segments(match(age,ages),
             fixed.eff$`0.025quant`,
             match(age,ages),
             fixed.eff$`0.975quant`,
             col = age.cols[match(age, ages)])
    
  }
  
  legend('bottomleft',
         cex = 0.7,
         col = age.cols,
         pch =19,
         legend = ages,
         bty = 'n')
}
dev.off()

## Random Slopes ####

if (grepl("randomSlopes",time.mod)) {
  pdf(paste0(folder.name,
             '/Plots/Betabinomial/Admin2/',
             country, '_',
             time.mod,
             '_admin2Bench_randomSlopes.pdf'),
      height = 5, width = 5)
  years.std <- ((plot.years)-mean(plot.years))/sd(plot.years)
  plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                        years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
  par(lend=1)
  plot(NA,
       xlim = range(years.std),
       ylim = plot.range,
       ylab = "effect",
       xlab="year (standardized)")
  abline(h = 0, lty = 2)
  region.cols <- viridis_pal(option = "A")(nrow(posteriorRandomSlopes))
  for (i in 1:nrow(posteriorRandomSlopes)) {
    abline(0, posteriorRandomSlopes$`0.5quant`[i],
           col=region.cols[i])
  }
  legend("topleft",
         legend = admin2.names[,1],
         col = region.cols,
         lty = 1,
         ncol= 2,
         cex = 0.5)
  dev.off()
}

## Spatial Effects ####
med.palette <- rev(brewer.pal(n = 5,
                              name = "RdBu"))
med.int <- classIntervals(round(spaces$median[spaces$label == "Total"], 3),
                          n = 5,
                          style = 'fixed',
                          fixedBreaks = quantile(round(spaces$median[spaces$label == "Total"], 
                                                       3),
                                                 c(0,.25,.5,.75,1)))
med.col <- findColours(med.int,
                       med.palette)

pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod,
           '_admin2Bench_spatial.pdf'))
par(mar = c(0.15,0.15,0.15,0.15),
    mai = c(0.1,0.1,0.1,0.1),
    lend=1)
plot(poly.adm2,
     col = med.col,
     xlim = poly.adm2@bbox['x',],
     ylim = poly.adm2@bbox['y',],
     axes = F)
legend('bottomleft',
       fill = med.palette,
       legend = names(attr(med.col, 'table')),
       bty = 'n', 
       cex = 0.75)
dev.off()

## Spaghetti All ####

# change the region names from Internal to GADM for legend labels
res.admin2$overall$region1.gadm <- 
  res.admin2$overall$region.gadm <- 
  res.admin2$overall$region.orig <- 
  res.admin2$overall$region
for (i in 1:nrow(admin2.names)) {
  res.admin2$overall$region[as.character(res.admin2$overall$region) == 
                              as.character(admin2.names$Internal[i])] <- paste(as.character(admin2.names$GADM[i]),
                                                                               as.character(poly.adm2$NAME_1[i]), sep =", ")
  res.admin2$overall$region.gadm[as.character(res.admin2$overall$region.orig) == 
                                   as.character(admin2.names$Internal[i])] <- paste(as.character(admin2.names$GADM[i]))
  res.admin2$overall$region1.gadm[as.character(res.admin2$overall$region.orig) == 
                                    as.character(admin2.names$Internal[i])] <- paste(as.character(poly.adm2$NAME_1[i]))
  
}

### By Admin1 ####
numberAreasTotal <- nrow(admin2.names)
numberPages <- nrow(admin1.names)

res.admin2.order <- res.admin2$overall %>% 
  filter(years.num == end.year) %>%
  arrange(region.gadm)
areaOrder <- res.admin2.order$region.orig

# loop and make plots
for (i in 1:numberPages) {
  
  if (i != numberPages) {
    #areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
    areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
                                            as.character(admin1.names$GADM[i])]
  } else {
    #areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
    areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
                                            as.character(admin1.names$GADM[i])]
  }
  tmp <- res.admin2$overall[res.admin2$overall$region.orig %in% areas,]
  
  pdf(paste0(folder.name,
             '/Plots/Betabinomial/Admin2/',
             country, '_',
             time.mod, 
             '_admin2Bench_spaghetti_allByAdmin1_',
             i,'.pdf'),
      height = 6, width = 6)
  {
    par(lend=1)
    
    g <- ggplot(tmp, aes(x = years.num,
                         y = median*1000,
                         col = region.gadm)) +
      geom_line() +
      geom_point() +
      theme_light() +
      xlab("Year") +
      ylab("U5MR: deaths per 1000 live births") +
      ggtitle(paste0(unique(tmp$region1.gadm))) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8)) +
      ylim(c(0, max(res.admin2$overall$median)*1000))
    g <- g + 
      guides(col = guide_legend(ncol=3,
                                title = "Admin 2")) 
    print(g)
    dev.off()
  }
}

### By Median ####
numberAreasPerPage <- 15
numberAreasTotal <- nrow(admin2.names)
numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)

res.admin2.order <- res.admin2$overall %>% 
  filter(years.num == end.year) %>%
  arrange(median)
areaOrder <- res.admin2.order$region.orig

numberAreasPerPage <- numberAreasTotal/numberPages

if(numberAreasTotal%%numberPages != 0){
  numberAreasPerPage <- floor(numberAreasPerPage)
}


pdf(paste0(folder.name,
           '/Plots/Betabinomial/Admin2/',
           country, '_',
           time.mod, 
           '_admin2Bench_spaghetti_allByMedian.pdf'),
    height = 6, width = 6)
par(lend=1)
for (i in 1:numberPages) {
  
  if(i != numberPages){
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
  }else if(i == numberPages){
    areas <- areaOrder[((i-1)*numberAreasPerPage + 1):numberAreasTotal]
  }

  tmp <- res.admin2$overall[res.admin2$overall$region.orig %in% areas,]

  {
    print(ggplot(tmp, aes(x = years.num,
                          y = median*1000,
                          col = region)) +
            geom_line() +
            geom_point() +
            theme_light() +
            xlab("Year") +
            ylab("U5MR: deaths per 1000 live births") +
            ggtitle(country) +
            theme(legend.position = "bottom",
                  legend.text = element_text(size = 6)) +
            guides(col = guide_legend(ncol=3,
                                      title = element_blank())) +
            ylim(c(0, max(res.admin2$overall$median)*1000)))

  }
}
dev.off()

### By GADM ####
pdf(paste0(folder.name, 
           '/Plots/Betabinomial/Admin2/',
           country, '_', 
           time.mod,
           '_admin2Bench_spaghetti_all_GADMorder.pdf'))
par(lend=1)

# order data by GADM
areaOrder <- admin2.names$Internal

# change the order 
# loop and make plots
for (i in 1:numberPages) {
  if (i != numberPages) {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
  } else {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
  }
  tmp <- res.admin2$overall[res.admin2$overall$region.orig %in% areas,]
  
  print(ggplot(tmp, aes(x = years.num, 
                        y = median*1000, col = region)) +
          geom_line() +
          geom_point() +
          theme_light() +
          xlab("Year") +
          ylab("U5MR: deaths per 1000 live births") +
          ggtitle(country) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8)) +
          guides(col = guide_legend(ncol=3,
                                    title = "Admin 2")) +
          ylim(c(0, max(res.admin2$overall$median)*1000)))
}

dev.off()

