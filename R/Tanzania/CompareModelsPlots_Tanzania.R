rm(list=ls())

#### Libraries ####
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

#### Parameters ####
country <- "Tanzania"
beg.year <- 1990
end.year <- 2020
cluster <- FALSE
message("If have the same subfolder structure as 
        AfricaAdmin2Estimates/Data/countryDataFolders/. Do nothing!\n
        Otherwise, edit the following paths as needed.\n")

data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '../../Analysis/R'
ihme.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '../HIV'

setwd(data.dir)

if(!exists("sheet_key", envir = .GlobalEnv)){
   source(paste0(code.dir.rel,'/LoadCommandCenter.R'))
}

CountryList <- sheets_read(sheet_key, sheet = "CountryList")
#CountryList <- read.csv('CountryList.csv')
folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
                       gsub(" ", "", folder.name))

#### which models are we comparing? (currently for admin1 and admin2, not for national) ####

# here, we are setting:
# - model.labels: labels that were used for the saved files, in order to load the data
# - model.names: names to be used for plotting, so they look nice
#
# some possible model labels and names:
# 1. RW2 main effect, RW2xICAR interaction, no random slopes
# - model.labels <- "rw2"
# - model.names <- "no RS: RW2 main, RW2xICAR"
# 2. RW2 main effect, AR1xICAR interaction, no random slopes
# - model.labels <- "ar1"
# - model.names <- "no RS: RW2 main, AR1xICAR"
# 3. RW2 main effect, AR1xICAR interaction, random slopes
# - model.labels <- "rw2main_randomSlopes_ar1xICAR"
# - model.names <- "RS: RW2 main, AR1xICAR"
# 4. RW2 main effect, RW1xICAR interaction, random slopes
# - model.labels <- "rw2main_randomSlopes_rw1xICAR"
# - model.names <- "RS: RW2 main, RW1xICAR"
#
# you can make plots for any combination of the above models by 
# setting model.labels and model.names to be vectors, for example:
# - model.labels <- c("rw2","ar1","rw2main_randomSlopes_ar1xICAR","rw2main_randomSlopes_rw1xICAR")
# - model.names <- c("no RS: RW2 main, RW2xICAR","no RS: RW2 main, AR1xICAR","RS: RW2 main, AR1xICAR","RS: RW2 main, RW1xICAR")

# model.labels <- c("rw2main_randomSlopes_ar1xICAR","rw2main_randomSlopes_rw1xICAR",
#                   "frameAgg_rw2main_randomSlopes_ar1xICAR", "frameAgg_rw2main_randomSlopes_rw1xICAR")
# model.names <- c("Unstratified: AR1xICAR","Unstratified: RW1xICAR",
#                  "Stratified: AR1xICAR","Stratified: RW1xICAR")

model.labels <- c("rw2main_randomSlopes_ar1xICAR","rw2main_randomSlopes_rw1xICAR")
model.names <- c("Unstratified: AR1xICAR","Unstratified: RW1xICAR")

#### do you want to make plots for national comparisons? ####

# set runNational to be TRUE to run the national comparisons and FALSE to not run them

runNational <- TRUE

#### what time model for the interaction with ICAR do you want to use for national comparisons? ####

# this could be one of the following, or a vector of multiple of them
# - "rw2"
# - "ar1"

time.mod.nationals <- c("rw2")

#### do you want to make plots for admin1 or admin2 results, or both? ####

# set admin.to.run to be:
# - "admin1" to make plots for admin 1 results
# - "admin2" to make plots for admin 2 results
# - c("admin1", "admin2") to make plots for both admin 1 and admin 2 results

admin.to.run <- c("admin2")

#### do you want to make plots for benchmarked or non-benchmarked results, or both? ####

# set bench.indicators.to.run to be:
# - FALSE in order to make plots for non-benchmarked analyses
# - TRUE in order to make plots for benchmarked analyses
# - c(FALSE, TRUE) in order to make plots for both non-benchmarked and benchmarked analyses

bench.indicators.to.run <- c(TRUE)

#### function to load admin 1 and/or admin 2 results, benchmarked and/or non-benchmarked ####

# load results function
# - input 
#   - model.labs: vector of model labels, from model.labels object above
#   - model.nams: vector of model names (used for plotting later), from model.names object above
#   - admin: either "admin1" or "admin2" in order to load results for admin 1 or admin 2, respectively
#   - bench: either TRUE or FALSE in order to load benchmarked or non-benchmarked results, respectively
# - output
#   - list with length equal to length(model.labs) with one component for each of the models from model.labs
#   - each of these components is itself a list with the following components
#     - [["res"]]: model results from INLA fit, loaded from e.g. countryDataFolders/Malawi/Malawi_res_....rda
#     - [["space"]]: spatial effects, loaded from e.g. countryDataFolders/Malawi/Malawi_..._noStrata_spatials.rda
#     - [["temp"]]: temporal effects, loaded from e.g. countryDataFolders/Malawi/Malawi_..._noStrata_temporals.rda
#     - [["st"]]: spatiotemporal effects, loaded from e.g. countryDataFolders/Malawi/Malawi_..._noStrata_spatiotemporals.rda
#       - currently, this is just the time interaction with ICAR and does NOT include the random slopes
#     - [["model.name"]]: name of the model, from model.nams argument
#   - therefore, the output is a list of lists

loadResults <- function(model.labs, model.nams, admin = NULL, bench = FALSE) {
  
  # check input is in correct format
  if (is.null(admin)) stop("must specify admin argument to be either 'admin1' or 'admin2'")
  if (length(admin) != 1) stop("admin argument must be either 'admin1' or 'admin2'")
  if (!(admin %in% c("admin1", "admin2"))) stop("admin argument must be either 'admin1' or 'admin2'")
  if (!is.vector(model.labs)) stop("model labels must be a vector")
  if (!is.vector(model.nams)) stop("model names must be a vector")
  if (length(model.labs) != length(model.nams)) stop("differing lengths between model labels and names")
  
  # store results
  results <- list()
  length(results) <- length(model.labs)
  names(results) <- model.labs
  
  # benchmarking identifier
  if (bench) bench.id <- "Bench" else if (!bench) bench.id <- ""
  
  # loop and load data into a list
  for (i in 1:length(model.labs)){
    if(grepl("frameAgg", model.labs[i])){
      temp.label <- gsub("Agg","",model.labs[i])
    }else{
      temp.label <- model.labs[i]
    }
    load(paste0(folder.name,'/', country, '_res_',model.labs[i],'_',admin,bench.id,'.rda'))
    load(paste0(folder.name,'/', country, '_',temp.label,'_',admin,bench.id,'_noStrata_spatials.rda'))
    load(paste0(folder.name,'/', country, '_',temp.label,'_',admin,bench.id,'_noStrata_spatiotemporals.rda'))
    load(paste0(folder.name,'/', country, '_',temp.label,'_',admin,bench.id,'_noStrata_temporals.rda'))
    
    spacetimes$model.lab <- model.labs[i]
    spacetimes$model.name <- model.nams[i]
    
    model.results <- list()
    if (admin == "admin1"){ 
      if(grepl("frameAgg", model.labs[i])){
        model.results$res <- res.frame.agg.admin1 
      }else{
        model.results$res <- res.admin1 
      }
      
    }else{
      if(grepl("frameAgg", model.labs[i])){
        model.results$res <- res.frame.agg.admin2 
      }else{
        model.results$res <- res.admin2
      }
    }
    model.results$space <- spaces
    model.results$temp<- temporals
    model.results$st <- spacetimes
    model.results$model.name <- model.nams[i]
    results[[model.labs[i]]] <- model.results
  }
  return(results)
}

#### Get Survey years #### 

#SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
#SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
#SurveyInfo <- read.csv("SurveyInfo.csv", header = T)
#surveys <- SurveyInfo$Survey.Year[SurveyInfo$Country == country &
#                                    SurveyInfo$`GPS.` == "Y"]
#frames <- SurveyInfo[SurveyInfo$Country == country, c("Survey.Year", "Frame",
#                                                      "PropFrame.")]

SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]


frames <- SurveyInfo[SurveyInfo$Country == country, c("Survey Year", "Frame",
                                                      "PropFrame?")]
names(frames)[match("Survey Year", names(frames))] <- "Survey.Year"
frames <- frames[!is.na(frames$Frame),]
frames.unique <- unique(frames$Frame)

#### Use HIV Adjusted data? ####

#HIV.sheet <- gs_read(sheet_key, ws = "HIV")
HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
#HIV.sheet <- read.csv("HIV.csv", header = T)
HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                unique(HIV.country$`UNAIDS data?`) == "Y")
#useHIVAdj <- (unique(HIV.country$MM.Adj.by.IGME) == "Y" & 
#                unique(HIV.country$UNAIDS.data.) == "Y")


#### Load Data ####
load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat.rda'))
load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat_Names.rda'))

load(paste0(folder.name, '/', country, '_cluster_dat.rda'))
mod.dat$years <- as.numeric(as.character(mod.dat$years))
dat.years <- sort(unique(mod.dat$years))
beg.years <- seq(beg.year, end.year,5)
end.years <- beg.years + 4
periods <- paste(beg.years, end.years, sep = "-")
mod.dat$period <- as.character(cut(mod.dat$years, breaks = c(beg.years, beg.years[length(beg.years)]+5),
                                   include.lowest = T, right = F, labels = periods))
mod.dat$strata.orig <- mod.dat$strata
mod.dat$strata <- mod.dat$urban
mod.dat$country <- as.character(country)

mod.dat$frame <- NA
for(i in 1:nrow(frames)){
  year <- frames$Survey.Year[i]
  frame <- as.character(frames$Frame[i])
  mod.dat$frame[mod.dat$survey == year] <- frame
}

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
                   '2', sep = "_"), list.files(paste0(folder.name, poly.file)))) != 0){
  poly.adm2 <- readOGR(dsn = poly.path,
                       layer = as.character(poly.layer.adm2))
}

if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

#### Load IHME data ####
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

if(country == "Malawi"){
  ihme.ests$adm1 <- ihme.ests$adm2
  ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
  doAdmin2 <- FALSE
}

source(paste0(hand.dir.rel,
              '/IHMEHand_', country, '.R'))

#### National ####

frame.cols <- c("deepskyblue", "deeppink", "navy",
                "orange")[1:length(frames.unique)]

if (runNational) {
  
  for (time.mod.national in time.mod.nationals) {

    load(paste0(folder.name,'/', country, '_', time.mod.national, '_natlBench.rda'))
    load(paste0(folder.name,'/', country, '_res_', time.mod.national, '_natlBench.rda'))
    load(paste0(folder.name,'/', country, '_frame_', time.mod.national, '_natlBench.rda'))
    load(paste0(folder.name,'/', country, '_res_frameAgg_', time.mod.national, '_natlBench.rda'))
    
    frame.legend <- unique(res.frame.agg.natl$overall$frame)
    pdf(paste0(folder.name, '/Plots/Betabinomial/', country, '_', time.mod.national, '_UrbRurByFrame_Bench.pdf'),
        width = 4, height = 4)
    {
      plot.min <- (min(res.frame.agg.natl$stratified$lower) -0.025)*1000
      plot.max <- (max(res.frame.agg.natl$stratified$upper) + 0.025)*1000
      
      plot(NA, xlim = c(beg.year,end.year),
           ylim = c(plot.min, plot.max),
           xlab = "Year",
           ylab = "U5MR")
      frame.idx <- 0
      for(frame in frame.legend){

        frame.idx <- frame.idx + 1   
        max.year <- max(mod.dat$years[mod.dat$frame == frame])
        frame.length <- length(beg.year:max.year)
        strata.idx <- 0
        for(strata in c("urban", "rural")){
          strata.idx <- strata.idx + 1
          
          tmp.idx <- grepl(frame, res.frame.agg.natl$stratified$strata) &
            grepl(strata, res.frame.agg.natl$stratified$strata)
          
          tmp <- res.frame.agg.natl$stratified[tmp.idx,]
          polygon(x = c(beg.year:max.year, rev(beg.year:max.year)),
                  y = c(tmp$upper[1:frame.length], rev(tmp$lower[1:frame.length]))*1000,
                  col = alpha(frame.cols[frame.idx],0.25),
                  border = frame.cols[frame.idx])
          lines(beg.year:max.year, tmp$median[1:frame.length]*1000,
                col = frame.cols[frame.idx], lty = strata.idx)
        }
      }
      legend('topright', lty = rep(c(1,2), length(frame.legend)),
             col = rep(frame.cols, each = 2),
             legend = paste(rep(frame.legend, each = 2),
                            rep(c("urban", "rural"), length(frame.legend)), sep = "-"),
             lwd = 2, bty = 'n', cex = 0.75)
    }
    dev.off()
    
    mod.cols <- c("navyblue", "deeppink")
    pdf(paste0(folder.name, '/Plots/Betabinomial/', country, '_', time.mod.national, '_StratvsNon_Bench.pdf'),
        width = 4, height = 4)
    {
      max.year <- max(mod.dat$years)
      max.id <- which.max(beg.year:end.year == max.year)
      plot.max <- max(c(res.natl$overall$upper,
                        res.frame.agg.natl$final$upper))*1000 + 25
      plot.min <- min(c(res.natl$overall$lower,
                        res.frame.agg.natl$final$lower))*1000 - 25
      
      plot(NA, xlim = c(beg.year, end.year),
           ylim = c(plot.min,plot.max),
           xlab = "Year",
           ylab = "U5MR")
      polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
              y = c(res.natl$overall$upper, rev(res.natl$overall$lower))*1000,
              col = alpha(mod.cols[1], .25),
              border = mod.cols[1])
      lines((beg.year:end.year)[1:max.id],
            res.natl$overall$median[1:max.id]*1000, lwd = 2, col = mod.cols[1])
      lines((beg.year:end.year)[(max.id +1):(end.year-beg.year + 1)],
            res.natl$overall$median[(max.id +1):(end.year-beg.year + 1)]*1000,
            lwd = 2, lty = 2, col = mod.cols[1])
      polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
              y = c(res.frame.agg.natl$final$upper, rev(res.frame.agg.natl$final$lower))*1000,
              col = alpha(mod.cols[2], .25),
              border = mod.cols[2])
      lines((beg.year:end.year)[1:max.id],
            res.frame.agg.natl$final$median[1:max.id]*1000, lwd = 2, col = mod.cols[2])
      lines((beg.year:end.year)[(max.id +1):(end.year-beg.year + 1)],
            res.frame.agg.natl$final$median[(max.id +1):(end.year-beg.year + 1)]*1000,
            lwd = 2, lty = 2, col = mod.cols[2])
      
      legend('topright', fill = alpha(mod.cols, .25),
             border = mod.cols, legend = c("Unstratified", "Stratified"),
             bty = 'n', cex = .75)
    }
    dev.off()
  }
  
}
  

#### Admin 1 and/or 2, bench and/or non-bench ####

# loop through admin 1 and admin 2 results
for (admin in admin.to.run) {
  
  # testing
  # admin <- "admin2"
  
  # load direct estimates
  if(useHIVAdj){
    load(paste0(folder.name, '/', country,
                '_directHIV_', admin, '.rda'))
    if (admin == "admin1") {
      direct.admin <- direct.admin1
    } else {
      direct.admin <- direct.admin2
    }
  }else{
    load(paste0(folder.name, '/', country,
                '_direct_', admin, '.rda'))
    if (admin == "admin1") {
      direct.admin <- direct.admin1
    } else {
      direct.admin <- direct.admin2
    }
  }
  
  # set current region names
  if (admin == "admin1") {
    current.admin.names.Internal <- admin1.names$Internal
    current.admin.names.toPlot <- admin1.names$GADM
    current.admin.names.adm1.adm2 <- NA
    current.admin.mat <- admin1.mat
  } else {
    current.admin.names.Internal <- admin2.names$Internal
    current.admin.names.adm1.adm2 <- data.frame(admin2 = poly.adm2$NAME_2,
                                                admin1 = poly.adm2$NAME_1)
    current.admin.names.toPlot <- paste0(poly.adm2$NAME_2, ",\n", poly.adm2$NAME_1)
    current.admin.mat <- admin2.mat
  }
  
  # loop through benchmarked and non-benchmarked results
  for (bench.ind in bench.indicators.to.run) {
    
    # testing
    # bench.ind <- TRUE
    
    # benchmarked results saving ID
    if (bench.ind) bench.id <- "Bench" else bench.id <- ""
    
    # load the results
    model.results <- loadResults(model.labels,model.names, admin = admin, bench = bench.ind)
    
    # data frame of spatiotemporal results for plotting
    st <- model.results[[1]]$st
    for (i in 2:length(model.results)) {
      st <- rbind.data.frame(st,model.results[[i]]$st)
    }
    st$region.name <- current.admin.names.toPlot[match(st$region, current.admin.names.Internal)]
    
    # model.cols <- brewer.pal(length(model.results),"Set1")
    model.cols <- #viridis_pal(option="A")(length(model.results))
    c("darkblue", "firebrick", "goldenrod", "darkgreen")
    pdf(paste0(folder.name, '/Plots/Betabinomial/', country, '_', 
               admin, bench.id,
               '_Spatiotemporal.pdf'),
        width = 16/1.3, height = 9/1.3)
    
    {
      par(mfrow = c(2,3))
      for(area in current.admin.names.Internal){
        plot.min <- min(st$lower)
        plot.max <- max(st$upper)
        tmp <- st[st$region == area,]
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(plot.min, plot.max),
             xlab = "Year", ylab = "Effect Size",
             main = current.admin.names.toPlot[match(area, current.admin.names.Internal)],
             cex.main = 0.8)
        abline(h=0, col = 'grey35', lty=2)
        for (model.index in 1:length(model.results)) {
          tmpcol <- model.cols[model.index]
          lines(beg.year:end.year, tmp$median[tmp$model.lab == model.labels[model.index]],
                lwd = 2, col = tmpcol, lty=1)
          polygon(c(beg.year:end.year, 
                    rev(beg.year:end.year)),
                  c(tmp$upper[tmp$model.lab == model.labels[model.index]], 
                    rev(tmp$lower[tmp$model.lab == model.labels[model.index]])),
                  col = alpha(tmpcol, 0.2), border = FALSE)
        }
        legend('topright', 
               col = alpha(model.cols[1:length(model.results)], 0.2), 
               bty = 'n', 
               cex = 0.75,
               pt.cex = 2,
               pch = 15,
               lty=0, lwd = 2,
               legend = model.names)
        legend('topright', 
               col = model.cols[1:length(model.results)],
               bty = 'n', 
               cex = 0.75,
               lty=1, lwd = 2,
               pch = NA,
               legend = rep("", length(model.results)))
      }
    }
    
    dev.off()
    
    ages <- unique(model.results[[1]]$temp$group)[1:6]
    
    pdf(paste0(folder.name, '/Plots/Betabinomial/', country, '_',
               admin, bench.id,
               '_Temporal.pdf'),
        width = 16/1.5, height = 9/1.5)
    
    {
      par(mfrow =c(2,3))
      for(age in ages){
        
        idx <- which(model.results[[1]]$temp$group == age &
                       !is.na(model.results[[1]]$temp$group))
        
        plot.min <- min(model.results[[1]]$temp$median[idx]) - 0.025
        plot.max <- max(model.results[[1]]$temp$median[idx]) + 0.025
        
        plot(NA, 
             xlab = "Year",
             ylab = "Effect size",
             xlim = c(beg.year,end.year),
             ylim = c(plot.min,plot.max),
             main = age)
        
        for (i in 1:length(model.results)) {
          lines( beg.year:end.year,
                 model.results[[model.labels[i]]]$temp$median[idx],
                 xlab = "Year",
                 ylab = "Effect size", 
                 ylim = c(plot.min,plot.max),
                 col = model.cols[i], lty = 1, lwd = 2)
        }
        legend('topright', lwd = 2, col = model.cols[1:length(model.results)],
               legend = model.names,
               bty = 'n', cex = 0.85)
      }
    }
    
    dev.off()
    
    survey.cols <- gray.colors(n = length(surveys),
                                start = 0,
                               end = .75, gamma = 2.2)
    
    pdf(paste0(folder.name, '/Plots/Betabinomial/', country, '_',
               admin, bench.id,
               '_modelCompare.pdf'),
        width = 8, height = 8)
    
    {
      
      par(mfrow = c(2,2), lend = 1)
      for(area in current.admin.names.Internal){
        
        tmp.direct <- direct.admin[direct.admin$region == as.character(area),]
        
        plot.min <- 1000
        plot.max <- 0
        for (i in 1:length(model.results)) {
          if(grepl("frameAgg", model.labels[i])){
            tmp <- model.results[[i]]$res$final[model.results[[i]]$res$final$region==
                                                          as.character(area),]
          }else{
            tmp <- model.results[[i]]$res$overall[model.results[[i]]$res$overall$region==
                                                          as.character(area),]
          }
          plot.min <- min(plot.min,
                          min(tmp$lower, na.rm=TRUE),
                          na.rm = TRUE)
          plot.max <- max(plot.max,
                          max(tmp$upper, na.rm=TRUE),
                          na.rm=TRUE)
        }
        plot.min <- min(plot.min,min(tmp.direct$mean[tmp.direct$region == as.character(area)], na.rm=TRUE), na.rm=TRUE)
        plot.max <- max(plot.max,max(tmp.direct$mean[tmp.direct$region == as.character(area)], na.rm=TRUE), na.rm=TRUE)
        
        plot.min <- plot.min*1000
        plot.max <- plot.max*1000
        
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(0, plot.max),
             xlab = "Year", ylab = "U5MR",
             main = current.admin.names.toPlot[match(area, current.admin.names.Internal)],
             cex.main = 0.8)
        
        surv.years <- seq(beg.year +2, end.year-3,5)
        svy.idx <- 0
        for(survey in surveys){
          svy.idx <- svy.idx + 1
          svy.line <- tmp.direct$mean[tmp.direct$surveyYears == survey]
          est.ids <- which(!is.na(svy.line))
          if(length(svy.line) != 0){
            points(surv.years[est.ids], svy.line[est.ids]*1000,
                   pch = 19, col = survey.cols[svy.idx])
            lines(surv.years[est.ids], svy.line[est.ids]*1000,
                  lwd = 1, col = survey.cols[svy.idx])
          }
        }
        
        for (i in 1:length(model.results)) {
          if(grepl("frameAgg", model.labels[i])){
            tmp <- model.results[[i]]$res$final[model.results[[i]]$res$final$region==
                                                  as.character(area),]
          }else{
            tmp <- model.results[[i]]$res$overall[model.results[[i]]$res$overall$region==
                                                    as.character(area),]
          }
          lines(beg.year:end.year, 
                tmp$median[tmp$region==as.character(area)]*1000,
                lwd = 2, col = model.cols[i])
        }
        legend('topright', 
               lwd = c(rep(1, length(surveys)),
                       rep(2,length(model.results))),
               col = c(survey.cols, model.cols), 
               pch = c(rep(19, length(surveys)),
                       rep(NA, length(model.results))),
               legend = c(surveys, model.names),
               bty = 'n', cex = 0.75)
        
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(0, plot.max),
             xlab = "Year", ylab = "U5MR",
             main = current.admin.names.toPlot[match(area, current.admin.names.Internal)],
             cex.main = 0.8)
        
        for (i in 1:length(model.results)) {
          if(grepl("frameAgg", model.labels[i])){
            tmp <- model.results[[i]]$res$final[model.results[[i]]$res$final$region==
                                                  as.character(area),]
          }else{
            tmp <- model.results[[i]]$res$overall[model.results[[i]]$res$overall$region==
                                                    as.character(area),]
          }
          polygon(c(beg.year:end.year, rev(beg.year:end.year)),
                  c(tmp$upper[tmp$region==as.character(area)], 
                    rev(tmp$lower[tmp$region==as.character(area)]))*1000,
                  col = alpha(model.cols[i], 0.25), border = FALSE)
        }
        
        legend('topright', fill = alpha(model.cols,0.25),
               bty = 'n', cex = 0.75,
               border = model.cols,
               legend = model.names)
      }
      
    }
    
    dev.off()
    
    pdf(paste0(folder.name, '/Plots/Betabinomial/', country, '_',
               admin, bench.id,
               '_modelIHMECompare.pdf'),
        width = 8, height = 8)
    
    {
      par(mfrow = c(2,2), lend = 1)
      for(area in current.admin.names.Internal){
        
        #testing
        # area <- current.admin.names.Internal[2]
        
        tmp.direct <- direct.admin[direct.admin$region == as.character(area),]
        if (admin == "admin1") {
          tmp.ihme <- ihme.ests[["adm1"]][ihme.ests[["adm1"]]$ADM1_NAME == 
                                            as.character(current.admin.names.toPlot[match(area, current.admin.names.Internal)]),]
        } else {
          tmp.ihme <- ihme.ests[["adm2"]][ihme.ests[["adm2"]]$ADM2_NAME == 
                                            as.character(current.admin.names.adm1.adm2$admin2)[match(area, current.admin.names.Internal)] &
                                            ihme.ests[["adm2"]]$ADM1_NAME == 
                                            as.character(current.admin.names.adm1.adm2$admin1)[match(area, current.admin.names.Internal)],]
        }

        plot.min <- 1000
        plot.max <- 0
        for (i in 1:length(model.results)) {
          if(grepl("frameAgg", model.labels[i])){
            tmp <- model.results[[i]]$res$final[model.results[[i]]$res$final$region==
                                                  as.character(area),]
          }else{
            tmp <- model.results[[i]]$res$overall[model.results[[i]]$res$overall$region==
                                                    as.character(area),]
          }
          plot.min <- min(plot.min,
                          min(tmp$lower, na.rm=TRUE),
                          na.rm = TRUE)
          plot.max <- max(plot.max,
                          max(tmp$upper, na.rm=TRUE),
                          na.rm=TRUE)
        }
        plot.min <- min(plot.min,min(tmp.direct$mean[tmp.direct$region == as.character(area)], na.rm=TRUE), na.rm=TRUE)
        plot.max <- max(plot.max,max(tmp.direct$mean[tmp.direct$region == as.character(area)], na.rm=TRUE), na.rm=TRUE)
        
        plot.min <- plot.min*1000
        plot.max <- plot.max*1000
        
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(0, plot.max),
             xlab = "Year", ylab = "U5MR",
             main = current.admin.names.toPlot[match(area, current.admin.names.Internal)],
             cex.main = 0.8)
        
        surv.years <- seq(beg.year +2, end.year-3,5)
        svy.idx <- 0
        for(survey in surveys){
          svy.idx <- svy.idx + 1
          svy.line <- tmp.direct$mean[tmp.direct$surveyYears == survey]
          est.ids <- which(!is.na(svy.line))
          if(length(svy.line) != 0){
            points(surv.years[est.ids], svy.line[est.ids]*1000,
                   pch = 19, col = survey.cols[svy.idx])
            lines(surv.years[est.ids], svy.line[est.ids]*1000,
                  lwd = 1, col = survey.cols[svy.idx])
          }
        }
        
        for (i in 1:length(model.results)) {
          if(grepl("frameAgg", model.labels[i])){
            tmp <- model.results[[i]]$res$final[model.results[[i]]$res$final$region==
                                                  as.character(area),]
          }else{
            tmp <- model.results[[i]]$res$overall[model.results[[i]]$res$overall$region==
                                                    as.character(area),]
          }
          lines(beg.year:end.year, 
                tmp$median[tmp$region==as.character(area)]*1000,
                lwd = 2, col = model.cols[i])
        }
        lines(tmp.ihme$year, tmp.ihme$mean*1000,
              lwd = 2, col = 'black')
        legend('topright', 
               lwd = c(rep(1, length(surveys)), rep(2,length(model.results)+1)),
               pch = c(rep(19, length(surveys)), rep(NA,length(model.results)+1)),
               col = c(survey.cols,
                       model.cols[1:length(model.names)], "black"),
               legend = c(surveys, model.names, "IHME"),
               bty = 'n', cex = 0.75)
        
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(0, plot.max),
             xlab = "Year", ylab = "U5MR",
             main = current.admin.names.toPlot[match(area, current.admin.names.Internal)])
        
        for (i in 1:length(model.results)) {
          if(grepl("frameAgg", model.labels[i])){
            tmp <- model.results[[i]]$res$final[model.results[[i]]$res$final$region==
                                                  as.character(area),]
          }else{
            tmp <- model.results[[i]]$res$overall[model.results[[i]]$res$overall$region==
                                                    as.character(area),]
          }
          polygon(c(beg.year:end.year, rev(beg.year:end.year)),
                  c(tmp$upper[tmp$region==as.character(area)], 
                    rev(tmp$lower[tmp$region==as.character(area)]))*1000,
                  col = alpha(model.cols[i], 0.25), border = FALSE)
        }
        polygon(c(tmp.ihme$year, rev(tmp.ihme$year)),
                c(tmp.ihme$upper,rev(tmp.ihme$lower))*1000,
                col = alpha('black', 0.25), border = FALSE)
        legend('topright', fill = alpha(c(model.cols[1:length(model.names)],"black"),0.25),
               bty = 'n', cex = 0.75,
               border = c(model.cols[1:length(model.names)], "black"),
               legend = c(model.names, "IHME"))
      }
      
    }
    
    dev.off()
    
    
    pdf(paste0(folder.name, '/Plots/Betabinomial/', country, '_',
               admin, bench.id,
               '_SpaceCompare.pdf'))
    
    {
      
      med.palette <- rev(brewer.pal(n = 7, name = "RdBu"))
      med.int <- c()
      for (i in 1:length(model.results)) {
        med.int <- c(med.int,round(model.results[[i]]$space$median[model.results[[i]]$space$label == "Total"], 3))
      }
      med.int <- classIntervals(med.int, n = 7, style = 'jenks')
      med.col <- findColours(med.int, med.palette)
      
      par(mfrow = c(ceiling(length(model.results)/2),2))
      par(mar = c(0,0,1,0))
      for (i in 1:length(model.results)) {
        if (admin == "admin1") {
          plot(poly.adm1, col = med.col[(nrow(admin1.mat)*(i-1)+1):(nrow(admin1.mat)*i)],
               xlim = poly.adm1@bbox['x',],
               ylim = poly.adm1@bbox['y',],
               axes = F, main = model.names[i])
        } else {
          plot(poly.adm2, col = med.col[(nrow(admin2.mat)*(i-1)+1):(nrow(admin2.mat)*i)],
               xlim = poly.adm2@bbox['x',],
               ylim = poly.adm2@bbox['y',],
               axes = F, main = model.names[i])
        }
        legend('topright', fill = med.palette,
               legend = names(attr(med.col, 'table')),
               bty = 'n', cex = 0.75)
      }
      
    }
    
    dev.off()
    
  }
}

