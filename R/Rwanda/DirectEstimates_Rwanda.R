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

country <- "Rwanda"
beg.year <- 1990
end.year <- 2019
usingGoogleSheets <- TRUE

## NB: Relative file paths defined
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

#### Get Survey years #### 
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
beg.years <- seq(beg.year, end.year - 4,5)
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


## Direct Estimates ####

mod.dat$v005 <- mod.dat$v005/1e6

births.list <- list()
svy.idx <- 0
for(survey in surveys){
  svy.idx <- svy.idx + 1
  births.list[[svy.idx]] <- mod.dat[mod.dat$survey == survey,] %>%
    as.data.frame()
  births.list[[svy.idx]]$died <- births.list[[svy.idx]]$Y
  births.list[[svy.idx]]$total <- as.numeric(births.list[[svy.idx]]$total)
}
names(births.list) <- surveys

### National ####

if(length(births.list) != 1){
  direct.natl <-  getDirectList(births.list, periods,
                                regionVar = "admin1.char",
                                timeVar = "period", 
                                clusterVar =  "~cluster",
                                ageVar = "age", Ntrials = "total",
                                weightsVar = "v005",national.only = T)
  direct.natl.yearly <- getDirectList(births.list, beg.year:end.year,
                                      regionVar = "admin1.char",
                                      timeVar = "years", 
                                      clusterVar =  "~cluster",
                                      ageVar = "age", Ntrials = "total",
                                      weightsVar = "v005",national.only = T)
}else{
  direct.natl <-  getDirect(as.data.frame(births.list[[1]]), periods,
                            regionVar = "admin1.char",
                            timeVar = "period", 
                            clusterVar =  "~cluster",
                            ageVar = "age", Ntrials = "total",
                            weightsVar = "v005",national.only = T)
  direct.natl$survey <- 1
  direct.natl$surveyYears <- surveys[1]
  
  direct.natl.yearly <-  getDirect(as.data.frame(births.list[[1]]), beg.year:end.year,
                                   regionVar = "admin1.char",
                                   timeVar = "years", 
                                   clusterVar =  "~cluster",
                                   ageVar = "age", Ntrials = "total",
                                   weightsVar = "v005",national.only = T)
  direct.natl.yearly$survey <- 1
  direct.natl.yearly$surveyYears <- surveys[1]
}

save(direct.natl, 
     file = paste0(folder.name,'/', 
                   country, '_direct_natl.rda'))
save(direct.natl.yearly, 
     file = paste0(folder.name,'/',
                   country, '_direct_natl_yearly.rda'))

### Admin1 ####
if(length(births.list) != 1){
  direct.admin1 <-  getDirectList(births.list, periods,
                                  regionVar = "admin1.char",
                                  timeVar = "period", 
                                  clusterVar =  "~cluster",
                                  ageVar = "age", Ntrials = "total",
                                  weightsVar = "v005",national.only = F)
}else{
  direct.admin1 <-  getDirect(as.data.frame(births.list[[1]]), periods,
                              regionVar = "admin1.char",
                              timeVar = "period", 
                              clusterVar =  "~cluster",
                              ageVar = "age", Ntrials = "total",
                              weightsVar = "v005",national.only = F)
  direct.admin1$survey <- 1
  direct.admin1$surveyYears <- surveys[1]
}

save(direct.admin1,
     file = paste0(folder.name, '/',
                   country, '_direct_admin1.rda'))


### Admin2 ####
if(doAdmin2){
  if(length(births.list) != 1){
    direct.admin2 <-  getDirectList(births.list, periods,
                                    regionVar = "admin2.char",
                                    timeVar = "period", 
                                    clusterVar =  "~cluster",
                                    ageVar = "age", Ntrials = "total",
                                    weightsVar = "v005",national.only = F)
  }else{
    direct.admin2 <-  getDirect(as.data.frame(births.list[[1]]), periods,
                                regionVar = "admin2.char",
                                timeVar = "period", 
                                clusterVar =  "~cluster",
                                ageVar = "age", Ntrials = "total",
                                weightsVar = "v005",national.only = F)
    direct.admin2$survey <- 1
    direct.admin2$surveyYears <- surveys[1]
  }
  
  
  save(direct.admin2, 
       file = paste0(folder.name, '/',
                     country, '_direct_admin2.rda'))
}

### HIV adjustment ####

if(doHIVAdj){
  load(paste0(hiv.dir.rel,
              'HIVAdjustments.rda'),
       envir = .GlobalEnv)
  hiv.adj <- hiv.adj[hiv.adj$country == country,]
  if(unique(hiv.adj$area)[1] == country){
    natl.unaids <- T
  }else{
    natl.unaids <- F
  }
  
  ## National Adjustments
  for(survey in surveys){
    if(natl.unaids){
      adj.frame <- hiv.adj[hiv.adj$survey == survey,]
      adj.varnames <- c("country", "years")
    }
    else{
      adj.frame <- hiv.adj[hiv.adj$survey == survey,]
      
      adj.frame <- aggregate(ratio ~ country + years,
                             data = adj.frame, FUN = mean)
      adj.varnames <- c("country", "years")
    }
    
    tmp.adj <- getAdjusted(direct.natl.yearly[direct.natl.yearly$surveyYears == survey,],
                           ratio = adj.frame, 
                           logit.lower = NULL,
                           logit.upper = NULL,
                           prob.upper = "upper",
                           prob.lower = "lower")
    direct.natl.yearly[direct.natl.yearly$surveyYears == survey,] <- 
      tmp.adj[ ,  match(colnames(direct.natl.yearly),
                        colnames(tmp.adj))]
  }
  
  ## Admin 1 and 2 Adjustments ##
  for(survey in surveys){
    if(natl.unaids){
      adj.frame <- hiv.adj[hiv.adj$survey == survey,]
      adj.varnames <- c("country", "years")
    }else{
      adj.frame <- hiv.adj[hiv.adj$survey == survey,]
      adj.varnames <- c("country", "region", "years")
    }
    
    for(area in admin1.names$GADM){
      if(natl.unaids){
        adj.frame.tmp <- adj.frame
        adj.frame.tmp <- adj.frame[adj.frame$years %in% 
                                     seq(beg.year+2, end.year-2, 5), ]
        adj.frame.tmp$years <- periods
        
      }else{
        if(country == "Zambia" & area == "North-Western"){
          adj.frame$area[adj.frame$area == "Northwestern"] <- area
        }
        adj.frame.tmp <- adj.frame[adj.frame$area == area &
                                     adj.frame$years %in%
                                     seq(beg.year+2, end.year-2, 5), ]
        adj.frame.tmp$years <- periods
      }
      
      area.int <- admin1.names$Internal[match(area, admin1.names$GADM)]
      tmp.adj <- getAdjusted(direct.admin1[direct.admin1$region == as.character(area.int) &
                                             direct.admin1$surveyYears == survey,],
                             ratio = adj.frame.tmp, 
                             logit.lower = NULL,
                             logit.upper = NULL,
                             prob.upper = "upper",
                             prob.lower = "lower")
      direct.admin1[direct.admin1$region == as.character(area.int) &
                      direct.admin1$surveyYears == survey,] <- 
        tmp.adj[ , match(colnames(direct.admin1),
                         colnames(tmp.adj))]
      
      if(exists('direct.admin2')){
        admin2.to.admin1 <- data.frame(GADM.adm2 = poly.adm2@data$NAME_2,
                                       Internal.adm2 = admin2.names$Internal,
                                       GADM.adm1 = poly.adm2@data$NAME_1,
                                       Internal.adm1 = NA)
        admin2.to.admin1$Internal.adm1 <- admin1.names$Internal[match(admin2.to.admin1$GADM.adm1,
                                                                      admin1.names$GADM)]
        admin2s <- admin2.to.admin1$Internal.adm2[admin2.to.admin1$Internal.adm1 == 
                                                    as.character(area.int)]
        tmp.adj <- getAdjusted(direct.admin2[direct.admin2$region %in% admin2s &
                                               direct.admin2$surveyYears == survey,],
                               ratio = adj.frame.tmp, 
                               logit.lower = NULL,
                               logit.upper = NULL,
                               prob.upper = "upper",
                               prob.lower = "lower")
        direct.admin2[direct.admin2$region %in% admin2s &
                        direct.admin2$surveyYears == survey,] <- 
          tmp.adj[ , match(colnames(direct.admin2), 
                           colnames(tmp.adj))]
      }
    }
  }

  save(direct.natl, file = paste0(folder.name, '/',
                                  country, '_directHIV_natl.rda'))
  save(direct.natl.yearly, file = paste0(folder.name, '/',
                                         country, '_directHIV_natl_yearly.rda'))
  save(direct.admin1, file = paste0(folder.name, '/',
                                    country, '_directHIV_admin1.rda'))
  if(exists('direct.admin2')){
    save(direct.admin2, file = paste0(folder.name, '/',
                                      country, '_directHIV_admin2.rda'))
  }
  
}

## Polygon Plots ####

if(!dir.exists(paths = paste0(folder.name,
                              '/Plots/', 'Direct'))){
  dir.create(path = paste0(folder.name,
                           '/Plots/', 'Direct'))
}

### National ####
med.palette <- brewer.pal(5, name = "Purples")
med.int <- classIntervals(round(direct.natl$logit.est, 2),
                          n = 5, style = 'jenks')
med.col <- findColours(med.int, med.palette)

for(survey in surveys){
  png(paste0(folder.name,
             "/Plots/Direct/",
             country, '_natl_', 
             survey,'_direct_poly.png'))
  
  par(mfrow = c(2,4))
  for(year in periods){
    idx <- which(direct.natl$surveyYears == survey &
                   direct.natl$years == year) 
    plot(poly.adm0, border = F, col = med.col[idx],
         axes = F, main = year)
  }
  plot(NA, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
  legend(x = "center",inset = 0,
         legend = names(attr(med.col, 'table')),
         fill = med.palette, cex= 1, horiz = FALSE, bty = 'n')
  
  
  
  dev.off()
}

### Admin1 ####

med.palette <- brewer.pal(n = 5, name = "Purples")
med.int <- classIntervals(round(direct.admin1$logit.est, 2),
                          n = 5, style = 'jenks')

med.col <- findColours(med.int, med.palette)

for(survey in surveys){
  png(paste0(folder.name,
             "/Plots/Direct/", 
             country, '_admin1_', 
             survey,'_direct_poly.png'))
  
  par(mfrow = c(2,4))
  for(year in periods){
    idx <- which(direct.admin1$surveyYears == survey &
                   direct.admin1$years == year) 
    plot(poly.adm1, border = F, col = med.col[idx],
         axes = F, main = year)
  }
  plot(NA, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
  legend(x = "center",inset = 0,
         legend = names(attr(med.col, 'table')),
         fill = med.palette, cex= .75, horiz = FALSE, bty = 'n')
  
  dev.off()
}

### Admin2 ####
if(doAdmin2){
  med.palette <- brewer.pal(n = 7, name = "Purples")
  med.int <- classIntervals(round(direct.admin2$logit.est, 2),
                            n = 7, style = 'jenks')
  med.col <- findColours(med.int, med.palette)
  
  for(survey in surveys){
    png(paste0(folder.name,
               "/Plots/Direct/",
               country, '_admin2_', 
               survey,'_direct_poly.png'))
    
    par(mfrow = c(2,4))
    for(year in periods){
      idx <- which(direct.admin2$surveyYears == survey &
                     direct.admin2$years == year) 
      plot(poly.adm2, border = F, col = med.col[idx],
           axes = F, main = year)
    }
    plot(NA, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
    legend(x = "center",inset = 0,
           legend = names(attr(med.col, 'table')),
           fill = med.palette, cex= .75, horiz = FALSE, bty = 'n')
    
    dev.off()
  }
}  

## Spaghetti Plots ####

cols <- rainbow(n.survey)
plot.years <- seq(beg.year + 2,
                  end.year, 5)

### National ####
pdf(paste0(folder.name,
           "/Plots/Direct/", 
           country, '_natl_direct_spaghetti.pdf'),
    height = 4, width = 4)
{
  plot.max <- max(direct.natl$upper+.025, na.rm = T)
  plot.min <- min(direct.natl$lower - 0.025, na.rm = T)
  for(survey in surveys){
    svy.idx <- match(survey, surveys) 
    pane.years <- jitter(plot.years)
    tmp <- direct.natl[direct.natl$surveyYears == survey,]
    tmp$width <- tmp$upper - tmp$lower
    tmp$cex2 <- median(tmp$width, na.rm = T)/tmp$width
    tmp$cex2[tmp$cex2 > 6] <- 6
    
    if(svy.idx== 1){
      par(mfrow = c(1,1))
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(plot.min, plot.max),
           xlim = c(beg.year, end.year),
           type = 'n', col = cols[svy.idx], lwd = 2,
           main = country)
      
      igme.years <- jitter(beg.year:max(igme.ests$year))
      lines(igme.years, igme.ests$OBS_VALUE/1000, lwd = 2, col  = 'grey37')
      lines(igme.years, igme.ests$UPPER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
      lines(igme.years, igme.ests$LOWER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
      ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
      lines(ihme.years, ihme.ests[[1]]$mean, lwd = 2, col  = 'darkgrey')
      lines(ihme.years, ihme.ests[[1]]$lower, lwd = 1, lty = 2, col  = 'darkgrey')
      lines(ihme.years, ihme.ests[[1]]$upper, lwd = 1, lty = 2, col  = 'darkgrey')
      
      lines(pane.years, tmp$mean, col = cols[svy.idx],
            lwd = 2)
      points(pane.years, tmp$mean,
             col = alpha(cols[svy.idx], 0.35),
             cex = tmp$cex2, pch = 19)
      
      # for(year.id in 1:length(periods)){
      #   segments(pane.years[year.id], tmp$upper[year.id],
      #            pane.years[year.id], tmp$lower[year.id],
      #            col = cols[svy.idx])
      # }
    }else{
      lines(pane.years, tmp$mean,
            type = 'l', col = cols[svy.idx],
            main = surveys[svy.idx], lwd = 2)
      points(pane.years, tmp$mean,
             col = alpha(cols[svy.idx], 0.35),
             cex = tmp$cex2, pch = 19)
      # for(year.id in 1:length(periods)){
      #   segments(pane.years[year.id], tmp$upper[year.id],
      #            pane.years[year.id], tmp$lower[year.id],
      #            col = cols[svy.idx])
      # }
    }
  }
  legend('topright', bty = 'n',
         col = c(cols, 'grey37', 'darkgrey'),
         lwd = 2, lty = 1,
         legend = c(surveys, 
                    "UN IGME",
                    "IHME"))
}
dev.off()
  
pdf(paste0(folder.name,
           "/Plots/Direct/",
           country,
           '_natl_direct_yearly_spaghetti.pdf'),
    width = 4, height = 4)
{
  plot.max <- max(direct.natl.yearly$upper+.025, na.rm = T)
  plot.min <- min(direct.natl.yearly$lower - 0.025, na.rm = T)
  for(survey in surveys){
    svy.idx <- match(survey, surveys) 
    pane.years <- jitter(beg.year:end.year)
    tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
    tmp$width <- tmp$upper - tmp$lower
    tmp$cex2 <- median(tmp$width, na.rm = T)/tmp$width
    tmp$cex2[tmp$cex2 > 6] <- 6
    
    if(svy.idx== 1){
      par(mfrow = c(1,1))
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(plot.min, plot.max),
           xlim = c(beg.year, end.year),
           type = 'n', col = cols[svy.idx], lwd = 2,
           main = country)
      
      igme.years <- jitter(beg.year:max(igme.ests$year))
      lines(igme.years, igme.ests$OBS_VALUE/1000, lwd = 2, col  = 'grey37')
      lines(igme.years, igme.ests$UPPER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
      lines(igme.years, igme.ests$LOWER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
      ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
      lines(ihme.years, ihme.ests[[1]]$mean, lwd = 2, col  = 'darkgrey')
      lines(ihme.years, ihme.ests[[1]]$lower, lwd = 1, lty = 2, col  = 'darkgrey')
      lines(ihme.years, ihme.ests[[1]]$upper, lwd = 1, lty = 2, col  = 'darkgrey')
      
      lines(pane.years, tmp$mean, col = cols[svy.idx],
            lwd = 2)
      
      points(pane.years, tmp$mean,
             col = alpha(cols[svy.idx], 0.35),
             cex = tmp$cex2, pch = 19)
      
      # for(year.id in 1:length(periods)){
      #   segments(pane.years[year.id], tmp$upper[year.id],
      #            pane.years[year.id], tmp$lower[year.id],
      #            col = cols[svy.idx])
      # }
    }else{
      lines(pane.years, tmp$mean,
            type = 'l', col = cols[svy.idx],
            main = surveys[svy.idx], lwd = 2)
      points(pane.years, tmp$mean,
             col = alpha(cols[svy.idx], 0.35),
             cex = tmp$cex2, pch = 19)
      # for(year.id in 1:length(periods)){
      #   segments(pane.years[year.id], tmp$upper[year.id],
      #            pane.years[year.id], tmp$lower[year.id],
      #            col = cols[svy.idx])
      # }
    }
  }
  
  legend('topright', bty = 'n',
         col = c(cols, 'grey37', 'darkgrey'),
         lwd = 2, lty = 1, 
         legend = c(surveys,
                    "UN IGME",
                    "IHME"))
}
dev.off()


### Admin1 ####

par(mfrow = c(1,1))
pdf(paste0(folder.name,
           "/Plots/Direct/",
           country,
           '_admin1_direct_spaghetti.pdf'),
    height = 6, width = 6)

for(area in 1:dim(poly.adm1)[1]){
  tmp.area <- direct.admin1[direct.admin1$region == 
                              as.character(admin1.names$Internal[area]),]
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  
  tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME ==
                               as.character(admin1.names$GADM[area]),]
  
  if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(tmp.area$upper+.025, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
    plot(NA,
         xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year),
         type = 'l', col = cols[svy.idx], lwd = 2,
         main = admin1.names$GADM[area])
  } else {
    for(survey in surveys){
      tmp <- tmp.area[tmp.area$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      pane.years <- jitter(plot.years)
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = admin1.names$GADM[area])
          
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                main = surveys[svy.idx], lwd = 2)
          
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          
          ihme.years <- jitter(tmp.ihme$year)
          lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = 'darkgrey')
          lines(ihme.years, tmp.ihme$lci, lwd = 1, lty = 2, col  = 'darkgrey')
          lines(ihme.years, tmp.ihme$uci, lwd = 1, lty = 2, col  = 'darkgrey')
          
          # for(year.id in 1:length(periods)){
          #   segments(pane.years[year.id], tmp$upper[year.id],
          #            pane.years[year.id], tmp$lower[year.id],
          #            col = cols[svy.idx])
          # }
        }else{
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = admin1.names$GADM[area])
        }
      }else{
        if(dim(tmp)[1] != 0){
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                main = surveys[svy.idx], lwd = 2)
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          # for(year.id in 1:length(periods)){
          #   segments(pane.years[year.id], tmp$upper[year.id],
          #            pane.years[year.id], tmp$lower[year.id],
          #            col = cols[svy.idx])
          # }
        } 
      }
      
      
    }
  }
  legend('topright', bty = 'n',
         col = c(cols, 'darkgrey'),
         lwd = 2, lty = 1, 
         legend = c(surveys, "IHME"))
}
dev.off()

### Admin2 ####
if(doAdmin2){
  par(mfrow = c(1,1))
  pdf(paste0(folder.name,
             "/Plots/Direct/",
             country,
             '_admin2_direct_spaghetti.pdf'),
      height = 6, width = 6)
  
  for(area in 1:dim(poly.adm2)[1]){
    tmp.area <- direct.admin2[direct.admin2$region ==
                                as.character(admin2.names$Internal[area]),]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    if(doAdmin2){
      tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == 
                                   as.character(admin2.names$GADM[area]),]
      
    }
    
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(tmp.area$upper+.025, na.rm = T)
    }else{
      plot.max <- 0.25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year),
           type = 'l', col = cols[svy.idx], lwd = 2,
           main = admin2.names$GADM[area])
    } else {
      for(survey in surveys){
        tmp <- tmp.area[tmp.area$surveyYears == survey,]
        svy.idx <- match(survey, surveys) 
        pane.years <- jitter(plot.years)
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = admin2.names$GADM[area])
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            if(doAdmin2){
              ihme.years <- jitter(tmp.ihme$year)
              lines(ihme.years, tmp.ihme$mean,
                    lwd = 2, col  = 'darkgrey')
              lines(ihme.years, tmp.ihme$lci,
                    lwd = 1, lty = 2, col  = 'darkgrey')
              lines(ihme.years, tmp.ihme$uci,
                    lwd = 1, lty = 2, col  = 'darkgrey')
            }
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = admin2.names$GADM[area])
          }
          
          
          # for(year.id in 1:length(periods)){
          #   segments(pane.years[year.id], tmp$upper[year.id],
          #            pane.years[year.id], tmp$lower[year.id],
          #            col = cols[svy.idx])
          # }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          }
          # for(year.id in 1:length(periods)){
          #   segments(pane.years[year.id], tmp$upper[year.id],
          #            pane.years[year.id], tmp$lower[year.id],
          #            col = cols[svy.idx])
          # }
        }
        
        if(!doAdmin2){
          legend('topright', bty = 'n', col = cols,
                 lwd = 2, lty = 1, legend = surveys)
        }else{
          legend('topright', bty = 'n', col = c(cols, 'darkgrey'),
                 lwd = 2, lty = 1, legend = c(surveys, "IHME"))
        }
      }
    }
  }
  dev.off()
}

