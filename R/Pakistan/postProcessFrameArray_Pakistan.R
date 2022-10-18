rm(list = ls())
logit <- function(x){log(x/(1-x))}
expit <- function(x){exp(x)/(1 + exp(x))}

#### Libraries ####
# devtools::install_github("bryandmartin/SUMMER",
#                          build_vignettes = F, force = T)
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
country <- "Pakistan"
cluster <- FALSE
message("If have the same subfolder structure as 
        AfricaAdmin2Estimates/Data/countryDataFolders/. Do nothing!\n
        Otherwise, edit the following paths as needed.\n")
# setwd('/homes/jlg0003/countryDataFolders')
# data.dir <- './toCluster'
# code.dir.rel <- '../../Analysis/R'
# igme.dir.rel <- '..'
# ihme.dir.rel <- '..'
# shapes.sub.dir <- '/shapeFiles_gadm'
# hiv.dir.rel <- '..'

data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '../../Analysis/R'
ihme.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '../HIV/'

setwd(data.dir)

if(!exists("sheet_key", envir = .GlobalEnv)){
  source(paste0(code.dir.rel,'/LoadCommandCenter.R'))
}
CountryList <- sheets_read(sheet_key, sheet = "CountryList")
# CountryList <- read.csv("CountryList.csv", header = T)

folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]

message('Where is IHMEHand_CountryName.rda?\n')
hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
                       gsub(" ", "", folder.name))
pop.dir.rel <- folder.name
#hand.dir.rel <- paste0(gsub(" ", "", folder.name))


#### More Params ####

beg.year <- 1990
end.year <- 2020
# time.mod <- "rw2main_randomSlopes_rw1xICAR"
time.mod <- "rw2main_randomSlopes_ar1xICAR"

#### Load polygon data ####
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

load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat.rda'))
load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat_Names.rda'))

#### Use HIV Adjusted data? ####
#HIV.sheet <- gs_read(sheet_key, ws = "HIV")
HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
#HIV.sheet <- read.csv("HIV.csv", header = T)
HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                unique(HIV.country$`UNAIDS data?`) == "Y")
#useHIVAdj <- (unique(HIV.country$MM.Adj.by.IGME) == "Y" &
#                unique(HIV.country$UNAIDS.data.) == "Y")
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

#### Load model data ####

load(paste0(folder.name,'/',country,'_cluster_dat.rda'),
     envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
dat.years <- sort(unique(mod.dat$years))
beg.years <- seq(beg.year, end.year,5)
end.years <- beg.years + 4
periods <- paste(beg.years, end.years, sep = "-")
mod.dat$period <- as.character(cut(mod.dat$years, breaks = c(beg.years, beg.years[length(beg.years)]+5),
                                   include.lowest = T, right = F, labels = periods))
mod.dat$urban <- tolower(mod.dat$urban)
mod.dat$strata.orig <- mod.dat$strata
mod.dat$strata <- mod.dat$urban
mod.dat$country <- as.character(country)

mod.dat$frame <- NA
for(i in 1:nrow(frames)){
  year <- frames$Survey.Year[i]
  frame <- as.character(frames$Frame[i])
  mod.dat$frame[mod.dat$survey == year] <- frame
}

#### Load IGME data ####

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

# #### Load UNPD data ####
# unpd.prop.urban <- read.csv(paste0(igme.dir.rel,
#                                    '/propUrban_UNPD.csv'),
#                             skip = 4)
# names(unpd.prop.urban) <- gsub("X","", names(unpd.prop.urban))
# unpd.prop.urban$`2020` <-unpd.prop.urban$`2019` <- unpd.prop.urban$`2018`
# unpd.prop.urban[,-c(1:4)] <- unpd.prop.urban[,-c(1:4)]/100 
# unpd.prop.urban <- unpd.prop.urban[unpd.prop.urban$Country.Name == country,]
# unpd.prop.urban <- unpd.prop.urban[ , c("Country.Name",
#                                         "Country.Code",
#                                         "Indicator.Name",
#                                         "Indicator.Code",
#                                         as.character(1990:2020))]

#### props urban ####
prop.urban <- read.csv(paste0(pop.dir.rel,
                              '/RW2_', country, '.csv'))
prop.urban.natl <- read.csv(paste0(pop.dir.rel,
                                   '/RW2_',
                                   country,'_national.csv'))


#### National Agg ####
load(paste0(folder.name,'/', country, 
            '_frame_rw2_natl.rda'))
load(paste0(folder.name,'/', country,
            '_res_frame_rw2_natl.rda'))

if(useHIVAdj){
  load(paste0(folder.name, '/',
              country, '_directHIV_natl_yearly.rda'), envir = .GlobalEnv)
}else{
  load(paste0(folder.name, '/',
              country, '_direct_natl_yearly.rda'), envir = .GlobalEnv)
}

load(paste0(folder.name, '/',
            country, '_res_natl_yearly_SmoothedDirect.rda'))
res.natl.yearly[,c("median", "lower", "upper")] <-
  res.natl.yearly[,c("median", "lower", "upper")]*1000

# props <- read.csv('NatlPropUrban.csv')
# props <- props[props$country == country,]
# strata.weights <- expand.grid(frame = levels(res.frame.eff.natl$overall$frame))
# strata.weights$urban <- props$propUrban
strata.weights <- expand.grid(frame = frames.unique,
                              years = beg.year:end.year)
for(frm in frames.unique){
  prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
  prop <- prop[order(prop$year),]
  max.yr <- max(prop$year)
  strata.weights$urban[strata.weights$frame == frm] <-
    #t(unpd.prop.urban[,as.character(beg.year:end.year)])
    c(prop$urban_prop,
      rep(prop$urban_prop[nrow(prop)],
          end.year - max.yr))
  
}
strata.weights$rural <- 1 - strata.weights$urban

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natl_strataWeights.pdf'),
    height = 4, width = 4)
par(lend=1)
plot(beg.year:end.year,
     strata.weights$urban[1:(end.year-beg.year+1)],
     xlab = "Year", ylab = "% Urban", type = 'l',
     lwd = 2, col = 'blue')
dev.off()
weight.frame <- expand.grid(years = beg.year:end.year)
weight.frame[,frames.unique] <- NA

year.ids <- unlist(lapply(res.frame.eff.natl$draws.est, function(x){x$year}))
strata.ids <- unlist(lapply(res.frame.eff.natl$draws.est, function(x){x$strata}))
agg <- list()
for(yr in beg.year:end.year){
  agg[[match(yr, beg.year:end.year)]] <- matrix(NA, nrow = length(frames.unique), 
                                                ncol = 1000)
  row.names(agg[[match(yr, beg.year:end.year)]]) <- frames.unique
  
}
names(agg) <- beg.year:end.year
for(i in 1:dim(weight.frame)[1]){
  yr <- weight.frame$years[i]
  
  var <- matrix(NA, nrow = length(frames.unique),
                ncol = length(frames.unique))
  
  frame.idx <- 0
  for(frame in frames.unique){
    frame.idx <- match(frame, frames.unique)
    urb.id <- which(year.ids == yr &
                      grepl(frame, strata.ids) &
                      grepl("urban", strata.ids))
    urb <- res.frame.eff.natl$draws.est[[urb.id]]$draws
    rur.id <- which(year.ids == yr &
                      grepl(frame, strata.ids) &
                      grepl("rural", strata.ids))
    rur <- res.frame.eff.natl$draws.est[[rur.id]]$draws
    
    if(sum(grepl("years", names(strata.weights))) != 0){
      q.urb <- strata.weights[strata.weights$frame == frame &
                                strata.weights$years == yr, "urban"]
      agg[[as.character(yr)]][frame,] <- urb*q.urb + rur*(1-q.urb)
    }else{
      q.urb <- strata.weights[strata.weights$frame == frame, "urban"]
      agg[[as.character(yr)]][frame,] <- urb*q.urb + rur*(1-q.urb)
    }
    
    name.1 <- row.names(agg[[as.character(yr)]])[1]
    var[1,1] <- var(logit(agg[[as.character(yr)]][name.1,]))
    for(j in 2:dim(var)[1]){
      name.j <- row.names(agg[[as.character(yr)]])[j]
      var[j,j] <- var(logit(agg[[as.character(yr)]][name.j,]))
      for(k in 1:(j-1)){
        name.k <- row.names(agg[[as.character(yr)]])[k]
        var[k,j]  <- var[j,k] <- cov(logit(agg[[as.character(yr)]][name.k,]),
                                     logit(agg[[as.character(yr)]][name.j,]))
      }
    }
    
    one <- rep(1, dim(var)[1])
    weight.frame[i, frames.unique] <- solve(t(one)%*%solve(var)%*%one)%*%(t(one)%*%solve(var))
  }
  
}

save(weight.frame, 
     file = paste0(folder.name, '/',
                   country, '_rw2_natl_frameWeights.rda'))

#frames <- colnames(weight.frame)[-1]
weight.month.frame <- aggregate(total~ frame + years,
                                data = mod.dat, FUN = sum)
weight.month.frame <- reshape(weight.month.frame,
                              idvar = "years",
                              timevar = "frame",
                              direction = "wide")
names(weight.month.frame) <- gsub("total.", "",
                                  names(weight.month.frame))
for(i in 1:nrow(weight.month.frame)){
  tmp <- weight.month.frame[i,frames.unique]
  tmp[is.na(tmp)] <- 0
  weight.month.frame[i,frames.unique] <- tmp/sum(tmp)
}


frame.cols <- c("deepskyblue", "deeppink", 
                "navy", "orange")[1:length(frames.unique)]



pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natl_frameWeights.pdf'),
    height = 4, width = 4)
{
  par(lend=1)
  plot(NA, xlim = c(beg.year, end.year),
       ylim = c(0,1), xlab = "Year",
       ylab = "Proportion")
  
  for(frame in frames.unique){
    frame.idx <- match(frame,frames.unique)
    if(frame.idx == 1){
      lower <- rep(0, times = end.year - beg.year + 1)
    }else{
      lower <- lower + weight.frame[,frames.unique[frame.idx - 1]]
    }
    
    if(frame.idx != length(frames.unique)){
      upper <- lower + weight.frame[,frames.unique[frame.idx]]
    }else{
      upper <- rep(1, end.year - beg.year + 1)
    }
    polygon(x = c(beg.year:end.year,
                  rev(beg.year:end.year)),
            y = c(upper,
                  rev(lower)),
            col = alpha(frame.cols[frame.idx], 0.25),
            border = F)
    widths <- upper-lower
    point <- which.max(widths)
    if(point < 7){
      point <- 7
    }
    
    if(point > 23){
      point <- 23
    }
    
    if(widths[point] < 0.5){
      cex <- 0.75
    }else{
      cex <- 1
    }
    text(x = (beg.year:end.year)[point],
         y = lower[point] +
           (upper[point]-lower[point])/2, cex = cex,
         label = frame, col = 'black')
  }
}
dev.off()


pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natl_agemonthWeights.pdf'),
    height = 4, width = 4)
{
  par(lend=1)
  max.year <- max(weight.month.frame$years)
  plot(NA, xlim = c(beg.year, max.year),
       ylim = c(0,1), xlab = "Year",
       ylab = "Proportion")
  
  for(frame in frames.unique){
    frame.idx <- match(frame,frames.unique)
    if(frame.idx == 1){
      lower <- rep(0, times = max.year - beg.year + 1)
    }else{
      lower <- lower + weight.month.frame[,frames.unique[frame.idx - 1]]
    }
    
    if(frame.idx != length(frames.unique)){
      upper <- lower + weight.month.frame[,frames.unique[frame.idx]]
    }else{
      upper <- rep(1, max.year - beg.year + 1)
    }
    polygon(x = c(beg.year:max.year,
                  rev(beg.year:max.year)),
            y = c(upper,
                  rev(lower)),
            col = alpha(frame.cols[frame.idx], 0.25),
            border = F)
    widths <- upper-lower
    point <- which.max(widths)
    if(point < 7){
      point <- 7
    }
    
    if(point > 23){
      point <- 23
    }
    
    if(widths[point] < 0.5){
      cex <- 0.75
    }else{
      cex <- 1
    }
    text(x = (beg.year:end.year)[point],
         y = lower[point] +
           (upper[point]-lower[point])/2, cex = cex,
         label = frame, col = 'black')
  }
}
dev.off()

res.frame.agg.natl <- res.frame.eff.natl
res.frame.agg.natl$final <- expand.grid(years = beg.year:end.year)
res.frame.agg.natl$final$years.num <- res.frame.agg.natl$final$years
res.frame.agg.natl$final[,c("median", "lower", "upper")] <- NA

for(i in 1:nrow(res.frame.agg.natl$final)){
  yr <- res.frame.agg.natl$final$years.num[i]
  weights <- weight.frame[weight.frame$years == yr,frames.unique]
  sum.w <- 0
  
  for(frame in frames.unique){
    sum.w <- sum.w + mean(logit(agg[[as.character(yr)]][frame,]))*weights[,frame]
  }
  
  var <- matrix(NA, nrow = length(frames.unique),
                ncol = length(frames.unique))
  name.1 <- row.names(agg[[as.character(yr)]])[1]
  var[1,1] <- var(logit(agg[[as.character(yr)]][name.1,]))
  for(j in 2:dim(var)[1]){
    name.j <- row.names(agg[[as.character(yr)]])[j]
    var[j,j] <- var(logit(agg[[as.character(yr)]][name.j,]))
    for(k in 1:(j-1)){
      name.k <- row.names(agg[[as.character(yr)]])[k]
      var[k,j]  <- var[j,k] <- cov(logit(agg[[as.character(yr)]][name.k,]),
                                   logit(agg[[as.character(yr)]][name.j,]))
    }
  }
  one <- matrix(1, nrow = length(frames.unique), ncol = 1)
  se <- sqrt(solve(t(one)%*%solve(var)%*%one))
  res.frame.agg.natl$final[i,c("median", "lower", "upper")] <- expit(c(sum.w, sum.w - 1.96*se, sum.w + 1.96*se))
}

save(res.frame.agg.natl,
     file = paste0(folder.name, '/', country,
                   '_res_frameAgg_rw2_natl.rda'))
direct.natl.yearly[,c("mean", "upper", "lower")] <-
  direct.natl.yearly[,c("mean", "upper", "lower")]*1000

#### Spaghetti Plot ####
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natl_frame_spaghetti.pdf'),
    height = 9, width = 3)
{
  tmp.area <- res.frame.agg.natl$final
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  tmp.ihme <- ihme.ests[[1]]
  tmp.ihme[,c("mean", "lower","upper")] <- 
    tmp.ihme[,c("mean", "lower","upper")]*1000
  cols <- rainbow(length(surveys)+1+1+1)
  cols[4] <- cols[3]
  cols[3] <- "orange"
  plot.years <- beg.year:end.year
  tmp.area$median <- tmp.area$median*1000
  tmp.area$upper <- tmp.area$upper*1000
  tmp.area$lower <- tmp.area$lower*1000
  
  
  par(mfrow = c(3,1), lend = 1)
  if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(direct.natl.yearly$mean+25, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
    plot(NA,
         xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year + 1),
         type = 'l', col = cols[1], lwd = 2,
         main = country)
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = 1, legend = c(surveys, "Smoothed"))
    
  } else {
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          pane.years <- jitter(tmp$years)
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                main = surveys[svy.idx], lwd = 2)
          
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          
          ihme.years <- jitter(tmp.ihme$year)
          lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = cols[length(surveys)+1])
          #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
          #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
          
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
               main =  paste0(admin2.names$GADM[area]))
        }
      }else{
        if(dim(tmp)[1] != 0){
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                lwd = 2)
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
    ihme.years <- jitter(tmp.ihme$year)
    lines(ihme.years, tmp.ihme$mean,
          lty = 1, lwd = 2, col = cols[length(surveys) + 1])
    igme.years <- jitter(igme.ests$year)
    lines(igme.years, igme.ests$OBS_VALUE,
          lty = 1, lwd = 2, col = cols[length(surveys)+2])
    # polygon(x = c(igme.years, rev(igme.years)),
    #         y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
    #         col = alpha(cols[length(surveys)+2], 0.15), border = F)
    # # lines(igme.years, igme.ests$UPPER_BOUND/1000,
    #       lty = 3, lwd = 1, col = 'gray35')
    # lines(igme.years, igme.ests$LOWER_BOUND/1000,
    #       lty = 3, lwd = 1, col = 'gray35')
    res.tmp <- tmp.area
    res.tmp$years <- as.numeric(as.character(res.tmp$years))
    
    pane.years <- jitter(beg.year:end.year)
    lines(pane.years[1:length(beg.year:max(mod.dat$years))],
          res.natl.yearly$median[1:length(beg.year:max(mod.dat$years))], 
          col = cols[length(surveys)+3],
          lwd = 2, lty = 1)
    lines(pane.years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.natl.yearly$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = cols[length(surveys)+3], 
          lwd = 2, lty = 2)
    lines(res.tmp$years[1:length(beg.year:max(mod.dat$years))], 
          res.tmp$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
          lwd = 2, lty = 1)
    lines(res.tmp$years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.tmp$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = 'black', 
          lwd = 2, lty = 2)
    
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = c(rep(1, length(cols)+1)),
           legend = c(surveys, 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'),
           cex = 0.6)
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx == 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          pane.years <- jitter(tmp$years[!is.na(tmp$mean)])
          polygon(x = c(pane.years,
                        rev(pane.years)),
                  y = c(tmp$upper[!is.na(tmp$upper)],
                        rev(tmp$lower[!is.na(tmp$lower)])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
          
          
        }else{
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main =  paste0(country))
          
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                lwd = 2)
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
        }
      }else{
        pane.years <- jitter(tmp$years[!is.na(tmp$mean)])
        polygon(x = c(pane.years,
                      rev(pane.years)),
                y = c(tmp$upper[!is.na(tmp$upper)],
                      rev(tmp$lower[!is.na(tmp$lower)])),
                col = alpha(cols[svy.idx], 0.25),
                border = FALSE)
      }
      
    }  
    
    legend('topright', bty = 'n', fill = alpha(cols[1:length(surveys)], 0.25),
           border = cols[1:length(surveys)],
           legend = c(surveys),
           cex = 0.6)
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx == 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          
        }
      }  
    }
    ihme.years <- jitter(tmp.ihme$year)
    polygon(x = c(ihme.years, rev(ihme.years)),
            y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
            col = alpha(cols[length(surveys)+1], 0.25),
            border = FALSE)
    
    
    
    igme.years <- jitter(igme.ests$year)
    polygon(x = c(igme.years, rev(igme.years)),
            y = c(igme.ests$UPPER_BOUND, rev(igme.ests$LOWER_BOUND)),
            col = alpha(cols[length(surveys)+2], 0.25),
            border = FALSE)
    res.tmp <- tmp.area
    res.tmp$years <- as.numeric(as.character(res.tmp$years))
    polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
            y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                  rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
            col = alpha(cols[length(surveys)+3], 0.25),
            border = FALSE)
    polygon(x = c(res.tmp$years, rev(res.tmp$years)),
            y = c(res.tmp$upper,
                  rev(res.tmp$lower)),
            col = alpha('black', 0.25),
            border = FALSE)
    lines(res.tmp$years[1:length(beg.year:max(mod.dat$years))], 
          res.tmp$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
          lwd = 2, lty = 1)
    lines(res.tmp$years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.tmp$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = 'black', 
          lwd = 2, lty = 2)
    legend('topright', bty = 'n', 
           fill = alpha(c(cols[-c(1:length(surveys))],
                          'black'), .25),
           border = c(cols[-c(1:length(surveys))],
                      'black'), cex = 0.65,
           legend = c('IHME', 'IGME',
                      'Smoothed Direct', 'Betabinomial'))
  }
  
  
}
dev.off()

hyperpar.table <- fit.frame.eff.natl$fit$summary.hyperpar
save(hyperpar.table,
     file = paste0(folder.name, '/', 
                   country, '_frame_rw2',
                   '_natl_hyperpar.rda'))


fixed.eff <- fit.frame.eff.natl$fit$summary.fixed
save(fixed.eff,
     file = paste0(folder.name, '/', 
                   country, '_frame_rw2',
                   '_natl_fixed.rda'))

temporals <- getDiag(fit.frame.eff.natl, field = "time",
                     year_label = beg.year:end.year)
ages <- c("0", "1-11", "12-23",
          "24-35", "36-47", "48-59")

save(temporals,
     file = paste0(folder.name, '/', 
                   country, '_rw2',
                   '_natl_frame_temporal.rda'))



pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natl_frame_temporal.pdf'), height = 5, width = 7.5)
{
  par(mfrow =c(2,3), lend = 1)
  for(age in ages){
    plot.idx <- 0
    age.idx <- grepl(paste0(age,":"),temporals$group)
    temp.age <- temporals[age.idx,]
    frame.cols <- c("deepskyblue", "deeppink", "navy",
                    "orange")[1:length(frames.unique)]
    frame.idx <- 0
    for(frame in frames.unique){
      
      frame.idx <- frame.idx + 1
      plot.idx <- plot.idx + 1
      
      urb.idx <- grepl(frame, temp.age$group) &
        grepl("urban", temp.age$group)
      rur.idx <- grepl(frame, temp.age$group) &
        grepl("rural", temp.age$group)
      
      fixed.urb.idx <-  which(grepl(frame, row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl("urban", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
      fixed.rur.idx <-  which(grepl(frame, row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl("rural", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
      fixed.urb.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.urb.idx]
      fixed.rur.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.rur.idx]
      
      plot.min <- min(c(temp.age$median + fixed.urb.eff,
                        temp.age$median + fixed.rur.eff))  - 0.025
      plot.max <- max(c(temp.age$median + fixed.urb.eff,
                        temp.age$median + fixed.rur.eff)) + 0.025
      
      if(plot.idx == 1){
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(plot.min, plot.max),
             xlab = "Year",
             ylab = "Effect size",
             main = age)
        # abline(h=0)
        # 
        # lines(beg.year:end.year, temporals$median[temporals$label == "IID"],
        #       col = "blue")
      }
      
      end.id <- which(beg.year:end.year == 
                        max(frames$Survey.Year[frames$Frame == frame])) + 7
      end.id <- min(end.id, (end.year - beg.year + 1))
      lines((beg.year:end.year)[1:end.id], (temp.age$median[urb.idx] + 
                                              fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.urb.idx])[1:end.id],
            col = frame.cols[frame.idx], lty = 1, lwd = 2)
      lines((beg.year:end.year)[1:end.id], (temp.age$median[rur.idx] + 
                                              fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.rur.idx])[1:end.id],
            col = frame.cols[frame.idx], lty = 2, lwd = 2)
    }
    
    legend('topright', lty = c(1,2,1,2), cex = 0.5,
           col = c(
             rep(frame.cols, each = 2)), lwd = 2,
           legend = c(
             paste0(rep(frames.unique, each = 2),
                    c("-urban", "-rural"))), bty = 'n')
  }
}
dev.off()

coef.table <- lapply(res.frame.agg.natl$draws, function(x){
  x$latent[grepl("age",row.names(x$latent))]
})
coef.table <- do.call(rbind, coef.table)
colnames(coef.table) <- row.names(res.frame.agg.natl$draws[[1]]$latent)[grepl("age",
                                                                              row.names(res.frame.agg.natl$draws[[1]]$latent))]
save(coef.table, file = paste0(folder.name, '/',
                               country, '_', time.mod, '_UrbRurEffects.rda'))

#### Odds ####


pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_frame_natl_odds.pdf'), height = 4, width = 6)
{
  par(mfrow =c(2,3), lend = 1)
  for(age in ages){
    plot.idx <- 0
    age.idx <- grepl(paste0(age,":"),temporals$group)
    temp.age <- temporals[age.idx,]
    frame.cols <- c("deepskyblue", "deeppink", "navy",
                    "orange")[1:length(frames.unique)]
    
    urb.idx <- grepl("urban", temp.age$group)
    rur.idx <- grepl("rural", temp.age$group)
    
    fixed.urb.idx <-  which(grepl("urban", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                              grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
    fixed.rur.idx <-  which(grepl("rural", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                              grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
    fixed.urb.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.urb.idx]
    fixed.rur.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.rur.idx]
    
    plot.min <- min(exp(outer(temp.age$median[rur.idx], fixed.rur.eff, FUN="+") -
                          outer(temp.age$median[urb.idx], fixed.urb.eff, FUN="+"))) - 0.025
    plot.max <- max(exp(outer(temp.age$median[rur.idx], fixed.rur.eff, FUN="+") -
                          outer(temp.age$median[urb.idx], fixed.urb.eff, FUN="+"))) + 0.025
    
    
    frame.idx <- 0
    for(frame in sort(frames.unique)){
      
      frame.idx <- match(frame, frames.unique)
      plot.idx <- plot.idx + 1
      
      urb.idx <- grepl(frame, temp.age$group) &
        grepl("urban", temp.age$group)
      rur.idx <- grepl(frame, temp.age$group) &
        grepl("rural", temp.age$group)
      
      fixed.urb.idx <-  which(grepl(frame, row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl("urban", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
      fixed.rur.idx <-  which(grepl(frame, row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl("rural", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
      fixed.urb.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.urb.idx]
      fixed.rur.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.rur.idx]
      
      if(plot.idx == 1){
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(plot.min, plot.max),
             xlab = "Year",
             ylab = "Odds",
             main = age)
        abline(h=1, col = 'black')

        # 
        # lines(beg.year:end.year, temporals$median[temporals$label == "IID"],
        #       col = "blue")
      }
      
      end.id <- which(beg.year:end.year == 
                        max(frames$Survey.Year[frames$Frame == frame])) + 7
      end.id <- min(end.id, (end.year - beg.year + 1))
      lines((beg.year:end.year)[1:end.id], 
            exp(temp.age$median[rur.idx] + 
                  fixed.rur.eff -
                  (temp.age$median[urb.idx] + 
                     fixed.urb.eff))[1:end.id],
            col = frame.cols[frame.idx], lty = 1, lwd = 2)
      
    }
    
    legend('topright', lty = 1, cex = 0.6, lwd = 2,
           col = c(rep(frame.cols, each = 1)),
           legend = frames.unique, bty = 'n')
  }
}
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_frame_natl_oddsU5MR.pdf'),
    height = 3, width = 4)
{
  par(mfrow =c(1,1), lend = 1)
  frame.cols <- c("deepskyblue", "deeppink", "navy",
                  "orange")[1:length(frames.unique)]
  plot(NA, xlim = c(beg.year, end.year),
       ylim = log(c(0.25, 10)),
       yaxt = 'n', 
       xlab = "Year",
       ylab = "(Rural Odds)/(Urban Odds)",
       main = country)
  abline(h=0, col = 'black')
  axis(2, at = log(c(0,0.5,1,2,5,10)+0.001),
       labels = c(0,0.5,1,2,5,10))
  frame.idx <- 0
  for(frame in sort(frames.unique)){
    
    frame.idx <- match(frame, sort(frames.unique))
    ests.frames <- lapply(res.frame.agg.natl$draws.est,
                          function(x){
                            data.frame(frame = x$strata,
                                       years = x$years)
                          })
    ests.frames <- do.call(rbind.data.frame, ests.frames)
    urb.idx <- which(grepl(frame,ests.frames$frame) &
                       grepl("urban", tolower(ests.frames$frame)))
    rur.idx <- which(grepl(frame,ests.frames$frame) &
                       grepl("rural", tolower(ests.frames$frame)))
    
    odds.frame <- matrix(NA, nrow = end.year - beg.year + 1,
                         ncol = 1000)
    odds.idx <- 0
    for(year in beg.year:end.year){
      odds.idx <- odds.idx + 1
      yrs.urb.idx <- which(ests.frames$years == year)
      yrs.urb.idx <- yrs.urb.idx[yrs.urb.idx %in% urb.idx]
      yrs.rur.idx <- which(ests.frames$years == year)
      yrs.rur.idx <- yrs.rur.idx[yrs.rur.idx %in% rur.idx]
      odds.frame[odds.idx,] <- (logit(res.frame.agg.natl$draws.est[[yrs.rur.idx]]$draws) -
        logit(res.frame.agg.natl$draws.est[[yrs.urb.idx]]$draws))
    }
    
    odds.summary <- as.data.frame(t(apply(odds.frame,1,quantile, c(0.025,0.5,0.975))))
    colnames(odds.summary) <- c("lower", "median","upper")

      # 
      # lines(beg.year:end.year, temporals$median[temporals$label == "IID"],
      #       col = "blue")
    
    
    end.id <- which(beg.year:end.year == 
                      max(frames$Survey.Year[frames$Frame == frame])) + 7
    end.id <- min(end.id, (end.year - beg.year + 1))
    
    lines((beg.year:end.year)[1:end.id], 
          odds.summary$median[1:end.id],
          col = frame.cols[frame.idx], lty = 1, lwd = 2)
    polygon(x = c((beg.year:end.year)[1:end.id],
                  rev((beg.year:end.year)[1:end.id])),
            y = c(odds.summary$upper[1:end.id],
                  rev(odds.summary$lower[1:end.id])),
            border = F, col = alpha(frame.cols[frame.idx], 0.25))
    
  }
  
  legend('topright', lty = 1, cex = 0.6, lwd = 2,
         col = c(rep(frame.cols, each = 1)),
         legend = sort(frames.unique), bty = 'n')
}

dev.off()


#### National Benchmark Agg ####
load(paste0(folder.name,'/', country, '_frame_rw2_natlBench.rda'))
load(paste0(folder.name,'/', country, '_res_frame_rw2_natlBench.rda'))
load(paste0(folder.name, '/', country, '_frame_rw2_natlBenchmarks.rda'))

load(paste0(folder.name, '/',
            country, '_res_natlBench_yearly_SmoothedDirect.rda'))
res.natl.yearly[,c("median", "lower", "upper")] <-
  res.natl.yearly[,c("median", "lower", "upper")]*1000


pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_frame_rw2_natlBenchmarks.pdf'),
    height = 4, width = 4)
{
  frame.cols <- c("deepskyblue", "deeppink", "navy",
                  "orange")[1:length(frames.unique)]
  frame.idx <- 0
  
  for(frame in frames.unique){
    frame.idx <- frame.idx + 1
    if(frame.idx == 1){
      par(lend = 1)
      plot(NA, xlim = c(beg.year, max(mod.dat$years)),
           ylim = c(min(bench.adj$ratio)-0.025,
                    max(bench.adj$ratio) + 0.025),
           xlab = "Year", ylab = "Offset",
           main = country)
      abline(h = 1)
    }
    
    tmp <- bench.adj$ratio[bench.adj$frame == frame]
    stop.id <- min(which(tmp == 1))-1
    lines(c(beg.year:end.year)[1:stop.id],
          tmp[1:stop.id],
          lwd = 2, col = frame.cols[frame.idx])
  }
  legend('topleft', lwd = 2, col = frame.cols,
         legend = frames.unique,bty ='n', cex = 0.75)
}
dev.off()

# props <- read.csv('NatlPropUrban.csv')
# props <- props[props$country == country,]
# strata.weights <- expand.grid(frame = levels(res.frame.eff.natl$overall$frame))
# strata.weights$urban <- props$propUrban
# strata.weights$rural <- 1 - strata.weights$urban

strata.weights <- expand.grid(frame =frames.unique,
                              years = beg.year:end.year)
for(frm in frames.unique){
  prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
  prop <- prop[order(prop$year),]
  max.yr <- max(prop$year)
  strata.weights$urban[strata.weights$frame == frm] <-
    #t(unpd.prop.urban[,as.character(beg.year:end.year)])
    c(prop$urban_prop,
      rep(prop$urban_prop[nrow(prop)],
          end.year - max.yr))
  
}
strata.weights$rural <- 1 - strata.weights$urban
weight.frame <- expand.grid(years = beg.year:end.year)
weight.frame[,frames.unique] <- NA

year.ids <- unlist(lapply(res.frame.eff.natl$draws.est, function(x){x$year}))
strata.ids <- unlist(lapply(res.frame.eff.natl$draws.est, function(x){x$strata}))
agg <- list()
for(yr in beg.year:end.year){
  agg[[match(yr, beg.year:end.year)]] <- matrix(NA, nrow =  dim(weight.frame[,-1])[2], 
                                                ncol = 1000)
  row.names(agg[[match(yr, beg.year:end.year)]]) <- frames.unique
  
}
names(agg) <- beg.year:end.year
for(i in 1:dim(weight.frame)[1]){
  yr <- weight.frame$years[i]
  
  var <- matrix(NA, nrow = length(frames.unique),
                ncol = length(frames.unique))
  
  frame.idx <- 0
  for(frame in frames.unique){
    frame.idx <- frame.idx + 1
    urb.id <- which(year.ids == yr &
                      grepl(frame, strata.ids) &
                      grepl("urban", strata.ids))
    urb <- res.frame.eff.natl$draws.est[[urb.id]]$draws
    rur.id <- which(year.ids == yr &
                      grepl(frame, strata.ids) &
                      grepl("rural", strata.ids))
    rur <- res.frame.eff.natl$draws.est[[rur.id]]$draws
    
    if(sum(grepl("years", names(strata.weights))) != 0){
      q.urb <- strata.weights[strata.weights$frame == frame &
                                strata.weights$years == yr, "urban"]
      agg[[as.character(yr)]][frame,] <- urb*q.urb + rur*(1-q.urb)
      
    }else{
      q.urb <- strata.weights[strata.weights$frame == frame, "urban"]
      agg[[as.character(yr)]][frame,] <- urb*q.urb + rur*(1-q.urb)
    }
    
    name.1 <- row.names(agg[[as.character(yr)]])[1]
    var[1,1] <- var(logit(agg[[as.character(yr)]][name.1,]))
    for(j in 2:dim(var)[1]){
      name.j <- row.names(agg[[as.character(yr)]])[j]
      var[j,j] <- var(logit(agg[[as.character(yr)]][name.j,]))
      for(k in 1:(j-1)){
        name.k <- row.names(agg[[as.character(yr)]])[k]
        var[k,j]  <- var[j,k] <- cov(logit(agg[[as.character(yr)]][name.k,]),
                                     logit(agg[[as.character(yr)]][name.j,]))
      }
    }
    
    one <- rep(1, dim(var)[1])
    weight.frame[i,frames.unique] <- solve(t(one)%*%solve(var)%*%one)%*%(t(one)%*%solve(var))
  }
  
}

save(weight.frame, file = paste0(folder.name, '/',
                                 country, '_rw2_natlBench_frameWeights.rda'))

#frames <- colnames(weight.frame)[-1]
weight.month.frame <- aggregate(total~ frame + years,
                                data = mod.dat, FUN = sum)
weight.month.frame <- reshape(weight.month.frame,
                              idvar = "years",
                              timevar = "frame",
                              direction = "wide")
names(weight.month.frame) <- gsub("total.", "",
                                  names(weight.month.frame))
for(i in 1:nrow(weight.month.frame)){
  tmp <- weight.month.frame[i,frames.unique]
  tmp[is.na(tmp)] <- 0
  weight.month.frame[i,frames.unique] <- tmp/sum(tmp)
}


frame.cols <- c("deepskyblue", "deeppink", "navy",
                "orange")[1:length(frames.unique)]



pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natlBench_frameWeights.pdf'), height = 4, width = 4)
{
  par(lend=1)
  plot(NA, xlim = c(beg.year, end.year),
       ylim = c(0,1), xlab = "Year",
       ylab = "Proportion")
  
  for(frame in frames.unique){
    frame.idx <- match(frame,frames.unique)
    if(frame.idx == 1){
      lower <- rep(0, times = end.year - beg.year + 1)
    }else{
      lower <- lower + weight.frame[,frames.unique[frame.idx - 1]]
    }
    
    if(frame.idx != length(frames.unique)){
      upper <- lower + weight.frame[,frames.unique[frame.idx]]
    }else{
      upper <- rep(1, end.year - beg.year + 1)
    }
    polygon(x = c(beg.year:end.year,
                  rev(beg.year:end.year)),
            y = c(upper,
                  rev(lower)),
            col = alpha(frame.cols[frame.idx], 0.25),
            border = F)
    widths <- upper-lower
    point <- which.max(widths)
    if(point < 7){
      point <- 7
    }
    
    if(point > 23){
      point <- 23
    }
    
    if(widths[point] < 0.5){
      cex <- 0.75
    }else{
      cex <- 1
    }
    text(x = (beg.year:end.year)[point],
         y = lower[point] +
           (upper[point]-lower[point])/2, cex = cex,
         label = frame, col = 'black')
  }
}
dev.off()


pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natlBench_agemonthWeights.pdf'), height = 4, width = 4)
{
  max.year <- max(weight.month.frame$years)
  par(lend=1)
  plot(NA, xlim = c(beg.year, max.year),
       ylim = c(0,1), xlab = "Year",
       ylab = "Proportion")
  
  for(frame in frames.unique){
    frame.idx <- match(frame,frames.unique)
    if(frame.idx == 1){
      lower <- rep(0, times = max.year - beg.year + 1)
    }else{
      lower <- lower + weight.month.frame[,frames.unique[frame.idx - 1]]
    }
    
    if(frame.idx != length(frames.unique)){
      upper <- lower + weight.month.frame[,frames.unique[frame.idx]]
    }else{
      upper <- rep(1, max.year - beg.year + 1)
    }
    polygon(x = c(beg.year:max.year,
                  rev(beg.year:max.year)),
            y = c(upper,
                  rev(lower)),
            col = alpha(frame.cols[frame.idx], 0.25),
            border = F)
    widths <- upper-lower
    point <- which.max(widths)
    if(point < 7){
      point <- 7
    }
    
    if(point > 23){
      point <- 23
    }
    
    if(widths[point] < 0.5){
      cex <- 0.75
    }else{
      cex <- 1
    }
    text(x = (beg.year:end.year)[point],
         y = lower[point] +
           (upper[point]-lower[point])/2, cex = cex,
         label = frame, col = 'black')
  }
}
dev.off()


res.frame.agg.natl <- res.frame.eff.natl
res.frame.agg.natl$final <- expand.grid(years = beg.year:end.year)
res.frame.agg.natl$final$years.num <- res.frame.agg.natl$final$years
res.frame.agg.natl$final[,c("median", "lower", "upper")] <- NA


for(i in 1:nrow(res.frame.agg.natl$final)){
  yr <- res.frame.agg.natl$final$years.num[i]
  weights <- weight.frame[weight.frame$years == yr,frames.unique]
  sum.w <- 0
  
  for(frame in frames.unique){
    sum.w <- sum.w + mean(logit(agg[[as.character(yr)]][frame,]))*weights[,frame]
  }
  
  var <- matrix(NA, nrow = length(frames.unique),
                ncol = length(frames.unique))
  name.1 <- row.names(agg[[as.character(yr)]])[1]
  var[1,1] <- var(logit(agg[[as.character(yr)]][name.1,]))
  for(j in 2:dim(var)[1]){
    name.j <- row.names(agg[[as.character(yr)]])[j]
    var[j,j] <- var(logit(agg[[as.character(yr)]][name.j,]))
    for(k in 1:(j-1)){
      name.k <- row.names(agg[[as.character(yr)]])[k]
      var[k,j]  <- var[j,k] <- cov(logit(agg[[as.character(yr)]][name.k,]),
                                   logit(agg[[as.character(yr)]][name.j,]))
    }
  }
  one <- matrix(1, nrow = length(frames.unique), ncol = 1)
  se <- sqrt(solve(t(one)%*%solve(var)%*%one))
  res.frame.agg.natl$final[i,c("median", "lower", "upper")] <- expit(c(sum.w, sum.w - 1.96*se, sum.w + 1.96*se))
}

save(res.frame.agg.natl, file = paste0(folder.name, '/', country,
                                       '_res_frameAgg_',
                                       'rw2_natlBench.rda'))
#### Spaghetti Plot ####
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natlBench_frame_spaghetti.pdf'),
    height = 9, width = 3)
{
  tmp.area <- res.frame.agg.natl$final
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  tmp.ihme <- ihme.ests[[1]]
  tmp.ihme[,c("mean", "lower","upper")] <- 
    tmp.ihme[,c("mean", "lower","upper")]*1000
  cols <- rainbow(length(surveys)+1+1+1)
  cols[4] <- cols[3]
  cols[3] <- "orange"
  plot.years <- beg.year:end.year
  tmp.area$median <- tmp.area$median*1000
  tmp.area$upper <- tmp.area$upper*1000
  tmp.area$lower <- tmp.area$lower*1000
  
  
  par(mfrow = c(3,1), lend=1)
  if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(direct.natl.yearly$mean+25, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
    plot(NA,
         xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year + 1),
         type = 'l', col = cols[1], lwd = 2,
         main = country)
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = 1, legend = c(surveys, "Smoothed"))
    
  } else {
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          pane.years <- jitter(tmp$years)
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                main = surveys[svy.idx], lwd = 2)
          
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          
          ihme.years <- jitter(tmp.ihme$year)
          lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = cols[length(surveys)+1])
          #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
          #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
          
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
               main =  paste0(admin2.names$GADM[area]))
        }
      }else{
        if(dim(tmp)[1] != 0){
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                lwd = 2)
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
    ihme.years <- jitter(tmp.ihme$year)
    lines(ihme.years, tmp.ihme$mean,
          lty = 1, lwd = 2, col = cols[length(surveys) + 1])
    igme.years <- jitter(igme.ests$year)
    lines(igme.years, igme.ests$OBS_VALUE,
          lty = 1, lwd = 2, col = cols[length(surveys)+2])
    # polygon(x = c(igme.years, rev(igme.years)),
    #         y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
    #         col = alpha(cols[length(surveys)+2], 0.15), border = F)
    # # lines(igme.years, igme.ests$UPPER_BOUND/1000,
    #       lty = 3, lwd = 1, col = 'gray35')
    # lines(igme.years, igme.ests$LOWER_BOUND/1000,
    #       lty = 3, lwd = 1, col = 'gray35')
    res.tmp <- tmp.area
    res.tmp$years <- as.numeric(as.character(res.tmp$years))
    
    pane.years <- jitter(beg.year:end.year)
    lines(pane.years[1:length(beg.year:max(mod.dat$years))],
          res.natl.yearly$median[1:length(beg.year:max(mod.dat$years))], 
          col = cols[length(surveys)+3],
          lwd = 2, lty = 1)
    lines(pane.years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.natl.yearly$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = cols[length(surveys)+3], 
          lwd = 2, lty = 2)
    lines(res.tmp$years[1:length(beg.year:max(mod.dat$years))], 
          res.tmp$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
          lwd = 2, lty = 1)
    lines(res.tmp$years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.tmp$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = 'black', 
          lwd = 2, lty = 2)
    
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = c(rep(1, length(cols)+1)),
           legend = c(surveys, 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'),
           cex = 0.6)
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx == 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          pane.years <- jitter(tmp$years[!is.na(tmp$mean)])
          polygon(x = c(pane.years,
                        rev(pane.years)),
                  y = c(tmp$upper[!is.na(tmp$upper)],
                        rev(tmp$lower[!is.na(tmp$lower)])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
          
          
        }else{
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main =  paste0(country))
          
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                lwd = 2)
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
        }
      }else{
        pane.years <- jitter(tmp$years[!is.na(tmp$mean)])
        polygon(x = c(pane.years,
                      rev(pane.years)),
                y = c(tmp$upper[!is.na(tmp$upper)],
                      rev(tmp$lower[!is.na(tmp$lower)])),
                col = alpha(cols[svy.idx], 0.25),
                border = FALSE)
      }
      
    }  
    
    legend('topright', bty = 'n', fill = alpha(cols[1:length(surveys)], 0.25),
           border = cols[1:length(surveys)],
           legend = c(surveys),
           cex = 0.6)
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx == 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          
        }
      }  
    }
    ihme.years <- jitter(tmp.ihme$year)
    polygon(x = c(ihme.years, rev(ihme.years)),
            y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
            col = alpha(cols[length(surveys)+1], 0.25),
            border = FALSE)
    
    
    
    igme.years <- jitter(igme.ests$year)
    polygon(x = c(igme.years, rev(igme.years)),
            y = c(igme.ests$UPPER_BOUND, rev(igme.ests$LOWER_BOUND)),
            col = alpha(cols[length(surveys)+2], 0.25),
            border = FALSE)
    res.tmp <- tmp.area
    res.tmp$years <- as.numeric(as.character(res.tmp$years))
    polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
            y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                  rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
            col = alpha(cols[length(surveys)+3], 0.25),
            border = FALSE)
    polygon(x = c(res.tmp$years, rev(res.tmp$years)),
            y = c(res.tmp$upper,
                  rev(res.tmp$lower)),
            col = alpha('black', 0.25),
            border = FALSE)
    lines(res.tmp$years[1:length(beg.year:max(mod.dat$years))], 
          res.tmp$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
          lwd = 2, lty = 1)
    lines(res.tmp$years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.tmp$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = 'black', 
          lwd = 2, lty = 2)
    legend('topright', bty = 'n', fill = alpha(c(cols[-c(1:length(surveys))],
                                                 'black'), .25),
           border = c(cols[-c(1:length(surveys))],
                      'black'), cex = 0.65,
           legend = c('IHME', 'IGME',
                      'Smoothed Direct', 'Betabinomial'))
  }
  
  
}
dev.off()

hyperpar.table <- fit.frame.eff.natl$fit$summary.hyperpar
save(hyperpar.table,
     file = paste0(folder.name, '/', 
                   country, '_frame_rw2',
                   '_natlBench_hyperpar.rda'))


fixed.eff <- fit.frame.eff.natl$fit$summary.fixed
save(fixed.eff,
     file = paste0(folder.name, '/', 
                   country, '_frame_rw2',
                   '_natlBench_fixed.rda'))


temporals <- getDiag(fit.frame.eff.natl, field = "time",
                     year_label = beg.year:end.year)
ages <- c("0", "1-11", "12-23",
          "24-35", "36-47", "48-59")

save(temporals,
     file = paste0(folder.name, '/', 
                   country, '_rw2',
                   '_natlBench_frame_temporal.rda'))

# frames <- levels(res.frame.agg.natl$overall$frame)

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natlBench_frame_temporal.pdf'),
    height = 5, width = 7.5)
{
  par(mfrow =c(2,3), lend=1)
  for(age in ages){
    plot.idx <- 0
    age.idx <- grepl(paste0(age,":"),temporals$group)
    temp.age <- temporals[age.idx,]
    frame.cols <- c("deepskyblue", "deeppink", "navy",
                    "orange")[1:length(frames.unique)]
    
    
    fixed.urb.idx <-  which(grepl("urban", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                              grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
    fixed.rur.idx <-  which(grepl("rural", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                              grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
    fixed.urb.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.urb.idx]
    fixed.rur.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.rur.idx]
    
    plot.min <- min(c(outer(temp.age$median, fixed.rur.eff, FUN="+"),
                      outer(temp.age$median, fixed.urb.eff, FUN="+"))) - 0.025
    plot.max <- max(c(outer(temp.age$median, fixed.rur.eff, FUN="+"),
                      outer(temp.age$median, fixed.urb.eff, FUN="+"))) + 0.025
    
    
    
    frame.idx <- 0
    for(frame in frames.unique){
      
      frame.idx <- frame.idx + 1
      plot.idx <- plot.idx + 1
      
      urb.idx <- grepl(frame, temp.age$group) &
        grepl("urban", temp.age$group)
      rur.idx <- grepl(frame, temp.age$group) &
        grepl("rural", temp.age$group)
      
      fixed.urb.idx <-  which(grepl(frame, row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl("urban", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
      fixed.rur.idx <-  which(grepl(frame, row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl("rural", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
      fixed.urb.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.urb.idx]
      fixed.rur.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.rur.idx]
      
      
      if(plot.idx == 1){
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(plot.min, plot.max),
             xlab = "Year",
             ylab = "Effect size",
             main = age)
        # abline(h=0)
        # 
        # lines(beg.year:end.year, temporals$median[temporals$label == "IID"],
        #       col = "blue")
      }
      
      end.id <- which(beg.year:end.year == 
                        max(frames$Survey.Year[frames$Frame == frame])) + 7
      end.id <- min(end.id, (end.year - beg.year + 1))
      lines((beg.year:end.year)[1:end.id], (temp.age$median[urb.idx] + 
                                              fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.urb.idx])[1:end.id],
            col = frame.cols[frame.idx], lty = 1, lwd = 2)
      lines((beg.year:end.year)[1:end.id], (temp.age$median[rur.idx] + 
                                              fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.rur.idx])[1:end.id],
            col = frame.cols[frame.idx], lty = 2, lwd = 2)
      
    }
    
    legend('topright', lty = c(1,2,1,2), cex = 0.5,
           col = c(
             rep(frame.cols, each = 2)), lwd = 2,
           legend = c(
             paste0(rep(frames.unique, each = 2),
                    c("-urban", "-rural"))), bty = 'n')
  }
}
dev.off()


#### Odds ####
coef.table <- lapply(res.frame.agg.natl$draws, function(x){
  x$latent[grepl("age",row.names(x$latent))]
})

coef.table <- do.call(rbind, coef.table)
colnames(coef.table) <- row.names(res.frame.agg.natl$draws[[1]]$latent)[grepl("age",
                                                                              row.names(res.frame.agg.natl$draws[[1]]$latent))]
save(coef.table, file = paste0(folder.name, '/',
                               country, '_rw2_bench_UrbRurEffects.rda'))



pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_frame_natlBench_odds.pdf'), height = 4, width = 6)
{
  par(mfrow =c(2,3), lend = 1)
  for(age in ages){
    plot.idx <- 0
    age.idx <- grepl(paste0(age,":"),temporals$group)
    temp.age <- temporals[age.idx,]
    frame.cols <- c("deepskyblue", "deeppink", "navy",
                    "orange")[1:length(frames.unique)]
    
    urb.idx <- grepl("urban", temp.age$group)
    rur.idx <- grepl("rural", temp.age$group)
    
    fixed.urb.idx <-  which(grepl("urban", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                              grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
    fixed.rur.idx <-  which(grepl("rural", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                              grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
    fixed.urb.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.urb.idx]
    fixed.rur.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.rur.idx]
    
    plot.min <- min(exp(outer(temp.age$median[rur.idx], fixed.rur.eff, FUN="+") -
                          outer(temp.age$median[urb.idx], fixed.urb.eff, FUN="+"))) - 0.025
    plot.max <- max(exp(outer(temp.age$median[rur.idx], fixed.rur.eff, FUN="+") -
                          outer(temp.age$median[urb.idx], fixed.urb.eff, FUN="+"))) + 0.025
    
    
    frame.idx <- 0
    for(frame in sort(frames.unique)){
      
      frame.idx <- match(frame, frames.unique)
      plot.idx <- plot.idx + 1
      
      urb.idx <- grepl(frame, temp.age$group) &
        grepl("urban", temp.age$group)
      rur.idx <- grepl(frame, temp.age$group) &
        grepl("rural", temp.age$group)
      
      fixed.urb.idx <-  which(grepl(frame, row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl("urban", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
      fixed.rur.idx <-  which(grepl(frame, row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl("rural", row.names(fit.frame.eff.natl$fit$summary.fixed)) &
                                grepl(paste0(age,":"), row.names(fit.frame.eff.natl$fit$summary.fixed)))
      fixed.urb.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.urb.idx]
      fixed.rur.eff <- fit.frame.eff.natl$fit$summary.fixed$`0.5quant`[fixed.rur.idx]
      
      if(plot.idx == 1){
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(plot.min, plot.max),
             xlab = "Year",
             ylab = "Odds",
             main = age)
        abline(h=1, col = 'black')
        # 
        # lines(beg.year:end.year, temporals$median[temporals$label == "IID"],
        #       col = "blue")
      }
      
      end.id <- which(beg.year:end.year == 
                        max(frames$Survey.Year[frames$Frame == frame])) + 7
      end.id <- min(end.id, (end.year - beg.year + 1))
      lines((beg.year:end.year)[1:end.id], 
            exp(temp.age$median[rur.idx] + 
                  fixed.rur.eff -
                  (temp.age$median[urb.idx] + 
                     fixed.urb.eff))[1:end.id],
            col = frame.cols[frame.idx], lty = 1, lwd = 2)
      
    }
    
    legend('topright', lty = 1, cex = 0.6, lwd = 2,
           col = c(rep(frame.cols, each = 1)),
           legend = frames.unique, bty = 'n')
  }
}
dev.off()


pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_frame_natlBench_oddsU5MR.pdf'), height = 4, width = 6)
{
  par(mfrow =c(1,1), lend = 1)
  frame.cols <- c("deepskyblue", "deeppink", "navy",
                  "orange")[1:length(frames.unique)]
  plot(NA, xlim = c(beg.year, end.year),
       ylim = log(c(0.25, 5)),
       yaxt = 'n', 
       xlab = "Year",
       ylab = "(Rural Odds)/(Urban Odds)",
       main = country)
  abline(h=0, col = 'black')
  axis(2, at = log(c(0,0.5,1,2,5)+0.001),
       labels = c(0,0.5,1,2,5))
  frame.idx <- 0
  for(frame in sort(frames.unique)){
    
    frame.idx <- match(frame, sort(frames.unique))
    ests.frames <- lapply(res.frame.agg.natl$draws.est,
                          function(x){
                            data.frame(frame = x$strata,
                                       years = x$years)
                          })
    ests.frames <- do.call(rbind.data.frame, ests.frames)
    urb.idx <- which(grepl(frame,ests.frames$frame) &
                       grepl("urban", tolower(ests.frames$frame)))
    rur.idx <- which(grepl(frame,ests.frames$frame) &
                       grepl("rural", tolower(ests.frames$frame)))
    
    odds.frame <- matrix(NA, nrow = end.year - beg.year + 1,
                         ncol = 1000)
    odds.idx <- 0
    for(year in beg.year:end.year){
      odds.idx <- odds.idx + 1
      yrs.urb.idx <- which(ests.frames$years == year)
      yrs.urb.idx <- yrs.urb.idx[yrs.urb.idx %in% urb.idx]
      yrs.rur.idx <- which(ests.frames$years == year)
      yrs.rur.idx <- yrs.rur.idx[yrs.rur.idx %in% rur.idx]
      odds.frame[odds.idx,] <- (logit(res.frame.agg.natl$draws.est[[yrs.rur.idx]]$draws) -
                                  logit(res.frame.agg.natl$draws.est[[yrs.urb.idx]]$draws))
    }
    
    odds.summary <- as.data.frame(t(apply(odds.frame,1,quantile, c(0.025,0.5,0.975))))
    colnames(odds.summary) <- c("lower", "median","upper")
    
    # 
    # lines(beg.year:end.year, temporals$median[temporals$label == "IID"],
    #       col = "blue")
    
    
    end.id <- which(beg.year:end.year == 
                      max(frames$Survey.Year[frames$Frame == frame])) + 7
    end.id <- min(end.id, (end.year - beg.year + 1))
    
    lines((beg.year:end.year)[1:end.id], 
          odds.summary$median[1:end.id],
          col = frame.cols[frame.idx], lty = 1, lwd = 2)
    polygon(x = c((beg.year:end.year)[1:end.id],
                  rev((beg.year:end.year)[1:end.id])),
            y = c(odds.summary$upper[1:end.id],
                  rev(odds.summary$lower[1:end.id])),
            border = F, col = alpha(frame.cols[frame.idx], 0.25))
    
  }
  
  legend('topright', lty = 1, cex = 0.6, lwd = 2,
         col = c(rep(frame.cols, each = 1)),
         legend = sort(frames.unique), bty = 'n')
}

dev.off()
#### Admin 1 Agg ####

#load(paste0(folder.name,'/', country, '_frame_', time.mod, '_admin1.rda'))
load(paste0(folder.name,'/', country, '_res_frame_', time.mod, '_admin1Bench.rda'))

load(paste0(folder.name,'/', country, '_', 
            time.mod, '_admin1Benchmarks.rda'))
load(paste0(folder.name, '/', country, 
            '_', time.mod,'_admin1_noStrata_temporals.rda'))
load(paste0(folder.name, '/',
            country, '_', time.mod,'_admin1_noStrata_spatials.rda'))
load(paste0(folder.name, '/',
            country, 
            '_', time.mod,'_admin1_noStrata_fixedeff.rda'))
load(paste0(folder.name, '/',
            country,'_',
            time.mod,'_admin1_noStrata_PosteriorInteractions.rda'))

if (grepl("randomSlopes",time.mod)) {
  load(paste0(folder.name, '/',
              country,'_',
              time.mod,'_admin1_noStrata_posteriorRandomSlopes.rda'))
}


if(useHIVAdj){
  load(paste0(folder.name, '/',
              country, '_directHIV_admin1.rda'), envir = .GlobalEnv)
}else{
  load(paste0(folder.name, '/',
              country, '_direct_admin1.rda'), envir = .GlobalEnv)
}

direct.admin1[,c("mean", "lower", "upper")] <- 
  direct.admin1[,c("mean", "lower", "upper")]*1000
load(paste0(folder.name, '/',
            country, '_res_admin1Bench_SmoothedDirect.rda'))


strata.weights <- expand.grid(frame = levels(res.frame.eff.admin1$overall$frame),
                              years = beg.year:end.year,
                              region = admin1.names$Internal)
strata.weights$urban <- NA
for(area in admin1.names$Internal){
  
  gadm <- admin1.names$GADM[match(area, admin1.names$Internal)]
  prop <- prop.urban[prop.urban$admin == as.character(gadm) &
                       prop.urban$year %in% beg.year:end.year,]
  max.yr <- max(prop$year)
  
  strata.weights$urban[strata.weights$region == as.character(area)] <- 
    c(prop$urban_prop,
      rep(prop$urban_prop[nrow(prop)],
          end.year - max.yr))
}
strata.weights$rural <- 1 - strata.weights$urban

weight.frame <- expand.grid(years = beg.year:end.year,
                            region = admin1.names$Internal)
weight.frame[,frames.unique] <- NA

year.ids <- unlist(lapply(res.frame.eff.admin1$draws.est, function(x){x$year}))
strata.ids <- unlist(lapply(res.frame.eff.admin1$draws.est, function(x){x$strata}))
area.ids <- unlist(lapply(res.frame.eff.admin1$draws.est, function(x){x$region}))

agg <- list()
for(yr in beg.year:end.year){
  for(area in admin1.names$Internal){
    agg.idx <- which(weight.frame$years == as.numeric(yr) &
                       weight.frame$region == as.character(area))
    agg[[agg.idx]] <- matrix(NA, nrow = length(frames.unique), 
                             ncol = 1000)
    row.names(agg[[agg.idx]]) <- frames.unique
    
  }
}


for(i in 1:dim(weight.frame)[1]){
  yr <- weight.frame$years[i]
  area <- weight.frame$region[i]
  area.adj <- area
  
  var <- matrix(NA, nrow = length(frames.unique),
                ncol = length(frames.unique))
  
  frame.idx <- 0
  for(frame in frames.unique){
    frame.idx <- match(frame, frames.unique)
    urb.id <- which(year.ids == yr &
                      grepl(frame, strata.ids) &
                      grepl("urban", strata.ids) &
                      area.ids == area)
    urb <- res.frame.eff.admin1$draws.est[[urb.id]]$draws
    rur.id <- which(year.ids == yr &
                      grepl(frame, strata.ids) &
                      grepl("rural", strata.ids) &
                      area.ids == area)
    rur <- res.frame.eff.admin1$draws.est[[rur.id]]$draws
    
    if(sum(grepl("year", names(strata.weights))) != 0){
      q.urb <- unique(strata.weights[strata.weights$frame == frame &
                                       strata.weights$years == yr &
                                       strata.weights$region == as.character(area.adj), "urban"])
      agg[[i]][frame,] <- urb*q.urb + rur*(1-q.urb)
    }else{
      q.urb <- strata.weights[strata.weights$frame == frame, "urban"]
      agg[[i]][frame,] <- urb*q.urb + rur*(1-q.urb)
    }
    
    name.1 <- row.names(agg[[i]])[1]
    var[1,1] <- var(logit(agg[[i]][name.1,]))
    for(j in 2:dim(var)[1]){
      name.j <- row.names(agg[[i]])[j]
      var[j,j] <- var(logit(agg[[i]][name.j,]))
      for(k in 1:(j-1)){
        name.k <- row.names(agg[[i]])[k]
        var[k,j]  <- var[j,k] <- cov(logit(agg[[i]][name.k,]),
                                     logit(agg[[i]][name.j,]))
      }
    }
    
    one <- rep(1, dim(var)[1])
    weight.frame[i, frames.unique] <- solve(t(one)%*%solve(var)%*%one)%*%(t(one)%*%solve(var))
  }
  
}


save(weight.frame, file = paste0(folder.name, '/',
                                 country, '_rw2_admin1Bench_frameWeights.rda'))

res.frame.agg.admin1 <- res.frame.eff.admin1
res.frame.agg.admin1$final <- expand.grid(years = beg.year:end.year,
                                          region = admin1.names$Internal)
res.frame.agg.admin1$final$years.num <- res.frame.agg.admin1$final$years
res.frame.agg.admin1$final[,c("median", "lower", "upper")] <- NA
res.frame.agg.admin1$draws.est <- NULL
for(i in 1:nrow(res.frame.agg.admin1$final)){
  yr <- res.frame.agg.admin1$final$years.num[i]
  area <- res.frame.agg.admin1$final$region[i]
  idx <- which(weight.frame$years == yr &
                 weight.frame$region == area)
  weights <- weight.frame[idx,frames.unique]
  sum.w <- 0
  
  for(frame in frames.unique){
    sum.w <- sum.w + mean(logit(agg[[idx]][frame,]))*weights[,frame]
  }
  
  var <- matrix(NA, nrow = length(frames.unique),
                ncol = length(frames.unique))
  name.1 <- row.names(agg[[idx]])[1]
  var[1,1] <- var(logit(agg[[idx]][name.1,]))
  for(j in 2:dim(var)[1]){
    name.j <- row.names(agg[[idx]])[j]
    var[j,j] <- var(logit(agg[[idx]][name.j,]))
    for(k in 1:(j-1)){
      name.k <- row.names(agg[[idx]])[k]
      var[k,j]  <- var[j,k] <- cov(logit(agg[[idx]][name.k,]),
                                   logit(agg[[idx]][name.j,]))
    }
  }
  one <- matrix(1, nrow = length(frames.unique), ncol = 1)
  se <- sqrt(solve(t(one)%*%solve(var)%*%one))
  tmp.list <- list(years = yr,
                   region = area,
                   draws = sum.w)
  res.frame.agg.admin1$final[i,c("median", "lower", "upper")] <- expit(c(sum.w, sum.w - 1.96*se, sum.w + 1.96*se))
  res.frame.agg.admin1$draws.est[[i]] <- tmp.list
  
}


save(res.frame.agg.admin1, file = paste0(folder.name, '/', country, '_res_frameAgg_',
                                         time.mod, '_admin1Bench.rda'))

#### Spaghetti Plot ####


pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin1Bench_frame_spaghetti.pdf'), height = 4, width = 8)
{
  area.idx <- 0
  for(area in admin1.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.frame.agg.admin1$final[res.frame.agg.admin1$final$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    # print(tmp.ihme)
    cols <- rainbow(length(surveys)+1+1)
    plot.years <- beg.year:end.year
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    res.tmp <- res.admin1[res.admin1$region == as.character(admin1.names$Internal[area.idx]),]
    res.tmp[,c("median", "lower", "upper")] <-
      res.tmp[,c("median", "lower", "upper")]*1000
    
    pane.years <- jitter(seq(beg.year+2, end.year-2, 5))
    est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
    
    
    par(mfrow = c(1,2), lend=1)
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper, direct.admin1$mean[direct.admin1$region == area]), na.rm = T) + 25
    }else{
      plot.max <- 0.25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           type = 'l', col = cols[1], lwd = 2,
           main = admin1.names$GADM[area.idx])
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = 1, legend = c(surveys, "Smoothed"))
      
    } else {
      
      for(survey in surveys){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = admin1.names$GADM[area.idx])
            pane.years <- jitter(seq(beg.year+2, end.year-2,5))
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            ihme.years <- jitter(tmp.ihme$year)
            lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            
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
                 main =  paste0(admin1.names$GADM[area.idx]))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
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
      
      pane.years <- jitter(seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      lines(pane.years[est.ids],
            res.tmp$median[est.ids], 
            col = cols[length(surveys)+2],
            lwd = 2, lty = 1)
      lines(pane.years[max(est.ids):length(pane.years)], 
            res.tmp$median[max(est.ids):length(pane.years)], 
            col = cols[length(surveys)+2], 
            lwd = 2, lty = 2)
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = c(rep(1, length(cols)+1)),
             legend = c(surveys, 'IHME', 'Smoothed Direct', 'Betabinomial'),
             cex = 0.6)
      
      
      for(survey in surveys){
        # debugging
        # survey <- surveys[1]
        # survey <- surveys[2]
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = admin1.names$GADM[area.idx])
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main =  paste0(admin1.names$GADM[area.idx]))
          }
        }
        
        
      }
      
      ihme.years <- tmp.ihme$year+2.5
      polygon(x = c(ihme.years, rev(ihme.years)),
              y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
              col = alpha(cols[length(surveys)+1], 0.25),
              border = FALSE)
      pane.years <- seq(beg.year+2.5,end.year-2.5,5)
      polygon(x = c(pane.years, rev(pane.years)),
              y = c(res.tmp$upper[1:length(pane.years)],
                    rev(res.tmp$lower[1:length(pane.years)])),
              col = alpha(cols[length(surveys)+2], 0.25),
              border = FALSE)
      
      polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      #      }
      legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                   'black'), .25),
             border = c(cols[(length(surveys)+1):length(cols)],
                        'black'),
             legend = c('IHME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
    }
    
    
  }
}
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod,
           '_admin1Bench_frame_spaghettiSingle.pdf'),
    height = 5, width = 5)
{
  area.idx <- 0
  for(area in admin1.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.frame.agg.admin1$final[res.frame.agg.admin1$final$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    # print(tmp.ihme)
    cols <- rainbow(length(surveys)+1+1)
    plot.years <- beg.year:end.year
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    res.tmp <- res.admin1[res.admin1$region == 
                            as.character(admin1.names$Internal[area.idx]),]
    res.tmp[,c("median", "lower", "upper")] <-
      res.tmp[,c("median", "lower", "upper")]*1000
    
    pane.years <- (seq(beg.year+2, end.year-2, 5))
    est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
    
    
    par(mfrow = c(1,1),lend=1)
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin1$upper[direct.admin1$region == as.character(area)]),
                      na.rm = T) + 25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           type = 'l', col = cols[1], lwd = 2,
           main = admin1.names$GADM[area.idx])
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = 1, legend = c(surveys, "Smoothed"))
      
    } else {
      
      for(survey in surveys){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = admin1.names$GADM[area.idx])
            pane.years <- (seq(beg.year+2, end.year-2,5))
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            
            #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            
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
                 main =  paste0(admin1.names$GADM[area.idx]))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      ihme.years <- (tmp.ihme$year)
      lines(ihme.years, tmp.ihme$mean,
            lwd = 2, col  = cols[length(surveys)+1])
      polygon(x = c(ihme.years,
                    rev(ihme.years)),
              y = c(tmp.ihme$upper,
                    rev(tmp.ihme$lower)),
              col = alpha(cols[length(surveys)+1], 0.25),
              border = FALSE)
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
      lines(pane.years[est.ids],
            res.tmp$median[est.ids], 
            col = cols[length(surveys)+2],
            lwd = 2, lty = 1)
      lines(pane.years[max(est.ids):length(pane.years)], 
            res.tmp$median[max(est.ids):length(pane.years)], 
            col = cols[length(surveys)+2], 
            lwd = 2, lty = 2)
      polygon(x = c(pane.years,
                    rev(pane.years)),
              y = c(res.tmp$upper[1:6],
                    rev(res.tmp$lower[1:6])),
              col = alpha(cols[length(surveys)+2], 0.25),
              border = FALSE)
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))],
            col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(beg.year:end.year,
                    rev(beg.year:end.year)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = c(rep(1, length(cols)+1)),
             legend = c(surveys, 'IHME', 'Smoothed Direct', 'Betabinomial'),
             cex = 0.6)
      
      
    }
    
    
  }
}
dev.off()


if (grepl("randomSlopes",time.mod)) {
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod, '_admin1Bench_frame_randomSlopes.pdf'), height = 5, width = 5)
  years.std <- ((beg.year:end.year)-mean(beg.year:end.year))/sd(beg.year:end.year)
  # plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.025quant","0.975quant")],
  #                       years.std[length(years.std)]*posteriorRandomSlopes[,c("0.025quant","0.975quant")]))
  plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                        years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
  par(lend=1)
  plot(NA,xlim=range(years.std),ylim = plot.range,
       ylab = "effect",xlab="year (standardized)")
  
  region.cols <- viridis_pal(option = "A")(nrow(posteriorRandomSlopes))
  for (i in 1:nrow(posteriorRandomSlopes)) {
    abline(0,posteriorRandomSlopes$`0.5quant`[i],col=region.cols[i])
    # polygon(x=c(years.std,rev(years.std)),
    #         y=c(years.std*posteriorRandomSlopes$`0.975quant`[i],rev(years.std*posteriorRandomSlopes$`0.025quant`[i])),
    #         col=alpha(region.cols[i],0.25),border = NA)
  }
  legend("topleft",legend=admin1.names[,1],col=region.cols,lty=1,ncol=2,cex=0.5)
  dev.off()
}

med.palette <- rev(brewer.pal(n = 7, name = "RdBu"))
med.int <- classIntervals(round(spaces$median[spaces$label == "Total"], 3),
                          n = 7, style = 'jenks')
med.col <- findColours(med.int, med.palette)

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin1Bench_frame_spatial.pdf'))
par(mar = c(0.25,0.25,0.25,0.25),lend=1)
plot(poly.adm1, col = med.col,
     xlim = poly.adm1@bbox['x',],
     ylim = poly.adm1@bbox['y',],
     axes = F)
legend('bottomleft', fill = med.palette,
       legend = names(attr(med.col, 'table')),
       bty = 'n', cex = 0.75)
dev.off()

#### Spaghetti plots for all regions over time ####
# change the region names from Internal to GADM for legend labels
res.frame.agg.admin1$final$region.orig <- res.frame.agg.admin1$final$region
levels(res.frame.agg.admin1$final$region) <- admin1.names$GADM[match(levels(res.frame.agg.admin1$final$region.orig),
                                                                     admin1.names$Internal)]

# make the plot
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin1Bench_frame_spaghetti_all.pdf'))
par(lend=1)
res.frame.agg.admin1$final$median1000 <-res.frame.agg.admin1$final$median*1000
ggplot(data = res.frame.agg.admin1$final) +
  geom_line(aes(x = years.num,
                y = median1000,
                color = region)) +
  geom_point(aes(x = years.num,
                 y = median1000,
                 color = region)) +
  ggtitle(country) + 
  theme(legend.position = "bottom") + 
  ylim(c(0, max(res.frame.agg.admin1$final$median)*1000)) +
  ylab("U5MR") + xlab("Year") + labs(color = "Admin 1")

dev.off()


#### Admin 2 Agg ####

#load(paste0(folder.name,'/', country, '_frame_', time.mod, '_admin2.rda'))
load(paste0(folder.name,'/', country, '_res_frame_', time.mod, '_admin2Bench.rda'))

load(paste0(folder.name,'/', country, '_', 
            time.mod, '_admin2Benchmarks.rda'))
load(paste0(folder.name, '/', country, 
            '_', time.mod,'_admin2Bench_noStrata_temporals.rda'))
load(paste0(folder.name, '/',
            country, '_', time.mod,'_admin2Bench_noStrata_spatials.rda'))
load(paste0(folder.name, '/',
            country, 
            '_', time.mod,'_admin2Bench_noStrata_fixedeff.rda'))
load(paste0(folder.name, '/',
            country,'_',
            time.mod,'_admin2Bench_noStrata_PosteriorInteractions.rda'))

if (grepl("randomSlopes",time.mod)) {
  load(paste0(folder.name, '/',
              country,'_',
              time.mod,'_admin2Bench_noStrata_posteriorRandomSlopes.rda'))
}


if(useHIVAdj){
  load(paste0(folder.name, '/',
              country, '_directHIV_admin2.rda'), envir = .GlobalEnv)
}else{
  load(paste0(folder.name, '/',
              country, '_direct_admin2.rda'), envir = .GlobalEnv)
}

direct.admin2[,c("mean", "lower", "upper")] <- 
  direct.admin2[,c("mean", "lower", "upper")]*1000


strata.weights <- expand.grid(years = beg.year:end.year,
                              region = admin2.names$Internal,
                              frame = frames.unique)
strata.weights$urban <- strata.weights$rural <- NA
adj.idx <-  match(admin2.names$GADM[match(strata.weights$region,
                                          admin2.names$Internal)],
                  poly.adm2@data$NAME_2)
strata.weights$region.adj <- poly.adm2@data$NAME_1[adj.idx]

for(area in unique(strata.weights$region.adj)){
  area <- as.character(area)
  prop <- prop.urban[prop.urban$admin == area,]
  prop <- prop[prop$year %in% beg.year:end.year,]
  prop.yrs <- sort(prop$year)
  max.yr <- max(prop.yrs)
  strata.weights$urban[strata.weights$region.adj == area] <- c(prop$urban_prop,
                                                               rep(prop$urban_prop[nrow(prop)],
                                                                   times = end.year-max.yr))
  
}
strata.weights$rural <- 1 - strata.weights$urban

weight.frame <- expand.grid(years = beg.year:end.year,
                            region = admin2.names$Internal)
weight.frame[,frames.unique] <- NA

year.ids <- unlist(lapply(res.frame.eff.admin2$draws.est, function(x){x$year}))
strata.ids <- unlist(lapply(res.frame.eff.admin2$draws.est, function(x){x$strata}))
area.ids <- unlist(lapply(res.frame.eff.admin2$draws.est, function(x){x$region}))
agg <- list()
for(yr in beg.year:end.year){
  for(area in admin2.names$Internal){
    agg.idx <- which(weight.frame$years == as.numeric(yr) &
                       weight.frame$region == as.character(area))
    agg[[agg.idx]] <- matrix(NA, nrow = length(frames.unique), 
                             ncol = 1000)
    row.names(agg[[agg.idx]]) <- frames.unique
    
  }
}


for(i in 1:dim(weight.frame)[1]){
  yr <- weight.frame$years[i]
  area <- weight.frame$region[i]
  area.adj <- as.character(unique(strata.weights$region.adj[strata.weights$region == area]))
  
  var <- matrix(NA, nrow = length(frames.unique),
                ncol = length(frames.unique))
  
  frame.idx <- 0
  for(frame in frames.unique){
    frame.idx <- match(frame, frames.unique)
    urb.id <- which(year.ids == yr &
                      grepl(frame, strata.ids) &
                      grepl("urban", strata.ids) &
                      area.ids == area)
    urb <- res.frame.eff.admin2$draws.est[[urb.id]]$draws
    rur.id <- which(year.ids == yr &
                      grepl(frame, strata.ids) &
                      grepl("rural", strata.ids) &
                      area.ids == area)
    rur <- res.frame.eff.admin2$draws.est[[rur.id]]$draws
    
    if(sum(grepl("year", names(strata.weights))) != 0){
      q.urb <- unique(strata.weights[strata.weights$frame == frame &
                                       strata.weights$years == yr &
                                       strata.weights$region.adj == as.character(area.adj), "urban"])
      agg[[i]][frame,] <- urb*q.urb + rur*(1-q.urb)
    }else{
      q.urb <- strata.weights[strata.weights$frame == frame, "urban"]
      agg[[i]][frame,] <- urb*q.urb + rur*(1-q.urb)
    }
    
    name.1 <- row.names(agg[[i]])[1]
    var[1,1] <- var(logit(agg[[i]][name.1,]))
    for(j in 2:dim(var)[1]){
      name.j <- row.names(agg[[i]])[j]
      var[j,j] <- var(logit(agg[[i]][name.j,]))
      for(k in 1:(j-1)){
        name.k <- row.names(agg[[i]])[k]
        var[k,j]  <- var[j,k] <- cov(logit(agg[[i]][name.k,]),
                                     logit(agg[[i]][name.j,]))
      }
    }
    
    one <- rep(1, dim(var)[1])
    weight.frame[i, frames.unique] <- solve(t(one)%*%solve(var)%*%one)%*%(t(one)%*%solve(var))
  }
  
}

save(weight.frame, file = paste0(folder.name, '/',
                                 country, '_rw2_admin2Bench_frameWeights.rda'))

res.frame.agg.admin2 <- res.frame.eff.admin2
res.frame.agg.admin2$final <- expand.grid(years = beg.year:end.year,
                                          region = admin2.names$Internal)
res.frame.agg.admin2$final$years.num <- res.frame.agg.admin2$final$years
res.frame.agg.admin2$final[,c("median", "lower", "upper")] <- NA
res.frame.agg.admin2$draws.est <- NULL
for(i in 1:nrow(res.frame.agg.admin2$final)){
  yr <- res.frame.agg.admin2$final$years.num[i]
  area <- res.frame.agg.admin2$final$region[i]
  idx <- which(weight.frame$years == yr &
    weight.frame$region == area)
  weights <- weight.frame[idx,frames.unique]
  sum.w <- 0
  
  for(frame in frames.unique){
    sum.w <- sum.w + mean(logit(agg[[idx]][frame,]))*weights[,frame]
  }
  
  var <- matrix(NA, nrow = length(frames.unique),
                ncol = length(frames.unique))
  name.1 <- row.names(agg[[idx]])[1]
  var[1,1] <- var(logit(agg[[idx]][name.1,]))
  for(j in 2:dim(var)[1]){
    name.j <- row.names(agg[[idx]])[j]
    var[j,j] <- var(logit(agg[[idx]][name.j,]))
    for(k in 1:(j-1)){
      name.k <- row.names(agg[[idx]])[k]
      var[k,j]  <- var[j,k] <- cov(logit(agg[[idx]][name.k,]),
                                   logit(agg[[idx]][name.j,]))
    }
  }
  one <- matrix(1, nrow = length(frames.unique), ncol = 1)
  se <- sqrt(solve(t(one)%*%solve(var)%*%one))
  tmp.list <- list(years = yr,
                   region = area,
                   draws = sum.w)
  res.frame.agg.admin2$final[i,c("median", "lower", "upper")] <- expit(c(sum.w, sum.w - 1.96*se, sum.w + 1.96*se))
  res.frame.agg.admin2$draws.est[[i]] <- tmp.list
  
}


save(res.frame.agg.admin2, file = paste0(folder.name, '/', country, '_res_frameAgg_',
                                         time.mod, '_admin2Bench.rda'))

#### Spaghetti Plot ####

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2Bench_frame_spaghetti.pdf'), height = 4, width = 8)
{
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.frame.agg.admin2$final[res.frame.agg.admin2$final$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == as.character(admin2.names$GADM[area.idx]) & 
                                 ihme.ests[[3]]$ADM1_NAME == as.character(poly.adm2$NAME_1[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <-
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    tmp.area[,c("median", "lower", "upper")] <- 
      tmp.area[,c("median", "lower", "upper")]*1000
    # print(tmp.ihme)
    cols <- rainbow(length(surveys)+1)
    plot.years <- beg.year:end.year
    pane.years <- jitter(seq(beg.year+2, end.year-2, 5))
    est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
    
    
    par(mfrow = c(1,2),lend=1)
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper, 
                        direct.admin2$mean[direct.admin2$region == area]),
                      na.rm = T) + 25
    }else{
      plot.max <- 0.25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           type = 'l', col = cols[1], lwd = 2,
           main = paste0(admin2.names$GADM[area.idx], ",\n", poly.adm2$NAME_1[area.idx]),
           cex.main = 0.8)
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = 1, legend = c(surveys, "Smoothed"))
      
    } else {
      
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = paste0(admin2.names$GADM[area.idx], ",\n", poly.adm2$NAME_1[area.idx]),
                 cex.main = 0.8)
            pane.years <- jitter(seq(beg.year+2, end.year-2,5))
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            ihme.years <- jitter(tmp.ihme$year)
            lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            
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
                 main = paste0(admin2.names$GADM[area.idx], ",\n", poly.adm2$NAME_1[area.idx]),
                 cex.main = 0.8)
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
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
      
      pane.years <- jitter(seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = c(rep(1, length(cols)+1)),
             legend = c(surveys, 'IHME', 'Betabinomial'),
             cex = 0.6)
      
      
      for(survey in surveys){
        # debugging
        # survey <- surveys[1]
        # survey <- surveys[2]
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
          }
        }
        
        
      }
      
      ihme.years <- tmp.ihme$year+2.5
      polygon(x = c(ihme.years, rev(ihme.years)),
              y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
              col = alpha(cols[length(surveys)+1], 0.25),
              border = FALSE)
      polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25), 
              border = FALSE)
      #      }
      legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                   'black'), .25),
             border = c(cols[(length(surveys)+1):length(cols)],
                        'black'),
             legend = c('IHME', 'Betabinomial'), cex = 0.75)
    }
    
    
  }
}
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod,
           '_admin2Bench_frame_spaghettiSingle.pdf'),
    height = 5, width = 5)
{
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.frame.agg.admin2$final[res.frame.agg.admin2$final$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == as.character(admin2.names$GADM[area.idx]) & 
                                 ihme.ests[[3]]$ADM1_NAME == as.character(poly.adm2$NAME_1[area.idx]),]    
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    cols <- rainbow(length(surveys)+1+1)
    plot.years <- beg.year:end.year
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    
    pane.years <- (seq(beg.year+2, end.year-2, 5))
    est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
    
    
    par(mfrow = c(1,1),lend=1)
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin2$upper[direct.admin2$region == as.character(area)]),
                      na.rm = T) + 25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           type = 'l', col = cols[1], lwd = 2,
           main = paste0(admin2.names$GADM[area.idx], ",\n", poly.adm2$NAME_1[area.idx]),
           cex.main = 0.8)
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = 1, legend = c(surveys, "Smoothed"))
      
    } else {
      
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = paste0(admin2.names$GADM[area.idx], ",\n", poly.adm2$NAME_1[area.idx]),
                 cex.main = 0.8)
            pane.years <- (seq(beg.year+2, end.year-2,5))
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            
            #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            
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
                 main = paste0(admin2.names$GADM[area.idx], ",\n", poly.adm2$NAME_1[area.idx]),
                 cex.main = 0.8)
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      ihme.years <- (tmp.ihme$year)
      lines(ihme.years, tmp.ihme$mean,
            lwd = 2, col  = cols[length(surveys)+1])
      polygon(x = c(ihme.years,
                    rev(ihme.years)),
              y = c(tmp.ihme$upper,
                    rev(tmp.ihme$lower)),
              col = alpha(cols[length(surveys)+1], 0.25),
              border = FALSE)
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))],
            col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(beg.year:end.year,
                    rev(beg.year:end.year)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = c(rep(1, length(cols)+1)),
             legend = c(surveys, 'IHME', 'Smoothed'),
             cex = 0.6)
      
      
    }
    
    
  }
}
dev.off()


if (grepl("randomSlopes",time.mod)) {
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod, '_admin2Bench_frame_randomSlopes.pdf'), height = 5, width = 5)
  years.std <- ((beg.year:end.year)-mean(beg.year:end.year))/sd(beg.year:end.year)
  # plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.025quant","0.975quant")],
  #                       years.std[length(years.std)]*posteriorRandomSlopes[,c("0.025quant","0.975quant")]))
  plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                        years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
  par(lend=1)
  plot(NA,xlim=range(years.std),ylim = plot.range,
       ylab = "effect",xlab="year (standardized)")
  
  region.cols <- viridis_pal(option = "A")(nrow(posteriorRandomSlopes))
  for (i in 1:nrow(posteriorRandomSlopes)) {
    abline(0,posteriorRandomSlopes$`0.5quant`[i],col=region.cols[i])
    # polygon(x=c(years.std,rev(years.std)),
    #         y=c(years.std*posteriorRandomSlopes$`0.975quant`[i],rev(years.std*posteriorRandomSlopes$`0.025quant`[i])),
    #         col=alpha(region.cols[i],0.25),border = NA)
  }
  legend("topleft",legend=admin2.names[,1],col=region.cols,lty=1,ncol=2,cex=0.5)
  dev.off()
}

med.palette <- rev(brewer.pal(n = 7, name = "RdBu"))
med.int <- classIntervals(round(spaces$median[spaces$label == "Total"], 3),
                          n = 7, style = 'jenks')
med.col <- findColours(med.int, med.palette)

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2Bench_frame_spatial.pdf'))
par(mar = c(0.25,0.25,0.25,0.25),lend=1)
plot(poly.adm2, col = med.col,
     xlim = poly.adm2@bbox['x',],
     ylim = poly.adm2@bbox['y',],
     axes = F)
legend('bottomleft', fill = med.palette,
       legend = names(attr(med.col, 'table')),
       bty = 'n', cex = 0.75)
dev.off()

#### Spaghetti plots for all regions over time ####

# change the region names from Internal to GADM for legend labels
res.frame.agg.admin2$final$region.orig <- res.frame.agg.admin2$final$region
res.frame.agg.admin2$final$region <- as.character(res.frame.agg.admin2$final$region)
for (i in 1:nrow(admin2.names)) {
  res.frame.agg.admin2$final$region[as.character(res.frame.agg.admin2$final$region.orig) == 
                                      as.character(admin2.names$Internal[i])] <- 
    paste(as.character(admin2.names$GADM[i]), 
          as.character(poly.adm2$NAME_1[i]), 
          sep =", ")
}

# make the plot ordered by magnitude of u5mr in 2020
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2Bench_frame_spaghetti_all.pdf'))
par(lend=1)

numberAreasPerPage <- 15
numberAreasTotal <- nrow(admin2.names)
numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)

# order data by median magnitude in 2020
res.admin2.2020 <- res.frame.agg.admin2$final %>% 
  filter(years.num == 2020) %>%
  arrange(median)
areaOrder <- res.admin2.2020$region.orig

# loop and make plots
for (i in 1:numberPages) {
  if (i != numberPages) {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
  } else {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
  }
  tmp <- res.frame.agg.admin2$final[res.frame.agg.admin2$final$region.orig %in% areas,]
  
  print(ggplot(tmp, aes(x = years.num, y = median*1000, col = region)) +
          geom_line() +
          geom_point() +
          theme_light() +
          xlab("Year") +
          ylab("U5MR: deaths per 1000 live births") +
          ggtitle(country) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8)) +
          guides(col = guide_legend(ncol=2)) +
          ylim(c(0, max(res.frame.agg.admin2$final$median)*1000)))
}

dev.off()

# make the plot ordered by admin1/admin2
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2Bench_frame_spaghetti_all_byAdmin1.pdf'))
par(lend=1)

# loop and make plots
for (i in 1:nrow(admin1.names)) {

  tmp <- res.frame.agg.admin2$final[grepl(paste0(", ", as.character(admin1.names$GADM)[i]), 
                                          res.frame.agg.admin2$final$region),]
  
  print(ggplot(tmp, aes(x = years.num, y = median*1000, col = region)) +
          geom_line() +
          geom_point() +
          theme_light() +
          xlab("Year") +
          ylab("U5MR: deaths per 1000 live births") +
          ggtitle(paste0(country, 
                        ": ", 
                        as.character(admin1.names$GADM)[i])) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8)) +
          guides(col = guide_legend(ncol=2)) +
          ylim(c(0, max(res.frame.agg.admin2$final$median)*1000)))
}

dev.off()



