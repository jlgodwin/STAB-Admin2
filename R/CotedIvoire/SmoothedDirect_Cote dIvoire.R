#  SmoothedDirect.R
#  author: Jessica Godwin
#  
#  sources: LoadCommandCenter.R
#           IHMEHand_CountryName.R


rm(list = ls())
setwd('~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/')
devtools::install_github("bryandmartin/SUMMER",
                         build_vignettes = F, force = T)


#### Libraries ####
library(SUMMER)
help(package = "SUMMER", help_type = "html")
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

source('../../Analysis/R/LoadCommandCenter.R')


#### Parameters ####

country <- "Cote dIvoire"
beg.year <- 1990
end.year <- 2019
doBenchmark <- T

beg.per <- seq(beg.year,end.year,5)
end.per <- beg.per + 4
periods <- paste(beg.per, end.per, sep = "-")

#CountryList <- gs_read(sheet_key, ws = "CountryList")
CountryList <- sheets_read(sheet_key, sheet = "CountryList")
folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
n.survey <- CountryList$nSurvey[CountryList$Country == country]

#### Use HIV Adjusted data? ####
#HIV.sheet <- gs_read(sheet_key, ws = "HIV")
HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                unique(HIV.country$`UNAIDS data?`) == "Y")

#### Get Survey years #### 

#SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]

#### Load polygon files ####

poly.file <- "/shapeFiles_gadm"
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

proj4string(poly.adm0) <- proj4string(poly.adm1) 
load(paste0(folder.name,'/shapeFiles_gadm/', country, '_Amat.rda'))
load(paste0(folder.name, '/shapeFiles_gadm/', country, '_Amat_Names.rda'))

#### Load data ####

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

#### Load IGME data ####
file.list <- list.files('../../Analysis/R')
igme.file <- file.list[grepl("IGME", file.list)]
igme.ests <- read.csv(paste0('../../Analysis/R/',igme.file),
                      header = T)
igme.ests <- igme.ests[igme.ests$INDICATOR == "Under-five mortality rate" &
                         igme.ests$SEX == "Total" &
                         igme.ests$SERIES_YEAR == "2019" &
                         igme.ests$SERIES_NAME == "UN IGME estimate 2019",]
igme.ests$year <- igme.ests$REF_DATE - 0.5
igme.ests <- igme.ests[igme.ests$REF_AREA == levels(igme.ests$REF_AREA)[45], ]
igme.ests <- igme.ests[order(igme.ests$year),]
igme.ests <- igme.ests[igme.ests$year %in% beg.year:end.year,]
igme.ests


#### Load IHME data ####
file.list <- list.files('../../Analysis/R')
ihme.files <- file.list[grepl("IHME", file.list)]

ihme.ests <- list()
ihme.ests[['adm0']] <- read.csv( paste0('../../Analysis/R/',
                                        ihme.files[grepl("ADM0", ihme.files)]),
                                 header = T)
ihme.ests[['adm1']] <- read.csv( paste0('../../Analysis/R/',
                                        ihme.files[grepl("ADMIN1", ihme.files)]),
                                 header = T)
ihme.ests[['adm2']] <- read.csv( paste0('../../Analysis/R/',
                                        ihme.files[grepl("ADMIN2", ihme.files)]),
                                 header = T)

# ihme.ests <- lapply(ihme.ests, function(x){
#   if(!(country %in% x$ADM0_NAME)){
#     message('\n Country name not found in one of the IHME files.\n')
#   }
#   x[x$ADM0_NAME == country,]
# })

ihme.ests$adm0 <- ihme.ests$adm0[ihme.ests$adm0$ADM0_NAME ==
                                   levels(ihme.ests$adm0$ADM0_NAME)[20],]
ihme.ests$adm1 <- ihme.ests$adm1[ihme.ests$adm1$ADM0_NAME ==
                                   levels(ihme.ests$adm1$ADM0_NAME)[6],]
ihme.ests$adm2 <- ihme.ests$adm2[ihme.ests$adm2$ADM0_NAME ==
                                   levels(ihme.ests$adm2$ADM0_NAME)[6],]

doAdmin2 <- TRUE
if(sum(!(admin1.names$GADM %in% ihme.ests$adm1$ADM1_NAME)) != 0){
  message("IHME Admin 1 names do not match GADM Admin 1 names.\n")
}

if(sum(!(admin1.names$GADM %in% ihme.ests$adm2$ADM2_NAME)) == 0){
  ihme.ests$adm1 <- ihme.ests$adm2
  ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
  doAdmin2 <- FALSE
  message("IHME Admin 2 names match GADM Admin 1 names.\n Changing Admin 1 to Admin 2.\n")
}else if(sum(!(admin1.names$GADM %in% ihme.ests$adm2$ADM2_NAME)) != 0){
  message("IHME Admin 2 names don't match GADM Admin 1 names either.\n")
}

stop("This part will be different for everyone!")
source(paste0('../../Analysis/countryAnalysisFolders/', folder.name,
              '/IHMEHand_', country, '.R'))


#### Aggregate surveys ####
data.natl <- aggregateSurvey(direct.natl)
data.natl.yearly <- aggregateSurvey(direct.natl.yearly)
data.admin1 <- aggregateSurvey(direct.admin1)


#### National Model ####
proj.per <- paste(end.year+1, end.year+5, sep = "-")
fit.natl <- fitINLA(data.natl, geo = NULL, Amat = NULL,
                    year_label = c(periods, proj.per),
                    year_range = c(beg.year, end.year + 5), is.yearly = F)
res.natl <- getSmoothed(fit.natl, year_range = c(beg.year, end.year+5),
                        year_label = c(periods, proj.per))
res.natl$years.num <- seq(beg.year+2, end.year+5, 5)
res.natl$region.gadm <- country
head(res.natl)
tail(res.natl)
file.out <- paste0(country, "_res_natl_SmoothedDirect.rda")
save(res.natl, file = paste0(folder.name, '/', file.out))

fit.natl.yearly <- fitINLA(data.natl.yearly, geo = NULL, Amat = NULL,
                           year_label = as.character(beg.year:(end.year + 5)),
                           year_range = c(beg.year, end.year + 5), is.yearly = F)
res.natl.yearly <- getSmoothed(fit.natl.yearly, year_range = c(beg.year, end.year + 5),
                               year_label = as.character(beg.year:(end.year + 5)))
res.natl.yearly$years.num <- beg.year:(end.year + 5)
res.natl.yearly$region.gadm <- country
head(res.natl.yearly)
tail(res.natl.yearly)
file.out <- paste0(country, "_res_natl_yearly_SmoothedDirect.rda")
save(res.natl.yearly, file = paste0(folder.name, '/', file.out))


#### Admin1 Model ####

fit.admin1 <- fitINLA(data.admin1, geo = poly.adm1, Amat = admin1.mat,
                      year_label = c(periods, "2020-2024"),
                      year_range = c(1990, 2024), is.yearly = F)
res.admin1 <- getSmoothed(fit.admin1, Amat = admin1.mat,
                          year_range = c(1990, 2024),
                          year_label = c(periods, "2020-2024"))
res.admin1$years.num <- seq(beg.year+2, end.year+5, 5)[match(res.admin1$years, c(periods, "2020-2024"))]
res.admin1$region.gadm <- admin1.names$GADM[match(res.admin1$region, admin1.names$Internal)]
head(res.admin1)
tail(res.admin1)


file.out <- paste0(country, "_res_admin1_SmoothedDirect.rda")
save(res.admin1, file = paste0(folder.name, '/', file.out))

#### Benchmarking ####

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
  
  data.natl <- getAdjusted(data.natl,
                           ratio = benchmark,
                           logit.lower = NULL,
                           logit.upper = NULL,
                           prob.upper = "upper",
                           prob.lower = "lower")
  fit.natl <- fitINLA(data.natl, geo = NULL, Amat = NULL,
                      year_label = c(periods, proj.per),
                      year_range = c(beg.year, end.year + 5), is.yearly = F)
  res.natl <- getSmoothed(fit.natl, 
                          year_label = c(periods, proj.per),
                          year_range = c(beg.year, end.year + 5))
  res.natl$years.num <- seq(beg.year+2, end.year+5, 5)
  res.natl$region.gadm <- country
  head(res.natl)
  tail(res.natl)
  
  
  file.out <- paste0(country, "_res_natlBench_SmoothedDirect.rda")
  
  save(res.natl, file = paste0(folder.name, '/', file.out))
  
  data.admin1 <- getAdjusted(data.admin1,
                             ratio = benchmark,
                             logit.lower = NULL,
                             logit.upper = NULL,
                             prob.upper = "upper",
                             prob.lower = "lower")
  
  fit.admin1 <- fitINLA(data.admin1, geo = poly.adm1, Amat = admin1.mat,
                        year_label = c(periods, proj.per),
                        year_range = c(beg.year, end.year + 5), is.yearly = F)
  res.admin1 <- getSmoothed(fit.admin1, Amat = admin1.mat,
                            year_label = c(periods, proj.per),
                            year_range = c(beg.year, end.year + 5))
  res.admin1$years.num <- seq(beg.year+2, end.year+5, 5)[match(res.admin1$years, c(periods, proj.per))]
  res.admin1$region.gadm <- admin1.names$GADM[match(res.admin1$region, admin1.names$Internal)]
  head(res.admin1)
  tail(res.admin1)
  
  file.out <- paste0(country, "_res_admin1Bench_SmoothedDirect.rda")
  
  save(res.admin1, file = paste0(folder.name, '/', file.out))
  
  benchmark <- data.frame(years = beg.year:end.year,
                          ratio = NA)
  benchmark$ratio <- data.natl.yearly$mean/(igme.ests$OBS_VALUE/1000)[match(beg.year:end.year, igme.ests$year)]
  data.natl.yearly <- getAdjusted(data.natl.yearly,
                           ratio = benchmark,
                           logit.lower = NULL,
                           logit.upper = NULL,
                           prob.upper = "upper",
                           prob.lower = "lower")
  fit.natl.yearly <- fitINLA(data.natl.yearly, geo = NULL, Amat = NULL,
                      year_label = as.character(beg.year:(end.year + 5)),
                      year_range = c(beg.year, (end.year + 5)),
                      is.yearly = F)
  res.natl.yearly<- getSmoothed(fit.natl.yearly, 
                          year_label = as.character(beg.year:(end.year + 5)),
                          year_range = c(beg.year, (end.year + 5)))
  res.natl.yearly$years.num <- beg.year:(end.year+5)
  res.natl.yearly$region.gadm <- country
  head(res.natl.yearly)
  tail(res.natl.yearly)
  
  file.out <- paste0(country, "_res_natlBench_yearly_SmoothedDirect.rda")
  save(res.natl.yearly, file = paste0(folder.name, '/', file.out))
  
}

#### Spaghetti Plots ####

if(!dir.exists(paths = paste0(folder.name, '/Plots/', 'SmoothedDirect'))){
  dir.create(path = paste0(folder.name, '/Plots/', 'SmoothedDirect'))
}


plot.file <- 
  pdf(paste0(folder.name,"/Plots/SmoothedDirect/", country,
             '_natl_SmoothedDirect.pdf'))
plot(res.natl, is.yearly = F, is.subnational = F) + ggtitle(country)
dev.off()

pdf(paste0(folder.name,"/Plots/SmoothedDirect/", country,
           '_admin1_SmoothedDirect.pdf'))
tmp.res <- res.admin1
tmp.res$region <- tmp.res$region.gadm
plot(tmp.res, is.yearly = F, is.subnational = T) 
dev.off()


cols <- rainbow(length(unique(direct.admin1$surveyYears)))
plot.years <- seq(1992, 2017, 5)

par(mfrow = c(1,1))
pdf(paste0(folder.name,"/Plots/SmoothedDirect/",
           country, '_natl_SmoothedDirect_spaghetti.pdf'))

direct.natl$width <- direct.natl$upper - direct.natl$lower
direct.natl$cex2 <- median(direct.natl$width, na.rm = T)/direct.natl$width
direct.natl$cex2[direct.natl$cex2 > 6] <- 6

if(dim(direct.natl)[1] != 0 & !(sum(is.na(direct.natl$mean)) == nrow(direct.natl))){
  plot.max <- max(direct.natl$upper+.025, na.rm = T)
}else{
  plot.max <- 0.25
}

if (nrow(direct.natl) >0 & sum(is.na(direct.natl$mean)) == nrow(direct.natl)) {
  plot(NA,
       xlab = "Year", ylab = "U5MR",
       ylim = c(0, plot.max),
       xlim = c(beg.year, end.year + 5),
       type = 'l', col = cols[svy.idx], lwd = 2,
       main = country)
  
  igme.years <- jitter(beg.year:max(igme.ests$year))
  lines(igme.years, igme.ests$OBS_VALUE/1000, lwd = 2, col  = 'grey37')
  lines(igme.years, igme.ests$UPPER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
  lines(igme.years, igme.ests$LOWER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
 
  ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
  lines(ihme.years, ihme.ests[[1]]$mean, lwd = 2, col  = 'darkgrey')
  lines(ihme.years, ihme.ests[[1]]$lower, lwd = 1, lty = 2, col  = 'darkgrey')
  lines(ihme.years, ihme.ests[[1]]$upper, lwd = 1, lty = 2, col  = 'darkgrey')
  
  
  legend('topright', bty = 'n', col = c(cols, 'grey37', 'darkgrey','black'),
         lwd = 2, lty = 1, legend = c(surveys, "UN IGME", "IHME", "Smoothed"))
  
} else {
  for(survey in surveys){
    tmp <- direct.natl[direct.natl$surveyYears == survey,]
    svy.idx <- match(survey, surveys) 
    pane.years <- jitter(plot.years)
    
    if(svy.idx== 1){
      if(dim(tmp)[1] != 0){
        plot(NA,
             xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year + 5),
             type = 'l', col = cols[svy.idx], lwd = 2,
             main = country)
        
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l', col = cols[svy.idx],
              lwd = 2)
        
        points(pane.years, tmp$mean, pch = 19,
               col = alpha(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        
        
        igme.years <- jitter(beg.year:max(igme.ests$year))
        lines(igme.years, igme.ests$OBS_VALUE/1000, lwd = 2, col  = 'grey37')
        lines(igme.years, igme.ests$UPPER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
        lines(igme.years, igme.ests$LOWER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
        
        ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
        lines(ihme.years, ihme.ests[[1]]$mean, lwd = 2, col  = 'darkgrey')
        lines(ihme.years, ihme.ests[[1]]$lower, lwd = 1, lty = 2, col  = 'darkgrey')
        lines(ihme.years, ihme.ests[[1]]$upper, lwd = 1, lty = 2, col  = 'darkgrey')
        
        
        
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
             main = country)
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
  
  }

lines(res.natl$years.num, res.natl$median, col = 'black', lwd = 2)
lines(res.natl$years.num, res.natl$upper, col = 'black', lwd = 1, lty = 2)
lines(res.natl$years.num, res.natl$lower, col = 'black', lwd = 1, lty = 2)
legend('topright', bty = 'n', col = c(cols, 'grey37', 'darkgrey','black'),
       lwd = 2, lty = 1, legend = c(surveys, "UN IGME", "IHME", "Smoothed"))

dev.off()

par(mfrow = c(1,1))
pdf(paste0(folder.name,"/Plots/SmoothedDirect/",
           country, '_natl_yearly_SmoothedDirect_spaghetti.pdf'))

direct.natl.yearly$width <- direct.natl.yearly$upper - direct.natl.yearly$lower
direct.natl.yearly$cex2 <- median(direct.natl.yearly$width, na.rm = T)/direct.natl.yearly$width
direct.natl.yearly$cex2[direct.natl.yearly$cex2 > 6] <- 6

if(dim(direct.natl.yearly)[1] != 0 & !(sum(is.na(direct.natl.yearly$mean)) == nrow(direct.natl.yearly))){
  plot.max <- max(direct.natl.yearly$upper+.025, na.rm = T)
}else{
  plot.max <- 0.25
}

if (nrow(direct.natl.yearly) >0 & sum(is.na(direct.natl.yearly$mean)) == nrow(direct.natl.yearly)) {
  plot(NA,
       xlab = "Year", ylab = "U5MR",
       ylim = c(0, plot.max),
       xlim = c(beg.year, end.year + 5),
       type = 'l', col = cols[svy.idx], lwd = 2,
       main = country)
  
  igme.years <- jitter(beg.year:max(igme.ests$year))
  lines(igme.years, igme.ests$OBS_VALUE/1000, lwd = 2, col  = 'grey37')
  lines(igme.years, igme.ests$UPPER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
  lines(igme.years, igme.ests$LOWER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
  
  ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
  lines(ihme.years, ihme.ests[[1]]$mean, lwd = 2, col  = 'darkgrey')
  lines(ihme.years, ihme.ests[[1]]$lower, lwd = 1, lty = 2, col  = 'darkgrey')
  lines(ihme.years, ihme.ests[[1]]$upper, lwd = 1, lty = 2, col  = 'darkgrey')
  
  
  legend('topright', bty = 'n', col = c(cols, 'grey37' , 'darkgrey','black'),
         lwd = 2, lty = 1, legend = c(surveys, "UN IGME", "IHME", "Smoothed"))
  
} else {
  for(survey in surveys){
    tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
    svy.idx <- match(survey, surveys) 
    pane.years <- jitter(beg.year:(end.year))
    
    if(svy.idx== 1){
      if(dim(tmp)[1] != 0){
        plot(NA,
             xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year + 5),
             type = 'l', col = cols[svy.idx], lwd = 2,
             main = country)
        
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l', col = cols[svy.idx],
              lwd = 2)
        
        points(pane.years, tmp$mean, pch = 19,
               col = alpha(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        
        
        igme.years <- jitter(beg.year:max(igme.ests$year))
        lines(igme.years, igme.ests$OBS_VALUE/1000, lwd = 2, col  = 'grey37')
        lines(igme.years, igme.ests$UPPER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
        lines(igme.years, igme.ests$LOWER_BOUND/1000, lwd = 1, lty = 2, col  = 'grey37')
        
        ihme.years <- jitter(min(ihme.ests[[1]]$year):max(ihme.ests[[1]]$year))
        lines(ihme.years, ihme.ests[[1]]$mean, lwd = 2, col  = 'darkgrey')
        lines(ihme.years, ihme.ests[[1]]$lower, lwd = 1, lty = 2, col  = 'darkgrey')
        lines(ihme.years, ihme.ests[[1]]$upper, lwd = 1, lty = 2, col  = 'darkgrey')
        
        
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
             main = country)
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
  
  legend('topright', bty = 'n', col = c(cols, 'grey37' , 'darkgrey','black'),
         lwd = 2, lty = 1, legend = c(surveys, "UN IGME", "IHME", "Smoothed"))
}

lines(res.natl.yearly$years.num, res.natl.yearly$median, col = 'black', lwd = 2)
lines(res.natl.yearly$years.num, res.natl.yearly$upper, col = 'black', lwd = 1, lty = 2)
lines(res.natl.yearly$years.num, res.natl.yearly$lower, col = 'black', lwd = 1, lty = 2)

dev.off()

## Admin 1 ##


par(mfrow = c(1,1))
pdf(paste0(folder.name,"/Plots/SmoothedDirect/",
           country, '_admin1_SmoothedDirect_spaghetti.pdf'))

for(area in 1:dim(poly.adm1)[1]){
  tmp.area <- direct.admin1[direct.admin1$region == as.character(admin1.names$Internal[area]),]
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  
  tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == 
                               as.character(admin1.names$GADM[area]),]
  
  res.area <- res.admin1[res.admin1$region == as.character(admin1.names$Internal[area]),]
  
  if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(tmp.area$upper+.025, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
    plot(NA,
         xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year + 5),
         type = 'l', col = cols[svy.idx], lwd = 2,
         main = admin1.names$GADM[area])
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = 1, legend = c(surveys, "Smoothed"))
    
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
               xlim = c(beg.year, end.year + 5),
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
  
  lines(res.area$years.num, res.area$median, col = 'black', lwd = 2)
  lines(res.area$years.num, res.area$upper, col = 'black', lwd = 1, lty = 2)
  lines(res.area$years.num, res.area$lower, col = 'black', lwd = 1, lty = 2)
 
  legend('topright', bty = 'n', col = c(cols, 'darkgrey', 'black'),
         lwd = 2, lty = 1, legend = c(surveys, 'IHME', "Smoothed"))
  
}
dev.off()




pdf(paste0(folder.name,"/Plots/SmoothedDirect/",
           country, '_admin1_SmoothedDirect_spaghetti2.pdf'),
    height = 4, width = 8)
par(mfrow = c(2,4))
for(area in 1:dim(poly.adm1)[1]){
  tmp.area <- direct.admin1[direct.admin1$region == as.character(admin1.names$Internal[area]),]
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  
  tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == 
                               as.character(admin1.names$GADM[area]),]
  
  
  res.area <- res.admin1[res.admin1$region == as.character(admin1.names$Internal[area]),]
  
  if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(tmp.area$upper+.025, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
    plot(NA,
         xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year + 5),
         type = 'l', col = cols[svy.idx], lwd = 2,
         main = admin1.names$GADM[area])
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = 1, legend = c(surveys, "Smoothed"))
    
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
               xlim = c(beg.year, end.year + 5),
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
  
  lines(res.area$years.num, res.area$median, col = 'black', lwd = 2)
  lines(res.area$years.num, res.area$upper, col = 'black', lwd = 1, lty = 2)
  lines(res.area$years.num, res.area$lower, col = 'black', lwd = 1, lty = 2)
  
  legend('topright', bty = 'n', col = c(cols, 'grey37', 'black'),
         lwd = 2, lty = 1, legend = c(surveys, "IHME", "Smoothed"),
         cex = .5)
}

dev.off()

#### Polygon plots ####

med.palette <- brewer.pal(n = 7, name = "Purples")
med.int <- classIntervals(round(res.admin1$median, 3),
                          n = 7, style = 'jenks')
med.col <- findColours(med.int, med.palette)

pdf(paste0(folder.name,"/Plots/SmoothedDirect/", country, '_admin1_', 
           'SmoothedDirect_poly.pdf'))

par(mfrow = c(2,4))
for(year in periods){
  idx <- which(res.admin1$years == year) 
  plot(poly.adm1, border = F, col = med.col[idx],
       axes = F, main = year)
}
plot(NA, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
legend(x = "center",inset = 0,
       legend = names(attr(med.col, 'table')),
       fill = med.palette, cex= .75, horiz = FALSE, bty = 'n')

dev.off()

