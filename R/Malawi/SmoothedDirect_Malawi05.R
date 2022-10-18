#  SmoothedDirect.R
#  author: Jessica Godwin
#  
#  sources: LoadCommandCenter.R
#           IHMEHand_CountryName.R


rm(list = ls())

#### Libraries ####
# devtools::install_github("bryandmartin/SUMMER",
# build_vignettes = F, force = T)
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


#### Parameters ####

country <- "Malawi"
beg.year <- 1990
end.year <- 2019
doBenchmark <- T

beg.years <- seq(1990,2015,5)
end.years <- beg.years + 4
periods <- paste(beg.years, end.years, sep = "-")


data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '../../Analysis/R'
ihme.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '../HIV/'
setwd(data.dir)
source(paste0(code.dir.rel,'/LoadCommandCenter.R'))


CountryList <- sheets_read(sheet_key, sheet = "CountryList")
# CountryList <- read.csv("CountryList.csv", header = T)

folder.name <- CountryList$folderName[CountryList$Country == country]

hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
                       gsub(" ", "", folder.name))

source(paste0(hand.dir.rel, 
              '/smoothDirect_20210112JLG.R'))
source(paste0(hand.dir.rel, 
              '/getSmoothed_20210112JLG.R'))

source(paste0(hand.dir.rel, 
              '/INLA-internal.R'))

source(paste0(hand.dir.rel, 
              '/INLA-internal-pc.R'))


gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
n.survey <- CountryList$nSurvey[CountryList$Country == country]

## Get Survey years #### 

#SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]

## Do HIV Adjustment? ####
#HIV.sheet <- gs_read(sheet_key, ws = "HIV")
HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                unique(HIV.country$`UNAIDS data?`) == "Y")


#### Load polygon data ####
poly.file <- shapes.sub.dir
poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                         '0', sep = "_")
# poly.layer.adm1 <- paste('gadm36', gadm.abbrev,
#                          '1', sep = "_")

poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                         '2', sep = "_")

poly.path <- paste0(folder.name, poly.file)
poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0))
# poly.adm1 <- readOGR(dsn = poly.path,
#                      layer = as.character(poly.layer.adm1))
poly.adm1 <- readOGR(dsn = 'Malawi/shapeFiles_MW2004DHS/shps/',
                     layer =  "sdr_subnational_boundaries")

# if(sum(grepl(paste('gadm36', gadm.abbrev,
#                    '2', sep = "_"), list.files(paste0(folder.name, poly.file)))) != 0){
#   poly.adm2 <- readOGR(dsn = poly.path,
#                        layer = as.character(poly.layer.adm2))
# }

if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat.rda'))
load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat_Names.rda'))

#### Load data ####

files.list <- list.files(paste0('./', folder.name))

if(useHIVAdj){
  if(sum(grep("HIV_", files.list)) == 0){
    warning("No HIV adjusted files founds.")
    load(paste0(folder.name, '/', country, '_direct_natl_yearly.rda'))
    load(paste0(folder.name, '/', country, '_direct_natl.rda'))
    load(paste0(folder.name, '/', country, '_direct_admin0.5.rda'))
  }else{
    load(paste0(folder.name, '/', country, '_directHIV_natl_yearly.rda'))
    load(paste0(folder.name, '/', country, '_directHIV_natl.rda'))
    load(paste0(folder.name, '/', country, '_directHIV_admin0.5.rda'))
  }
}else{
  load(paste0(folder.name, '/', country, '_direct_natl_yearly.rda'))
  load(paste0(folder.name, '/', country, '_direct_natl.rda'))
  load(paste0(folder.name, '/', country, '_direct_admin0.5.rda'))
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

# doAdmin2 <- TRUE
# if(country == "Malawi"){
#   ihme.ests$adm1 <- ihme.ests$adm2
#   ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
doAdmin2 <- FALSE
# }

# source(paste0(hand.dir.rel,
#               '/IHMEHand_', country, '.R'))




#### Aggregate surveys ####
data.natl <- aggregateSurvey(direct.natl)
data.natl.yearly <- aggregateSurvey(direct.natl.yearly)
data.admin1 <- aggregateSurvey(direct.admin1)


#### National Model ####
# proj.per <- paste(end.year+1, end.year+5, sep = "-")
# fit.natl <- smoothDirect(data.natl, Amat = NULL,
#                              year_label = c(periods, 
#                                             proj.per),
#                              year_range = c(beg.year,
#                                             end.year+5),
#                              is.yearly = TRUE, m = 5,
#                          include_time_unstruct = TRUE)
# 
# res.natl <- getSmoothed(fit.natl, year_range = c(beg.year, end.year+5),
#                         year_label = c(periods, proj.per))
# res.natl$years.num <- seq(beg.year+2, end.year+5, 5)
# res.natl$region.gadm <- country
# head(res.natl)
# tail(res.natl)
# file.out <- paste0(country, "_res_natl_SmoothedDirect.rda")
# save(res.natl, file = paste0(folder.name, '/', file.out))

fit.natl.yearly.iid.m1 <- smoothDirect(data.natl.yearly,
                                       Amat = NULL,
                                       year_label = as.character(beg.year:(end.year)),
                                       year_range = c(beg.year, end.year),
                                       is.yearly = F, m = 1,
                                       include_time_unstruct = TRUE)
res.natl.yearly.iid.m1 <- getSmoothed(fit.natl.yearly.iid.m1,
                                      year_range = c(beg.year, end.year),
                                      year_label = as.character(beg.year:(end.year)),
                                      save.draws = TRUE)

fit.natl.yearly.noiid.m1 <- smoothDirect(data.natl.yearly,
                                         Amat = NULL,
                                         year_label = as.character(beg.year:(end.year)),
                                         year_range = c(beg.year, end.year),
                                         is.yearly = FALSE, m = 1,
                                         include_time_unstruct = FALSE)
res.natl.yearly.noiid.m1 <- getSmoothed(fit.natl.yearly.noiid.m1,
                                        year_range = c(beg.year, end.year),
                                        year_label = as.character,
                                        save.draws = TRUE)


res.natl.yearly.noiid.m1$results$years.num <- 
  res.natl.yearly.iid.m1$results$years.num <- beg.year:(end.year)
res.natl.yearly.noiid.m1$results$region.gadm <-
  res.natl.yearly.iid.m1$results$region.gadm <- country
head(res.natl.yearly.noiid.m1$results)
tail(res.natl.yearly.noiid.m1$results)
head(res.natl.yearly.iid.m1$results)
tail(res.natl.yearly.iid.m1$results)
file.out <- paste0(country, "_res_natl_yearly_noiid_m1_SmoothedDirect.rda")
save(res.natl.yearly.noiid.m1, file = paste0(folder.name, '/', file.out))
file.out <- paste0(country, "_res_natl_yearly_iid_m1_SmoothedDirect.rda")
save(res.natl.yearly.iid.m1, file = paste0(folder.name, '/', file.out))


pdf('Malawi05_NatlYearlyTemporal.pdf',
    height = 4, width = 8)
{
  par(mfrow = c(1,2))
  
  plot(beg.year:end.year,
       res.natl.yearly.iid.m1$results$logit.median[1:30],
       lwd = 2,
       xlab = "Year",
       ylab = "Median logit U5MR", type = 'l')
  polygon(c(beg.year:end.year,
            rev(beg.year:end.year)),
          c(res.natl.yearly.iid.m1$results$logit.upper[1:30],
            rev(res.natl.yearly.iid.m1$results$logit.lower[1:30])),
          border = FALSE,
          col = alpha('black', 0.25))
  lines(beg.year:end.year,
        res.natl.yearly.noiid.m1$results$logit.median,
        col = 'blue',
        lwd = 2)
  polygon(c(beg.year:end.year,
            rev(beg.year:end.year)),
          c(res.natl.yearly.noiid.m1$results$logit.upper[1:30],
            rev(res.natl.yearly.noiid.m1$results$logit.lower[1:30])),
          border = FALSE,
          col = alpha('blue', 0.25))
  points(data.natl.yearly$years,
         data.natl.yearly$logit.est, col = 'red', pch = 16)
  legend('bottomleft',
         pch = c(16, NA,NA),
         lwd = c(NA, 2,2),
         col = c('red', 'black', 'blue'),
         legend = c('H-T meta-analyis',
                    'Smoothed w/ IID', 'Smoothed no IID'), bty = 'n', cex = 0.8)
  
  plot(NA,
       xlim = c(beg.year, end.year),
       ylim = c(min(fit.natl.yearly.iid.m1$fit$summary.random$time.struct$`0.025quant`),
                max(fit.natl.yearly.iid.m1$fit$summary.random$time.struct$`0.975quant`)),
       xlab = "Year",
       ylab = "logit temporal effect")
  abline(h=0)
  lines(beg.year:end.year,
        fit.natl.yearly.iid.m1$fit$summary.random$time.struct$`0.5quant`, lwd = 2, col = 'blue')
  lines(beg.year:end.year,
        fit.natl.yearly.iid.m1$fit$summary.random$time.unstruct$`0.5quant`, lwd = 2, col = 'red')
  lines(beg.year:end.year,
        fit.natl.yearly.iid.m1$fit$summary.random$time.struct$`0.5quant`+
          fit.natl.yearly.iid.m1$fit$summary.random$time.unstruct$`0.5quant`, lwd = 2, col = 'purple')
  legend('topright',
         lwd = 2,
         col = c("red", "blue", "purple"),
         legend = c("IID", "RW2", "IID + RW2"),
         bty = 'n', cex = 0.8)
}
dev.off()

pdf('Malawi05_NatlYearlyTemporal_IIDvsnoIID.pdf',
    height = 4, width = 8)
{
  par(mfrow = c(1,2))
  
  plot(beg.year:end.year,
       res.natl.yearly.iid.m1$results$logit.median[1:30],
       lwd = 2,
       xlab = "Year",
       ylab = "Median logit U5MR", type = 'l')
  polygon(c(beg.year:end.year,
            rev(beg.year:end.year)),
          c(res.natl.yearly.iid.m1$results$logit.upper[1:30],
            rev(res.natl.yearly.iid.m1$results$logit.lower[1:30])),
          border = FALSE,
          col = alpha('black', 0.25))
  points(data.natl.yearly$years,
         data.natl.yearly$logit.est, col = 'red', pch = 16)
  legend('bottomleft',
         pch = c(16, NA),
         lwd = c(NA, 2),
         col = c('red', 'black'),
         legend = c('H-T meta-analyis',
                    'Smoothed w/ IID'),
         bty = 'n', cex = 0.8)
  plot(beg.year:end.year,
       res.natl.yearly.noiid.m1$results$logit.median[1:30],
       lwd = 2,
       xlab = "Year", col = 'navy',
       ylab = "Median logit U5MR", type = 'l')
  polygon(c(beg.year:end.year,
            rev(beg.year:end.year)),
          c(res.natl.yearly.noiid.m1$results$logit.upper[1:30],
            rev(res.natl.yearly.noiid.m1$results$logit.lower[1:30])),
          border = FALSE,
          col = alpha('navy', 0.25))
  points(data.natl.yearly$years,
         data.natl.yearly$logit.est, col = 'red', pch = 16)
  legend('bottomleft',
         pch = c(16, NA),
         lwd = c(NA, 2),
         col = c('red', 'navy'),
         legend = c('H-T meta-analyis',
                    'Smoothed w/ no IID'),
         bty = 'n', cex = 0.8)
}
dev.off()


#### Admin1 Model ####

fit.admin1.iid.m1 <- smoothDirect(data.admin1,
                                  Amat = admin0.5.mat,
                                  year_label = beg.year:end.year,
                                  year_range = c(1990, end.year), 
                                  is.yearly = F, m=1,
                                  type.st = 4, st.time.model = 'rw2',
                                  include_time_unstruct = TRUE)

res.admin1.iid.m1 <- getSmoothed(fit.admin1.iid.m1,
                                 Amat = admin0.5.mat,
                                 year_range = c(1990, end.year),
                                 year_label = beg.year:end.year,
                                 CI = 0.90, save.draws = TRUE)
fit.admin1.noiid.m1 <- smoothDirect(data.admin1,
                                    Amat = admin0.5.mat,
                                    year_label = beg.year:end.year,
                                    year_range = c(1990, end.year), 
                                    is.yearly = F, m=1,
                                    type.st = 4, st.time.model = 'rw2',
                                    include_time_unstruct = FALSE)

res.admin1.noiid.m1 <- getSmoothed(fit.admin1.noiid.m1,
                                   Amat = admin0.5.mat,
                                   year_range = c(1990, end.year),
                                   year_label = beg.year:end.year,
                                   CI = 0.90, save.draws = TRUE)

res.admin1.noiid.m1$results$region.gadm <- 
  res.admin1.iid.m1$results$region.gadm <- admin0.5.names$GADM[match(res.admin1.noiid.m1$results$region,
                                                                     admin0.5.names$Internal)]
head(res.admin1.noiid.m1$results)
tail(res.admin1.noiid.m1$results)
head(res.admin1.iid.m1$results)
tail(res.admin1.iid.m1$results)


file.out <- paste0(country, "_res_admin0.5_iid_m1_SmoothedDirect_CI90.rda")
save(res.admin1.iid.m1, file = paste0(folder.name, '/', file.out))
write.csv(res.admin1.iid.m1$results,
          file = paste0(folder.name, '/', 
                        paste0(country, "_res_admin0.5_iid_m1_SmoothedDirect_CI90.csv")),
          row.names = FALSE)

file.out <- paste0(country, "_res_admin0.5_noiid_m1_SmoothedDirect_CI90.rda")
save(res.admin1.noiid.m1, file = paste0(folder.name, '/', file.out))
write.csv(res.admin1.noiid.m1$results,
          file = paste0(folder.name, '/', 
                        paste0(country, "_res_admin0.5_noiid_m1_SmoothedDirect_CI90.csv")),
          row.names = FALSE)

#### Benchmarking ####

if(doBenchmark){
  
  # benchmark <- igme.period <- 
  #   data.frame(period = periods,
  #              median = NA)
  # names(benchmark) <- c("years", "ratio")
  # 
  # for (i in 1:length(periods)) {
  #   igme.period$median[i] <- median(igme.ests$OBS_VALUE[which(igme.ests$year %in% 
  #                                                               (beg.year + c(((i-1)*5):(5*i - 1))))]/1000)
  # }
  # 
  # 
  # benchmark$ratio <- data.natl$mean/igme.period$median
  # 
  # data.natl <- getAdjusted(data.natl,
  #                          ratio = benchmark,
  #                          logit.lower = NULL,
  #                          logit.upper = NULL,
  #                          prob.upper = "upper",
  #                          prob.lower = "lower")
  # fit.natl <- fitINLA(data.natl, geo = NULL, Amat = NULL,
  #                     year_label = c(periods, proj.per),
  #                     year_range = c(beg.year, end.year + 5), is.yearly = F)
  # res.natl <- getSmoothed(fit.natl, 
  #                         year_label = c(periods, proj.per),
  #                         year_range = c(beg.year, end.year + 5))
  # res.natl$years.num <- seq(beg.year+2, end.year+5, 5)
  # res.natl$region.gadm <- country
  # head(res.natl)
  # tail(res.natl)
  # 
  # 
  # file.out <- paste0(country, "_res_natlBench_SmoothedDirect.rda")
  # 
  # save(res.natl, file = paste0(folder.name, '/', file.out))
  
  #### Calculate Benchmarks ####
  benchmark <- data.frame(years = beg.year:end.year,
                          ratio.direct = NA,
                          ratio.iid = NA,
                          ratio.noiid = NA)
  benchmark$ratio.direct <- (data.natl.yearly$mean[match(beg.year:end.year, res.natl.yearly.iid.m1$results$years.num)])/
    (igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)]/1000)
  benchmark$ratio.iid <- (res.natl.yearly.iid.m1$results$median[match(beg.year:end.year, res.natl.yearly.iid.m1$results$years.num)])/
    (igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)]/1000)
  benchmark$ratio.noiid <- (res.natl.yearly.noiid.m1$results$median[match(beg.year:end.year, res.natl.yearly.iid.m1$results$years.num)])/
    (igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)]/1000)
  
  pdf('Malawi0.5_unbenched.pdf')
  {
    par(mfrow = c(1,1),
        lend = 1)
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         xlab = "Year",
         ylab = "U5MR",
         col = 'black', lwd = 2)
    lines(res.natl.yearly.iid.m1$results$years.num[match(beg.year:end.year,
                                                         res.natl.yearly.iid.m1$results$years.num)], 
          res.natl.yearly.iid.m1$results$median[match(beg.year:end.year,
                                                      res.natl.yearly.iid.m1$results$years.num)]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.iid.m1$results$upper*1000,
              rev(res.natl.yearly.iid.m1$results$lower*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.natl.yearly.noiid.m1$results$years.num[match(beg.year:end.year,
                                                           res.natl.yearly.noiid.m1$results$years.num)], 
          res.natl.yearly.noiid.m1$results$median[match(beg.year:end.year,
                                                        res.natl.yearly.noiid.m1$results$years.num)]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.noiid.m1$results$upper*1000,
              rev(res.natl.yearly.noiid.m1$results$lower*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.natl.yearly$years[match(beg.year:end.year, 
                                        data.natl.yearly$years)],
           data.natl.yearly$mean*1000,
           col = 'black', pch = 16)
    legend('topright',
           pch = c(16, rep(NA,3)),
           lty = c(NA, 1, 1, 2),
           col = c('black', 'red', 'blue', 'black'),
           legend = c('H-T estimates', 'Smoothed w/ IID',
                      'Smoothed no IID', 'IGME'))
  }
  dev.off()
  
  pdf('Malawi0.5_Benchmarks.pdf')
  {
    par(mfrow = c(1,1),
        lend = 1)
    plot(NA,
         xlim = c(beg.year, end.year),
         ylim = c(min(benchmark[,-1], na.rm = TRUE),
                  max(benchmark[,-1], na.rm = TRUE)),
         type = 'n',
         xlab = "Year",
         ylab = "U5MR_t/IGME_t",
         col = 'black', lwd = 2)
    abline(h=1)
    lines(beg.year:end.year,
          benchmark$ratio.direct,
          lwd = 2)
    lines(beg.year:end.year,
          benchmark$ratio.iid,
          lwd = 2, col = 'red')
    lines(beg.year:end.year,
          benchmark$ratio.noiid,
          lwd = 2, col = 'navy')
    legend('topleft', lwd = 2,
           col = c('black', 'red', 'navy'),
           legend = c('Direct',
                      "Smoothed w/ IID",
                      "Smoothed no IID"),
           bty = 'n')
  }
  dev.off()
  
  save(benchmark, file = paste0(country, '/',
                                country, '_admin0.5_Benchmarks.rda'))
  
  #### National Yearly Benchmarking ####
  
  data.natl.yearly.bench <- getAdjusted(data.natl.yearly,
                                        ratio = benchmark,
                                        adj = 'ratio.direct',
                                        logit.lower = NULL,
                                        logit.upper = NULL,
                                        prob.upper = "upper",
                                        prob.lower = "lower")
  fit.natl.yearly.iid.bench <- smoothDirect(data.natl.yearly.bench,
                                            Amat = NULL,
                                            year_label = as.character(beg.year:(end.year)),
                                            year_range = c(beg.year, (end.year)),
                                            is.yearly = F, m = 1,
                                            include_time_unstruct = TRUE)
  res.natl.yearly.iid.bench<- getSmoothed(fit.natl.yearly.iid.bench, 
                                          year_label = as.character(beg.year:(end.year)),
                                          year_range = c(beg.year, (end.year)),
                                          save.draws = FALSE)
  
  fit.natl.yearly.noiid.bench <- smoothDirect(data.natl.yearly.bench,
                                              Amat = NULL,
                                              year_label = as.character(beg.year:(end.year)),
                                              year_range = c(beg.year, (end.year)),
                                              is.yearly = F, m = 1,
                                              include_time_unstruct = FALSE)
  res.natl.yearly.noiid.bench<- getSmoothed(fit.natl.yearly.noiid.bench, 
                                            year_label = as.character(beg.year:(end.year)),
                                            year_range = c(beg.year, (end.year)),
                                            save.draws = FALSE)
  
  file.out <- paste0(country, "_res_natlBench_iid_yearly_SmoothedDirect.rda")
  save(res.natl.yearly.iid.bench, file = paste0(folder.name, '/', file.out))
  
  file.out <- paste0(country, "_res_natlBench_noiid_yearly_SmoothedDirect.rda")
  save(res.natl.yearly.noiid.bench, file = paste0(folder.name, '/', file.out))
  
  pdf("Malawi_benched_natl_yearly.pdf",
      height = 4, width = 8)
  {
    par(mfrow = c(1,2),
        lend = 1)
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         xlab = "Year",
         ylab = "U5MR", main = "Unbenchmarked",
         col = 'black', lwd = 2)
    lines(res.natl.yearly.iid.m1$results$years.num[match(beg.year:end.year,
                                                         res.natl.yearly.iid.m1$results$years.num)], 
          res.natl.yearly.iid.m1$results$median[match(beg.year:end.year,
                                                      res.natl.yearly.iid.m1$results$years.num)]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.iid.m1$results$upper*1000,
              rev(res.natl.yearly.iid.m1$results$lower*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.natl.yearly.noiid.m1$results$years.num[match(beg.year:end.year,
                                                           res.natl.yearly.noiid.m1$results$years.num)], 
          res.natl.yearly.noiid.m1$results$median[match(beg.year:end.year,
                                                        res.natl.yearly.noiid.m1$results$years.num)]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.noiid.m1$results$upper*1000,
              rev(res.natl.yearly.noiid.m1$results$lower*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.natl.yearly$years[match(beg.year:end.year, 
                                        data.natl.yearly$years)],
           data.natl.yearly$mean*1000,
           col = 'black', pch = 16)
    legend('topright',
           pch = c(16, rep(NA,3)),
           lty = c(NA, 1, 1, 2), bty ='n',cex=0.8,
           col = c('black', 'red', 'blue', 'black'),
           legend = c('H-T estimates', 'Smoothed w/ IID',
                      'Smoothed no IID', 'IGME'))
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         xlab = "Year",
         ylab = "U5MR", main = "Benchmarked: Direct",
         col = 'black', lwd = 2)
    lines(res.natl.yearly.iid.bench$years.num[match(beg.year:end.year,
                                                    res.natl.yearly.iid.bench$years.num)], 
          res.natl.yearly.iid.bench$median[match(beg.year:end.year,
                                                 res.natl.yearly.iid.bench$years.num)]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.iid.bench$upper*1000,
              rev(res.natl.yearly.iid.bench$lower*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.natl.yearly.noiid.bench$years.num[match(beg.year:end.year,
                                                      res.natl.yearly.noiid.bench$years.num)], 
          res.natl.yearly.noiid.bench$median[match(beg.year:end.year,
                                                   res.natl.yearly.noiid.bench$years.num)]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.noiid.bench$upper*1000,
              rev(res.natl.yearly.noiid.bench$lower*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.natl.yearly.bench$years[match(beg.year:end.year, 
                                              data.natl.yearly.bench$years)],
           data.natl.yearly.bench$mean*1000,
           col = 'black', pch = 16)
    legend('topright', bty = 'n', cex = 0.8,
           pch = c(16, rep(NA,3)),
           lty = c(NA, 1, 1, 2),
           col = c('black', 'red', 'blue', 'black'),
           legend = c('H-T estimates', 'Smoothed w/ IID',
                      'Smoothed no IID', 'IGME'))
    
  }
  dev.off()
  
  res.natl.yearly.iid.posthoc.bench <- res.natl.yearly.iid.m1$results
  res.natl.yearly.noiid.posthoc.bench <- res.natl.yearly.noiid.m1$results
  index <- 0
  for(yr in beg.year:end.year){
    
    bench.iid <- benchmark$ratio.iid[benchmark$years == yr]
    bench.noiid <- benchmark$ratio.noiid[benchmark$years == yr]
    
    index <- index + 1 
    res.index <- which(res.natl.yearly.iid.m1$result$years.num == yr)
    
    tmp.iid <- res.natl.yearly.iid.m1$draws.est[index,]/bench.iid
    tmp.noiid <- res.natl.yearly.noiid.m1$draws.est[index,]/bench.noiid
    
    res.natl.yearly.noiid.posthoc.bench[res.index,
                                        c("lower", "median", "upper")] <- 
      quantile(tmp.noiid, 
               c(0.05, 0.5, 0.95))
    res.natl.yearly.noiid.posthoc.bench[res.index,
                                        paste0("logit.",
                                               c("lower", "median", "upper"))] <- 
      quantile(logit(tmp.noiid), 
               c(0.05, 0.5, 0.95))
    res.natl.yearly.iid.posthoc.bench[res.index,
                                      c("lower", "median", "upper")] <- 
      quantile(tmp.iid, 
               c(0.05, 0.5, 0.95))
    res.natl.yearly.iid.posthoc.bench[res.index,
                                      paste0("logit.",
                                             c("lower", "median", "upper"))] <- 
      quantile(logit(tmp.iid), 
               c(0.05, 0.5, 0.95))
  }
  
  file.out <- paste0(country, "_res_natlBench_posthoc_iid_yearly_SmoothedDirect.rda")
  save(res.natl.yearly.iid.posthoc.bench, file = paste0(folder.name, '/', file.out))
  
  file.out <- paste0(country, "_res_natlBench_posthoc_noiid_yearly_SmoothedDirect.rda")
  save(res.natl.yearly.noiid.posthoc.bench, file = paste0(folder.name, '/', file.out))
  
  
  pdf("Malawi_benched_posthoc_natl_yearly.pdf",
      height = 4, width = 8)
  {
    par(mfrow = c(1,2),
        lend = 1)
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         xlab = "Year",
         ylab = "U5MR", main = "Unbenchmarked",
         col = 'black', lwd = 2)
    lines(res.natl.yearly.iid.m1$results$years.num[match(beg.year:end.year,
                                                         res.natl.yearly.iid.m1$results$years.num)], 
          res.natl.yearly.iid.m1$results$median[match(beg.year:end.year,
                                                      res.natl.yearly.iid.m1$results$years.num)]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.iid.m1$results$upper*1000,
              rev(res.natl.yearly.iid.m1$results$lower*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.natl.yearly.noiid.m1$results$years.num[match(beg.year:end.year,
                                                           res.natl.yearly.noiid.m1$results$years.num)], 
          res.natl.yearly.noiid.m1$results$median[match(beg.year:end.year,
                                                        res.natl.yearly.noiid.m1$results$years.num)]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.noiid.m1$results$upper*1000,
              rev(res.natl.yearly.noiid.m1$results$lower*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.natl.yearly$years[match(beg.year:end.year, 
                                        data.natl.yearly$years)],
           data.natl.yearly$mean*1000,
           col = 'black', pch = 16)
    legend('topright',
           pch = c(16, rep(NA,3)),
           lty = c(NA, 1, 1, 2), bty ='n',cex=0.8,
           col = c('black', 'red', 'blue', 'black'),
           legend = c('H-T estimates', 'Smoothed w/ IID',
                      'Smoothed no IID', 'IGME'))
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         xlab = "Year",
         ylab = "U5MR", main = "Benchmarked: Post-hoc",
         col = 'black', lwd = 2)
    lines(res.natl.yearly.iid.posthoc.bench$years.num[match(beg.year:end.year,
                                                            res.natl.yearly.iid.posthoc.bench$years.num)], 
          res.natl.yearly.iid.posthoc.bench$median[match(beg.year:end.year,
                                                         res.natl.yearly.iid.posthoc.bench$years.num)]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.iid.posthoc.bench$upper*1000,
              rev(res.natl.yearly.iid.posthoc.bench$lower*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.natl.yearly.noiid.posthoc.bench$years.num[match(beg.year:end.year,
                                                              res.natl.yearly.noiid.posthoc.bench$years.num)], 
          res.natl.yearly.noiid.posthoc.bench$median[match(beg.year:end.year,
                                                           res.natl.yearly.noiid.posthoc.bench$years.num)]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.noiid.posthoc.bench$upper*1000,
              rev(res.natl.yearly.noiid.posthoc.bench$lower*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.natl.yearly$years[match(beg.year:end.year, 
                                        data.natl.yearly$years)],
           data.natl.yearly$mean*1000,
           col = 'black', pch = 16)
    legend('topright', bty = 'n', cex = 0.8,
           pch = c(16, rep(NA,3)),
           lty = c(NA, 1, 1, 2),
           lwd = c(NA, 2, 2, 2),
           col = c('black', 'red', 'blue', 'black'),
           legend = c('H-T estimates', 'Smoothed w/ IID',
                      'Smoothed no IID', 'IGME'))
    
  }
  dev.off()
  
  
  pdf("Malawi_benchedAll_natl_yearly.pdf",
      height = 8, width = 8)
  {
    par(mfrow = c(2,2),
        lend = 1)
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         xlab = "Year",
         ylab = "U5MR", main = "Unbenchmarked",
         col = 'black', lwd = 2)
    lines(res.natl.yearly.iid.m1$results$years.num[match(beg.year:end.year,
                                                         res.natl.yearly.iid.m1$results$years.num)], 
          res.natl.yearly.iid.m1$results$median[match(beg.year:end.year,
                                                      res.natl.yearly.iid.m1$results$years.num)]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.iid.m1$results$upper*1000,
              rev(res.natl.yearly.iid.m1$results$lower*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.natl.yearly.noiid.m1$results$years.num[match(beg.year:end.year,
                                                           res.natl.yearly.noiid.m1$results$years.num)], 
          res.natl.yearly.noiid.m1$results$median[match(beg.year:end.year,
                                                        res.natl.yearly.noiid.m1$results$years.num)]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.noiid.m1$results$upper*1000,
              rev(res.natl.yearly.noiid.m1$results$lower*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.natl.yearly$years[match(beg.year:end.year, 
                                        data.natl.yearly$years)],
           data.natl.yearly$mean*1000,
           col = 'black', pch = 16)
    
    
    plot(NA, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
    legend(x = "center", bty = 'n', 
           pch = c(16, rep(NA,3)),
           lty = c(NA, 1, 1, 2),
           lwd = c(NA, 2, 2, 2),
           col = c('black', 'red', 'blue', 'black'),
           legend = c('H-T estimates', 'Smoothed w/ IID',
                      'Smoothed no IID', 'IGME'))
    
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         xlab = "Year",
         ylab = "U5MR", main = "Benchmarked: Direct",
         col = 'black', lwd = 2)
    lines(res.natl.yearly.iid.bench$years.num[match(beg.year:end.year,
                                                    res.natl.yearly.iid.bench$years.num)], 
          res.natl.yearly.iid.bench$median[match(beg.year:end.year,
                                                 res.natl.yearly.iid.bench$years.num)]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.iid.bench$upper*1000,
              rev(res.natl.yearly.iid.bench$lower*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.natl.yearly.noiid.bench$years.num[match(beg.year:end.year,
                                                      res.natl.yearly.noiid.bench$years.num)], 
          res.natl.yearly.noiid.bench$median[match(beg.year:end.year,
                                                   res.natl.yearly.noiid.bench$years.num)]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.noiid.bench$upper*1000,
              rev(res.natl.yearly.noiid.bench$lower*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.natl.yearly.bench$years[match(beg.year:end.year, 
                                              data.natl.yearly.bench$years)],
           data.natl.yearly.bench$mean*1000,
           col = 'black', pch = 16)
    
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         xlab = "Year",
         ylab = "U5MR", main = "Benchmarked: Post-hoc",
         col = 'black', lwd = 2)
    lines(res.natl.yearly.iid.posthoc.bench$years.num[match(beg.year:end.year,
                                                            res.natl.yearly.iid.posthoc.bench$years.num)], 
          res.natl.yearly.iid.posthoc.bench$median[match(beg.year:end.year,
                                                         res.natl.yearly.iid.posthoc.bench$years.num)]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.iid.posthoc.bench$upper*1000,
              rev(res.natl.yearly.iid.posthoc.bench$lower*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.natl.yearly.noiid.posthoc.bench$years.num[match(beg.year:end.year,
                                                              res.natl.yearly.noiid.posthoc.bench$years.num)], 
          res.natl.yearly.noiid.posthoc.bench$median[match(beg.year:end.year,
                                                           res.natl.yearly.noiid.posthoc.bench$years.num)]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.natl.yearly.noiid.posthoc.bench$upper*1000,
              rev(res.natl.yearly.noiid.posthoc.bench$lower*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.natl.yearly$years[match(beg.year:end.year, 
                                        data.natl.yearly$years)],
           data.natl.yearly$mean*1000,
           col = 'black', pch = 16)
    
  }
  dev.off()
  
  pdf("Malawi_bench_natl_yearly_DirectvsPosthoc.pdf",
      height =4, width = 8)
  {
    par(mfrow = c(1,2),
        lend = 1)
    plot(res.natl.yearly.iid.bench$median,
         res.natl.yearly.iid.posthoc.bench$median,
         type = 'n',
         xlab = "Benchmark: Direct",
         ylab = "Benchmark: Post-hoc",
         main = "Smoothed w/ IID",
         col = 'black', lwd = 2)
    abline(0,1, lty = 2)
    points(res.natl.yearly.iid.bench$median,
           res.natl.yearly.iid.posthoc.bench$median,
           pch = 16,
           col = rainbow(end.year-beg.year+1))
    legend('topleft', bty = 'n',
           pch = 16,
           col = rainbow(end.year-beg.year+1)[seq(1,26,5)],
           legend = (beg.year:end.year)[seq(1,26,5)])
    
    plot(res.natl.yearly.noiid.bench$median,
         res.natl.yearly.noiid.posthoc.bench$median,
         type = 'n',
         xlab = "Benchmark: Direct",
         ylab = "Benchmark: Post-hoc",
         main = "Smoothed no IID",
         col = 'black', lwd = 2)
    abline(0,1, lty = 2)
    points(res.natl.yearly.noiid.bench$median,
           res.natl.yearly.noiid.posthoc.bench$median,
           pch = 16,
           col = rainbow(end.year-beg.year+1))
    legend('topleft', bty = 'n',
           pch = 16,
           col = rainbow(end.year-beg.year+1)[seq(1,26,5)],
           legend = (beg.year:end.year)[seq(1,26,5)])
  }
  dev.off()
  
  #### Admin 1 Benchmarking ####
  data.admin1.bench <- getAdjusted(data.admin1,
                                   ratio = benchmark,
                                   adj = 'ratio.direct',
                                   logit.lower = NULL,
                                   logit.upper = NULL,
                                   prob.upper = "upper",
                                   prob.lower = "lower")
  
  fit.admin1.iid.bench <- smoothDirect(data.admin1.bench, 
                                       Amat = admin0.5.mat,
                                       year_label = beg.year:end.year,
                                       year_range = c(beg.year, end.year), 
                                       is.yearly = F, m=1,
                                       type.st = 4, st.time.model = 'rw2',
                                       include_time_unstruct = TRUE)
  res.admin1.iid.bench <- getSmoothed(fit.admin1.iid.bench, 
                                      Amat = admin0.5.mat, 
                                      year_label = beg.year:end.year,
                                      year_range = c(beg.year, end.year),
                                      CI = 0.9,
                                      save.draws = FALSE)
  fit.admin1.noiid.bench <- smoothDirect(data.admin1.bench, 
                                         Amat = admin0.5.mat,
                                         year_label = beg.year:end.year,
                                         year_range = c(beg.year, end.year), 
                                         is.yearly = F, m=1,
                                         type.st = 4, st.time.model = 'rw2',
                                         include_time_unstruct = FALSE)
  res.admin1.noiid.bench <- getSmoothed(fit.admin1.noiid.bench, 
                                        Amat = admin0.5.mat, 
                                        year_label = beg.year:end.year,
                                        year_range = c(beg.year, end.year),
                                        CI = 0.9,
                                        save.draws = FALSE)
  #res.admin1$years.num <- seq(beg.year+2, end.year+5, 5)[match(res.admin1$years, c(periods, proj.per))]
  res.admin1.iid.bench$region.gadm <-
    res.admin1.noiid.bench$region.gadm <- admin0.5.names$GADM[match(res.admin1$region, 
                                                                    admin0.5.names$Internal)]
  head(res.admin1.iid.bench)
  tail(res.admin1.iid.bench)
  head(res.admin1.noiid.bench)
  tail(res.admin1.noiid.bench)
  
  file.out <- paste0(country, "_res_admin0.5Bench_iid_SmoothedDirect_CI90.rda")
  
  save(res.admin1.iid.bench, file = paste0(folder.name, '/', file.out))
  write.csv(res.admin1.iid.bench,
            file = paste0(folder.name, '/',
                          country,
                          "_res_admin0.5Bench_iid_SmoothedDirect_CI90.csv"),
            row.names = FALSE)
  file.out <- paste0(country, "_res_admin0.5Bench_noiid_SmoothedDirect_CI90.rda")
  
  save(res.admin1.noiid.bench, file = paste0(folder.name, '/', file.out))
  write.csv(res.admin1.noiid.bench,
            file = paste0(folder.name, '/',
                          country,
                          "_res_admin0.5Bench_noiid_SmoothedDirect_CI90.csv"),
            row.names = FALSE)
  
  res.admin1.iid.posthoc.bench <- res.admin1.iid.m1$results
  res.admin1.noiid.posthoc.bench <- res.admin1.noiid.m1$results
  lincombs.info <- fit.admin1.iid.m1$lincombs.info
  for(yr in beg.year:end.year){
    
    bench.iid <- benchmark$ratio.iid[benchmark$years == yr]
    bench.noiid <- benchmark$ratio.noiid[benchmark$years == yr]
    
    for(area in rownames(admin0.5.mat)){
      index <- lincombs.info$Index[lincombs.info$District == match(area, 
                                                                   rownames(admin0.5.mat)) &
                                     lincombs.info$Year == match(yr, beg.year:end.year)]
      res.index <- which(res.admin1.iid.m1$results$region == area &
                           res.admin1.iid.m1$result$years == yr)
      
      tmp.iid <- res.admin1.iid.m1$draws.est[index,]/bench.iid
      tmp.noiid <- res.admin1.noiid.m1$draws.est[index,]/bench.noiid
      
      res.admin1.noiid.posthoc.bench[res.index,
                                     c("lower", "median", "upper")] <- 
        quantile(tmp.noiid, 
                 c(0.05, 0.5, 0.95))
      res.admin1.noiid.posthoc.bench[res.index,
                                     paste0("logit.",
                                            c("lower", "median", "upper"))] <- 
        quantile(logit(tmp.noiid), 
                 c(0.05, 0.5, 0.95))
      res.admin1.iid.posthoc.bench[res.index,
                                   c("lower", "median", "upper")] <- 
        quantile(tmp.iid, 
                 c(0.05, 0.5, 0.95))
      res.admin1.iid.posthoc.bench[res.index,
                                   paste0("logit.",
                                          c("lower", "median", "upper"))] <- 
        quantile(logit(tmp.iid), 
                 c(0.05, 0.5, 0.95))
    }
  }
  
  file.out <- paste0(country, "_res_admin0.5Bench_iid_posthoc_SmoothedDirect_CI90.rda")
  
  save(res.admin1.iid.posthoc.bench, file = paste0(folder.name, '/', file.out))
  write.csv(res.admin1.iid.posthoc.bench,
            file = paste0(folder.name, '/',
                          country,
                          "_res_admin0.5Bench_iid_posthoc_SmoothedDirect_CI90.csv"),
            row.names = FALSE)
  file.out <- paste0(country, "_res_admin0.5Bench_noiid_posthoc_SmoothedDirect_CI90.rda")
  
  save(res.admin1.noiid.posthoc.bench, file = paste0(folder.name, '/', file.out))
  write.csv(res.admin1.noiid.posthoc.bench,
            file = paste0(folder.name, '/',
                          country,
                          "_res_admin0.5Bench_noiid_posthoc_SmoothedDirect_CI90.csv"),
            row.names = FALSE)
  
pdf("Malawi_benchedAll_admin0.5.pdf",
    height = 8, width = 8) 
  par(mfrow = c(2,2),
      lend = 1)
  for(area in rownames(admin0.5.mat)){
    res.index <- which(res.admin1.iid.posthoc.bench$region == area)
    title.area <- unique(as.character(res.admin1.iid.posthoc.bench$region.gadm[res.index]))
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         ylim = c(0,250),
         type = 'n',
         xlab = "Year",
         ylab = "U5MR", 
         main = paste0(title.area,
                       ": Unbenchmarked"),
         col = 'black', lwd = 2)
    lines(res.admin1.iid.m1$results$years.num[res.index], 
          res.admin1.iid.m1$results$median[res.index]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.admin1.iid.m1$results$upper[res.index]*1000,
              rev(res.admin1.iid.m1$results$lower[res.index]*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.admin1.noiid.m1$results$years.num[res.index], 
          res.admin1.noiid.m1$results$median[res.index]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.admin1.noiid.m1$results$upper[res.index]*1000,
              rev(res.admin1.noiid.m1$results$lower[res.index]*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.admin1$years[data.admin1$region == area],
           data.admin1$mean[data.admin1$region == area]*1000,
           col = 'black', pch = 16)
    plot(NA, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
    legend(x = "center", bty = 'n', 
           pch = c(16, rep(NA,3)),
           lty = c(NA, 1, 1, 2),
           lwd = c(NA, 2, 2, 2),
           col = c('black', 'red', 'navy', 'black'),
           legend = c('H-T estimates', 'Smoothed w/ IID',
                      'Smoothed no IID', 'IGME'))
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         ylim = c(0, 250),
         xlab = "Year",
         ylab = "U5MR", main = "Benchmarked: Direct",
         col = 'black', lwd = 2)
    lines(res.admin1.iid.bench$years.num[res.index], 
          res.admin1.iid.bench$median[res.index]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.admin1.iid.bench$upper[res.index]*1000,
              rev(res.admin1.iid.bench$lower[res.index]*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.admin1.noiid.bench$years.num[res.index], 
          res.admin1.noiid.bench$median[res.index]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.admin1.noiid.bench$upper[res.index]*1000,
              rev(res.admin1.noiid.bench$lower[res.index]*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    
    points(data.admin1.bench$years[data.admin1.bench$region == area],
           data.admin1.bench$mean[data.admin1.bench$region == area]*1000,
           col = 'black', pch = 16)
   
    
    plot(beg.year:end.year,
         igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
         type = 'n',
         ylim = c(0, 250),
         xlab = "Year",
         ylab = "U5MR", main = "Benchmarked: Post-Hoc",
         col = 'black', lwd = 2)
    lines(res.admin1.iid.posthoc.bench$years.num[res.admin1.iid.posthoc.bench$region == area], 
          res.admin1.iid.posthoc.bench$median[res.admin1.iid.posthoc.bench$region == area]*1000,
          col = 'red', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.admin1.iid.posthoc.bench$upper[res.admin1.iid.posthoc.bench$region == area]*1000,
              rev(res.admin1.iid.posthoc.bench$lower[res.admin1.iid.posthoc.bench$region == area]*1000)),
            col = alpha('red', 0.25),
            border = FALSE)
    lines(res.admin1.noiid.posthoc.bench$years.num[res.admin1.noiid.posthoc.bench$region == area], 
          res.admin1.noiid.posthoc.bench$median[res.admin1.noiid.posthoc.bench$region == area]*1000,
          col = 'navy', lwd = 2)
    polygon(c(beg.year:end.year,
              end.year:beg.year),
            c(res.admin1.noiid.posthoc.bench$upper[res.admin1.noiid.posthoc.bench$region == area]*1000,
              rev(res.admin1.noiid.posthoc.bench$lower[res.admin1.noiid.posthoc.bench$region == area]*1000)),
            col = alpha('navy', 0.25),
            border = FALSE)
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
          lwd = 2, lty = 2, col = 'black')
    
    points(data.admin1$years[data.admin1.bench$region == area],
           data.admin1$mean[data.admin1.bench$region == area]*1000,
           col = 'black', pch = 16)

  }
dev.off()

pdf("Malawi_benchedAll_admin0.5_2014-2019.pdf",
    height = 8, width = 8) 
par(mfrow = c(2,2),
    lend = 1)
for(area in rownames(admin0.5.mat)){
  res.index <- which(res.admin1.iid.posthoc.bench$region == area &
                       res.admin1.iid.posthoc.bench$years %in% 2014:2019)
  title.area <- unique(as.character(res.admin1.iid.posthoc.bench$region.gadm[res.index]))
  offsets <- c(-.15, -.05, .05, .15)
  plot(2014:end.year,
       igme.ests$OBS_VALUE[match(2014:end.year, igme.ests$year)],
       ylim = c(20,80),
       xlim = c(2013.5, 2019.5),
       type = 'n',
       xlab = "Year",
       ylab = "U5MR", 
       main = title.area,
       col = 'black', lwd = 2)
  lines(data.admin1$years[data.admin1$region == area][25:30],
         data.admin1$mean[data.admin1$region == area][25:30]*1000,
         col = 'grey30', lwd = 2)
  lines(data.admin1.bench$years[data.admin1$region == area][25:30],
         data.admin1.bench$mean[data.admin1$region == area][25:30]*1000,
         col = 'grey75', lwd = 2)
  lines(2014:end.year,
        igme.ests$OBS_VALUE[match(2014:end.year, igme.ests$year)],
        lwd = 2, col = 'black')

  points(res.admin1.iid.bench$years.num[res.index] + offsets[1], 
        res.admin1.iid.bench$median[res.index]*1000,
        col = 'firebrick', pch = 15)
  
  points(res.admin1.noiid.bench$years.num[res.index] + offsets[2], 
        res.admin1.noiid.bench$median[res.index]*1000,
        pch = 15, col = 'navy')
  points(res.admin1.iid.posthoc.bench$years.num[res.index] + offsets[3], 
        res.admin1.iid.posthoc.bench$median[res.index]*1000,
        pch = 15, col = 'red')
  points(res.admin1.noiid.posthoc.bench$years.num[res.index] + offsets[4], 
        res.admin1.noiid.posthoc.bench$median[res.index]*1000,
        pch = 15, col = 'blue')
 }
plot(NA, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
legend(x = "center", bty = 'n', 
       pch = c(rep(NA, 3),
               rep(15, 4)),
       lty = c(rep(1, 3),
               rep(NA, 4)),
       lwd = c(rep(2, 3),
               rep(NA,4)),
       col = c('black', 'grey30', 'grey75',
               'firebrick', 'navy',
               'red', 'blue'),
       legend = c('IGME', 'H-T', 'H-T Bench',
                  'Direct: IID',
                  'Direct: no IID',
                  'Post-hoc: IID',
                  'Post-hoc: no IID'))
dev.off()


par(mfrow = c(1,1),
    lend = 1)
for(area in rownames(admin0.5.mat)){
  res.index <- which(res.admin1.noiid.posthoc.bench$region == area)
  title.area <- unique(as.character(res.admin1.noiid.posthoc.bench$region.gadm[res.index]))
  svy.cols <- rainbow(length(surveys))
  pdf(paste0("Malawi_benched_posthoc_noIID_admin0.5_",
             title.area, ".pdf"),
      height = 6, width = 6) 
  plot(beg.year:end.year,
       igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
       ylim = c(0,250),
       type = 'n',
       xlab = "Year",
       ylab = "U5MR", 
       main = title.area,
       col = 'black', lwd = 2)

  lines(beg.year:end.year,
        igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
        lwd = 2, lty = 2, col = 'black')
  
  for(svy in surveys){
    points(direct.admin1$years[direct.admin1$region == area &
                                 direct.admin1$surveyYears == svy],
           direct.admin1$mean[direct.admin1$region == area &
                              direct.admin1$surveyYears == svy]*1000,
           col = svy.cols[match(svy,
                                surveys)], pch = 16)
  }
  points(data.admin1$years[data.admin1.bench$region == area],
         data.admin1$mean[data.admin1.bench$region == area]*1000,
         col = 'black', pch = 4, lwd = 2)
  
  lines(res.admin1.noiid.posthoc.bench$years.num[res.admin1.noiid.posthoc.bench$region == area], 
        res.admin1.noiid.posthoc.bench$median[res.admin1.noiid.posthoc.bench$region == area]*1000,
        col = 'black', lwd = 2)
  polygon(c(beg.year:end.year,
            end.year:beg.year),
          c(res.admin1.noiid.posthoc.bench$upper[res.admin1.noiid.posthoc.bench$region == area]*1000,
            rev(res.admin1.noiid.posthoc.bench$lower[res.admin1.noiid.posthoc.bench$region == area]*1000)),
          col = alpha('black', 0.25),
          border = FALSE)
  
  legend('topright',
         pch = c(rep(16,4), 4,
                 NA, NA),
         lty = c(rep(NA, 5),
                 1, 2),
         lwd = c(rep(NA, 4), 2,
                 2, 2),
         col = c(svy.cols,
                 rep('black', 3)),
         legend = c(paste0("DHS ", surveys),
                    "Meta-analysis",
                    "Smoothed",
                    "IGME"),
         bty = 'n')

dev.off()
}


par(mfrow = c(1,1),
    lend = 1)
for(area in rownames(admin0.5.mat)){
  res.index <- which(res.admin1.noiid.posthoc.bench$region == area)
  title.area <- unique(as.character(res.admin1.noiid.posthoc.bench$region.gadm[res.index]))
  svy.cols <- rainbow(length(surveys))
  pdf(paste0("Malawi_benched_posthoc_noIID_admin0.5_",
             title.area, "_2014-2019_.pdf"),
      height = 6, width = 6) 
  plot(beg.year:end.year,
       igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
       xlim = c(2013,2019),
       ylim = c(0,250),
       type = 'n',
       xlab = "Year",
       ylab = "U5MR", 
       main = title.area,
       col = 'black', lwd = 2)
  
  lines(beg.year:end.year,
        igme.ests$OBS_VALUE[match(beg.year:end.year, igme.ests$year)],
        lwd = 2, lty = 2, col = 'black')
  
  for(svy in surveys){
    points(direct.admin1$years[direct.admin1$region == area &
                                 direct.admin1$surveyYears == svy],
           direct.admin1$mean[direct.admin1$region == area &
                                direct.admin1$surveyYears == svy]*1000,
           col = svy.cols[match(svy,
                                surveys)], pch = 16)
  }
  points(data.admin1$years[data.admin1.bench$region == area],
         data.admin1$mean[data.admin1.bench$region == area]*1000,
         col = 'black', pch = 4, lwd = 2)
  
  lines(res.admin1.iid.bench$years.num[res.admin1.iid.bench$region == area], 
        res.admin1.iid.bench$median[res.admin1.iid.bench$region == area]*1000,
        col = 'blue', lwd = 2)
  polygon(c(beg.year:end.year,
            end.year:beg.year),
          c(res.admin1.iid.bench$upper[res.admin1.iid.bench$region == area]*1000,
            rev(res.admin1.iid.bench$lower[res.admin1.iid.bench$region == area]*1000)),
          col = alpha('blue', 0.25),
          border = FALSE)
  
  
  lines(res.admin1.noiid.posthoc.bench$years.num[res.admin1.noiid.posthoc.bench$region == area], 
        res.admin1.noiid.posthoc.bench$median[res.admin1.noiid.posthoc.bench$region == area]*1000,
        col = 'black', lwd = 2)
  polygon(c(beg.year:end.year,
            end.year:beg.year),
          c(res.admin1.noiid.posthoc.bench$upper[res.admin1.noiid.posthoc.bench$region == area]*1000,
            rev(res.admin1.noiid.posthoc.bench$lower[res.admin1.noiid.posthoc.bench$region == area]*1000)),
          col = alpha('black', 0.25),
          border = FALSE)
  
  legend('topright',
         pch = c(rep(16,4), 4,
                 NA, NA, NA),
         lty = c(rep(NA, 5),
                 1, 1, 2),
         lwd = c(rep(NA, 4), 2,
                 2, 2, 2),
         col = c(svy.cols,
                 rep('black', 2),
                 'blue', 'black'),
         legend = c(paste0("DHS ", surveys),
                    "Meta-analysis",
                    "Smoothed",
                    "Smoothed- Previous",
                    "IGME"),
         bty = 'n')
  
  dev.off()
}

pdf("Malawi_bench_admin0.5_DirectvsPosthoc.pdf",
    height =4, width = 8)
{
  par(mfrow = c(1,2),
      lend = 1)
  for(area in rownames(admin0.5.mat)){
    res.index <- which(res.admin1.iid.posthoc.bench$region == area)
    title.area <- unique(as.character(res.admin1.iid.posthoc.bench$region.gadm[res.index]))
    plot(res.admin1.iid.bench$median[res.index],
         res.admin1.iid.posthoc.bench$median[res.index],
         type = 'n',
         xlab = "Benchmark: Direct",
         ylab = "Benchmark: Post-hoc",
         main = paste0(title.area,
                       ": Smoothed w/ IID"),
         col = 'black', lwd = 2)
    abline(0,1, lty = 2)
    points(res.admin1.iid.bench$median[res.index],
           res.admin1.iid.posthoc.bench$median[res.index],
           pch = 16,
           col = rainbow(end.year-beg.year+1))
    legend('topleft', bty = 'n',
           pch = 16,
           col = rainbow(end.year-beg.year+1)[seq(1,26,5)],
           legend = (beg.year:end.year)[seq(1,26,5)])
    
    plot(res.admin1.noiid.bench$median[res.index],
         res.admin1.noiid.posthoc.bench$median[res.index],
         type = 'n',
         xlab = "Benchmark: Direct",
         ylab = "Benchmark: Post-hoc",
         main = "Smoothed no IID",
         col = 'black', lwd = 2)
    abline(0,1, lty = 2)
    points(res.admin1.noiid.bench$median[res.index],
           res.admin1.noiid.posthoc.bench$median[res.index],
           pch = 16,
           col = rainbow(end.year-beg.year+1))
    legend('topleft', bty = 'n',
           pch = 16,
           col = rainbow(end.year-beg.year+1)[seq(1,26,5)],
           legend = (beg.year:end.year)[seq(1,26,5)])
  }
}
dev.off()



}

#### Spaghetti Plots ####

if(!dir.exists(paths = paste0(folder.name, '/Plots/', 'SmoothedDirect'))){
  dir.create(path = paste0(folder.name, '/Plots/', 'SmoothedDirect'))
}



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
    pane.years <- jitter(as.numeric(tmp$years))
    
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

plot.years <- beg.year:end.year
par(mfrow = c(1,1))
pdf(paste0(folder.name,"/Plots/SmoothedDirect/",
           country, '_admin0.5_SmoothedDirect_spaghetti.pdf'))

for(area in 1:dim(poly.adm1)[1]){
  tmp.area <- direct.admin1[direct.admin1$region ==
                              as.character(admin0.5.names$Internal[area]),]
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  
  ihme.ests[[2]]$ADM1_NAME[ihme.ests[[2]]$ADM1_NAME == "Northern Region"] <- "Northern"
  ihme.ests[[2]]$ADM1_NAME[ihme.ests[[2]]$ADM1_NAME == "Central Region"] <- "Central"
  ihme.ests[[2]]$ADM1_NAME[ihme.ests[[2]]$ADM1_NAME == "Southern Region"] <- "Southern"
  tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == 
                               as.character(admin0.5.names$GADM[area]),]
  
  res.area <- res.admin1[res.admin1$region == as.character(admin0.5.names$Internal[area]),]
  
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
         main = admin0.5.names$GADM[area])
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
               main = admin0.5.names$GADM[area])
          
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
               main = admin0.5.names$GADM[area])
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
par(mfrow = c(1,3))
for(area in 1:dim(poly.adm1)[1]){
  tmp.area <- direct.admin1[direct.admin1$region == as.character(admin0.5.names$Internal[area]),]
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  
  tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == 
                               as.character(admin0.5.names$GADM[area]),]
  
  
  res.area <- res.admin1[res.admin1$region == as.character(admin0.5.names$Internal[area]),]
  
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
         main = admin0.5.names$GADM[area])
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
               main = admin0.5.names$GADM[area])
          
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
               main = admin0.5.names$GADM[area])
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
for(year in beg.year:end.year){
  idx <- which(res.admin1$years == year) 
  plot(poly.adm1, border = F, col = med.col[idx],
       axes = F, main = year)
}

plot(NA, xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
legend(x = "center",inset = 0,
       legend = names(attr(med.col, 'table')),
       fill = med.palette, cex= .75, horiz = FALSE, bty = 'n')

dev.off()

