#  DirectEstimates.R
#  author: Jessica Godwin
#  
#  sources: LoadCommandCenter.R
#           IHMEHand_CountryName.R


rm(list = ls())
setwd('~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/')
devtools::install_github("bryandmartin/SUMMER",
                         build_vignettes = F, force = T)


## Libraries ####
library(SUMMER)
help(package = "SUMMER", help_type = "html")
#utils::browseVignettes(package = "SUMMER")
library(classInt)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(rgdal)
library(scales)
library(survey)

source('../../Analysis/R/LoadCommandCenter.R')


## Parameters ####

country <- "Comoros"
beg.year <- 1990
end.year <- 2019

#CountryList <- gs_read(sheet_key, ws = "CountryList")
CountryList <- sheets_read(sheet_key, sheet = "CountryList")
folder.name <- CountryList$folderName[CountryList$Country == country]
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
doHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
               unique(HIV.country$`UNAIDS data?`) == "Y")


## Load polygon files ####

poly.file <- "/shapeFiles_gadm"
poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                         '0', sep = "_")
poly.layer.adm1 <- paste('gadm36', gadm.abbrev,
                         '1', sep = "_")

poly.path <- paste0(folder.name, poly.file)
poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0))
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1))

proj4string(poly.adm0) <- proj4string(poly.adm1) <- proj4string(poly.adm2)
load(paste0(folder.name,'/shapeFiles_gadm/', country, '_Amat.rda'))
load(paste0(folder.name, '/shapeFiles_gadm/', country, '_Amat_Names.rda'))

## Load data ####

load(paste0(folder.name,'/',country,'_cluster_dat.rda'))
mod.dat$years <- as.numeric(as.character(mod.dat$years))
dat.years <- sort(unique(mod.dat$years))
beg.years <- seq(1990,2015,5)
end.years <- beg.years + 4
periods <- paste(beg.years, end.years, sep = "-")
mod.dat$period <- as.character(cut(mod.dat$years, breaks = c(beg.years, beg.years[length(beg.years)]+5),
                                   include.lowest = T, right = F, labels = periods))
{
## Load IGME ####
file.list <- list.files('../../Analysis/R')
igme.file <- file.list[grepl("IGME", file.list)]
igme.ests <- read.csv(paste0('../../Analysis/R/',igme.file),
                      header = T)
igme.ests <- igme.ests[igme.ests$INDICATOR == "Under-five mortality rate" &
                         igme.ests$SEX == "Total" &
                         igme.ests$SERIES_YEAR == "2019" &
                         igme.ests$SERIES_NAME == "UN IGME estimate 2019",]
igme.ests$year <- igme.ests$REF_DATE - 0.5
igme.ests <- igme.ests[igme.ests$REF_AREA == country, ]
igme.ests <- igme.ests[order(igme.ests$year),]
igme.ests <- igme.ests[igme.ests$year %in% beg.year:end.year,]
}

## Load IHME #### 
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

ihme.ests <- lapply(ihme.ests, function(x){
  if(!(country %in% x$ADM0_NAME)){
    message('\n Country name not found in one of the IHME files.\n')
  }
  x[x$ADM0_NAME == country,]
})
#Country name not found in one of the IHME files

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
              '/IHMEHand_', folder.name, '.R'))


## National Direct ####

mod.dat$v005 <- mod.dat$v005/1e6

births.list <- list()
svy.idx <- 0
for(survey in surveys){
  svy.idx <- svy.idx + 1
  births.list[[svy.idx]] <- mod.dat[mod.dat$survey == survey,] %>% as.data.frame()
  births.list[[svy.idx]]$died <- births.list[[svy.idx]]$Y
  births.list[[svy.idx]]$total <- as.numeric(births.list[[svy.idx]]$total)
}
names(births.list) <- surveys

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

save(direct.natl, file = paste0(folder.name,'/', country, '_direct_natl.rda'))
save(direct.natl.yearly, file = paste0(folder.name,'/', country, '_direct_natl_yearly.rda'))

## Admin1 Direct ####
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


save(direct.admin1, file = paste0(folder.name, '/', country, '_direct_admin1.rda'))


## Admin2 Direct ####
#Deleted, doesn't have admin 2


## HIV adjustment ####

if(doHIVAdj){
  natl.unaids <- unique(HIV.country$`National UNAIDS`) == "Y"
  subnatl.unaids <- unique(HIV.country$`Subnational UNAIDS`) == "Y"
  natl.default <- unique(HIV.country$`National Default Data`) == "Y"
  
  if(subnatl.unaids){
    ## Subnational HIV ##
    file.list <- Sys.glob(paste0(folder.name, '/HIV/', country, '_*_*.csv'))
    file.list <- file.list[!grepl("Subnational",file.list)]
    file.list <- file.list[!grepl("National", file.list)]
    file.list.per <- file.list[!grepl("Yearly", file.list)]
    file.list.yearly <- file.list[grepl("Yearly", file.list)]
    rm(file.list)
    
    file.areas <- strsplit(file.list.per, "_")
    file.areas <- unique(unlist(lapply(file.areas, function(x){
      x[2]
    })))
    
    missing <- which(!(file.areas %in% admin1.names$GADM))
    if(length(missing) != 0){
      message(message("\n", file.areas[missing], " is mismatched to GADM spelling.",
                  "\n Check the object called file.name.key!!!"))
    }
    file.name.key <- data.frame(File = sort(file.areas),
                                GADM = sort(admin1.names$GADM),
                                Internal = admin1.names$Internal)
    
    admin2.to.admin1 <- data.frame(GADM.adm2 = poly.adm2@data$NAME_2,
                                   Internal.adm2 = admin2.names$Internal,
                                   GADM.adm1 = poly.adm2@data$NAME_1,
                                   Internal.adm1 = NA)
    admin2.to.admin1$Internal.adm1 <- file.name.key$Internal[match(admin2.to.admin1$GADM.adm1, 
                                                                   file.name.key$GADM)]
    
    ## National Adjustments ##
    hiv.adj.summer <- expand.grid(years = periods)
    hiv.adj.yearly.summer <- expand.grid(years = beg.year:end.year)
    hiv.adj.yearly.summer$ratio <- hiv.adj.summer$ratio <- NA
    
    hiv.adj <- tmp.adj <- NULL
    for(survey in surveys){
      hiv.adj <- read.csv(paste0(folder.name,
                                 '/HIV/', country, '_National_', survey, '.csv'),
                          header = T)
      
      
      beg.years <- seq(beg.year, end.year, 5)
      years.before.beg <- survey - beg.years
      #years.before.end <- survey - end.years
      years.before.mm <- strsplit(as.character(hiv.adj[-1 ,1]), "-")
      years.before.mm <- lapply(years.before.mm, as.numeric)
      
      hiv.adj.summer$ratio <- 
        sapply(1:length(years.before.beg), function(i){
          test <- years.before.beg[i]
          
          test.list <- unlist( lapply(years.before.mm,
                                      function(x){
                                        x[1] == test
                                      }))
          
          idx <- which(test.list) + 1
          if(length(idx) == 1){
            return(hiv.adj[idx, 2])
          }else{
            return(1)
          }
          
        })
      
      tmp.adj <- getAdjusted(direct.natl[direct.natl$surveyYears == survey,],
                             ratio = hiv.adj.summer, 
                             logit.lower = NULL,
                             logit.upper = NULL,
                             prob.upper = "upper",
                             prob.lower = "lower")
      direct.natl[direct.natl$surveyYears == survey,] <- tmp.adj[ , match(colnames(direct.natl), 
                                                                          colnames(tmp.adj))]
      
      hiv.adj.yearly <- read.csv(paste0(folder.name,
                                        '/HIV/', folder.name, '_National_', survey, '_Yearly.csv'),
                                 header = T)
      
      years.before.beg <- survey - beg.year:end.year
      #years.before.end <- survey - end.years
      years.before.mm <- as.numeric(hiv.adj.yearly[-1 ,1])
      
      hiv.adj.yearly.summer$ratio <- 
        sapply(1:length(years.before.beg), function(i){
          test <- years.before.beg[i]
          
          test <- years.before.mm == test
          
          
          idx <- which(test) + 1
          if(length(idx) == 1){
            return(hiv.adj.yearly[idx, 2])
          }else{
            return(1)
          }
          
        })
      
      tmp.adj <- getAdjusted(direct.natl.yearly[direct.natl.yearly$surveyYears == survey,],
                             ratio = hiv.adj.yearly.summer, 
                             logit.lower = NULL,
                             logit.upper = NULL,
                             prob.upper = "upper",
                             prob.lower = "lower")
      direct.natl.yearly[direct.natl.yearly$surveyYears == survey,] <- tmp.adj[ ,  match(colnames(direct.natl.yearly), 
                                                                                         colnames(tmp.adj))]
    }
    
    ## Admin 1 and 2 Adjustments ##
    for(survey in surveys){
      for(i in 1:dim(file.name.key)[1]){
        file.names <- file.name.key[i,]
        
        file.in <- file.list.per[grepl(file.names$File, file.list.per)]
        file.in <- file.in[grepl(survey, file.in)]
        
        hiv.adj <- read.csv(file.in, header = T)
        
        beg.years <- seq(beg.year, end.year, 5)
        years.before.beg <- survey - beg.years
        #years.before.end <- survey - end.years
        years.before.mm <- strsplit(as.character(hiv.adj[-1 ,1]), "-")
        years.before.mm <- lapply(years.before.mm, as.numeric)
        
        hiv.adj.summer$ratio <- 
          sapply(1:length(years.before.beg), function(i){
            test <- years.before.beg[i]
            
            test.list <- unlist( lapply(years.before.mm,
                                        function(x){
                                          x[1] == test
                                        }))
            
            idx <- which(test.list) + 1
            if(length(idx) == 1){
              return(hiv.adj[idx, 2])
            }else{
              return(1)
            }
            
          })
        
        tmp.adj <- getAdjusted(direct.admin1[direct.admin1$region ==  
                                               as.character(file.names$Internal) &
                                               direct.admin1$surveyYears == survey,],
                               ratio = hiv.adj.summer, 
                               logit.lower = NULL,
                               logit.upper = NULL,
                               prob.upper = "upper",
                               prob.lower = "lower")
        direct.admin1[direct.admin1$region ==  
                        as.character(file.names$Internal) &
                        direct.admin1$surveyYears == survey,] <- tmp.adj[ , match(colnames(direct.admin1), 
                                                                                  colnames(tmp.adj))]
        
        if(exists('direct.admin2')){
          admin2s <- admin2.to.admin1$Internal.adm2[admin2.to.admin1$Internal.adm1 == 
                                                      as.character(file.names$Internal)]
          tmp.adj <- getAdjusted(direct.admin2[direct.admin2$region %in% admin2s &
                                                 direct.admin2$surveyYears == survey,],
                                 ratio = hiv.adj.summer, 
                                 logit.lower = NULL,
                                 logit.upper = NULL,
                                 prob.upper = "upper",
                                 prob.lower = "lower")
          direct.admin2[direct.admin2$region %in% admin2s &
                          direct.admin2$surveyYears == survey,] <- tmp.adj[ , match(colnames(direct.admin1), 
                                                                                    colnames(tmp.adj))]
        }
      }
    }
    
    
    rm(hiv.adj, hiv.adj.summer, hiv.adj.yearly, hiv.adj.yearly.summer, tmp.adj)
    
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
    
    
  }else if(natl.unaids){
    ## National HIV ##
    
    hiv.adj.summer <- expand.grid(years = periods)
    hiv.adj.yearly.summer <- expand.grid(years = beg.year:end.year)
    hiv.adj.yearly.summer$ratio <- hiv.adj.summer$ratio <- NA
    
    hiv.adj <- tmp.adj <- NULL
    
    for(survey in surveys){
      hiv.adj <- read.csv(paste0(folder.name,
                                 '/HIV/', country, '_', survey, '.csv'),
                          header = T)
      
      
      beg.years <- seq(beg.year, end.year, 5)
      years.before.beg <- survey - beg.years
      #years.before.end <- survey - end.years
      years.before.mm <- strsplit(as.character(hiv.adj[-1 ,1]), "-")
      years.before.mm <- lapply(years.before.mm, as.numeric)
      
      ## National Adjustments ##
      hiv.adj.summer$ratio <- 
        sapply(1:length(years.before.beg), function(i){
          test <- years.before.beg[i]
          
          test.list <- unlist( lapply(years.before.mm,
                                      function(x){
                                        x[1] == test
                                      }))
          
          idx <- which(test.list) + 1
          if(length(idx) == 1){
            return(hiv.adj[idx, 2])
          }else{
            return(1)
          }
          
        })
      
      tmp.adj <- getAdjusted(direct.natl[direct.natl$surveyYears == survey,],
                             ratio = hiv.adj.summer, 
                             logit.lower = NULL,
                             logit.upper = NULL,
                             prob.upper = "upper",
                             prob.lower = "lower")
      direct.natl[direct.natl$surveyYears == survey,] <- tmp.adj[ , match(colnames(direct.natl), 
                                                                          colnames(tmp.adj))]
      
      ## Admin 1 and Admin 2 ## 
      tmp.adj <- getAdjusted(direct.admin1[direct.admin1$surveyYears == survey,],
                             ratio = hiv.adj.summer,
                             logit.lower = NULL,
                             logit.upper = NULL,
                             prob.upper = "upper",
                             prob.lower = "lower")
      direct.admin1[direct.admin1$surveyYears == survey, ] <- tmp.adj[ , match(colnames(direct.admin1), 
                                                                               colnames(tmp.adj))]
      if(exists('direct.admin2')){
        
        tmp.adj <- getAdjusted(direct.admin2[direct.admin2$surveyYears == survey,],
                               ratio = hiv.adj.summer,
                               logit.lower = NULL,
                               logit.upper = NULL,
                               prob.upper = "upper",
                               prob.lower = "lower")
        direct.admin2[direct.admin2$surveyYears == survey, ] <- tmp.adj[ , match(colnames(direct.admin2), 
                                                                                 colnames(tmp.adj))]
      }
      
      hiv.adj.yearly <- read.csv(paste0(folder.name,
                                        '/HIV/', country, '_', survey, '_Yearly.csv'),
                                 header = T)
      
      years.before.beg <- survey - beg.year:end.year
      #years.before.end <- survey - end.years
      years.before.mm <- as.numeric(hiv.adj.yearly[-1 ,1])
      
      hiv.adj.yearly.summer$ratio <- 
        sapply(1:length(years.before.beg), function(i){
          test <- years.before.beg[i]
          
          test <- years.before.mm == test
          
          
          idx <- which(test) + 1
          if(length(idx) == 1){
            return(hiv.adj.yearly[idx, 2])
          }else{
            return(1)
          }
          
        })
      
      tmp.adj <- getAdjusted(direct.natl.yearly[direct.natl.yearly$surveyYears == survey,],
                             ratio = hiv.adj.yearly.summer, 
                             logit.lower = NULL,
                             logit.upper = NULL,
                             prob.upper = "upper",
                             prob.lower = "lower")
      direct.natl.yearly[direct.natl.yearly$surveyYears == survey,] <- tmp.adj[ ,  match(colnames(direct.natl.yearly), 
                                                                                         colnames(tmp.adj))]
    }
    
    rm(hiv.adj, hiv.adj.summer, hiv.adj.yearly, hiv.adj.yearly.summer, tmp.adj)
    
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
    
  }else if(natl.default){}
  
}



## Polygon Plots ####

if(!dir.exists(paths = paste0(folder.name, '/Plots/', 'Direct'))){
  dir.create(path = paste0(folder.name, '/Plots/', 'Direct'))
}

## National ##
med.palette <- brewer.pal(5, name = "Purples")
med.int <- classIntervals(round(direct.natl$logit.est, 2),
                          n = 5, style = 'jenks')
med.col <- findColours(med.int, med.palette)

for(survey in surveys){
  png(paste0(folder.name,"/Plots/Direct/", country, '_natl_', 
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

## Admin1 ##

med.palette <- brewer.pal(n = 5, name = "Purples")
med.int <- classIntervals(round(direct.admin1$logit.est, 2),
                          n = 5, style = 'jenks')
med.col <- findColours(med.int, med.palette)

for(survey in surveys){
  png(paste0(folder.name,"/Plots/Direct/", country, '_admin1_', 
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

## Admin2 ##

#No admin 2

## Spaghetti Plots ####
#IHME code deleted

cols <- rainbow(n.survey)
plot.years <- seq(1992, 2017, 5)

## National ##
pdf(paste0(folder.name,"/Plots/Direct/", country, '_natl_direct_spaghetti.pdf'))
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
legend('topright', bty = 'n', col = c(cols, 'grey37', 'darkgrey'),
       lwd = 2, lty = 1, legend = c(surveys, "UN IGME", "IHME"))
dev.off()

pdf(paste0(folder.name,"/Plots/Direct/", country, '_natl_direct_yearly_spaghetti.pdf'))
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

legend('topright', bty = 'n', col = c(cols, 'grey37', 'darkgrey'),
       lwd = 2, lty = 1, legend = c(surveys, "UN IGME", "IHME"))
dev.off()

## Admin1 ##

par(mfrow = c(1,1))
pdf(paste0(folder.name,"/Plots/Direct/", country, '_admin1_direct_spaghetti.pdf'))

for(area in 1:dim(poly.adm1)[1]){
  tmp.area <- direct.admin1[direct.admin1$region == as.character(admin1.names$Internal[area]),]
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  
  tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$area == as.character(admin1.names$GADM[area]),]
  
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
  legend('topright', bty = 'n', col = c(cols, 'darkgrey'),
         lwd = 2, lty = 1, legend = c(surveys, "IHME"))
}
dev.off()

## Admin2 ##
#No admin 2

