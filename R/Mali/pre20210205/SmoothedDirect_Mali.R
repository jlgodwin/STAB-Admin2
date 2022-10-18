#  SmoothedDirect.R
#  author: Jessica Godwin
#  
#  sources: LoadCommandCenter.R
#           


rm(list = ls())
#setwd('~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/')
setwd('~/Dropbox/OSU/Research_IPR/AfricaAdmin2Estimates/Data/countryDataFolders/')

devtools::install_github("bryandmartin/SUMMER",
                         build_vignettes = F, force = T)


#### Libraries ####
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
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

source('../../Analysis/R/LoadCommandCenter.R')


#### Parameters ####

country <- "Mali"
beg.year <- 1990
end.year <- 2019

CountryList <- gs_read(sheet_key, ws = "CountryList")
folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
n.survey <- CountryList$nSurvey[CountryList$Country == country]

#### Get Survey years #### 

SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]

#### Load polygon files ####

poly.file <- "/shapeFiles_gadm/"
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

#### Load data ####

load(paste0(folder.name, '/', country, '_direct_natl.rda'))
load(paste0(folder.name, '/', country, '_direct_admin1.rda'))
load(paste0(folder.name, '/shapeFiles_gadm/', country, '_Amat.rda'))
if(exists("admin1.mat")){
  colnames(admin1.mat) <- rownames(admin1.mat)
}
if(exists("admin2.mat")){
  colnames(admin2.mat) <- rownames(admin2.mat)
}


data.natl <- aggregateSurvey(direct.natl)
data.admin1 <- aggregateSurvey(direct.admin1)

beg.per <- seq(beg.year,end.year,5)
end.per <- beg.per + 4
periods <- paste(beg.per, end.per, sep = "-")


#### National Model ####
fit.natl <- fitINLA(data.natl, geo = NULL, Amat = NULL,
                    year_label = c(periods, "2020-2024"),
                    year_range = c(1990, 2024), is.yearly = F)
res.natl <- getSmoothed(fit.natl, year_range = c(1990, 2024),
                        year_label = c(periods, "2020-2024"))
res.natl$years.num <- seq(beg.year+2, end.year+5, 5)
head(res.natl)
tail(res.natl)

save(res.natl, file = paste0(folder.name, '/',
                             country, '_res_natl_SmoothedDirect.rda'))

#### Admin1 Model ####

names.idx <- which(data.admin1$region != "All")
replace.names <- as.character(colnames(admin1.mat)[data.admin1$region[names.idx]])
data.admin1$region <- as.character(data.admin1$region)
data.admin1$region[names.idx] <- replace.names

fit.admin1 <- fitINLA(data.admin1, geo = poly.adm1, Amat = admin1.mat,
                    year_label = c(periods, "2020-2024"),
                    year_range = c(1990, 2024), is.yearly = F)
res.admin1 <- getSmoothed(fit.admin1, Amat = admin1.mat,
                        year_range = c(1990, 2024),
                        year_label = c(periods, "2020-2024"))
res.admin1$years.num <- seq(beg.year+2, end.year+5, 5)[match(res.admin1$years, c(periods, "2020-2024"))]
head(res.admin1)
tail(res.admin1)
save(res.admin1, file = paste0(folder.name, '/',
                               country, '_res_admin1_SmoothedDirect.rda'))

#### Plots ####

if(!dir.exists(paths = paste0(folder.name, '/Plots/', 'SmoothedDirect'))){
  dir.create(path = paste0(folder.name, '/Plots/', 'SmoothedDirect'))
}

pdf(paste0(folder.name,"/Plots/SmoothedDirect/", country,
           '_natl_SmoothedDirect.pdf'))
plot(res.natl, is.yearly = F, is.subnational = F)
dev.off()

pdf(paste0(folder.name,"/Plots/SmoothedDirect/", country,
           '_admin1_SmoothedDirect.pdf'))
plot(res.admin1, is.yearly = F, is.subnational = T)
dev.off()

cols <- rainbow(length(unique(direct.admin1$surveyYears)))
plot.years <- seq(1992, 2017, 5)

par(mfrow = c(1,1))
pdf(paste0(folder.name,"/Plots/SmoothedDirect/",
           country, '_admin1_SmoothedDirect_spaghetti.pdf'))

for(area in 1:dim(poly.adm1)[1]){
  tmp.area <- direct.admin1[direct.admin1$region == area,]
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  
  res.area <- res.admin1[res.admin1$region == colnames(admin1.mat)[area],]
  
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
         main = poly.adm1@data$NAME_1[area])
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
               main = poly.adm1@data$NAME_1[area])
          
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
               main = poly.adm2@data$NAME_2[area])
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
    
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = 1, legend = c(surveys, "Smoothed"))
  }
  
  lines(res.area$years.num, res.area$median, col = 'black', lwd = 2)
  lines(res.area$years.num, res.area$upper, col = 'black', lwd = 1, lty = 2)
  lines(res.area$years.num, res.area$lower, col = 'black', lwd = 1, lty = 2)
}
dev.off()

