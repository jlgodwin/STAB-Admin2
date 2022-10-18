#  DataProcessing.R
#  author: Jessica Godwin
#  edited for Mali: Yue Chu
#  sources: LoadCommandCenter.R
#           


rm(list = ls())
#setwd('~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/')
setwd('~/Dropbox/OSU/Research_IPR/AfricaAdmin2Estimates/Data/countryDataFolders/')
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
poly.adm2 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm2))
proj4string(poly.adm0) <- proj4string(poly.adm1) <- proj4string(poly.adm2)
load(paste0(folder.name,'/shapeFiles_gadm/', country, '_Amat.rda'))

#### Load data ####

load(paste0(folder.name,'/',country,'_cluster_dat.rda'))
mod.dat$years <- as.numeric(as.character(mod.dat$years))
dat.years <- sort(unique(mod.dat$years))
beg.years <- seq(1990,2015,5)
end.years <- beg.years + 4
periods <- paste(beg.years, end.years, sep = "-")
mod.dat$period <- as.character(cut(mod.dat$years, breaks = c(beg.years, beg.years[length(beg.years)]+5),
                                   include.lowest = T, right = F, labels = periods))

#### National Direct ####

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
  direct.natl <-  SUMMER::getDirectList(births.list, periods,
                regionVar = "admin1",
                timeVar = "period", 
                clusterVar =  "~cluster",
                ageVar = "age", Ntrials = "total",
                weightsVar = "v005",national.only = T)
}else{
  direct.natl <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), periods,
            regionVar = "admin1",
            timeVar = "period", 
            clusterVar =  "~cluster",
            ageVar = "age", Ntrials = "total",
            weightsVar = "v005",national.only = T)
}

save(direct.natl, file = paste0(folder.name,'/', country, '_direct_natl.rda'))

#### Admin1 Direct ####
if(length(births.list) != 1){
  direct.admin1 <-  getDirectList(births.list, periods,
                regionVar = "admin1",
                timeVar = "period", 
                clusterVar =  "~cluster",
                ageVar = "age", Ntrials = "total",
                weightsVar = "v005",national.only = F)
}else{
  direct.admin1 <-  getDirect(as.data.frame(births.list[[1]]), periods,
            regionVar = "admin1",
            timeVar = "period", 
            clusterVar =  "~cluster",
            ageVar = "age", Ntrials = "total",
            weightsVar = "v005",national.only = F)
}


save(direct.admin1, file = paste0(folder.name, '/', country, '_direct_admin1.rda'))


#### Admin2 Direct ####
if(length(births.list) != 1){
  direct.admin2 <-  getDirectList(births.list, periods,
                regionVar = "admin2",
                timeVar = "period", 
                clusterVar =  "~cluster",
                ageVar = "age", Ntrials = "total",
                weightsVar = "v005",national.only = F)
}else{
  direct.admin2 <-  getDirect(as.data.frame(births.list[[1]]), periods,
            regionVar = "admin2",
            timeVar = "period", 
            clusterVar =  "~cluster",
            ageVar = "age", Ntrials = "total",
            weightsVar = "v005",national.only = F)
}


save(direct.admin2, file = paste0(folder.name, '/', country, '_direct_admin2.rda'))

#### Polygon Plots ####

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

med.palette <- brewer.pal(n = 7, name = "Purples")
med.int <- classIntervals(round(direct.admin2$logit.est, 2),
                          n = 7, style = 'jenks')
med.col <- findColours(med.int, med.palette)

for(survey in surveys){
  png(paste0(folder.name,"/Plots/Direct/", country, '_admin2_', 
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


#### Spaghetti Plots ####

cols <- rainbow(n.survey)
plot.years <- seq(1992, 2017, 5)

## National ##
pdf(paste0(folder.name,"/Plots/Direct/", country, '_natl_direct_spaghetti.pdf'))
plot.max <- max(direct.natl$upper+.025, na.rm = T)
for(survey in surveys){
    svy.idx <- match(survey, surveys) 
    pane.years <- jitter(plot.years)
    tmp <- direct.natl[direct.natl$surveyYears == survey,]
    tmp$width <- tmp$upper - tmp$lower
    tmp$cex2 <- median(tmp$width, na.rm = T)/tmp$width
    tmp$cex2[tmp$cex2 > 6] <- 6
    
    if(svy.idx== 1){
        par(mfrow = c(1,1))
        plot(pane.years, tmp$mean,
             xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year),
             type = 'l', col = cols[svy.idx], lwd = 2,
             main = country)
        
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
    
    legend('topright', bty = 'n', col = cols,
           lwd = 2, lty = 1, legend = surveys)
}
dev.off()

## Admin1 ##
par(mfrow = c(1,1))
pdf(paste0(folder.name,"/Plots/Direct/", country, '_admin1_direct_spaghetti.pdf'))

for(area in 1:dim(poly.adm1)[1]){
    tmp.area <- direct.admin1[direct.admin1$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    
    plot.max <- max(tmp.area$upper+.025, na.rm = T)
    for(survey in surveys){
        tmp <- tmp.area[tmp.area$surveyYears == survey,]
        svy.idx <- match(survey, surveys) 
        pane.years <- jitter(plot.years)
        
        if(svy.idx== 1){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = poly.adm1@data$NAME_1[area])
            
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2, )
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
        }else{
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2, )
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
        }
        
        legend('topright', bty = 'n', col = cols,
               lwd = 2, lty = 1, legend = surveys)
    }
}
dev.off()

## Admin2 ##
par(mfrow = c(1,1))
pdf(paste0(folder.name,"/Plots/Direct/", country, '_admin2_direct_spaghetti.pdf'))

for(area in 1:dim(poly.adm2)[1]){
    tmp.area <- direct.admin2[direct.admin2$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    if(dim(tmp.area)[1] != 0){
        plot.max <- max(tmp.area$upper+.025, na.rm = T)
    }else{
        plot.max <- 0.25
    }
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
                     main = poly.adm2@data$NAME_2[area])
                lines(pane.years, tmp$mean, cex = tmp$cex2,
                      type = 'l', col = cols[svy.idx],
                      main = surveys[svy.idx], lwd = 2)
                
                points(pane.years, tmp$mean, pch = 19,
                       col = alpha(cols[svy.idx], 0.35),
                       cex = tmp$cex2, )
            }else{
                plot(NA,
                     xlab = "Year", ylab = "U5MR",
                     ylim = c(0, plot.max),
                     xlim = c(beg.year, end.year),
                     type = 'l', col = cols[svy.idx], lwd = 2,
                     main = poly.adm2@data$NAME_2[area])
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
                       cex = tmp$cex2, )
            }
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
        }
        
        legend('topright', bty = 'n', col = cols,
               lwd = 2, lty = 1, legend = surveys)
    }
}
dev.off()







# #### Spaghetti Plots ####
# 
# cols <- rainbow(n.survey)
# plot.years <- seq(1992, 2017, 5)
# 
# ## National ##
# pdf(paste0(folder.name,"/Plots/Direct/", country, '_natl_direct_spaghetti.pdf'))
# plot.max <- max(direct.natl$upper+.025, na.rm = T)
# for(survey in surveys){
#   svy.idx <- match(survey, surveys) 
#   pane.years <- jitter(plot.years)
#   tmp <- direct.natl[direct.natl$surveyYears == survey,]
#   tmp$width <- tmp$upper - tmp$lower
#   tmp$cex2 <- median(tmp$width, na.rm = T)/tmp$width
#   tmp$cex2[tmp$cex2 > 6] <- 6
#   
#   if(svy.idx== 1){
#     par(mfrow = c(1,1))
#     plot(pane.years, tmp$mean,
#          xlab = "Year", ylab = "U5MR",
#          ylim = c(0, plot.max),
#          xlim = c(beg.year, end.year),
#          type = 'l', col = cols[svy.idx], lwd = 2,
#          main = country)
#     
#     points(pane.years, tmp$mean,
#            col = alpha(cols[svy.idx], 0.35),
#            cex = tmp$cex2, pch = 19)
#     
#     # for(year.id in 1:length(periods)){
#     #   segments(pane.years[year.id], tmp$upper[year.id],
#     #            pane.years[year.id], tmp$lower[year.id],
#     #            col = cols[svy.idx])
#     # }
#   }else{
#     lines(pane.years, tmp$mean,
#           type = 'l', col = cols[svy.idx],
#           main = surveys[svy.idx], lwd = 2)
#     points(pane.years, tmp$mean,
#            col = alpha(cols[svy.idx], 0.35),
#            cex = tmp$cex2, pch = 19)
#     # for(year.id in 1:length(periods)){
#     #   segments(pane.years[year.id], tmp$upper[year.id],
#     #            pane.years[year.id], tmp$lower[year.id],
#     #            col = cols[svy.idx])
#     # }
#   }
#   
#   legend('topright', bty = 'n', col = cols,
#          lwd = 2, lty = 1, legend = surveys)
# }
# dev.off()
# 
# ## Admin1 ##
# par(mfrow = c(1,1))
# pdf(paste0(folder.name,"/Plots/Direct/", country, '_admin1_direct_spaghetti.pdf'))
# 
# for(area in 1:dim(poly.adm1)[1]){
#   tmp.area <- direct.admin1[direct.admin1$region == area,]
#   tmp.area$width <- tmp.area$upper - tmp.area$lower
#   tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
#   tmp.area$cex2[tmp.area$cex2 > 6] <- 6
#   
#   plot.max <- max(tmp.area$upper+.025, na.rm = T)
#   for(survey in surveys){
#     if (!(survey %in% tmp.area$surveyYears)) next
#     tmp <- tmp.area[tmp.area$surveyYears == survey,]
#     svy.idx <- match(survey, surveys) 
#     pane.years <- jitter(plot.years)
#     
#     if(svy.idx== 1){
#       plot(pane.years, tmp$mean,
#            xlab = "Year", ylab = "U5MR",
#            ylim = c(0, plot.max),
#            xlim = c(beg.year, end.year),
#            type = 'l', col = cols[svy.idx], lwd = 2,
#            main = poly.adm1@data$NAME_1[area])
#       
#       points(pane.years, tmp$mean, pch = 19,
#              col = alpha(cols[svy.idx], 0.35),
#              cex = tmp$cex2)
#       # for(year.id in 1:length(periods)){
#       #   segments(pane.years[year.id], tmp$upper[year.id],
#       #            pane.years[year.id], tmp$lower[year.id],
#       #            col = cols[svy.idx])
#       # }
#     }else{
#       lines(pane.years, tmp$mean, cex = tmp$cex2,
#             type = 'l', col = cols[svy.idx],
#             main = surveys[svy.idx], lwd = 2)
#       points(pane.years, tmp$mean, pch = 19,
#              col = alpha(cols[svy.idx], 0.35),
#              cex = tmp$cex2)
#       # for(year.id in 1:length(periods)){
#       #   segments(pane.years[year.id], tmp$upper[year.id],
#       #            pane.years[year.id], tmp$lower[year.id],
#       #            col = cols[svy.idx])
#       # }
#     }
#     
#     legend('topright', bty = 'n', col = cols,
#            lwd = 2, lty = 1, legend = surveys)
#   }
# }
# dev.off()
# 
# ## Admin2 ##
# par(mfrow = c(1,1))
# pdf(paste0(folder.name,"/Plots/Direct/", country, '_admin2_direct_spaghetti.pdf'))
# 
# for(area in 1:dim(poly.adm2)[1]){
#   tmp.area <- direct.admin2[direct.admin2$region == area,]
#   tmp.area$width <- tmp.area$upper - tmp.area$lower
#   tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
#   tmp.area$cex2[tmp.area$cex2 > 6] <- 6
#   
#   if (sum(is.na(tmp.area$logit.est)) == nrow(tmp.area)) next
#   
#   if(dim(tmp.area)[1] != 0){
#     plot.max <- max(tmp.area$upper+.025, na.rm = T)
#   }else{
#     plot.max <- 0.25
#   }
#   startplot <- 0
#   for(survey in surveys){
#     if (!(survey %in% tmp.area$surveyYears)) next
#     startplot <- startplot+1
#     tmp <- tmp.area[tmp.area$surveyYears == survey,]
#     svy.idx <- match(survey, surveys) 
#     pane.years <- jitter(plot.years)
#     
#     if(startplot==1){
#       if(dim(tmp)[1] != 0){
#       plot(pane.years, tmp$mean,
#            xlab = "Year", ylab = "U5MR",
#            ylim = c(0, plot.max),
#            xlim = c(beg.year, end.year),
#            type = 'l', col = cols[svy.idx], lwd = 2,
#            main = poly.adm2@data$NAME_2[area])
#         
#         points(pane.years, tmp$mean, pch = 19,
#                col = alpha(cols[svy.idx], 0.35),
#                cex = tmp$cex2)
#       }else{
#         plot(NA,
#              xlab = "Year", ylab = "U5MR",
#              ylim = c(0, plot.max),
#              xlim = c(beg.year, end.year),
#              type = 'l', col = cols[svy.idx], lwd = 2,
#              main = poly.adm2@data$NAME_2[area])
#       }
#       
#       
#       # for(year.id in 1:length(periods)){
#       #   segments(pane.years[year.id], tmp$upper[year.id],
#       #            pane.years[year.id], tmp$lower[year.id],
#       #            col = cols[svy.idx])
#       # }
#     }else{
#       if(dim(tmp)[1] != 0){
#       lines(pane.years, tmp$mean, cex = tmp$cex2,
#             type = 'l', col = cols[svy.idx],
#             main = surveys[svy.idx], lwd = 2)
#       points(pane.years, tmp$mean, pch = 19,
#              col = alpha(cols[svy.idx], 0.35),
#              cex = tmp$cex2)
#       }
#       # for(year.id in 1:length(periods)){
#       #   segments(pane.years[year.id], tmp$upper[year.id],
#       #            pane.years[year.id], tmp$lower[year.id],
#       #            col = cols[svy.idx])
#       # }
#     }
#     
#     legend('topright', bty = 'n', col = cols,
#            lwd = 2, lty = 1, legend = surveys)
#   }
# }
# dev.off()
# 
# 



