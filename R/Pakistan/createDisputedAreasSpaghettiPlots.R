rm(list = ls())
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
library(cartography)
library(rgeos)

#### Parameters ####
country <- "Pakistan"
cluster <- FALSE
message("If have the same subfolder structure as 
        AfricaAdmin2Estimates/Data/countryDataFolders/. Do nothing!\n
        Otherwise, edit the following paths as needed.\n")

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
#CountryList <- read.csv("CountryList.csv", header = T)

folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]

message('Where is IHMEHand_CountryName.rda?\n')
hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
                       gsub(" ", "", folder.name))
#hand.dir.rel <- paste0(gsub(" ", "", folder.name))


#### More Params ####

beg.year <- 1990
end.year <- 2020
# time.mod <- "rw2"
# time.mod <- "ar1"
# time.mod <- "rw2_rw1"
time.mod <- "rw2main_randomSlopes_rw1xICAR"
# time.mod <- "rw2main_randomSlopes_ar1xICAR"

#### Load polygon data ####
poly.file <- shapes.sub.dir
poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                         '0', sep = "_")
poly.layer.adm1 <- paste('gadm36', gadm.abbrev,
                         '1_excluding_disputed', sep = "_")
poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                         '2_excluding_disputed', sep = "_")

poly.path <- paste0(folder.name, poly.file)
poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0))
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1))
if(sum(grepl(paste('gadm36', gadm.abbrev,
                   '2_excluding_disputed', sep = "_"), list.files(paste0(folder.name, poly.file)))) != 0){
    poly.adm2 <- readOGR(dsn = poly.path,
                         layer = as.character(poly.layer.adm2))
}

if(exists("poly.adm2")){
    proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
    proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(folder.name, 
            shapes.sub.dir,
            '/', country, '_Amat.rda'))
load(paste0(folder.name, 
            shapes.sub.dir,
            '/', country, '_Amat_Names.rda'))

disputed_names <- c("Azad Kashmir", "Northern Areas")

admin1.names <- admin1.names[!(admin1.names$GADM %in% disputed_names),]
admin1.mat <- admin1.mat[which(rownames(admin1.mat) %in% 
                                   as.character(admin1.names$Internal)),
                         which(rownames(admin1.mat) %in% 
                                   as.character(admin1.names$Internal))]

admin2.names <- admin2.names[!(admin2.names$GADM %in% disputed_names),]
admin2.mat <- admin2.mat[which(rownames(admin2.mat) %in% 
                                   as.character(admin2.names$Internal)),
                         which(rownames(admin2.mat) %in% 
                                   as.character(admin2.names$Internal))]


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
SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
#SurveyInfo <- read.csv("SurveyInfo.csv", header = T)
#surveys <- SurveyInfo$Survey.Year[SurveyInfo$Country == country &
#                                    SurveyInfo$`GPS.` == "Y"]

#SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                        SurveyInfo$`GPS?` == "Y"]
survey.legends <- SurveyInfo$`OfficialSurveyName`[SurveyInfo$Country == country &
                                                      SurveyInfo$`GPS?` == "Y"]


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

mod.dat$strata.orig <- mod.dat$strata
mod.dat$strata <- mod.dat$urban
mod.dat$country <- as.character(country)

cols <- rainbow(8+1+1+1)
cols <- cols[c(1,3,7,2,4,11,9,5,6,8,10)]

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

# load admin 2  benchmarked data

# reloading these in case you're just running the benchmark section of the code
if(useHIVAdj){
    load(paste0(folder.name, '/',
                country, '_directHIV_admin2.rda'), envir = .GlobalEnv)
}else{
    load(paste0(folder.name, '/',
                country, '_direct_admin2.rda'), envir = .GlobalEnv)
}

direct.admin2[,c("mean", "lower", "upper")] <- 
    direct.admin2[,c("mean", "lower", "upper")]*1000


load(paste0(folder.name,'/', country,
            '_res_', time.mod, '_admin2Bench.rda'))
write.csv(res.admin2$overall,
          file = paste0(folder.name, '/',
                        country, '_res_', time.mod,'_admin2Bench.csv'),
          row.names = FALSE)
load(paste0(folder.name,'/', country, '_', 
            time.mod, '_admin2Benchmarks.rda'))
load(paste0(folder.name, '/', country, 
            '_', time.mod,'_admin2Bench_noStrata_temporals.rda'))
load(paste0(folder.name, '/',
            country, '_', time.mod,'_admin2Bench_noStrata_spatials.rda'))
load(paste0(folder.name, '/',
            country, 
            '_', time.mod, '_admin2Bench_noStrata_fixedeff.rda'))

if (grepl("randomSlopes",time.mod)) {
    load(paste0(folder.name, '/',
                country,'_',
                time.mod,'_admin2Bench_noStrata_posteriorRandomSlopes.rda'))
}


# delete disputed areas



# make spaghetti plots (individual)
areaOrder <- admin2.names$Internal
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod,
           '_admin2Bench_noStrata_spaghetti_noIHME_6per_excluding_disputed.pdf'),
    height = 9, width = 6)
{
    par(mfrow = c(3,2),lend=1)
    area.idx <- 0
    for(area in areaOrder){
        area.idx <- match(area, admin2.names$Internal)
        tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
        tmp.area$width <- tmp.area$upper - tmp.area$lower
        tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
        tmp.area$cex2[tmp.area$cex2 > 6] <- 6
        tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME ==
                                       as.character(admin2.names$GADM[area.idx]) &
                                       ihme.ests[[3]]$ADM1_NAME == as.character(poly.adm2@data$NAME_1[area.idx]),]
        tmp.ihme[ ,c("mean", "lower", "upper")] <- 
            tmp.ihme[ ,c("mean", "lower", "upper")]*1000
        # print(tmp.ihme)
        cols <- rainbow(11)
        cols <- cols[c(1,3,7,2,4,11,9,5,6,8,10)]
        plot.years <- beg.year:end.year
        tmp.area[,c("median", "lower","upper")] <-
            tmp.area[,c("median", "lower","upper")]*1000
        
        
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
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], 
                              sep = ", "))
            legend('topright', bty = 'n', col = c(cols[1:length(surveys)], 'black'),
                   lwd = 2, lty = 1, legend = c(survey.legends, "Betabinomial"))
            
        } else {
            
            for(survey in surveys){
                tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                                         direct.admin2$region ==
                                         as.character(admin2.names$Internal[area.idx]),]
                svy.idx <- match(survey, surveys) 
                
                pane.years <- rep(NA, length(periods)-1)
                
                for(idx in 1:(length(periods)-1)){
                    id.yr <- seq(beg.year,end.year-5,5)[idx]
                    dat.in <- id.yr:(id.yr + 4) %in% mod.dat$years[mod.dat$survey == survey]
                    if(sum(dat.in) != 0){
                        pane.years[idx] <- mean((id.yr:(id.yr + 4))[dat.in])
                    }
                }
                
                est.ids <- which(!is.na(pane.years))
                
                
                if(svy.idx== 1){
                    if(dim(tmp)[1] != 0){
                        plot(NA,
                             xlab = "Year", ylab = "U5MR",
                             ylim = c(0, plot.max),
                             xlim = c(beg.year, end.year),
                             type = 'l', col = cols[svy.idx], lwd = 2,
                             main = paste(admin2.names$GADM[area.idx],
                                          poly.adm2$NAME_1[area.idx], 
                                          sep = ", "))
                        
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
                             main =  paste(admin2.names$GADM[area.idx],
                                           poly.adm2$NAME_1[area.idx], 
                                           sep = ", "))
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
            
            # lines(tmp.ihme$year, 
            #       tmp.ihme$mean, lwd = 2,
            #       lty = 1, 
            #       col  = cols[9])
            # polygon(c(tmp.ihme$year,
            #           rev(tmp.ihme$year)),
            #         c(tmp.ihme$upper,
            #           rev(tmp.ihme$lower)),
            #         col = alpha(cols[9], 0.25),
            #         border = FALSE)
            
            
            
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
            legend('topright', bty = 'n', col = c(cols[c(1:length(surveys))], 'black'),
                   lwd = 2, lty = c(rep(1, length(surveys)+1)),
                   legend = c(survey.legends, "Betabinomial"),
                   cex = 0.6)
            
            
        }
        
        
    }
}
dev.off()

# make spaghetti all plots
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

numberAreasPerPage <- 30
numberAreasTotal <- nrow(admin2.names)
numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)
# order data by median magnitude in 2020
res.admin2.order <- res.admin2$overall %>% 
    filter(years.num == 2019) %>%
    arrange(median)
areaOrder <- res.admin2.order$region.orig[res.admin2.order$region.orig %in% admin2.names$Internal]

numberAreasPerPage <- numberAreasTotal/numberPages

if(numberAreasTotal%%numberPages != 0){
    numberAreasPerPage <- floor(numberAreasPerPage)
}
# pagesWithLessAreas <- NULL
# if(numberAreasTotal %% numberAreasPerPage != 0){
#   pagesWithLessAreas <- (numberPages-1):numberPages
# }
#   
# loop and make plots

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_',
           time.mod, 
           '_admin2Bench_spaghetti_allByMedian_excluding_disputed.pdf'),
    height = 6, width = 6)
par(lend=1)
for (i in 1:numberPages) {
    
    if(i != numberPages){
        areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
    }else if(i == numberPages){
        areas <- areaOrder[((i-1)*numberAreasPerPage + 1):numberAreasTotal]
    }
    # areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
    #                                         as.character(admin1.names$GADM[i])]
    
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


