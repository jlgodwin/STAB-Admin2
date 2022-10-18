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
library(stringr)
#### Parameters ####
country <- "Rwanda"
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

#setwd("C:\\Users\\jiang14\\Documents\\Rwanda")
setwd(data.dir)
if(!exists("sheet_key", envir = .GlobalEnv)){
  source(paste0(code.dir.rel, '/',
                'LoadCommandCenter.R'))
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
time.mod <- "rw2main_randomSlopes_rw1xICAR"

#### Load IGME data ####
#### Load polygon data ####
poly.file <- paste0(folder.name, '/',
                    shapes.sub.dir)
poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                         '0', sep = "_")
poly.layer.adm1 <- paste('gadm36', gadm.abbrev,
                         '1', sep = "_")
poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                         '2', sep = "_")

poly.path <- paste0(poly.file)
poly.adm0  <- readOGR(dsn = poly.path,
                      layer = as.character(poly.layer.adm0))
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1))
if(sum(grepl(paste('gadm36', gadm.abbrev,
                   '2', sep = "_"), list.files(paste0(poly.file)))) != 0){
  poly.adm2 <- readOGR(dsn = poly.path,
                       layer = as.character(poly.layer.adm2))
}

if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(folder.name, '/',
            shapes.sub.dir, '/',
            country, '_Amat.rda'))
load(paste0(folder.name, '/',
            shapes.sub.dir, '/',
            country, '_Amat_Names.rda'))


#### Use HIV Adjusted data? ####
#HIV.sheet <- gs_read(sheet_key, ws = "HIV")
HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
#HIV.sheet <- read.csv("HIV.csv", header = T)
HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                unique(HIV.country$`UNAIDS data?`) == "Y")
#useHIVAdj <- (unique(HIV.country$MM.Adj.by.IGME) == "Y" &
#                unique(HIV.country$UNAIDS.data.) == "Y")
useHIVAdj
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
surveys <- surveys
survey.legends <- survey.legends

#### Load model data ####
# folder.name <- "Rwanda_new"
load(paste0(folder.name, '/',
            country,'_cluster_dat.rda'),
     envir = .GlobalEnv)
unique(mod.dat$survey)


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

file.list <- list.files(code.dir.rel)
igme.file <- file.list[grepl("Results.csv", file.list)]
igme.ests <- read.csv(paste0(code.dir.rel,
                             '/',igme.file),
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
file.list <- list.files(code.dir.rel)
ihme.files <- file.list[grepl("IHME", file.list)]
ihme.dir.rel <- code.dir.rel
ihme.ests <- list()
ihme.ests[['adm0']] <- read.csv( paste0( ihme.dir.rel,'/',
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

source(paste0(hand.dir.rel,'/',
              'IHMEHand_', country, '.R'))


# directory with data on excess deaths
ed_data_dir <- paste0(folder.name, '/',
                      "worldpop/")
# affected admin 2 indices
affected_admin_2_ind <- c(1:30)
affected_admin_2_names <- 
  read.csv(paste0(ed_data_dir,
                  "Rwanda_admin2_U1_5.csv"))$names[affected_admin_2_ind] %>%
  as.character()


tbl_1_5 <- read.csv(paste0(ed_data_dir, "Rwanda_admin2_U1_5.csv")) %>%
  dplyr::select(-X) %>%
  mutate(region = paste0("admin2_", 1:length(unique(names)))) %>%
  pivot_longer(names_to = "year", values_to = "pop_1_5", -c(names, region)) %>%
  mutate(year = as.numeric(str_sub(year, 5, -1)))

tbl_0_1 <- read.csv(paste0(ed_data_dir, "Rwanda_admin2_U0_1.csv")) %>%
  dplyr::select(-X) %>%
  pivot_longer(names_to = "year", values_to = "pop_0_1", -c(names)) %>%
  mutate(year = as.numeric(str_sub(year, 5, -1)))


# excess death mortality rate table -- to be computed!
edmr_tbl <- tbl_1_5 %>%
  left_join(tbl_0_1, by = c("names", "year")) %>%
  mutate(pop_0_5 = pop_1_5 + pop_0_1)


#### U5 POPULATION HINDCAST ####
# hindcasts of u5 population -- need to do this more intelligently
for (yr in 1990:1999) {
  rows_to_add <- edmr_tbl %>%
    filter(year == 2000) %>%
    mutate(year = yr)
  edmr_tbl <- bind_rows(edmr_tbl, rows_to_add)
}

edmr_tbl <- edmr_tbl[order(edmr_tbl$year),]

area.cols <- rainbow(dim(admin2.mat)[1])

pdf(paste0(folder.name, '/Plots/',
           country, '_u5_pop.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1),
      lend = 1)
  
  y.lim <- range(edmr_tbl$pop_0_5/1000)
  
  plot(NA,
       xlim = c(beg.year, end.year),
       ylim = y.lim,
       xlab = "Year",
       ylab = 'Population under age 5')
  
  index <- 0
  for(area in affected_admin_2_names){
    index <- index + 1
    lines(edmr_tbl$year[edmr_tbl$names == area],
          edmr_tbl$pop_0_5[edmr_tbl$names == area]/1000,
          lwd = 2, col = area.cols[index])
  }
  legend('topleft', 
         lwd = 2, ncol = 3,
         col = area.cols,
         legend = affected_admin_2_names,
         bty = 'n', cex = 0.7)
}
dev.off()

# the years and regions are not perfectly sorted!
# sort by years and regions
region_id <- edmr_tbl$region %>% strsplit(split = "_") %>% sapply(function(x) x[2])
edmr_tbl <- edmr_tbl %>% mutate(region_id = as.numeric(region_id))
edmr_tbl <- edmr_tbl %>% arrange(year)
edmr_tbl <- edmr_tbl %>% arrange(region_id)


# Identify affected years and admin 2 areas (for Rwanda all areas were affected)
edmr_tbl <- edmr_tbl %>%
  mutate(crisis = (year %in% 1993:1999) & (names %in% affected_admin_2_names))
crisis =  (edmr_tbl$year %in% 1993:1999) & (edmr_tbl$names %in% affected_admin_2_names)


# get national U5 excess deaths for 1993-1999 genocide
igme_crisis_df <- read.csv(paste0("../ExcessDeathData/Crisis_Under5_deaths2020.csv"))
natl_u5_ed <-  c(rep(0,3),igme_crisis_df$Crisis.d0.5[igme_crisis_df$Year %in% (c(1993:1999)+0.5) &
                                                       igme_crisis_df$ISO3Code == "RWA"],rep(0,21)) %>% rep(each = 30)

yearsum_0_1 <- edmr_tbl %>% group_by(year) %>% summarise(pop_0_1_yearsum = sum(pop_0_1))
edmr_tbl <- yearsum_0_1 %>% left_join(edmr_tbl, by =c("year"))
yearsum_1_5 <- edmr_tbl %>% group_by(year) %>% summarise(pop_1_5_yearsum = sum(pop_1_5))
edmr_tbl <- yearsum_1_5 %>% left_join(edmr_tbl, by =c("year"))

edmr_tbl <- edmr_tbl %>%
  mutate(ed_0_1 = (0.2) * natl_u5_ed * pop_0_1 * crisis / pop_0_1_yearsum * crisis,
         ed_1_5 = (0.8) * natl_u5_ed * pop_1_5 * crisis / pop_1_5_yearsum * crisis) %>%
  mutate(ed_1m0 = ed_0_1 / pop_0_1,
         ed_4m1 = ed_1_5 / pop_1_5) %>%
  mutate(ed_1q0 = ed_1m0 / (1 + (1 - (0.3)) * ed_1m0),
         ed_4q1 = 4 * ed_4m1 / (1 + (4 - 4 * (0.4)) * ed_4m1)) %>%
  mutate(ed_5q0 = 1 - (1 - ed_1q0) * (1 - ed_4q1))

pdf(paste0(folder.name,'/Plots/',
           country, '_ED.pdf'),
    height = 8, width = 8)
{
  par(mfrow=c(2,2),
      lend = 1)
  
  y.lim <- range(edmr_tbl$ed_0_1 + 
                   edmr_tbl$ed_1_5)
  plot(NA,
       xlim = c(1992,2000),
       ylim = y.lim,
       xlab = "Year",
       ylab = 'Excess deaths under age 5')
  abline(h = 0)
  index <- 0
  for(area in affected_admin_2_names){
    index <- index + 1
    points(edmr_tbl$year[edmr_tbl$names == area],
           edmr_tbl$ed_0_1[edmr_tbl$names == area] +
             edmr_tbl$ed_1_5[edmr_tbl$names == area],
           lwd = 1, pch = 16, col = area.cols[index])
  }
  # points(beg.year:end.year,
  #       edmr_tbl$ed_5q0[edmr_tbl$names == area],
  #       lwd = 2, lty = 2)
  legend('topright', 
         pch = 16,
         ncol = 3,
         col = c(area.cols),
         legend = c(affected_admin_2_names),
         bty = 'n', cex = 0.4)
  
  plot(NA,
       xlim = c(1992,2000),
       ylim = c(-1,1),
       xlab = "Year",
       ylab = 'Excess deaths under age 5')
  abline(h = 0)
  index <- 0
  for(area in affected_admin_2_names){
    index <- index + 1
    points(edmr_tbl$year[edmr_tbl$names == area],
           edmr_tbl$ed_0_1[edmr_tbl$names == area] +
             edmr_tbl$ed_1_5[edmr_tbl$names == area],
           lwd = 1,pch = 16, col = area.cols[index])
  }
  # points(beg.year:end.year,
  #       edmr_tbl$ed_5q0[edmr_tbl$names == area],
  #       lwd = 2, lty = 2)
  
  y.lim <- range(natl_u5_ed[seq(1,930,30)])
  plot(NA,
       xlim = c(1992,2000),
       ylim = y.lim,
       xlab = "Year",
       ylab = 'Excess deaths under age 5')
  abline(h = 0)
  
  lines(beg.year:end.year,
        natl_u5_ed[seq(1,930,30)],
        pch = 16, lty = 2, type = 'b')
  legend('topright', 
         pch = 16,lty = 2,
         ncol = 1, col = 'black',
         legend = "National",
         bty = 'n', cex = 1)
  plot(NA,
       xlim = c(1992,2000),
       ylim = c(-5, 20),
       xlab = "Year",
       ylab = 'Excess deaths under age 5')
  abline(h = 0)
  
  lines(beg.year:end.year,
        natl_u5_ed[seq(1,930,30)],
        pch = 16, type = 'b', lty =2 )
  legend('topright', 
         pch = 16,
         ncol = 1, col = 'black',
         legend = "National",
         bty = 'n', cex = 1)
}
dev.off()


load(paste0(folder.name,'/',country,'_res_',time.mod,'_admin2.rda'))
load(paste0(folder.name,'/',country, '_', time.mod, '_admin2Benchmarks.rda'))

# pull out all the unadjusted draws and reshape
draws_mat <- do.call(rbind, lapply(res.admin2$draws.est, function(x) x$draws))
colnames(draws_mat) <- paste0("draw_", 1:1000)
draws_tbl <- as_tibble(draws_mat) %>%
  mutate(years = as.factor(sapply(res.admin2$draws.est, 
                                  function(x) x$years)), 
         region = sapply(res.admin2$draws.est, function(x) x$region))

# adjust draws for crisis EDMR
draws_tbl <- res.admin2$overall %>%
  dplyr::select(region, years) %>%
  left_join(draws_tbl, by = c("years", "region")) %>%
  left_join(mutate(edmr_tbl, years = as.factor(year)) %>%
              dplyr::select(region, years, ed_5q0, pop_0_5),
            by = c("years", "region")) %>%
  pivot_longer(starts_with("draw"), names_to = "draw",
               values_to = "u5mr")  %>%
  mutate(adj_u5mr =  ed_5q0 + u5mr) 

pdf(paste0(folder.name,'/Plots/',
           country, '_EDvsunadjusted.pdf'),
    width = 5, height = 5)
{
  par(mfrow=c(1,1),
      par(lend = 1))
  
  plot(NA,
       xlim = c(100,400),
       ylim = c(100,400),
       xlab = "Unadjusted, unbenchmarked U5MR",
       ylab = "Unbenchmarked U5MR + ED Adjustment")
  abline(0,1, lty = 2)
  yr.cols <- rainbow(length(1992:2000))
  for(yr in 1992:2000){
    before <- after <- rep(NA, length(admin2.names$Internal))
    for(area in admin2.names$Internal){
      area.idx <- match(area, admin2.names$Internal)
      before[area.idx] <- median(1000*draws_tbl$u5mr[draws_tbl$years == yr &
                                                       draws_tbl$region == area])
      after[area.idx] <- median(1000*draws_tbl$adj_u5mr[draws_tbl$years == yr &
                                                          draws_tbl$region == area])
    }
    points(before,
           after,
           pch = 16, col = yr.cols[match(yr,
                                         1992:2000)])
  }
  legend('bottomright',
         pch = 16,
         col = yr.cols,
         legend = 1992:2000,
         bty = 'n')
}
dev.off()


# change draws.est slot to store adjusted results
for (i in 1:length(res.admin2$draws.est)) {
  yr <- res.admin2$draws.est[[i]]$years
  reg <- res.admin2$draws.est[[i]]$region
  res.admin2$draws.est[[i]]$draws <- (draws_tbl %>% filter(years == yr & region == reg))$adj_u5mr
}


# aggregate admin 2 U5MR to get national U5MR for all years
agg_natl_draws_tbl <- draws_tbl %>%
  group_by(years, draw) %>%
  dplyr::summarize(u5mr_unadj = sum(pop_0_5 * u5mr) / sum(pop_0_5),
                   u5mr = sum(pop_0_5 * adj_u5mr) / sum(pop_0_5))

# compute median of national U5MR estimates
med_agg_natl_draws <- agg_natl_draws_tbl %>%
  group_by(years) %>%
  dplyr::summarize( u5mr_upper = quantile(u5mr, 0.95),
                    u5mr_lower = quantile(u5mr, 0.05),
                    u5mr = median(u5mr),
                    u5mr_unadj = median(u5mr_unadj)) %>%
  mutate(bench = u5mr * 1000 / igme.ests$OBS_VALUE[match(years,
                                                         igme.ests$year)],
         bench_unadj = u5mr_unadj * 1000/igme.ests$OBS_VALUE[match(years,
                                                                   igme.ests$year)])


################################################################################
### NATIONAL EXCESS DEATH ADJUSTMENT                                        ####
################################################################################

# calculate post hoc benchmarks by comparing with IGME

# Smooth out benchmarks and forecast for 2016-2020 using RW2 model
smoothed_bench <- med_agg_natl_draws  
smoothed_bench$bench[smoothed_bench$years %in% 1993:1999] <- NA
smoothed_bench$bench[(nrow(smoothed_bench) - 4):nrow(smoothed_bench)] <- NA
smoothed_res <- inla(bench ~ f(as.numeric(years), model = "rw2"), data = smoothed_bench,control.predictor=list(compute=TRUE))
#med_agg_natl_draws$smoothed_bench = smoothed_res$summary.linear.predictor$mean[27:31]
med_agg_natl_draws$smoothed_bench = smoothed_res$summary.linear.predictor$mean

pdf(paste0(folder.name, '/',
           country, '_benchmarks_compare.pdf'),
    width = 8, height = 4)
{
  par(mfrow = c(1,2),
      lend = 1)
  plot(NA, 
       xlim = c(beg.year, end.year),
       ylim = c(.75,1.25),
       xlab = "Year",
       ylab = "Benchmark")
  abline(h=1)
  
  lines(beg.year:end.year,
        bench.adj$ratio, lwd = 2,
        col = 'black')
  lines(beg.year:end.year,
        med_agg_natl_draws$bench, lwd = 2,
        col = 'red')
  
  lines(beg.year:end.year,
        med_agg_natl_draws$smoothed_bench,
        col = 'navy', lwd = 2)
  legend('top', bty = 'n',
         lwd = 2, lty = 1, cex = .6,
         col = c('black', 'red', 'navy'),
         legend = c("Betabinomial",
                    "Posthoc: unsmoothed",
                    "Posthoc: smoothed"))
  
  plot(NA, 
       xlim = c(1992,2000),
       ylim = c(.75,1.25),
       xlab = "Year",
       ylab = "Benchmark")
  abline(h=1)
  
  lines(beg.year:end.year,
        bench.adj$ratio, lwd = 2,
        col = 'black')
  lines(beg.year:end.year,
        med_agg_natl_draws$bench, lwd = 2,
        col = 'red')
  
  lines(beg.year:end.year,
        med_agg_natl_draws$smoothed_bench,
        col = 'navy', lwd = 2)
}
dev.off()
# restore the aggregated results to the national results
load(paste0(folder.name,'/', country, '_res_rw2_natl.rda'))
res.natl.smoothbench <- 
  res.natl.unsmoothbench <- res.natl
res.natl.smoothbench$overall$median <- med_agg_natl_draws$u5mr/med_agg_natl_draws$smoothed_bench
res.natl.smoothbench$overall$upper <- med_agg_natl_draws$u5mr_upper/med_agg_natl_draws$smoothed_bench
res.natl.smoothbench$overall$lower <- med_agg_natl_draws$u5mr_lower/med_agg_natl_draws$smoothed_bench
save(res.natl.smoothbench,
     file = paste0(folder.name,'/', country, '_res_rw2_natlED_SmoothedBench_CI90.rda'))

res.natl.unsmoothbench$overall$median <- med_agg_natl_draws$u5mr/med_agg_natl_draws$bench
res.natl.unsmoothbench$overall$upper <- med_agg_natl_draws$u5mr_upper/med_agg_natl_draws$bench
res.natl.unsmoothbench$overall$lower <- med_agg_natl_draws$u5mr_lower/med_agg_natl_draws$bench
save(res.natl.unsmoothbench,
     file = paste0(folder.name,'/', country, '_res_rw2_natlED_unSmoothedBench_CI90.rda'))

load(paste0(folder.name, '/',
            country, '_directHIV_natl_yearly.rda'))
pdf(paste0(folder.name, '/Plots/',
           country, '_natlEDBench_compare.pdf'),
    width = 8, height = 4)
{
  svy.cols <- rainbow(length(surveys))
  par(mfrow = c(1,2), lend = 1)
  y.lim <- range(c(res.natl.smoothbench$overall$median,
                   res.natl.unsmoothbench$overall$median),
                 na.rm = TRUE)*1000 + c(-5,5)
  tmp.direct <- direct.natl.yearly
  plot(NA,
       xlim = c(beg.year,end.year-1),
       ylim = y.lim,
       xlab = "Year",
       ylab = "U5MR")
  for(svy in surveys){
    points(tmp.direct$years[tmp.direct$surveyYears == svy],
           tmp.direct$mean[tmp.direct$surveyYears == svy]*1000,
           pch = 16, col = svy.cols[match(svy, surveys)])
  }
  lines(beg.year:end.year,
        res.natl.unsmoothbench$overall$median*1000,
        lwd = 2, col = 'firebrick')
  polygon(c(end.year:beg.year,
            beg.year:end.year),
          c(rev(res.natl.unsmoothbench$overall$lower),
            (res.natl.unsmoothbench$overall$upper))*1000,
          col = alpha('firebrick', 0.25), border = FALSE)
  lines(c(beg.year:end.year)[1:30],
        res.natl.smoothbench$overall$median[1:30]*1000,
        lwd = 2, col = 'navy')
  polygon(c(c(end.year:beg.year)[-1],
            c(beg.year:end.year)[1:30]),
          c(rev(res.natl.smoothbench$overall$lower[1:30]),
            (res.natl.smoothbench$overall$upper[1:30]))*1000,
          col = alpha('navy', 0.25), border = FALSE)
  lines(beg.year:end.year,
        igme.ests$OBS_VALUE[match(beg.year:end.year,
                                  igme.ests$year)],
        lwd = 2, lty = 2)

  plot(NA,
       xlim = c(1992,2000),
       ylim = y.lim,
       xlab = "Year",
       ylab = "U5MR")
  
  for(svy in surveys){
    points(tmp.direct$years[tmp.direct$surveyYears == svy],
           tmp.direct$mean[tmp.direct$surveyYears == svy]*1000,
           pch = 16, col = svy.cols[match(svy, surveys)])
  }
  lines(beg.year:end.year,
        res.natl.unsmoothbench$overall$median*1000,
        lwd = 2, col = 'firebrick')
  polygon(c(end.year:beg.year,
            beg.year:end.year),
          c(rev(res.natl.unsmoothbench$overall$lower),
            (res.natl.unsmoothbench$overall$upper))*1000,
          col = alpha('firebrick', 0.25), border = FALSE)
  lines(c(beg.year:end.year)[1:30],
        res.natl.smoothbench$overall$median[1:30]*1000,
        lwd = 2, col = 'navy')
  polygon(c(c(end.year:beg.year)[-1],
            c(beg.year:end.year)[1:30]),
          c(rev(res.natl.smoothbench$overall$lower[1:30]),
            (res.natl.smoothbench$overall$upper[1:30]))*1000,
          col = alpha('navy', 0.25), border = FALSE)
  lines(beg.year:end.year,
        igme.ests$OBS_VALUE[match(beg.year:end.year,
                                  igme.ests$year)],
        lwd = 2, lty = 2)
  legend('bottom',
         lty = c(1,1,2,
                 rep(NA, length(surveys))),
         pch = c(rep(NA,3),
                 rep(16, length(surveys))),
         lwd = 2, bty = 'n',
         cex = 0.6,
         col = c('navy',
                 'firebrick',
                 'black',
                 svy.cols),
         legend = c('Post-hoc: smoothed',
                    'Post-hoc: unsmoothed',
                    'IGME', survey.legends))
}
dev.off()
bench.adj$est <- res.natl$overall$median
bench.adj$ratio <- med_agg_natl_draws$bench
save(bench.adj, file = paste0(folder.name,'/', country, '_',
                              time.mod, '_natlEDBenchmarks.rda'))
load(file = paste0(folder.name,'/', country, '_res_rw2_natlEDBenchCI90.rda'))
load(file = paste0(folder.name,'/', country, '_', time.mod, '_natlEDBenchmarks.rda'))
res.natl$overall$median <- res.natl$overall$median*bench.adj$ratio
res.natl$overall$upper <- res.natl$overall$upper*bench.adj$ratio
res.natl$overall$lower <- res.natl$overall$lower*bench.adj$ratio
save(res.natl, file = paste0( folder.name, '/',
                              country, '_res_',
                              time.mod, '_natlEDCI90.rda'))


summary_adj_tbl <- draws_tbl %>%
  left_join(med_agg_natl_draws %>% dplyr::select(-u5mr),
            by = "years") %>%
  group_by(years, region) %>%
  dplyr::summarize(upper_smooth = quantile(adj_u5mr / smoothed_bench, .95,
                                           na.rm = TRUE),
                   lower_smooth = quantile(adj_u5mr / smoothed_bench, .05,
                                           na.rm = TRUE),
                   median_smooth = median(adj_u5mr / smoothed_bench,
                                          na.rm = TRUE),
                   upper = quantile(adj_u5mr/bench , .95,
                                    na.rm = TRUE),
                   lower = quantile(adj_u5mr/bench , .05,
                                    na.rm = TRUE),
                   median = median(adj_u5mr/bench,
                                   na.rm = TRUE))

load(paste0(folder.name, '/',
            country, '_directHIV_admin2.rda'))

res.admin2$overall <- res.admin2$overall %>%
  dplyr::select(-c(median, lower, upper)) %>%
  left_join(summary_adj_tbl, by = c("years", "region"))

svy.cols <- rainbow(length(surveys))
pdf(paste0(folder.name, '/',
           country, '_admin2_benched_compare.pdf'),
    height = 8, width = 8)
{
  par(mfrow = c(2,2),
      lend = 1)
  y.lim <- range(summary_adj_tbl[,-c(1:2)],
                 na.rm = TRUE)*1000
  for(area in admin2.names$Internal){
    area.title <- admin2.names$GADM[match(area,
                                          admin2.names$Internal)]
    tmp.direct <- direct.admin2[direct.admin2$region == as.character(area),]
    tmp <- summary_adj_tbl[summary_adj_tbl$region == area,]
    plot(NA,
         xlim = c(beg.year, 2019),
         ylim = y.lim,
         xlab = "Year",
         ylab = "U5MR",
         main = area.title)
    
    for(svy in surveys){
      pane.years <- rep(NA, length(periods)-1)
      
      for(idx in 1:(length(periods)-1)){
        id.yr <- seq(beg.year,end.year-5,5)[idx]
        dat.in <- id.yr:(id.yr + 4) %in% 
          mod.dat$years[mod.dat$survey == svy]
        if(sum(dat.in) != 0){
          pane.years[idx] <- mean((id.yr:(id.yr + 4))[dat.in])
        }
      }
      points(pane.years,
             tmp.direct$mean[tmp.direct$surveyYears == svy]*1000,
             pch = 16, col = svy.cols[match(svy, surveys)])
    }
    lines(beg.year:end.year,
          tmp$median_smooth*1000,
          col = 'navy', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower_smooth),
              tmp$upper_smooth)*1000,
            border = FALSE,
            col = alpha('navy', 0.25))
    
    lines(beg.year:end.year,
          tmp$median*1000,
          col = 'black', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower),
              tmp$upper)*1000,
            border = FALSE,
            col = alpha('black', 0.25))
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year,
                                    igme.ests$year)],
          lwd = 2, lty = 2)
    
    legend('topright',
           lwd = 2, cex = .65,
           pch = c(rep(NA, 3),
                   rep(16, length(surveys))),
           lty = c(1,1,2,
                   rep(NA, length(surveys))),
           col = c('navy', 'black', 'black',
                   svy.cols),
           legend = c('Posthoc: smoothed',
                      'Posthoc: unsmoothed',
                      'IGME',
                      survey.legends),
           bty = 'n')
    plot(NA,
         xlim = c(1992,2000),
         ylim = y.lim,
         xlab = "Year",
         ylab = "U5MR")
    
    for(svy in surveys){
      pane.years <- rep(NA, length(periods)-1)
      
      for(idx in 1:(length(periods)-1)){
        id.yr <- seq(beg.year,end.year-5,5)[idx]
        dat.in <- id.yr:(id.yr + 4) %in% 
          mod.dat$years[mod.dat$survey == svy]
        if(sum(dat.in) != 0){
          pane.years[idx] <- mean((id.yr:(id.yr + 4))[dat.in])
        }
      }
      points(pane.years,
             tmp.direct$mean[tmp.direct$surveyYears == svy]*1000,
             pch = 16, col = svy.cols[match(svy, surveys)])
    }
    lines(beg.year:end.year,
          tmp$median_smooth*1000,
          col = 'navy', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower_smooth),
              tmp$upper_smooth)*1000,
            border = FALSE,
            col = alpha('navy', 0.25))
    
    lines(beg.year:end.year,
          tmp$median*1000,
          col = 'black', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower),
              tmp$upper)*1000,
            border = FALSE,
            col = alpha('black', 0.5))
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year,
                                    igme.ests$year)],
          lwd = 2, lty = 2)
    
    
  }
}
dev.off()

################################################################################
#### ADMIN 2 EXCESS DEATH ADJUSTMENT                                        ####
################################################################################

#### SAVE ADJUSTED RESULTS ####
save(res.admin2, file = paste0( folder.name, '/',
                                country, '_res_',
                                time.mod,
                                '_admin2EDBenchCI90.rda'))
write.csv(res.admin2$overall, row.names = FALSE, file = paste0( folder.name, '/',
                                     country, '_res_',
                                     time.mod,
                                     '_admin2EDBenchCI90.csv'))
bench.adj$est <-  med_agg_natl_draws$u5mr
bench.adj$ratio <-  med_agg_natl_draws$bench
save(bench.adj, file = paste0(folder.name,'/', country, '_', 
                              time.mod, '_admin2EDBenchmarks.rda'))
load(file = paste0( folder.name, '/',country, '_res_',
                    time.mod, '_admin2EDBenchCI90.rda'))
load(file = paste0(folder.name,'/', country, '_', time.mod, 
                   '_admin2EDBenchmarks.rda'))
res.admin2$overall$median <- res.admin2$overall$median*bench.adj$ratio
res.admin2$overall$upper <- res.admin2$overall$upper*bench.adj$ratio
res.admin2$overall$lower <- res.admin2$overall$lower*bench.adj$ratio
save(res.admin2, file = paste0( folder.name, '/',
                                country,
                                '_res_', time.mod,
                                '_admin2EDCI90.rda'))

################################################################################
#### ADMIN 1 EXCESS DEATH ADJUSTMENT                                        ####
################################################################################


tbl_1_5 <- read.csv(paste0(ed_data_dir, "Rwanda_admin2_U1_5.csv")) %>%
  dplyr::select(-X) %>%
  mutate(region = paste0("admin2_", 1:length(unique(names)))) %>%
  pivot_longer(names_to = "year", values_to = "pop_1_5", -c(names, region)) %>%
  mutate(year = as.numeric(str_sub(year, 5, -1)))

tbl_0_1 <- read.csv(paste0(ed_data_dir, "Rwanda_admin2_U0_1.csv")) %>%
  dplyr::select(-X) %>%
  pivot_longer(names_to = "year", values_to = "pop_0_1", -c(names)) %>%
  mutate(year = as.numeric(str_sub(year, 5, -1)))

# excess death mortality rate -- to be computed!
edmr_tbl <- tbl_1_5 %>%
  left_join(tbl_0_1, by = c("names", "year")) %>%
  mutate(pop_0_5 = pop_1_5 + pop_0_1)


# match up admin 1 and admin 2 names
load(paste0(folder.name, '/',
            "Rwanda_cluster_dat.rda"))
load(paste0(folder.name, '/',
            "Rwanda_Amat_names.rda"))
lookup_adm1_adm2 <- mod.dat %>%
  group_by(admin2.name, admin1.name) %>%
  summarize() %>%
  ungroup()

# affected admin 1 indices
affected_admin_1_ind <- c(1:5)
affected_admin_1_names <- 
  admin1.names$GADM[affected_admin_1_ind] %>%
  as.character()

edmr_tbl <- edmr_tbl %>%
  mutate(names = as.character(names)) %>%
  rename(admin2.name = names) %>%
  left_join(lookup_adm1_adm2, by = "admin2.name") %>%
  dplyr::select(-admin2.name, -region) %>%
  group_by(admin1.name, year) %>% 
  summarize_all(sum) %>%
  left_join(admin1.names %>% rename(admin1.name = GADM), by = "admin1.name") %>%
  rename(region = Internal) %>%
  ungroup()
#### U5 POPULATION HINDCAST ####
# hindcasts of u5 population -- need to do this more intelligently
for (yr in 1990:1999) {
  rows_to_add <- edmr_tbl %>%
    filter(year == 2000) %>%
    mutate(year = yr)
  edmr_tbl <- bind_rows(edmr_tbl, rows_to_add)
}

edmr_tbl <- edmr_tbl %>%
  mutate(crisis = (year %in% 1993:1999) & (admin1.name %in% affected_admin_1_names))

region_id <- as.character(edmr_tbl$region) %>% strsplit(split = "_") %>% sapply(function(x) x[2])
edmr_tbl <- edmr_tbl %>% mutate(region_id = as.numeric(region_id))
edmr_tbl <- edmr_tbl %>% arrange(year)
edmr_tbl <- edmr_tbl %>% arrange(region_id)

area.cols <- rainbow(length(affected_admin_1_names))

pdf(paste0(folder.name, '/Plots/',
           country, '_admin1_u5_pop.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1),
      lend = 1)
  
  y.lim <- range(edmr_tbl$pop_0_5/1000)
  
  plot(NA,
       xlim = c(beg.year, end.year),
       ylim = y.lim,
       xlab = "Year",
       ylab = 'Population under age 5')
  
  index <- 0
  for(area in affected_admin_1_names){
    index <- index + 1
    lines(edmr_tbl$year[edmr_tbl$admin1.name == area],
          edmr_tbl$pop_0_5[edmr_tbl$admin1.name == area]/1000,
          lwd = 2, col = area.cols[index])
  }
  legend('topleft', 
         lwd = 2, ncol = 1,
         col = area.cols,
         legend = affected_admin_1_names,
         bty = 'n', cex = 0.7)
}
dev.off()

# get national U5 excess deaths 
natl_u5_ed <-  c(rep(0,3),igme_crisis_df$Crisis.d0.5[igme_crisis_df$Year %in% (c(1993:1999)+0.5) &
                                                       igme_crisis_df$ISO3Code == "RWA"],rep(0,21)) %>% rep(each = 5)


yearsum_0_1 <- edmr_tbl %>% group_by(year) %>% summarise(pop_0_1_yearsum = sum(pop_0_1))
edmr_tbl <- yearsum_0_1 %>% left_join(edmr_tbl, by =c("year"))
yearsum_1_5 <- edmr_tbl %>% group_by(year) %>% summarise(pop_1_5_yearsum = sum(pop_1_5))
edmr_tbl <- yearsum_1_5 %>% left_join(edmr_tbl, by =c("year"))

# compute excess death mortality rates from 2008 cyclone
edmr_tbl <- edmr_tbl %>%
  mutate(ed_0_1 = (0.2) * natl_u5_ed * pop_0_1 * crisis / pop_0_1_yearsum * crisis,
         ed_1_5 = (0.8) * natl_u5_ed * pop_1_5 * crisis / pop_1_5_yearsum * crisis) %>%
  mutate(ed_1m0 = ed_0_1 / pop_0_1,
         ed_4m1 = ed_1_5 / pop_1_5) %>%
  mutate(ed_1q0 = ed_1m0 / (1 + (1 - (0.3)) * ed_1m0),
         ed_4q1 = 4 * ed_4m1 / (1 + (4 - 4 * (0.4)) * ed_4m1)) %>%
  mutate(ed_5q0 = 1 - (1 - ed_1q0) * (1 - ed_4q1))

pdf(paste0(folder.name,'/Plots/',
           country, '_admin1_ED.pdf'),
    height = 8, width = 8)

{
  par(mfrow=c(2,2),
      lend = 1)
  
  y.lim <- range(edmr_tbl$ed_0_1 + 
                   edmr_tbl$ed_1_5)
  plot(NA,
       xlim = c(1992,2000),
       ylim = y.lim,
       xlab = "Year",
       ylab = 'Excess deaths under age 5')
  abline(h = 0)
  index <- 0
  for(area in affected_admin_1_names){
    index <- index + 1
    points(edmr_tbl$year[edmr_tbl$admin1.name == area],
           edmr_tbl$ed_0_1[edmr_tbl$admin1.name == area] +
             edmr_tbl$ed_1_5[edmr_tbl$admin1.name == area],
           lwd = 1, pch = 16, col = area.cols[index])
  }
  # points(beg.year:end.year,
  #       edmr_tbl$ed_5q0[edmr_tbl$names == area],
  #       lwd = 2, lty = 2)
  legend('topright', 
         pch = 16,
         ncol = 1,
         col = c(area.cols),
         legend = c(affected_admin_1_names),
         bty = 'n', cex = 0.4)
  
  plot(NA,
       xlim = c(1992,2000),
       ylim = c(-1,1),
       xlab = "Year",
       ylab = 'Excess deaths under age 5')
  abline(h = 0)
  index <- 0
  for(area in affected_admin_1_names){
    index <- index + 1
    points(edmr_tbl$year[edmr_tbl$admin1.name == area],
           edmr_tbl$ed_0_1[edmr_tbl$admin1.name == area] +
             edmr_tbl$ed_1_5[edmr_tbl$admin1.name == area],
           lwd = 1,pch = 16, col = area.cols[index])
  }
  # points(beg.year:end.year,
  #       edmr_tbl$ed_5q0[edmr_tbl$names == area],
  #       lwd = 2, lty = 2)
  
  y.lim <- range(natl_u5_ed[seq(1,155,5)])
  plot(NA,
       xlim = c(1992,2000),
       ylim = y.lim,
       xlab = "Year",
       ylab = 'Excess deaths under age 5')
  abline(h = 0)
  
  lines(beg.year:end.year,
        natl_u5_ed[seq(1,155,5)],
        pch = 16, lty = 2, type = 'b')
  
  legend('topright', 
         pch = 16,lty = 2,
         ncol = 1, col = 'black',
         legend = "National",
         bty = 'n', cex = 1)
  plot(NA,
       xlim = c(1992,2000),
       ylim = c(-5, 20),
       xlab = "Year",
       ylab = 'Excess deaths under age 5')
  abline(h = 0)
  
  lines(beg.year:end.year,
        natl_u5_ed[seq(1,155,5)],
        pch = 16, type = 'b', lty =2 )
  legend('topright', 
         pch = 16,
         ncol = 1, col = 'black',
         legend = "National",
         bty = 'n', cex = 1)
}
dev.off()
#### LOAD UNADJUSTED RESULTS ####
# load res.admin1 object from .rda 
load(paste0(folder.name,'/', country,
            '_res_', time.mod, '_admin1.rda'))
load(paste0(folder.name,'/', country, '_', 
            time.mod, '_admin1Benchmarks.rda'))

# pull out all the unadjusted draws and reshape
draws_mat <- do.call(rbind, lapply(res.admin1$draws.est, function(x) x$draws))
colnames(draws_mat) <- paste0("draw_", 1:1000)
draws_tbl <- as_tibble(draws_mat) %>%
  mutate(years = as.factor(sapply(res.admin1$draws.est, 
                                  function(x) x$years)), 
         region = sapply(res.admin1$draws.est, function(x) x$region))

edmr_tbl$region <- as.character(edmr_tbl$region)

# adjust draws for crisis EDMR
draws_tbl <- res.admin1$overall %>%
  dplyr::select(region, years) %>%
  left_join(draws_tbl, by = c("years", "region")) %>%
  left_join(mutate(edmr_tbl, years = as.factor(year)) %>%
              dplyr::select(region, years, ed_5q0, pop_0_5),
            by = c("years", "region")) %>%
  pivot_longer(starts_with("draw"), names_to = "draw",
               values_to = "u5mr")  %>%
  mutate(adj_u5mr =  ed_5q0 + u5mr) 

pdf(paste0(folder.name,'/Plots/',
           country, '_admin1_EDvsunadjusted.pdf'),
    width = 5, height = 5)
{
  par(mfrow=c(1,1),
      par(lend = 1))
  
  plot(NA,
       xlim = c(100,400),
       ylim = c(100,400),
       xlab = "Unadjusted, unbenchmarked U5MR",
       ylab = "Unbenchmarked U5MR + ED Adjustment")
  abline(0,1, lty = 2)
  yr.cols <- rainbow(length(1992:2000))
  for(yr in 1992:2000){
    before <- after <- rep(NA, length(admin1.names$Internal))
    for(area in admin1.names$Internal){
      area.idx <- match(area, admin1.names$Internal)
      before[area.idx] <- median(1000*draws_tbl$u5mr[draws_tbl$years == yr &
                                                       draws_tbl$region == area])
      after[area.idx] <- median(1000*draws_tbl$adj_u5mr[draws_tbl$years == yr &
                                                          draws_tbl$region == area])
    }
    points(before,
           after,
           pch = 16, col = yr.cols[match(yr,
                                         1992:2000)])
  }
  legend('bottomright',
         pch = 16,
         col = yr.cols,
         legend = 1992:2000,
         bty = 'n')
}
dev.off()

# change draws.est slot to store adjusted results
for (i in 1:length(res.admin1$draws.est)) {
  yr <- res.admin1$draws.est[[i]]$years
  reg <- res.admin1$draws.est[[i]]$region
  res.admin1$draws.est[[i]]$draws <- (draws_tbl %>% filter(years == yr & region == reg))$adj_u5mr
}

# aggregate admin 1 U5MR to get national U5MR for all years
agg_natl_draws_tbl <- draws_tbl %>%
  group_by(years, draw) %>%
  dplyr::summarize(u5mr_unadj = sum(pop_0_5*u5mr)/sum(pop_0_5),
                   u5mr = sum(pop_0_5 * adj_u5mr) / sum(pop_0_5))

# compute median of national U5MR estimates
med_agg_natl_draws <- agg_natl_draws_tbl %>%
  group_by(years) %>%
  dplyr::summarize(u5mr = median(u5mr),
                   u5mr_unadj = median(u5mr_unadj)) %>%
  mutate(bench = u5mr * 1000 / igme.ests$OBS_VALUE[match(years,
                                                         igme.ests$year)],
         bench_unadj = u5mr_unadj * 1000/igme.ests$OBS_VALUE[match(years,
                                                                   igme.ests$year)])
# calculate post hoc benchmarks by comparing with IGME

# Smooth out benchmarks and forecast for 2016-2020 using RW2 model
smoothed_bench <- med_agg_natl_draws  
#  use data in 1990 - 1992
smoothed_bench$bench[smoothed_bench$years %in% 1993:1999] <- NA
smoothed_bench$bench[(nrow(smoothed_bench) - 4):nrow(smoothed_bench)] <- NA
smoothed_res <- inla(bench ~ f(as.numeric(years), model = "rw2"), data = smoothed_bench,
                     control.predictor=list(compute=TRUE))
med_agg_natl_draws$smoothed_bench = smoothed_res$summary.linear.predictor$mean

pdf(paste0(folder.name, '/',
           country, '_admin1_benchmarks_compare.pdf'),
    width = 8, height = 4)
{
  par(mfrow = c(1,2),
      lend = 1)
  plot(NA, 
       xlim = c(beg.year, end.year),
       ylim = c(.5,1.5),
       xlab = "Year",
       ylab = "Benchmark")
  abline(h=1)
  
  lines(beg.year:end.year,
        bench.adj$ratio, lwd = 2,
        col = 'black')
  lines(beg.year:end.year,
        med_agg_natl_draws$bench, lwd = 2,
        col = 'red')
  lines(beg.year:end.year,
        med_agg_natl_draws$smoothed_bench,
        col = 'navy', lwd = 2)
  legend('top', bty = 'n',
         lwd = 2, lty = c(1,1,2,1), cex = .6,
         col = c('black', 'red', 'navy'),
         legend = c("Betabinomial",
                    "Posthoc: unsmoothed",
                    "Posthoc: smoothed"))
  
  plot(NA, 
       xlim = c(1992,2000),
       ylim = c(.75,1.25),
       xlab = "Year",
       ylab = "Benchmark")
  abline(h=1)
  
  lines(beg.year:end.year,
        bench.adj$ratio, lwd = 2,
        col = 'black')
  lines(beg.year:end.year,
        med_agg_natl_draws$bench, lwd = 2,
        col = 'red')
  lines(beg.year:end.year,
        med_agg_natl_draws$smoothed_bench,
        col = 'navy', lwd = 2)
}
dev.off()

load(paste0(folder.name,'/', country, '_res_rw2_natl.rda'))
res.natl.smoothbench <- 
  res.natl.unsmoothbench <- res.natl
res.natl.smoothbench$overall$median <- med_agg_natl_draws$u5mr/med_agg_natl_draws$smoothed_bench
res.natl.unsmoothbench$overall$median <- med_agg_natl_draws$u5mr/med_agg_natl_draws$bench

pdf(paste0(folder.name, '/Plots/',
           country, '_natlEDBench_admin1_compare.pdf'),
    width = 8, height = 4)
{
  par(mfrow = c(1,2), lend = 1)
  y.lim <- range(c(res.natl.smoothbench$overall$median,
                   res.natl.unsmoothbench$overall$median),
                 na.rm = TRUE)*1000 + c(-5,5)
  
  plot(NA,
       xlim = c(beg.year,end.year-1),
       ylim = y.lim,
       xlab = "Year",
       ylab = "U5MR")
  
  lines(beg.year:end.year,
        res.natl.unsmoothbench$overall$median*1000,
        lwd = 2, col = 'red')
  lines(c(beg.year:end.year)[1:30],
        res.natl.smoothbench$overall$median[1:30]*1000,
        lwd = 2, col = 'blue')
  lines(beg.year:end.year,
        igme.ests$OBS_VALUE[match(beg.year:end.year,
                                  igme.ests$year)],
        lwd = 2, lty = 2)
  legend('topright',
         lty = c(1,1,2),
         lwd = 2, bty = 'n',
         cex = 0.8,
         col = c('blue',
                 'red',
                 'black'),
         legend = c('Post-hoc: smoothed',
                    'Post-hoc: unsmoothed',
                    'IGME'))
  plot(NA,
       xlim = c(1992,2000),
       ylim = y.lim,
       xlab = "Year",
       ylab = "U5MR")
  
  lines(beg.year:end.year,
        res.natl.unsmoothbench$overall$median*1000,
        lwd = 2, col = 'red')
  lines(c(beg.year:end.year)[1:30],
        res.natl.smoothbench$overall$median[1:30]*1000,
        lwd = 2, col = 'blue')
  lines(beg.year:end.year,
        igme.ests$OBS_VALUE[match(beg.year:end.year,
                                  igme.ests$year)],
        lwd = 2, lty = 2)
}
dev.off()


summary_adj_tbl <- draws_tbl %>%
  left_join(med_agg_natl_draws %>% dplyr::select(-u5mr), by = "years") %>%
  group_by(years, region) %>%
  dplyr::summarize(upper_smooth = quantile(adj_u5mr / smoothed_bench, .95,
                                           na.rm = TRUE),
                   lower_smooth = quantile(adj_u5mr / smoothed_bench, .05,
                                           na.rm = TRUE),
                   median_smooth = median(adj_u5mr / smoothed_bench,
                                          na.rm = TRUE),
                   upper = quantile(adj_u5mr / bench, .95,
                                    na.rm = TRUE),
                   lower = quantile(adj_u5mr / bench, .05,
                                    na.rm = TRUE),
                   median = median(adj_u5mr / bench,
                                   na.rm = TRUE))

load(paste0(folder.name, '/',
            country, '_directHIV_admin1.rda'))
pdf(paste0(folder.name, '/',
           country, '_admin1_benched_compare.pdf'),
    height = 8, width = 8)
{
  par(mfrow = c(1,2),
      lend = 1)
  y.lim <- range(summary_adj_tbl[,-c(1:2)],
                 na.rm = TRUE)*1000
  for(area in admin1.names$Internal){
    area.title <- admin1.names$GADM[match(area,
                                          admin1.names$Internal)]
    tmp.direct <- direct.admin1[direct.admin1$region == as.character(area),]
    tmp <- summary_adj_tbl[summary_adj_tbl$region == area,]
    plot(NA,
         xlim = c(beg.year, 2019),
         ylim = y.lim,
         xlab = "Year",
         ylab = "U5MR",
         main = area.title)
    
    for(svy in surveys){
      pane.years <- rep(NA, length(periods)-1)
      
      for(idx in 1:(length(periods)-1)){
        id.yr <- seq(beg.year,end.year-5,5)[idx]
        dat.in <- id.yr:(id.yr + 4) %in% 
          mod.dat$years[mod.dat$survey == svy]
        if(sum(dat.in) != 0){
          pane.years[idx] <- mean((id.yr:(id.yr + 4))[dat.in])
        }
      }
      points(pane.years,
             tmp.direct$mean[tmp.direct$surveyYears == svy]*1000,
             pch = 16, col = svy.cols[match(svy, surveys)])
    }
    lines(beg.year:end.year,
          tmp$median_smooth*1000,
          col = 'navy', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower_smooth),
              tmp$upper_smooth)*1000,
            border = FALSE,
            col = alpha('navy', 0.25))
    
    lines(beg.year:end.year,
          tmp$median*1000,
          col = 'black', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower),
              tmp$upper)*1000,
            border = FALSE,
            col = alpha('black', 0.25))
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year,
                                    igme.ests$year)],
          lwd = 2, lty = 2)
    
    legend('topright',
           lwd = 2, cex = .65,
           pch = c(rep(NA, 3),
                   rep(16, length(surveys))),
           lty = c(1,1,2,
                   rep(NA, length(surveys))),
           col = c('navy', 'black', 'black',
                   svy.cols),
           legend = c('Posthoc: smoothed',
                      'Posthoc: unsmoothed',
                      'IGME',
                      survey.legends),
           bty = 'n')
    plot(NA,
         xlim = c(1992,2000),
         ylim = y.lim,
         xlab = "Year",
         ylab = "U5MR")
    
    for(svy in surveys){
      pane.years <- rep(NA, length(periods)-1)
      
      for(idx in 1:(length(periods)-1)){
        id.yr <- seq(beg.year,end.year-5,5)[idx]
        dat.in <- id.yr:(id.yr + 4) %in% 
          mod.dat$years[mod.dat$survey == svy]
        if(sum(dat.in) != 0){
          pane.years[idx] <- mean((id.yr:(id.yr + 4))[dat.in])
        }
      }
      points(pane.years,
             tmp.direct$mean[tmp.direct$surveyYears == svy]*1000,
             pch = 16, col = svy.cols[match(svy, surveys)])
    }
    lines(beg.year:end.year,
          tmp$median_smooth*1000,
          col = 'navy', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower_smooth),
              tmp$upper_smooth)*1000,
            border = FALSE,
            col = alpha('navy', 0.25))
    
    lines(beg.year:end.year,
          tmp$median*1000,
          col = 'black', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower),
              tmp$upper)*1000,
            border = FALSE,
            col = alpha('black', 0.25))
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year,
                                    igme.ests$year)],
          lwd = 2, lty = 2)
    
    
  }
}
dev.off()

pdf(paste0(folder.name, '/',
           country, '_admin1ED_unsmoothedBench.pdf'),
    height = 8, width = 8)
{
  par(mfrow = c(2,2),
      lend = 1)
  y.lim <- range(summary_adj_tbl[,-c(1:2)],
                 na.rm = TRUE)*1000
  for(area in admin1.names$Internal){
    area.title <- admin1.names$GADM[match(area,
                                          admin1.names$Internal)]
    tmp.direct <- direct.admin1[direct.admin1$region == as.character(area),]
    tmp <- summary_adj_tbl[summary_adj_tbl$region == area,]
    plot(NA,
         xlim = c(beg.year, 2019),
         ylim = y.lim,
         xlab = "Year",
         ylab = "U5MR",
         main = area.title)
    
    for(svy in surveys){
      pane.years <- rep(NA, length(periods)-1)
      
      for(idx in 1:(length(periods)-1)){
        id.yr <- seq(beg.year,end.year-5,5)[idx]
        dat.in <- id.yr:(id.yr + 4) %in% 
          mod.dat$years[mod.dat$survey == svy]
        if(sum(dat.in) != 0){
          pane.years[idx] <- mean((id.yr:(id.yr + 4))[dat.in])
        }
      }
      points(pane.years,
             tmp.direct$mean[tmp.direct$surveyYears == svy]*1000,
             pch = 16, col = svy.cols[match(svy, surveys)])
    }
    
    lines(beg.year:end.year,
          tmp$median*1000,
          col = 'black', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower),
              tmp$upper)*1000,
            border = FALSE,
            col = alpha('black', 0.25))
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year,
                                    igme.ests$year)],
          lwd = 2, lty = 2)
    
    legend('topright',
           lwd = 2, cex = .65,
           pch = c(rep(NA, 2),
                   rep(16, length(surveys))),
           lty = c(1,2,
                   rep(NA, length(surveys))),
           col = c('black', 'black',
                   svy.cols),
           legend = c('Posthoc: unsmoothed',
                      'IGME',
                      survey.legends),
           bty = 'n')
    plot(NA,
         xlim = c(1992,2000),
         ylim = y.lim,
         xlab = "Year",
         ylab = "U5MR")
    
    for(svy in surveys){
      pane.years <- rep(NA, length(periods)-1)
      
      for(idx in 1:(length(periods)-1)){
        id.yr <- seq(beg.year,end.year-5,5)[idx]
        dat.in <- id.yr:(id.yr + 4) %in% 
          mod.dat$years[mod.dat$survey == svy]
        if(sum(dat.in) != 0){
          pane.years[idx] <- mean((id.yr:(id.yr + 4))[dat.in])
        }
      }
      points(pane.years,
             tmp.direct$mean[tmp.direct$surveyYears == svy]*1000,
             pch = 16, col = svy.cols[match(svy, surveys)])
    }
    
    lines(beg.year:end.year,
          tmp$median*1000,
          col = 'black', lwd = 2)
    polygon(c(end.year:beg.year,
              beg.year:end.year),
            c(rev(tmp$lower),
              tmp$upper)*1000,
            border = FALSE,
            col = alpha('black', 0.25))
    lines(beg.year:end.year,
          igme.ests$OBS_VALUE[match(beg.year:end.year,
                                    igme.ests$year)],
          lwd = 2, lty = 2)

  }
}
dev.off()

res.admin1$overall <- res.admin1$overall %>%
  dplyr::select(-c(median, lower, upper)) %>%
  left_join(summary_adj_tbl, by = c("years", "region"))


#### SAVE ADJUSTED RESULTS ####
save(res.admin1, file = paste0(folder.name,'/', country,
                               '_res_', time.mod,
                               '_admin1EDBenchCI90.rda'))
write.csv(res.admin1$overall, file = paste0(folder.name,'/', country,
                                            '_res_', time.mod,
                                            '_admin1EDBenchCI90.csv'),
          row.names = FALSE)
bench.adj$est <-  med_agg_natl_draws$u5mr
bench.adj$ratio <-  med_agg_natl_draws$bench
save(bench.adj, file = paste0(folder.name,'/', country, '_', 
                              time.mod, '_admin1EDBenchmarks.rda'))

load(file = paste0( folder.name, '/',country, '_res_',
                    time.mod, '_admin1EDBenchCI90.rda'))
load(file = paste0(folder.name,'/', country, '_',
                   time.mod, '_admin1EDBenchmarks.rda'))
res.admin1$overall$median <- res.admin1$overall$median*bench.adj$ratio
res.admin1$overall$upper <- res.admin1$overall$upper*bench.adj$ratio
res.admin1$overall$lower <- res.admin1$overall$lower*bench.adj$ratio
save(res.admin1, file = paste0( folder.name, '/',
                                country, '_res_',
                                time.mod, '_admin1EDCI90.rda'))

load(file = paste0( folder.name, '/',country, '_res_',
                    time.mod, '_admin1EDBenchCI90.rda'))
load(file = paste0( folder.name, '/',country, '_res_',
                    time.mod, '_admin2EDBenchCI90.rda'))
y.lim <- range(c(res.admin1$overall$median,
                 res.admin2$overal$median),
               na.rm = TRUE)*1000
pdf(paste0(folder.name, '/Plots/',
           country, '_IGME_admin1_2_compare.pdf'),
    height = 4, width = 8)
{
  par(mfrow = c(1,2),
      lend = 1)
  plot(NA,
       xlim = c(beg.year,end.year),
       ylim = y.lim,
       xlab = "Year",
       ylab = "U5MR")
  points(res.admin2$overall$years.num,
         res.admin2$overall$median*1000,
         pch = 16, col = alpha('goldenrod', 0.25))
  points(res.admin1$overall$years.num,
         res.admin1$overall$median*1000,
         pch = 16, col = alpha('blue', 0.5))
  points(igme.ests$year,
         igme.ests$OBS_VALUE,
         pch = 16)
  legend('topright',
         pch = 16, bty = 'n',
         col = c('black',
                 alpha('blue', 0.5),
                 alpha('goldenrod', 0.25)),
         legend = c("IGME",
                    "Admin-1",
                    "Admin-2"))
  plot(NA,
       xlim = c(1992,2000),
       ylim = y.lim,
       xlab = "Year",
       ylab = "U5MR")
  points(res.admin2$overall$years.num,
         res.admin2$overall$median*1000,
         pch = 16, col = alpha('goldenrod', 0.25))
  points(res.admin1$overall$years.num,
         res.admin1$overall$median*1000,
         pch = 16, col = alpha('blue', 0.5))
  points(igme.ests$year,
         igme.ests$OBS_VALUE,
         pch = 16)
}
dev.off()

