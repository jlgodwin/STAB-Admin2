#' filename: Betabinomial.R
#' author: Jessica Godwin
#'  
#' sources: LoadCommandCenter.R
#'          IHMEHand_CountryName.R
#'
#' loads: all files in /shapeFiles_gadm 
#'        CountryName_Amat_Names.rda
#'        CountryName_Amat.rda
#'        CountryName_cluster_dat.rda
#'        Data/HIV/HIVAdjustments.rda
#'        CountryName_HIVnames.key.rda
#'        IGME_U5MR_20191007.csv
#'        IHME_LMICS_U5M_2000_2017_Q_UNDER5_ADM0_Y2019M10D16.CSV
#'        IHME_AFRICA_U5M_1998_2017_UNDER5_ADMIN2_Y2017M09D25.CSV
#'        IHME_AFRICA_U5M_1998_2017_UNDER5_ADMIN1_Y2017M09D25.CSV
#'        
#'        For doNatl:
#'        CountryName_directHIV_natl_yearly.rda OR
#'        Countryname_direct_natl_yearly.rda
#'        CountryName_res_natl_yearly_SmoothedDirect.rda
#'        CountryName_res_natlBench_yearly_SmoothedDirect.rda
#'        
#'        For doAdmin1:
#'        CountryName_directHIV_admin1.rda OR
#'        Countryname_direct_admin1.rda
#'        CountryName_res_admin1_SmoothedDirect.rda
#'        
#'        For doAdmin2:
#'        CountryName_directHIV_admin2.rda OR
#'        Countryname_direct_admin2.rda



rm(list = ls())


#####

fitBetabin <- function(country, type.st, beg.year, survey.effect = TRUE,
                       end.year, doBenchmark = NULL, frame.strata,
                       doAdmin2 = TRUE, 
                       doRandomSlopesRW1 = TRUE,
                       doRandomSlopesAR1 = TRUE,
                       doAdmin1 = TRUE, doNatl = TRUE,
                       refit = TRUE,
                       refitBench = TRUE, 
                       loadSamples = FALSE,
                       code.dir.rel, igme.dir.rel,
                       ihme.dir.rel, hand.dir.rel,
                       shapes.sub.dir, hiv.dir.rel,
                       pop.dir.rel){
  
  #### Get country metadata ####
  #CountryList <- sheets_read(sheet_key, sheet = "CountryList")
  CountryList <- read.csv('CountryList.csv', header = T)
  #message(paste0("Starting function for ", country, ".\n"))
  
  folder.name <- CountryList$folderName[CountryList$Country == country]
  gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
  n.survey <- CountryList$nSurvey[CountryList$Country == country]
  
  #### Use HIV Adjusted data? ####
  #HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
  HIV.sheet <- read.csv("HIV.csv", header = T)
  HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
  # useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
  #                unique(HIV.country$`UNAIDS data?`) == "Y")
  useHIVAdj <- (unique(HIV.country$MM.Adj.by.IGME) == "Y" & 
                  unique(HIV.country$UNAIDS.data.) == "Y")
  
  #### Get Survey years #### 
  
  #SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
  #SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
  SurveyInfo <- read.csv('SurveyInfo.csv')
  frames <- SurveyInfo[SurveyInfo$Country == country, c("Survey.Year", "Frame",
                                                        "PropFrame.")]
  #frames <- SurveyInfo[SurveyInfo$Country == country, c("Survey Year", "Frame",
  #                                                      "PropFrame?")]
  #names(frames)[match("Survey Year", names(frames))] <- "Survey.Year"
  
  frames <- frames[!is.na(frames$Frame),]
  #### Load polygon files ####
  
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
  load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat.rda'),
       envir = .GlobalEnv)
  load(paste0(folder.name, shapes.sub.dir, '/', country, '_Amat_Names.rda'),
       envir = .GlobalEnv)
  
  #### Load data ####
  
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
  
  
  
  
  #### Load UNPD data ####
  unpd.prop.urban <- read.csv(paste0(igme.dir.rel,
                                     '/propUrban_UNPD.csv'),
                              skip = 4)
  names(unpd.prop.urban) <- gsub("X","", names(unpd.prop.urban))
  unpd.prop.urban$`2020` <-unpd.prop.urban$`2019` <- unpd.prop.urban$`2018`
  unpd.prop.urban[,-c(1:4)] <- unpd.prop.urban[,-c(1:4)]/100 
  unpd.prop.urban <- unpd.prop.urban[unpd.prop.urban$Country.Name == country,]
  unpd.prop.urban <- unpd.prop.urban[ , c("Country.Name",
                                          "Country.Code",
                                          "Indicator.Name",
                                          "Indicator.Code",
                                          as.character(1990:2020))]
  
  #### Load Props Urban ####
  prop.urban <- read.csv(paste0(pop.dir.rel,
                                '/RW2_', country, '.csv'))
  prop.urban.natl <- read.csv(paste0(pop.dir.rel,
                                     '/RW2_',
                                     country,'_national.csv'))
  #### Frame Strata ####
  if(frame.strata){
    
    if(length(unique(mod.dat$survey)) == 1){
      survey.effect = FALSE
      warning("survey.effect was set to TRUE but there is only 1 survey.  Resetting survey.effect to FALSE")
    }
    
    mod.dat$frame <- NA
    for(i in 1:nrow(frames)){
      year <- frames$Survey.Year[i]
      frame <- as.character(frames$Frame[i])
      mod.dat$frame[mod.dat$survey == year] <- frame
    }
    
    #### National ####
    
    if(doNatl){
      mod.dat$region <- "All"
      
      
      if(useHIVAdj){
        load(paste0(hiv.dir.rel,'/HIVAdjustments.rda'), envir = .GlobalEnv)
        hiv.adj <- hiv.adj[hiv.adj$country == country,]
        if(unique(hiv.adj$area)[1] == country){
          natl.unaids <- T
        }else{
          natl.unaids <- F
        }
        
        if(natl.unaids){
          adj.frame <- hiv.adj
          adj.varnames <- c("country", "survey", "years")
        }else{
          adj.frame <- hiv.adj
          
          if(sum(grepl("HIVnames.key", list.files(paste0("./",folder.name)))) != 0){
            load(paste0(folder.name, '/', country, '_HIVnames.key.rda'),
                 envir = .GlobalEnv)
            mod.dat <- merge(mod.dat,
                             HIVnames.key[,c("LONGNUM", "LATNUM", "dhs.name")],
                             by = c("LONGNUM", "LATNUM"))
            names(mod.dat)[which(names(mod.dat) == 'dhs.name')] <- "area"
          }else if(sum(!(admin1.names$GADM %in% hiv.adj$area)) != 0){
            mod.dat$area <- mod.dat$admin1.name
          }
          adj.varnames <- c("country", "survey", "years", "area")
        }
        
        
        
      }else{
        adj.frame <- expand.grid(years = beg.year:end.year,
                                 country = country)
        adj.frame$ratio <- 1
        adj.varnames <- c("country", "years")
      }
      
      
      message(paste0("Starting stratified rw2 natl model for ", country, ".\n"))
      
      fit.frame.eff.natl <- fitINLA2(data = mod.dat, family = "betabinomial", 
                                     Amat = NULL, geo = NULL, 
                                     year_label = beg.year:end.year, rw = 2,
                                     type.st = type.st, 
                                     bias.adj = adj.frame,
                                     bias.adj.by = c(adj.varnames),
                                     age.groups = levels(mod.dat$age),
                                     age.n = c(1,11,12,12,12,12), 
                                     age.rw.group = c(1,2,3,3,3,3),
                                     verbose = TRUE, overdisp.mean = -7.5,
                                     overdisp.prec = 0.39,
                                     survey.effect = survey.effect, 
                                     strata.time.effect = TRUE)
      save(fit.frame.eff.natl, file = paste0(folder.name, '/',
                                             country, '_frame_rw2_natl.rda'))
      
      frame.names <- fit.frame.eff.natl$strata.base[grepl(tolower("urban"), fit.frame.eff.natl$strata.base)]
      frame.legend <- unique(mod.dat$frame)
      
      if(length(unique(frame.legend)) != 1){
        strata.weights <- expand.grid(years = beg.year:end.year,
                                      frame = frame.legend)
      }else{
        strata.weights <- expand.grid(years = beg.year:end.year,
                                      frame = unique(as.character(mod.dat$frame)))
      }
      
      for(frm in frame.legend){
        prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
        prop <- prop[order(prop$year),]
        min.yr <- min(prop$year)
        max.yr <- max(prop$year)
        strata.weights$urban[strata.weights$frame == frm] <-
          #t(unpd.prop.urban[,as.character(beg.year:end.year)])
          c(rep(prop$urban_prop[1],
                min.yr - beg.year),
            prop$urban_prop,
            rep(prop$urban_prop[nrow(prop)],
                end.year - max.yr))
      }
      strata.weights$rural <- 1 - strata.weights$urban
      
      res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.natl, 
                                        year_range = beg.year:end.year, 
                                        year_label = beg.year:end.year, nsim = 1000, 
                                        weight.strata = strata.weights,
                                        weight.frame = NULL, save.draws.est = TRUE,
                                        draws = NULL, save.draws = TRUE)
      save(res.frame.eff.natl, file = paste0(folder.name, '/',
                                             country, '_res_frame_rw2_natl.rda'))
      
      if(doBenchmark & !useHIVAdj){
        
        message(paste0("Starting stratified rw2 national benchmarking model for ",
                       country, ".\n"))
        
        bench.adj <- expand.grid(country = country,
                                 years = beg.year:end.year,
                                 frame = unique(mod.dat$frame))
        bench.adj$est <- bench.adj$igme <- NA
        for(i in 1:nrow(bench.adj)){
          yr <- bench.adj$years[i]
          frm <- bench.adj$frame[i]
          bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                  res.frame.eff.natl$overall$frame == frm]
          max.year <- max(mod.dat$years[mod.dat$frame == frm])
          if(yr %in% beg.year:max.year){
            bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
          }else{
            bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                     res.frame.eff.natl$overall$frame == frm]
          }          }
        bench.adj$ratio <- bench.adj$est/bench.adj$igme
        save(bench.adj, file = paste0(folder.name, '/',
                                      country, '_frame_rw2_natlBenchmarks.rda'))
        
        fit.frame.eff.natl <- fitINLA2(data = mod.dat, family = "betabinomial", 
                                       Amat = NULL, geo = NULL, 
                                       year_label = beg.year:end.year, rw = 2,
                                       type.st = type.st, 
                                       bias.adj = bench.adj,
                                       bias.adj.by = c(adj.varnames, "frame"),
                                       age.groups = levels(mod.dat$age),
                                       age.n = c(1,11,12,12,12,12), 
                                       age.rw.group = c(1,2,3,3,3,3),
                                       verbose = TRUE, overdisp.mean = -7.5,
                                       overdisp.prec = 0.39,
                                       survey.effect = survey.effect, 
                                       strata.time.effect = TRUE)
        save(fit.frame.eff.natl, file = paste0(folder.name, '/',
                                               country, '_frame_rw2_natlBench.rda'))
        
   
        if(length(unique(frame.legend)) != 1){
          strata.weights <- expand.grid(years = beg.year:end.year,
                                        frame = frame.legend)
        }else{
          strata.weights <- expand.grid(years = beg.year:end.year,
                                        frame = unique(as.character(mod.dat$frame)))
        }
        for(frm in frame.legend){
          prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
          prop <- prop[order(prop$year),]
          min.yr <- min(prop$year)
          max.yr <- max(prop$year)
          
          strata.weights$urban[strata.weights$frame == frm] <-
            #t(unpd.prop.urban[,as.character(beg.year:end.year)])
            c(rep(prop$urban_prop[1],
                  min.yr - beg.year),
                  prop$urban_prop,
              rep(prop$urban_prop[nrow(prop)],
                  end.year - max.yr))
        }
        strata.weights$rural <- 1 - strata.weights$urban
        
        
        res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.natl, 
                                          year_range = beg.year:end.year, 
                                          year_label = beg.year:end.year,
                                          nsim = 1000, 
                                          weight.strata = strata.weights,
                                          weight.frame = NULL, 
                                          save.draws.est = TRUE,
                                          draws = NULL, save.draws = TRUE)
        save(res.frame.eff.natl, file = paste0(folder.name, '/',
                                               country, '_res_frame_rw2_natlBench.rda'))
        
      }else if(doBenchmark & useHIVAdj){
        
        message(paste0("Starting stratified rw2 national benchmarking model for ",
                       country, ".\n"))
        load(paste0(folder.name, '/',
                    country,
                    '_res_natlBench_yearly_SmoothedDirect.rda'),
             envir = .GlobalEnv)
        
        bench.adj <- expand.grid(country = country,
                                 years = beg.year:end.year,
                                 frame = unique(mod.dat$frame))
        bench.adj$est <- bench.adj$igme <- NA
        for(i in 1:nrow(bench.adj)){
          yr <- bench.adj$years[i]
          frm <- bench.adj$frame[i]
          bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                  res.frame.eff.natl$overall$frame == frm]
          max.year <- max(mod.dat$years[mod.dat$frame == frm])
          if(yr %in% beg.year:max.year){
            bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
          }else{
            bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr &
                                                                     res.frame.eff.natl$overall$frame == frm]
          }        
        }
        bench.adj$ratio <- bench.adj$est/bench.adj$igme
        save(bench.adj, file = paste0(folder.name, '/',
                                      country, '_frame_rw2_natlBenchmarks.rda'))
        
        hiv.adj$frame <- 0
        for(svy in unique(hiv.adj$survey)){
          hiv.adj$frame[hiv.adj$survey == svy] <- unique(as.character(frames$Frame[frames$Survey.Year == svy]))
        }
        bench.adj <- merge(bench.adj, hiv.adj,
                           by = c('country', 'years', 'frame'),
                           suffixes = c('.bench', '.hiv'))
        bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
        
        
        fit.frame.eff.natl <- fitINLA2(data = mod.dat, family = "betabinomial", 
                                       Amat = NULL, geo = NULL, 
                                       year_label = beg.year:end.year, rw = 2,
                                       type.st = type.st, 
                                       bias.adj = bench.adj,
                                       bias.adj.by = c(adj.varnames, "frame"),
                                       age.groups = levels(mod.dat$age),
                                       age.n = c(1,11,12,12,12,12), 
                                       age.rw.group = c(1,2,3,3,3,3),
                                       verbose = TRUE,
                                       overdisp.mean = -7.5,
                                       overdisp.prec = 0.39,
                                       survey.effect = survey.effect, 
                                       strata.time.effect = TRUE)
        save(fit.frame.eff.natl, 
             file = paste0(folder.name, '/',
                           country, '_frame_rw2_natlBench.rda'))
        if(length(unique(frame.legend)) != 1){
          strata.weights <- expand.grid(years = beg.year:end.year,
                                        frame = frame.legend)
        }else{
          strata.weights <- expand.grid(years = beg.year:end.year,
                                        frame = unique(as.character(mod.dat$frame)))
        }
        strata.weights <- expand.grid(frame = frame.legend,
                                      years = beg.year:end.year)
        for(frm in frame.legend){
          prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
          prop <- prop[order(prop$year),]
          min.yr <- min(prop$year)
          max.yr <- max(prop$year)
          strata.weights$urban[strata.weights$frame == frm] <-
            #t(unpd.prop.urban[,as.character(beg.year:end.year)])
            c(rep(prop$urban_prop[1],
                  min.yr - beg.year),
              prop$urban_prop,
              rep(prop$urban_prop[nrow(prop)],
                  end.year - max.yr))
        }
        strata.weights$rural <- 1 - strata.weights$urban
        
        res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.natl, 
                                          year_range = beg.year:end.year, 
                                          year_label = beg.year:end.year,
                                          nsim = 1000, 
                                          weight.strata = strata.weights,
                                          weight.frame = NULL,
                                          save.draws.est = TRUE,
                                          draws = NULL, save.draws = TRUE)
        save(res.frame.eff.natl, 
             file = paste0(folder.name, '/',
                           country, '_res_frame_rw2_natlBench.rda'))
      }
    }
    
    ####  Admin 1 ####
    
    if(doAdmin1){
      
      mod.dat$region <- mod.dat$admin1.char
      
      if(useHIVAdj){
        load(paste0(hiv.dir.rel,'/HIVAdjustments.rda'), envir = .GlobalEnv)
        hiv.adj <- hiv.adj[hiv.adj$country == country,]
        if(unique(hiv.adj$area)[1] == country){
          natl.unaids <- T
        }else{
          natl.unaids <- F
        }
        
        if(natl.unaids){
          adj.frame <- hiv.adj
          adj.varnames <- c("country", "survey", "years")
        }else{
          adj.frame <- hiv.adj
          
          if(sum(grepl("HIVnames.key", list.files(paste0("./",folder.name)))) != 0){
            load(paste0(folder.name, '/', country, '_HIVnames.key.rda'),
                 envir = .GlobalEnv)
            mod.dat <- merge(mod.dat,
                             HIVnames.key[,c("LONGNUM", "LATNUM", "dhs.name")],
                             by = c("LONGNUM", "LATNUM"))
            names(mod.dat)[which(names(mod.dat) == 'dhs.name')] <- "area"
          }else if(sum(!(admin1.names$GADM %in% hiv.adj$area)) != 0){
            mod.dat$area <- mod.dat$admin1.name
          }
          
          adj.varnames <- c("country", "survey", "years", "area")
        }
      }else{
        adj.varnames <- c("country", "years")
      }
      
 
      #### random slopes + RW1xICAR ####
      if (doRandomSlopesRW1) {
        message(paste0("Starting stratified random slopes RW1xICAR admin 1 model for ", country, ".\n"))
        bench.adj <- expand.grid(country = country,
                                 years = beg.year:end.year,
                                 frame = unique(mod.dat$frame))
        bench.adj$ratio <- 1.0
        
        if(useHIVAdj){
          hiv.adj$frame <- 0
          for(svy in unique(hiv.adj$survey)){
            hiv.adj$frame[hiv.adj$survey == svy] <- unique(as.character(frames$Frame[frames$Survey.Year == svy]))
          }
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years', 'frame'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
        }
        
        if(!file.exists(paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_rw1xICAR_admin1.rda')) | refit){
          
          
          fit.frame.eff.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                           Amat = admin1.mat, geo = poly.adm1,
                                           year_label = beg.year:end.year,
                                           rw = 2, ar = 0, st.rw = 1,
                                           pc.st.slope.u = 1, 
                                           pc.st.slope.alpha = 0.01,
                                           type.st = type.st,
                                           bias.adj = bench.adj,
                                           bias.adj.by = c(adj.varnames),
                                           age.groups = levels(mod.dat$age),
                                           age.n = c(1,11,12,12,12,12),
                                           age.rw.group = c(1,2,3,3,3,3),
                                           verbose = FALSE,
                                           survey.effect = survey.effect,
                                           strata.time.effect = TRUE)
          
          # options = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE)
          save(fit.frame.eff.admin1, 
               file = paste0(folder.name, '/',
                             country, '_frame_rw2main_randomSlopes_rw1xICAR_admin1.rda'))
        }else{
          load(paste0(folder.name, '/', country,
                      '_frame_rw2main_randomSlopes_rw1xICAR_admin1.rda'), envir = .GlobalEnv)
        }
        
        hyperpar.table <- fit.frame.eff.admin1$fit$summary.hyperpar
        save(hyperpar.table,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin1_noStrata_hyperpar.rda'))
        
        temporals <- getDiag(fit.frame.eff.admin1, field = "time",
                             year_label = beg.year:end.year)
        save(temporals,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin1_noStrata_temporals.rda'))
        spaces <- getDiag(fit.frame.eff.admin1, field = "space",
                          Amat = admin1.mat)
        save(spaces,
             file = paste0(folder.name, '/',
                           country, 
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin1_noStrata_spatials.rda'))
        spacetimes <- getDiag(fit.frame.eff.admin1, field = "spacetime",
                              year_label =  beg.year:end.year,
                              Amat = admin1.mat)
        save(spacetimes,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin1_noStrata_spatiotemporals.rda'))
        
        fixed.eff.table <- fit.frame.eff.admin1$fit$summary.fixed
        save(fixed.eff.table,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin1_noStrata_fixedeff.rda'))
        
        PosteriorInteractions <- fit.frame.eff.admin1$fit$summary.random$time.area
        save(PosteriorInteractions,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin1_noStrata_PosteriorInteractions.rda'))
        
        posteriorRandomSlopes <- fit.frame.eff.admin1$fit$summary.random$st.slope
        save(posteriorRandomSlopes,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin1_noStrata_posteriorRandomSlopes.rda'))
        
        if(loadSamples & refit){
          stop('loadSamples cannot be TRUE while refit = TRUE')
        }else if(loadSamples & !refit){
          load(paste0(folder.name, '/',
                      country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin1.rda'),
               envir = .GlobalEnv)
        }else if(!loadSamples){
          
          if(length(unique(mod.dat$frame)) >1 ){
            frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
            frame.legend <- unlist(lapply(frame.names, function(x){
              split <- strsplit(x, "-")
              return(split[[1]][1])
            }))
          }else{
            frame.legend <- as.character(unique(mod.dat$frame))
          }          
          
          if(length(unique(frame.legend)) != 1){
            strata.weights <- expand.grid(years = beg.year:end.year,
                                          region = admin1.names$Internal,
                                          frame = frame.legend)
          }else{
            strata.weights <- expand.grid(years = beg.year:end.year,
                                          region = admin1.names$Internal,
                                          frame = unique(as.character(mod.dat$frame)))
          }
          strata.weights$urban <- strata.weights$rural <- NA
          
          for(area in admin1.names$GADM){
            int <- admin1.names$Internal[match(area, admin1.names$GADM)]
            prop <- prop.urban[prop.urban$admin == area,]
            prop <- prop[prop$year %in% beg.year:end.year,]
            prop.yrs <- sort(prop$year)
            min.yr  <- min(prop.yrs)
            max.yr <- max(prop.yrs)
            strata.weights$urban[strata.weights$region == int] <- c(rep(prop$urban_prop[1],
                                                                        min.yr - beg.year),
                                                                    prop$urban_prop,
                                                                    rep(prop$urban_prop[nrow(prop)],
                                                                        end.year - max.yr))
            
          }
          strata.weights$rural <- 1- strata.weights$urban
          
          res.frame.eff.admin1 <- getSmoothed(inla_mod = fit.frame.eff.admin1,
                                              year_range = beg.year:end.year,
                                              year_label = beg.year:end.year,
                                              nsim = 1000,
                                              Amat = admin1.mat, 
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights,
                                              weight.frame = NULL,
                                              draws = NULL, save.draws = TRUE)
          save(res.frame.eff.admin1,
               file = paste0(folder.name, '/',
                             country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin1.rda'))
        }
        
        #### Benchmark ####
        if(doBenchmark & !useHIVAdj){
          message(paste0("Starting stratified random slopes with RW1xICAR admin 1 benchmarking model for ",
                         country, ".\n"))
          
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_frame_rw2main_randomSlopes_rw1xICAR_admin1.rda'), envir = .GlobalEnv)
            }
            
            
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            strata.weights.bench <- expand.grid(frame = frame.legend,
                                                years = beg.year:end.year)
            strata.weights.bench$urban <- NA
            for(frm in frame.legend){
              prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
              prop <- prop[order(prop$year),]
              min.yr <- min(prop$year)
              max.yr <- max(prop$year)
              strata.weights.bench$urban[strata.weights.bench$frame == frm] <-
                #t(unpd.prop.urban[,as.character(beg.year:end.year)])
                c(rep(prop$urban_prop[1],
                      min.yr - beg.year),
                  prop$urban_prop,
                  rep(prop$urban_prop[nrow(prop)],
                      end.year - max.yr))
            }
            strata.weights.bench$rural <- 1 - strata.weights.bench$urban
            
            res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.admin1, 
                                              year_range = beg.year:end.year, 
                                              year_label = beg.year:end.year,
                                              nsim = 1000, 
                                              Amat = admin1.mat, 
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights.bench,
                                              weight.frame = NULL,
                                              include_subnational = FALSE,
                                              draws = NULL, save.draws = TRUE)
            
            
            rm(fit.frame.eff.admin1)
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year,
                                     frame = unique(mod.dat$frame))
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              frm <- bench.adj$frame[i]
              bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                      res.frame.eff.natl$overall$frame == frm]
              max.year <- max(mod.dat$years[mod.dat$frame == frm])
              if(yr %in% beg.year:max.year){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                         res.frame.eff.natl$overall$frame == frm]
              }          
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            
            save(bench.adj, 
                 file = paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_rw1xICAR_admin1Benchmarks.rda'))
            
            fit.frame.eff.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                             Amat = admin1.mat, geo = poly.adm1,
                                             year_label = beg.year:end.year,
                                             rw = 2, ar = 0, st.rw = 1,
                                             pc.st.slope.u = 1, 
                                             pc.st.slope.alpha = 0.01,
                                             type.st = type.st,
                                             bias.adj = bench.adj, 
                                             overdisp.mean = -7.5,
                                             overdisp.prec = 0.39,
                                             bias.adj.by = c(adj.varnames, 'frame'),
                                             age.groups = levels(mod.dat$age),
                                             age.n = c(1,11,12,12,12,12),
                                             age.rw.group = c(1,2,3,3,3,3),
                                             verbose = FALSE,
                                             survey.effect = survey.effect,
                                             strata.time.effect = TRUE)
            save(fit.frame.eff.admin1, 
                 file = paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench.rda'), envir = .GlobalEnv)
          }
          
          
          hyperpar.table <- fit.frame.eff.admin1$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.frame.eff.admin1$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_fixedeff.rda'))
          
          temporals <- getDiag(fit.frame.eff.admin1, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_temporals.rda'))
          
          spaces <- getDiag(fit.frame.eff.admin1, field = "space",
                            Amat = admin1.mat)
          save(spaces, 
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.frame.eff.admin1, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin1.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_spatiotemporals.rda'))
          
          PosteriorInteractions <- fit.frame.eff.admin1$fit$summary.random$time.area
          save(PosteriorInteractions,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_PosteriorInteractions.rda'))
          
          posteriorRandomSlopes <- fit.frame.eff.admin1$fit$summary.random$st.slope
          save(posteriorRandomSlopes,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_posteriorRandomSlopes.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            if(length(unique(frame.legend)) != 1){
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin1.names$Internal,
                                            frame = frame.legend)
            }else{
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin1.names$Internal,
                                            frame = unique(as.character(mod.dat$frame)))
            }
            strata.weights$urban <- strata.weights$rural <- NA
            
            
            for(area in admin1.names$GADM){
              int <- admin1.names$Internal[match(area, admin1.names$GADM)]
              prop <- prop.urban[prop.urban$admin == area,]
              prop <- prop[prop$year %in% beg.year:end.year,]
              prop.yrs <- sort(prop$year)
              min.yr <- min(prop.yrs)
              max.yr <- max(prop.yrs)
              strata.weights$urban[strata.weights$region == int] <-c(rep(prop$urban_prop[1],
                                                                         min.yr - beg.year),
                                                                     prop$urban_prop,
                                                                     rep(prop$urban_prop[nrow(prop)],
                                                                         end.year - max.yr))
              }
            strata.weights$rural <- 1- strata.weights$urban
            
            res.frame.eff.admin1 <- getSmoothed(inla_mod = fit.frame.eff.admin1, Amat = admin1.mat,
                                                year_range = beg.year:end.year, 
                                                year_label = beg.year:end.year,
                                                nsim = 1000, save.draws.est = TRUE,
                                                weight.strata = strata.weights,
                                                weight.frame = NULL,
                                                draws = NULL, save.draws = TRUE)
            save(res.frame.eff.admin1, file = paste0(folder.name, '/',
                                                     country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench.rda'))
          }
          
          
          
        }else if(doBenchmark & useHIVAdj){
          
          message(paste0("Starting stratified random slopes RW1xICAR admin 1 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_frame_rw2main_randomSlopes_rw1xICAR_admin1.rda'), envir = .GlobalEnv)
            }
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            strata.weights.bench <- expand.grid(frame = frame.legend,
                                                years = beg.year:end.year)
            for(frm in frame.legend){
              prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
              prop <- prop[order(prop$year),]
              min.yr <- min(prop$year)
              max.yr <- max(prop$year)
              strata.weights.bench$urban[strata.weights.bench$frame == frm] <-
                #t(unpd.prop.urban[,as.character(beg.year:end.year)])
                c(rep(prop$urban_prop[1],
                      min.yr - beg.year),
                  prop$urban_prop,
                  rep(prop$urban_prop[nrow(prop)],
                      end.year - max.yr))
            }
            strata.weights.bench$rural <- 1 - strata.weights.bench$urban
            
            
            res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.admin1, 
                                              year_range = beg.year:end.year, 
                                              year_label = beg.year:end.year,
                                              nsim = 1000, 
                                              Amat = admin1.mat,
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights.bench,
                                              weight.frame = NULL,
                                              include_subnational = FALSE,
                                              draws = NULL, save.draws = TRUE)
            
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year,
                                     frame = frame.legend)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              frm <- bench.adj$frame[i]
              bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                      res.frame.eff.natl$overall$frame == frm]
              max.year <- max(mod.dat$years[mod.dat$frame == frm])
              if(yr %in% beg.year:max.year){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                         res.frame.eff.natl$overall$frame == frm]
              }          
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, 
                 file = paste0(folder.name, '/',
                               country,
                               '_frame_rw2main_randomSlopes_rw1xICAR_admin1Benchmarks.rda'))
            
            bench.adj <- merge(bench.adj, hiv.adj,
                               by = c('country', 'years'),
                               suffixes = c('.bench', '.hiv'))
            bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
            bench.adj$frame <- bench.adj$frame.bench
            
            fit.frame.eff.admin1 <- fitINLA2(data = mod.dat, 
                                             family = "betabinomial",
                                             Amat = admin1.mat, geo = poly.adm1,
                                             year_label = beg.year:end.year,
                                             rw = 2, ar = 0, st.rw = 1,
                                             pc.st.slope.u = 1, 
                                             pc.st.slope.alpha = 0.01,
                                             type.st = type.st,
                                             bias.adj = bench.adj, 
                                             overdisp.mean = -7.5,
                                             overdisp.prec = 0.39,
                                             bias.adj.by = c(adj.varnames, 'frame'),
                                             age.groups = levels(mod.dat$age),
                                             age.n = c(1,11,12,12,12,12),
                                             age.rw.group = c(1,2,3,3,3,3),
                                             verbose = FALSE,
                                             survey.effect = survey.effect,
                                             strata.time.effect = TRUE)
            save(fit.frame.eff.admin1,
                 file = paste0(folder.name, '/',
                               country,
                               '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench.rda'), envir = .GlobalEnv) 
          }
          
          hyperpar.table <- fit.frame.eff.admin1$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.frame.eff.admin1$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.frame.eff.admin1, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.frame.eff.admin1, field = "space",
                            Amat = admin1.mat)
          save(spaces, file = paste0(folder.name, '/',
                                     country, 
                                     '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.frame.eff.admin1, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin1.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_spatiotemporals.rda'))
          
          PosteriorInteractions <- fit.frame.eff.admin1$fit$summary.random$time.area
          save(PosteriorInteractions,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_PosteriorInteractions.rda'))
          
          posteriorRandomSlopes <- fit.frame.eff.admin1$fit$summary.random$st.slope
          save(posteriorRandomSlopes,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench_noStrata_posteriorRandomSlopes.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench = TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            if(length(unique(frame.legend)) != 1){
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin1.names$Internal,
                                            frame = frame.legend)
            }else{
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin1.names$Internal,
                                            frame = unique(as.character(mod.dat$frame)))
            }
            strata.weights$urban <- strata.weights$rural <- NA
            
            
            for(area in admin1.names$GADM){
              int <- admin1.names$Internal[match(area, admin1.names$GADM)]
              prop <- prop.urban[prop.urban$admin == area,]
              prop <- prop[prop$year %in% beg.year:end.year,]
              prop.yrs <- sort(prop$year)
              min.yr <- min(prop.yrs)
              max.yr <- max(prop.yrs)
              strata.weights$urban[strata.weights$region == int] <-c(rep(prop$urban_prop[1],
                                                                         min.yr - beg.year),
                                                                     prop$urban_prop,
                                                                     rep(prop$urban_prop[nrow(prop)],
                                                                         end.year - max.yr))
            }
            strata.weights$rural <- 1 - strata.weights$urban
            
            res.frame.eff.admin1 <- getSmoothed(inla_mod = fit.frame.eff.admin1, 
                                                year_range = beg.year:end.year, 
                                                year_label = beg.year:end.year,
                                                nsim = 1000, save.draws.est = TRUE,
                                                weight.strata = strata.weights,
                                                Amat = admin1.mat,
                                                weight.frame = NULL,
                                                draws = NULL, save.draws = TRUE)
            save(res.frame.eff.admin1, 
                 file = paste0(folder.name, '/',
                               country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin1Bench.rda'))
          }
        }
        
      }
      
      #### random slopes + AR1xICAR ####
      if (doRandomSlopesAR1) {
        message(paste0("Starting stratified random slopes AR1xICAR admin 1 model for ", country, ".\n"))
        bench.adj <- expand.grid(country = country,
                                 years = beg.year:end.year,
                                 frame = unique(mod.dat$frame))
        bench.adj$ratio <- 1.0
        
        if(useHIVAdj){
          hiv.adj$frame <- 0
          for(svy in unique(hiv.adj$survey)){
            hiv.adj$frame[hiv.adj$survey == svy] <- unique(as.character(frames$Frame[frames$Survey.Year == svy]))
          }
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years', 'frame'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
        }
        
        if(!file.exists(paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_ar1xICAR_admin1.rda')) | refit){
          
          
          fit.frame.eff.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                           Amat = admin1.mat, geo = poly.adm1,
                                           year_label = beg.year:end.year,
                                           rw = 2, ar = 1,
                                           pc.st.slope.u = 1, 
                                           pc.st.slope.alpha = 0.01,
                                           type.st = type.st,
                                           bias.adj = bench.adj,
                                           bias.adj.by = c(adj.varnames),
                                           age.groups = levels(mod.dat$age),
                                           age.n = c(1,11,12,12,12,12),
                                           age.rw.group = c(1,2,3,3,3,3),
                                           verbose = FALSE,
                                           survey.effect = survey.effect,
                                           strata.time.effect = TRUE)
          
          # options = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE)
          save(fit.frame.eff.admin1, 
               file = paste0(folder.name, '/',
                             country, '_frame_rw2main_randomSlopes_ar1xICAR_admin1.rda'))
        }else{
          load(paste0(folder.name, '/', country,
                      '_frame_rw2main_randomSlopes_ar1xICAR_admin1.rda'), envir = .GlobalEnv)
        }
        
        hyperpar.table <- fit.frame.eff.admin1$fit$summary.hyperpar
        save(hyperpar.table,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin1_noStrata_hyperpar.rda'))
        
        temporals <- getDiag(fit.frame.eff.admin1, field = "time",
                             year_label = beg.year:end.year)
        save(temporals,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin1_noStrata_temporals.rda'))
        spaces <- getDiag(fit.frame.eff.admin1, field = "space",
                          Amat = admin1.mat)
        save(spaces,
             file = paste0(folder.name, '/',
                           country, 
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin1_noStrata_spatials.rda'))
        spacetimes <- getDiag(fit.frame.eff.admin1, field = "spacetime",
                              year_label =  beg.year:end.year,
                              Amat = admin1.mat)
        save(spacetimes,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin1_noStrata_spatiotemporals.rda'))
        
        fixed.eff.table <- fit.frame.eff.admin1$fit$summary.fixed
        save(fixed.eff.table,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin1_noStrata_fixedeff.rda'))
        
        PosteriorInteractions <- fit.frame.eff.admin1$fit$summary.random$time.area
        save(PosteriorInteractions,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin1_noStrata_PosteriorInteractions.rda'))
        
        posteriorRandomSlopes <- fit.frame.eff.admin1$fit$summary.random$st.slope
        save(posteriorRandomSlopes,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin1_noStrata_posteriorRandomSlopes.rda'))
        
        if(loadSamples & refit){
          stop('loadSamples cannot be TRUE while refit = TRUE')
        }else if(loadSamples & !refit){
          load(paste0(folder.name, '/',
                      country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin1.rda'),
               envir = .GlobalEnv)
        }else if(!loadSamples){
          if(length(unique(mod.dat$frame)) >1 ){
            frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
            frame.legend <- unlist(lapply(frame.names, function(x){
              split <- strsplit(x, "-")
              return(split[[1]][1])
            }))
          }else{
            frame.legend <- as.character(unique(mod.dat$frame))
          }    
          
          if(length(unique(frame.legend)) != 1){
            strata.weights <- expand.grid(years = beg.year:end.year,
                                          region = admin1.names$Internal,
                                          frame = frame.legend)
          }else{
            strata.weights <- expand.grid(years = beg.year:end.year,
                                          region = admin1.names$Internal,
                                          frame = unique(as.character(mod.dat$frame)))
          }
          
          strata.weights$urban <- strata.weights$rural <- NA
          #frame.file <- list.files('../Population/Frames')
          #frame.file <- frame.file[grepl(country, frame.file)]
          #frame.csv <- read.csv(paste0('../Population/Frames',
          #                             frame.file))
          #for(area in frame.csv$Area){
          # int <- admin1.names$Internal[match(area, admin1.names$GADM)]
          #  prop <- frame.csv$UrbanClusters[frame.csv$Area == area]/frame.csv$TotalClusters[frame.csv$Area == area]
          #  frame.csv$urban[frame.csv$Area == area] <- prop
          #  strata.weights$urban[strata.weights$region == int] <- prop
          #}
          
          for(area in admin1.names$GADM){
            int <- admin1.names$Internal[match(area, admin1.names$GADM)]
            prop <- prop.urban[prop.urban$admin == area,]
            prop <- prop[prop$year %in% beg.year:end.year,]
            prop.yrs <- sort(prop$year)
            min.yr <- min(prop.yrs)
            max.yr <- max(prop.yrs)
            strata.weights$urban[strata.weights$region == int] <- c(rep(prop$urban_prop[1],
                                                                        min.yr - beg.year),
                                                                    prop$urban_prop,
                                                                    rep(prop$urban_prop[nrow(prop)],
                                                                        end.year - max.yr))
            
          }
          strata.weights$rural <- 1- strata.weights$urban
          
          res.frame.eff.admin1 <- getSmoothed(inla_mod = fit.frame.eff.admin1,
                                              year_range = beg.year:end.year,
                                              year_label = beg.year:end.year,
                                              nsim = 1000,
                                              Amat = admin1.mat,
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights,
                                              weight.frame = NULL, 
                                              draws = NULL, save.draws = TRUE)
          save(res.frame.eff.admin1,
               file = paste0(folder.name, '/',
                             country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin1.rda'))
        }
        
        #### Benchmark ####
        if(doBenchmark & !useHIVAdj){
          message(paste0("Starting stratified random slopes with AR1xICAR admin 1 benchmarking model for ",
                         country, ".\n"))
          
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_frame_rw2main_randomSlopes_ar1xICAR_admin1.rda'), envir = .GlobalEnv)
            }
            
            
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            strata.weights.bench <- expand.grid(frame = frame.legend,
                                                years = beg.year:end.year)
            strata.weights.bench$urban <- NA
            for(frm in frame.legend){
              prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
              prop <- prop[order(prop$year),]
              min.yr <- min(prop$year)
              max.yr <- max(prop$year)
              strata.weights.bench$urban[strata.weights.bench$frame == frm] <-
                #t(unpd.prop.urban[,as.character(beg.year:end.year)])
                c(rep(prop$urban_prop[1],
                      min.yr - beg.year),
                  prop$urban_prop,
                  rep(prop$urban_prop[nrow(prop)],
                      end.year - max.yr))
            }
            strata.weights.bench$rural <- 1 - strata.weights.bench$urban
            
            res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.admin1, 
                                              year_range = beg.year:end.year, 
                                              year_label = beg.year:end.year,
                                              nsim = 1000, 
                                              Amat = admin1.mat, 
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights.bench,
                                              weight.frame = NULL,
                                              include_subnational = FALSE,
                                              draws = NULL, save.draws = TRUE)
            
            
            rm(fit.frame.eff.admin1)
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year,
                                     frame = unique(mod.dat$frame))
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              frm <- bench.adj$frame[i]
              bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                      res.frame.eff.natl$overall$frame == frm]
              max.year <- max(mod.dat$years[mod.dat$frame == frm])
              if(yr %in% beg.year:max.year){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                         res.frame.eff.natl$overall$frame == frm]
              }          
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            
            save(bench.adj, 
                 file = paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_ar1xICAR_admin1Benchmarks.rda'))
            
            fit.frame.eff.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                             Amat = admin1.mat, geo = poly.adm1,
                                             year_label = beg.year:end.year,
                                             rw = 2, ar = 1,
                                             pc.st.slope.u = 1, 
                                             pc.st.slope.alpha = 0.01,
                                             type.st = type.st,
                                             bias.adj = bench.adj, 
                                             overdisp.mean = -7.5,
                                             overdisp.prec = 0.39,
                                             bias.adj.by = c(adj.varnames, 'frame'),
                                             age.groups = levels(mod.dat$age),
                                             age.n = c(1,11,12,12,12,12),
                                             age.rw.group = c(1,2,3,3,3,3),
                                             verbose = FALSE,
                                             survey.effect = survey.effect,
                                             strata.time.effect = TRUE)
            save(fit.frame.eff.admin1, 
                 file = paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench.rda'), envir = .GlobalEnv)
          }
          
          
          hyperpar.table <- fit.frame.eff.admin1$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.frame.eff.admin1$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_fixedeff.rda'))
          
          temporals <- getDiag(fit.frame.eff.admin1, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_temporals.rda'))
          
          spaces <- getDiag(fit.frame.eff.admin1, field = "space",
                            Amat = admin1.mat)
          save(spaces, 
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.frame.eff.admin1, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin1.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_spatiotemporals.rda'))
          
          PosteriorInteractions <- fit.frame.eff.admin1$fit$summary.random$time.area
          save(PosteriorInteractions,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_PosteriorInteractions.rda'))
          
          posteriorRandomSlopes <- fit.frame.eff.admin1$fit$summary.random$st.slope
          save(posteriorRandomSlopes,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_posteriorRandomSlopes.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            if(length(unique(frame.legend)) != 1){
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin1.names$Internal,
                                            frame = frame.legend)
            }else{
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin1.names$Internal,
                                            frame = unique(as.character(mod.dat$frame)))
            }
            
            strata.weights$urban <- strata.weights$rural <- NA
            
            
            for(area in admin1.names$GADM){
              int <- admin1.names$Internal[match(area, admin1.names$GADM)]
              prop <- prop.urban[prop.urban$admin == area,]
              prop <- prop[prop$year %in% beg.year:end.year,]
              prop.yrs <- sort(prop$year)
              min.yr <- min(prop.yrs)
              max.yr <- max(prop.yrs)
              strata.weights$urban[strata.weights$region == int] <-c(rep(prop$urban_prop[1],
                                                                         min.yr - beg.year),
                                                                     prop$urban_prop,
                                                                     rep(prop$urban_prop[nrow(prop)],
                                                                         end.year - max.yr))
              
            }
            strata.weights$rural <- 1- strata.weights$urban
            
            res.frame.eff.admin1 <- getSmoothed(inla_mod = fit.frame.eff.admin1,
                                                Amat = admin1.mat,
                                                year_range = beg.year:end.year, 
                                                year_label = beg.year:end.year,
                                                nsim = 1000, save.draws.est = TRUE,
                                                weight.strata = strata.weights,
                                                weight.frame = NULL,
                                                draws = NULL, save.draws = TRUE)
            save(res.frame.eff.admin1, file = paste0(folder.name, '/',
                                                     country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench.rda'))
          }
          
          
          
        }else if(doBenchmark & useHIVAdj){
          
          message(paste0("Starting stratified random slopes AR1xICAR admin 1 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_frame_rw2main_randomSlopes_ar1xICAR_admin1.rda'), envir = .GlobalEnv)
            }
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            strata.weights.bench <- expand.grid(frame = frame.legend,
                                                years = beg.year:end.year)
            for(frm in frame.legend){
              prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
              prop <- prop[order(prop$year),]
              min.yr <- min(prop$year)
              max.yr <- max(prop$year)
              strata.weights.bench$urban[strata.weights.bench$frame == frm] <-
                #t(unpd.prop.urban[,as.character(beg.year:end.year)])
                c(rep(prop$urban_prop[1],
                      min.yr - beg.year),
                  prop$urban_prop,
                  rep(prop$urban_prop[nrow(prop)],
                      end.year - max.yr))
            }
            strata.weights.bench$rural <- 1 - strata.weights.bench$urban
            
            
            res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.admin1, 
                                              year_range = beg.year:end.year, 
                                              year_label = beg.year:end.year,
                                              nsim = 1000,
                                              Amat = admin1.mat,
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights.bench,
                                              weight.frame = NULL,
                                              include_subnational = FALSE,
                                              draws = NULL, save.draws = TRUE)
            
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year,
                                     frame = frame.legend)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              frm <- bench.adj$frame[i]
              bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                      res.frame.eff.natl$overall$frame == frm]
              max.year <- max(mod.dat$years[mod.dat$frame == frm])
              if(yr %in% beg.year:max.year){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                         res.frame.eff.natl$overall$frame == frm]
              }          
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, 
                 file = paste0(folder.name, '/',
                               country,
                               '_frame_rw2main_randomSlopes_ar1xICAR_admin1Benchmarks.rda'))
            
            bench.adj <- merge(bench.adj, hiv.adj,
                               by = c('country', 'years'),
                               suffixes = c('.bench', '.hiv'))
            bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
            bench.adj$frame <- bench.adj$frame.bench
            
            fit.frame.eff.admin1 <- fitINLA2(data = mod.dat, 
                                             family = "betabinomial",
                                             Amat = admin1.mat, geo = poly.adm1,
                                             year_label = beg.year:end.year,
                                             rw = 2, ar = 1,
                                             pc.st.slope.u = 1, 
                                             pc.st.slope.alpha = 0.01,
                                             type.st = type.st,
                                             bias.adj = bench.adj, 
                                             overdisp.mean = -7.5,
                                             overdisp.prec = 0.39,
                                             bias.adj.by = c(adj.varnames, 'frame'),
                                             age.groups = levels(mod.dat$age),
                                             age.n = c(1,11,12,12,12,12),
                                             age.rw.group = c(1,2,3,3,3,3),
                                             verbose = FALSE,
                                             survey.effect = survey.effect,
                                             strata.time.effect = TRUE)
            save(fit.frame.eff.admin1,
                 file = paste0(folder.name, '/',
                               country,
                               '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench.rda'), envir = .GlobalEnv) 
          }
          
          hyperpar.table <- fit.frame.eff.admin1$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.frame.eff.admin1$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.frame.eff.admin1, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.frame.eff.admin1, field = "space",
                            Amat = admin1.mat)
          save(spaces, file = paste0(folder.name, '/',
                                     country, 
                                     '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.frame.eff.admin1, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin1.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_spatiotemporals.rda'))
          
          PosteriorInteractions <- fit.frame.eff.admin1$fit$summary.random$time.area
          save(PosteriorInteractions,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_PosteriorInteractions.rda'))
          
          posteriorRandomSlopes <- fit.frame.eff.admin1$fit$summary.random$st.slope
          save(posteriorRandomSlopes,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench_noStrata_posteriorRandomSlopes.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench = TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){ 
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin1$strata.base[grepl(tolower("urban"), fit.frame.eff.admin1$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            if(length(unique(frame.legend)) != 1){
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin1.names$Internal,
                                            frame = frame.legend)
            }else{
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin1.names$Internal,
                                            frame = unique(as.character(mod.dat$frame)))
            }
            
            strata.weights$urban <- strata.weights$rural <- NA
            
            
            for(area in admin1.names$GADM){
              int <- admin1.names$Internal[match(area, admin1.names$GADM)]
              prop <- prop.urban[prop.urban$admin == area,]
              prop <- prop[prop$year %in% beg.year:end.year,]
              prop.yrs <- sort(prop$year)
              min.yr <- min(prop.yrs)
              max.yr <- max(prop.yrs)
              strata.weights$urban[strata.weights$region == int] <- c(rep(prop$urban_prop[1],
                                                                          min.yr - beg.year),
                                                                      prop$urban_prop,
                                                                      rep(prop$urban_prop[nrow(prop)],
                                                                          end.year - max.yr))
              
            }
            strata.weights$rural <- 1- strata.weights$urban
            
            res.frame.eff.admin1 <- getSmoothed(inla_mod = fit.frame.eff.admin1, 
                                                year_range = beg.year:end.year, 
                                                year_label = beg.year:end.year,
                                                nsim = 1000, save.draws.est=TRUE,
                                                weight.strata = strata.weights,
                                                Amat = admin1.mat,
                                                weight.frame = NULL,
                                                draws = NULL, save.draws = TRUE)
            save(res.frame.eff.admin1, 
                 file = paste0(folder.name, '/',
                               country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin1Bench.rda'))
          }
        }
        
      }
    }
    
    ####  Admin 2 ####
    #### random slopes + RW1xICAR ####
    if(doAdmin2){
      mod.dat$region <- mod.dat$admin2.char
      
      if(useHIVAdj){
        load(paste0(hiv.dir.rel,'/HIVAdjustments.rda'), envir = .GlobalEnv)
        hiv.adj <- hiv.adj[hiv.adj$country == country,]
        if(unique(hiv.adj$area)[1] == country){
          natl.unaids <- T
        }else{
          natl.unaids <- F
        }
        
        if(natl.unaids){
          adj.frame <- hiv.adj
          adj.varnames <- c("country", "survey", "years")
        }else{
          adj.frame <- hiv.adj
          
          if(sum(grepl("HIVnames.key", list.files(paste0("./",folder.name)))) != 0){
            load(paste0(folder.name, '/', country, '_HIVnames.key.rda'),
                 envir = .GlobalEnv)
            mod.dat <- merge(mod.dat,
                             HIVnames.key[,c("LONGNUM", "LATNUM", "dhs.name")],
                             by = c("LONGNUM", "LATNUM"))
            names(mod.dat)[which(names(mod.dat) == 'dhs.name')] <- "area"
          }else if(sum(!(admin1.names$GADM %in% hiv.adj$area)) != 0){
            mod.dat$area <- mod.dat$admin1.name
          }
          
          adj.varnames <- c("country", "survey", "years", "area")
        }
      }else{
        adj.varnames <- c("country", "years")
      }
      
      if (doRandomSlopesRW1) {
        message(paste0("Starting stratified random slopes RW1xICAR admin 2 model for ", country, ".\n"))
        bench.adj <- expand.grid(country = country,
                                 years = beg.year:end.year,
                                 frame = unique(mod.dat$frame))
        bench.adj$ratio <- 1.0
        
        if(useHIVAdj){
          hiv.adj$frame <- 0
          for(svy in unique(hiv.adj$survey)){
            hiv.adj$frame[hiv.adj$survey == svy] <- unique(as.character(frames$Frame[frames$Survey.Year == svy]))
          }
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years', 'frame'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
        }
        
        if(!file.exists(paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_rw1xICAR_admin2.rda')) | refit){
          
          
          fit.frame.eff.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                           Amat = admin2.mat, geo = poly.adm2,
                                           year_label = beg.year:end.year,
                                           rw = 2, ar = 0, st.rw = 1,
                                           pc.st.slope.u = 1, 
                                           pc.st.slope.alpha = 0.01,
                                           type.st = type.st,
                                           bias.adj = bench.adj,
                                           bias.adj.by = c(adj.varnames),
                                           age.groups = levels(mod.dat$age),
                                           age.n = c(1,11,12,12,12,12),
                                           age.rw.group = c(1,2,3,3,3,3),
                                           verbose = FALSE,
                                           survey.effect = survey.effect,
                                           strata.time.effect = TRUE)
          
          # options = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE)
          save(fit.frame.eff.admin2, 
               file = paste0(folder.name, '/',
                             country, '_frame_rw2main_randomSlopes_rw1xICAR_admin2.rda'))
        }else{
          load(paste0(folder.name, '/', country,
                      '_frame_rw2main_randomSlopes_rw1xICAR_admin2.rda'), envir = .GlobalEnv)
        }
        
        hyperpar.table <- fit.frame.eff.admin2$fit$summary.hyperpar
        save(hyperpar.table,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin2_noStrata_hyperpar.rda'))
        
        temporals <- getDiag(fit.frame.eff.admin2, field = "time",
                             year_label = beg.year:end.year)
        save(temporals,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin2_noStrata_temporals.rda'))
        spaces <- getDiag(fit.frame.eff.admin2, field = "space",
                          Amat = admin2.mat)
        save(spaces,
             file = paste0(folder.name, '/',
                           country, 
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin2_noStrata_spatials.rda'))
        spacetimes <- getDiag(fit.frame.eff.admin2, field = "spacetime",
                              year_label =  beg.year:end.year,
                              Amat = admin2.mat)
        save(spacetimes,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin2_noStrata_spatiotemporals.rda'))
        
        fixed.eff.table <- fit.frame.eff.admin2$fit$summary.fixed
        save(fixed.eff.table,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin2_noStrata_fixedeff.rda'))
        
        PosteriorInteractions <- fit.frame.eff.admin2$fit$summary.random$time.area
        save(PosteriorInteractions,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin2_noStrata_PosteriorInteractions.rda'))
        
        posteriorRandomSlopes <- fit.frame.eff.admin2$fit$summary.random$st.slope
        save(posteriorRandomSlopes,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_rw1xICAR_admin2_noStrata_posteriorRandomSlopes.rda'))
        
        if(loadSamples & refit){
          stop('loadSamples cannot be TRUE while refit = TRUE')
        }else if(loadSamples & !refit){
          load(paste0(folder.name, '/',
                      country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin2.rda'),
               envir = .GlobalEnv)
        }else if(!loadSamples){
          if(length(unique(mod.dat$frame)) >1 ){
            frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
            frame.legend <- unlist(lapply(frame.names, function(x){
              split <- strsplit(x, "-")
              return(split[[1]][1])
            }))
          }else{
            frame.legend <- as.character(unique(mod.dat$frame))
          }    
          if(length(unique(frame.legend)) != 1){
            strata.weights <- expand.grid(years = beg.year:end.year,
                                          region = admin2.names$Internal,
                                          frame = frame.legend)
          }else{
            strata.weights <- expand.grid(years = beg.year:end.year,
                                          region = admin2.names$Internal,
                                          frame = unique(as.character(mod.dat$frame)))
          }
          
          strata.weights$urban <- strata.weights$rural <- NA
          adj.idx <-  match(admin2.names$GADM[match(strata.weights$region,
                                                    admin2.names$Internal)],
                            poly.adm2@data$NAME_2)
          strata.weights$region.adj <- poly.adm2@data$NAME_1[adj.idx]
          
          for(area in unique(strata.weights$region.adj)){
            area <- as.character(area)
            # int <- c(as.character(admin2.names$Internal[match(area,
            #                                                   admin2.names$GADM)]),
            #          as.character(admin2.names$Internal[match(area,
            #                                                   admin2.names$GADM)]))
            # int <- int[!is.na(as.character(int))]
            prop <- prop.urban[prop.urban$admin == area,]
            prop <- prop[prop$year %in% beg.year:end.year,]
            prop.yrs <- sort(prop$year)
            min.yr <- min(prop.yrs)
            max.yr <- max(prop.yrs)
            strata.weights$urban[strata.weights$region.adj == area] <- c(rep(prop$urban_prop[1],
                                                                             min.yr - beg.year),
                                                                         prop$urban_prop,
                                                                         rep(prop$urban_prop[nrow(prop)],
                                                                             end.year - max.yr))
            
          }
          strata.weights$rural <- 1- strata.weights$urban
          
          res.frame.eff.admin2 <- getSmoothed(inla_mod = fit.frame.eff.admin2,
                                              year_range = beg.year:end.year,
                                              year_label = beg.year:end.year,
                                              nsim = 1000,
                                              Amat = admin2.mat, 
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights,
                                              weight.frame = NULL,
                                              draws = NULL, save.draws = TRUE)
          save(res.frame.eff.admin2,
               file = paste0(folder.name, '/',
                             country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin2.rda'))
        }
        
        #### Benchmark ####
        if(doBenchmark & !useHIVAdj){
          message(paste0("Starting stratified random slopes with RW1xICAR admin 1 benchmarking model for ",
                         country, ".\n"))
          
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_frame_rw2main_randomSlopes_rw1xICAR_admin2.rda'), envir = .GlobalEnv)
            }
            
            
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            strata.weights.bench <- expand.grid(frame = frame.legend,
                                                years = beg.year:end.year)
            strata.weights.bench$urban <- NA
            for(frm in frame.legend){
              prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
              prop <- prop[order(prop$year),]
              min.yr <- min(prop$year)
              max.yr <- max(prop$year)
              strata.weights.bench$urban[strata.weights.bench$frame == frm] <-
                #t(unpd.prop.urban[,as.character(beg.year:end.year)])
                c(rep(prop$urban_prop[1],
                      min.yr - beg.year),
                  prop$urban_prop,
                  rep(prop$urban_prop[nrow(prop)],
                      end.year - max.yr))
            }
            strata.weights.bench$rural <- 1 - strata.weights.bench$urban
            
            res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.admin2, 
                                              year_range = beg.year:end.year, 
                                              year_label = beg.year:end.year,
                                              nsim = 1000, 
                                              Amat = admin2.mat, 
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights.bench,
                                              weight.frame = NULL,
                                              include_subnational = FALSE,
                                              draws = NULL, save.draws = TRUE)
            
            
            rm(fit.frame.eff.admin2)
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year,
                                     frame = unique(mod.dat$frame))
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              frm <- bench.adj$frame[i]
              bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                      res.frame.eff.natl$overall$frame == frm]
              max.year <- max(mod.dat$years[mod.dat$frame == frm])
              if(yr %in% beg.year:max.year){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                         res.frame.eff.natl$overall$frame == frm]
              }          
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            
            save(bench.adj, 
                 file = paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_rw1xICAR_admin2Benchmarks.rda'))
            
            fit.frame.eff.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                             Amat = admin2.mat, geo = poly.adm2,
                                             year_label = beg.year:end.year,
                                             rw = 2, ar = 0, st.rw = 1,
                                             pc.st.slope.u = 1, 
                                             pc.st.slope.alpha = 0.01,
                                             type.st = type.st,
                                             bias.adj = bench.adj, 
                                             overdisp.mean = -7.5,
                                             overdisp.prec = 0.39,
                                             bias.adj.by = c(adj.varnames, 'frame'),
                                             age.groups = levels(mod.dat$age),
                                             age.n = c(1,11,12,12,12,12),
                                             age.rw.group = c(1,2,3,3,3,3),
                                             verbose = FALSE,
                                             survey.effect = survey.effect,
                                             strata.time.effect = TRUE)
            save(fit.frame.eff.admin2, 
                 file = paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench.rda'), envir = .GlobalEnv)
          }
          
          
          hyperpar.table <- fit.frame.eff.admin2$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.frame.eff.admin2$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_fixedeff.rda'))
          
          temporals <- getDiag(fit.frame.eff.admin2, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_temporals.rda'))
          
          spaces <- getDiag(fit.frame.eff.admin2, field = "space",
                            Amat = admin2.mat)
          save(spaces, 
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.frame.eff.admin2, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin2.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_spatiotemporals.rda'))
          
          PosteriorInteractions <- fit.frame.eff.admin2$fit$summary.random$time.area
          save(PosteriorInteractions,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_PosteriorInteractions.rda'))
          
          posteriorRandomSlopes <- fit.frame.eff.admin2$fit$summary.random$st.slope
          save(posteriorRandomSlopes,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_posteriorRandomSlopes.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            if(length(unique(frame.legend)) != 1){
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin2.names$Internal,
                                            frame = frame.legend)
            }else{
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin2.names$Internal,
                                            frame = unique(as.character(mod.dat$frame)))
            }
            
            strata.weights$urban <- strata.weights$rural <- NA
            
            adj.idx <-  match(admin2.names$GADM[match(strata.weights$region,
                                                      admin2.names$Internal)],
                              poly.adm2@data$NAME_2)
            strata.weights$region.adj <- poly.adm2@data$NAME_1[adj.idx]
            
            for(area in unique(strata.weights$region.adj)){
              area <- as.character(area)
              # int <- c(as.character(admin2.names$Internal[match(area,
              #                                                   admin2.names$GADM)]),
              #          as.character(admin2.names$Internal[match(area,
              #                                                   admin2.names$GADM)]))
              # int <- int[!is.na(as.character(int))]
              prop <- prop.urban[prop.urban$admin == area,]
              prop <- prop[prop$year %in% beg.year:end.year,]
              prop.yrs <- sort(prop$year)
              min.yr <- min(prop.yrs)
              max.yr <- max(prop.yrs)
              strata.weights$urban[strata.weights$region.adj == area] <- c(rep(prop$urban_prop[1],
                                                                               min.yr - beg.year),
                                                                           prop$urban_prop,
                                                                           rep(prop$urban_prop[nrow(prop)],
                                                                               end.year - max.yr))
              
            }
            strata.weights$rural <- 1- strata.weights$urban
            
            res.frame.eff.admin2 <- getSmoothed(inla_mod = fit.frame.eff.admin2, Amat = admin2.mat,
                                                year_range = beg.year:end.year, 
                                                year_label = beg.year:end.year,
                                                nsim = 1000, save.draws.est = TRUE,
                                                weight.strata = strata.weights,
                                                weight.frame = NULL,
                                                draws = NULL, save.draws = TRUE)
            save(res.frame.eff.admin2, file = paste0(folder.name, '/',
                                                     country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench.rda'))
          }
          
          
          
        }else if(doBenchmark & useHIVAdj){
          
          message(paste0("Starting stratified random slopes RW1xICAR admin 1 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_frame_rw2main_randomSlopes_rw1xICAR_admin2.rda'), envir = .GlobalEnv)
            }
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            strata.weights.bench <- expand.grid(frame = frame.legend,
                                                years = beg.year:end.year)
            for(frm in frame.legend){
              prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
              prop <- prop[order(prop$year),]
              min.yr <- min(prop$year)
              max.yr <- max(prop$year)
              strata.weights.bench$urban[strata.weights.bench$frame == frm] <-
                #t(unpd.prop.urban[,as.character(beg.year:end.year)])
                c(rep(prop$urban_prop[1],
                      min.yr - beg.year),
                  prop$urban_prop,
                  rep(prop$urban_prop[nrow(prop)],
                      end.year - max.yr))
            }
            strata.weights.bench$rural <- 1 - strata.weights.bench$urban
            
            
            res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.admin2, 
                                              year_range = beg.year:end.year, 
                                              year_label = beg.year:end.year,
                                              nsim = 1000, 
                                              Amat = admin2.mat,
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights.bench,
                                              weight.frame = NULL,
                                              include_subnational = FALSE,
                                              draws = NULL, save.draws = TRUE)
            
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year,
                                     frame = frame.legend)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              frm <- bench.adj$frame[i]
              bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                      res.frame.eff.natl$overall$frame == frm]
              max.year <- max(mod.dat$years[mod.dat$frame == frm])
              if(yr %in% beg.year:max.year){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                         res.frame.eff.natl$overall$frame == frm]
              }          
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, 
                 file = paste0(folder.name, '/',
                               country,
                               '_frame_rw2main_randomSlopes_rw1xICAR_admin2Benchmarks.rda'))
            
            bench.adj <- merge(bench.adj, hiv.adj,
                               by = c('country', 'years'),
                               suffixes = c('.bench', '.hiv'))
            bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
            bench.adj$frame <- bench.adj$frame.bench
            
            fit.frame.eff.admin2 <- fitINLA2(data = mod.dat, 
                                             family = "betabinomial",
                                             Amat = admin2.mat, geo = poly.adm2,
                                             year_label = beg.year:end.year,
                                             rw = 2, ar = 0, st.rw = 1,
                                             pc.st.slope.u = 1, 
                                             pc.st.slope.alpha = 0.01,
                                             type.st = type.st,
                                             bias.adj = bench.adj, 
                                             overdisp.mean = -7.5,
                                             overdisp.prec = 0.39,
                                             bias.adj.by = c(adj.varnames, 'frame'),
                                             age.groups = levels(mod.dat$age),
                                             age.n = c(1,11,12,12,12,12),
                                             age.rw.group = c(1,2,3,3,3,3),
                                             verbose = FALSE,
                                             survey.effect = survey.effect,
                                             strata.time.effect = TRUE)
            save(fit.frame.eff.admin2,
                 file = paste0(folder.name, '/',
                               country,
                               '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench.rda'), envir = .GlobalEnv) 
          }
          
          hyperpar.table <- fit.frame.eff.admin2$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.frame.eff.admin2$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.frame.eff.admin2, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.frame.eff.admin2, field = "space",
                            Amat = admin2.mat)
          save(spaces, file = paste0(folder.name, '/',
                                     country, 
                                     '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.frame.eff.admin2, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin2.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_spatiotemporals.rda'))
          
          PosteriorInteractions <- fit.frame.eff.admin2$fit$summary.random$time.area
          save(PosteriorInteractions,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_PosteriorInteractions.rda'))
          
          posteriorRandomSlopes <- fit.frame.eff.admin2$fit$summary.random$st.slope
          save(posteriorRandomSlopes,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench_noStrata_posteriorRandomSlopes.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench = TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            if(length(unique(frame.legend)) != 1){
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin2.names$Internal,
                                            frame = frame.legend)
            }else{
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin2.names$Internal,
                                            frame = unique(as.character(mod.dat$frame)))
            }
            
            strata.weights$urban <- strata.weights$rural <- NA
            
            adj.idx <-  match(admin2.names$GADM[match(strata.weights$region,
                                                      admin2.names$Internal)],
                              poly.adm2@data$NAME_2)
            strata.weights$region.adj <- poly.adm2@data$NAME_1[adj.idx]
            
            for(area in unique(strata.weights$region.adj)){
              area <- as.character(area)
              # int <- c(as.character(admin2.names$Internal[match(area,
              #                                                   admin2.names$GADM)]),
              #          as.character(admin2.names$Internal[match(area,
              #                                                   admin2.names$GADM)]))
              # int <- int[!is.na(as.character(int))]
              prop <- prop.urban[prop.urban$admin == area,]
              prop <- prop[prop$year %in% beg.year:end.year,]
              prop.yrs <- sort(prop$year)
              min.yr <- min(prop.yrs)
              max.yr <- max(prop.yrs)
              strata.weights$urban[strata.weights$region.adj == area] <- c(rep(prop$urban_prop[1],
                                                                               min.yr - beg.year),
                                                                           prop$urban_prop,
                                                                           rep(prop$urban_prop[nrow(prop)],
                                                                               end.year - max.yr))
              
            }
            strata.weights$rural <- 1- strata.weights$urban
            
            res.frame.eff.admin2 <- getSmoothed(inla_mod = fit.frame.eff.admin2, 
                                                year_range = beg.year:end.year, 
                                                year_label = beg.year:end.year,
                                                nsim = 1000, save.draws.est = TRUE,
                                                weight.strata = strata.weights,
                                                Amat = admin2.mat,
                                                weight.frame = NULL,
                                                draws = NULL, save.draws = TRUE)
            save(res.frame.eff.admin2, 
                 file = paste0(folder.name, '/',
                               country, '_res_frame_rw2main_randomSlopes_rw1xICAR_admin2Bench.rda'))
          }
        }
        
      }
      
      #### random slopes + AR1xICAR ####
      if (doRandomSlopesAR1) {
        message(paste0("Starting stratified random slopes AR1xICAR admin 1 model for ", country, ".\n"))
        bench.adj <- expand.grid(country = country,
                                 years = beg.year:end.year,
                                 frame = unique(mod.dat$frame))
        bench.adj$ratio <- 1.0
        
        if(useHIVAdj){
          hiv.adj$frame <- 0
          for(svy in unique(hiv.adj$survey)){
            hiv.adj$frame[hiv.adj$survey == svy] <- unique(as.character(frames$Frame[frames$Survey.Year == svy]))
          }
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years', 'frame'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
        }
        
        if(!file.exists(paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_ar1xICAR_admin2.rda')) | refit){
          
          
          fit.frame.eff.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                           Amat = admin2.mat, geo = poly.adm2,
                                           year_label = beg.year:end.year,
                                           rw = 2, ar = 1,
                                           pc.st.slope.u = 1, 
                                           pc.st.slope.alpha = 0.01,
                                           type.st = type.st,
                                           bias.adj = bench.adj,
                                           bias.adj.by = c(adj.varnames),
                                           age.groups = levels(mod.dat$age),
                                           age.n = c(1,11,12,12,12,12),
                                           age.rw.group = c(1,2,3,3,3,3),
                                           verbose = FALSE,
                                           survey.effect = survey.effect,
                                           strata.time.effect = TRUE)
          
          # options = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE)
          save(fit.frame.eff.admin2, 
               file = paste0(folder.name, '/',
                             country, '_frame_rw2main_randomSlopes_ar1xICAR_admin2.rda'))
        }else{
          load(paste0(folder.name, '/', country,
                      '_frame_rw2main_randomSlopes_ar1xICAR_admin2.rda'), envir = .GlobalEnv)
        }
        
        hyperpar.table <- fit.frame.eff.admin2$fit$summary.hyperpar
        save(hyperpar.table,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin2_noStrata_hyperpar.rda'))
        
        temporals <- getDiag(fit.frame.eff.admin2, field = "time",
                             year_label = beg.year:end.year)
        save(temporals,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin2_noStrata_temporals.rda'))
        spaces <- getDiag(fit.frame.eff.admin2, field = "space",
                          Amat = admin2.mat)
        save(spaces,
             file = paste0(folder.name, '/',
                           country, 
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin2_noStrata_spatials.rda'))
        spacetimes <- getDiag(fit.frame.eff.admin2, field = "spacetime",
                              year_label =  beg.year:end.year,
                              Amat = admin2.mat)
        save(spacetimes,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin2_noStrata_spatiotemporals.rda'))
        
        fixed.eff.table <- fit.frame.eff.admin2$fit$summary.fixed
        save(fixed.eff.table,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin2_noStrata_fixedeff.rda'))
        
        PosteriorInteractions <- fit.frame.eff.admin2$fit$summary.random$time.area
        save(PosteriorInteractions,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin2_noStrata_PosteriorInteractions.rda'))
        
        posteriorRandomSlopes <- fit.frame.eff.admin2$fit$summary.random$st.slope
        save(posteriorRandomSlopes,
             file = paste0(folder.name, '/',
                           country,
                           '_frame_rw2main_randomSlopes_ar1xICAR_admin2_noStrata_posteriorRandomSlopes.rda'))
        
        if(loadSamples & refit){
          stop('loadSamples cannot be TRUE while refit = TRUE')
        }else if(loadSamples & !refit){
          load(paste0(folder.name, '/',
                      country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin2.rda'),
               envir = .GlobalEnv)
        }else if(!loadSamples){
          if(length(unique(mod.dat$frame)) >1 ){
            frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
            frame.legend <- unlist(lapply(frame.names, function(x){
              split <- strsplit(x, "-")
              return(split[[1]][1])
            }))
          }else{
            frame.legend <- as.character(unique(mod.dat$frame))
          }    
          
          if(length(unique(frame.legend)) != 1){
            strata.weights <- expand.grid(years = beg.year:end.year,
                                          region = admin2.names$Internal,
                                          frame = frame.legend)
          }else{
            strata.weights <- expand.grid(years = beg.year:end.year,
                                          region = admin2.names$Internal,
                                          frame = unique(as.character(mod.dat$frame)))
          }
          
          strata.weights$urban <- strata.weights$rural <- NA
          adj.idx <-  match(admin2.names$GADM[match(strata.weights$region,
                                                    admin2.names$Internal)],
                            poly.adm2@data$NAME_2)
          strata.weights$region.adj <- poly.adm2@data$NAME_1[adj.idx]
          
          for(area in unique(strata.weights$region.adj)){
            area <- as.character(area)
            # int <- c(as.character(admin2.names$Internal[match(area,
            #                                                   admin2.names$GADM)]),
            #          as.character(admin2.names$Internal[match(area,
            #                                                   admin2.names$GADM)]))
            # int <- int[!is.na(as.character(int))]
            prop <- prop.urban[prop.urban$admin == area,]
            prop <- prop[prop$year %in% beg.year:end.year,]
            prop.yrs <- sort(prop$year)
            min.yr <- min(prop.yrs)
            max.yr <- max(prop.yrs)
            strata.weights$urban[strata.weights$region.adj == area] <- c(rep(prop$urban_prop[1],
                                                                             min.yr - beg.year),
                                                                         prop$urban_prop,
                                                                         rep(prop$urban_prop[nrow(prop)],
                                                                             end.year - max.yr))
            
          }
          strata.weights$rural <- 1- strata.weights$urban
          
          res.frame.eff.admin2 <- getSmoothed(inla_mod = fit.frame.eff.admin2,
                                              year_range = beg.year:end.year,
                                              year_label = beg.year:end.year,
                                              nsim = 1000,
                                              Amat = admin2.mat,
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights,
                                              weight.frame = NULL,
                                              draws = NULL, save.draws = TRUE)
          save(res.frame.eff.admin2,
               file = paste0(folder.name, '/',
                             country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin2.rda'))
        }
        
        #### Benchmark ####
        if(doBenchmark & !useHIVAdj){
          message(paste0("Starting stratified random slopes with AR1xICAR admin 1 benchmarking model for ",
                         country, ".\n"))
          
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_frame_rw2main_randomSlopes_ar1xICAR_admin2.rda'), envir = .GlobalEnv)
            }
            
            
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            strata.weights.bench <- expand.grid(frame = frame.legend,
                                                years = beg.year:end.year)
            strata.weights.bench$urban <- NA
            for(frm in frame.legend){
              prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
              prop <- prop[order(prop$year),]
              min.yr <- min(prop$year)
              max.yr <- max(prop$year)
              strata.weights.bench$urban[strata.weights.bench$frame == frm] <-
                #t(unpd.prop.urban[,as.character(beg.year:end.year)])
                c(rep(prop$urban_prop[1],
                      min.yr - beg.year),
                  prop$urban_prop,
                  rep(prop$urban_prop[nrow(prop)],
                      end.year - max.yr))
            }
            strata.weights.bench$rural <- 1 - strata.weights.bench$urban
            
            res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.admin2, 
                                              year_range = beg.year:end.year, 
                                              year_label = beg.year:end.year,
                                              nsim = 1000, 
                                              Amat = admin2.mat, 
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights.bench,
                                              weight.frame = NULL,
                                              include_subnational = FALSE,
                                              draws = NULL, save.draws = TRUE)
            
            
            rm(fit.frame.eff.admin2)
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year,
                                     frame = unique(mod.dat$frame))
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              frm <- bench.adj$frame[i]
              bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                      res.frame.eff.natl$overall$frame == frm]
              max.year <- max(mod.dat$years[mod.dat$frame == frm])
              if(yr %in% beg.year:max.year){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                         res.frame.eff.natl$overall$frame == frm]
              }          
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            
            save(bench.adj, 
                 file = paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_ar1xICAR_admin2Benchmarks.rda'))
            
            fit.frame.eff.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                             Amat = admin2.mat, geo = poly.adm2,
                                             year_label = beg.year:end.year,
                                             rw = 2, ar = 1,
                                             pc.st.slope.u = 1, 
                                             pc.st.slope.alpha = 0.01,
                                             type.st = type.st,
                                             bias.adj = bench.adj, 
                                             overdisp.mean = -7.5,
                                             overdisp.prec = 0.39,
                                             bias.adj.by = c(adj.varnames, 'frame'),
                                             age.groups = levels(mod.dat$age),
                                             age.n = c(1,11,12,12,12,12),
                                             age.rw.group = c(1,2,3,3,3,3),
                                             verbose = FALSE,
                                             survey.effect = survey.effect,
                                             strata.time.effect = TRUE)
            save(fit.frame.eff.admin2, 
                 file = paste0(folder.name, '/',
                               country, '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench.rda'), envir = .GlobalEnv)
          }
          
          
          hyperpar.table <- fit.frame.eff.admin2$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.frame.eff.admin2$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_fixedeff.rda'))
          
          temporals <- getDiag(fit.frame.eff.admin2, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_temporals.rda'))
          
          spaces <- getDiag(fit.frame.eff.admin2, field = "space",
                            Amat = admin2.mat)
          save(spaces, 
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.frame.eff.admin2, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin2.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_spatiotemporals.rda'))
          
          PosteriorInteractions <- fit.frame.eff.admin2$fit$summary.random$time.area
          save(PosteriorInteractions,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_PosteriorInteractions.rda'))
          
          posteriorRandomSlopes <- fit.frame.eff.admin2$fit$summary.random$st.slope
          save(posteriorRandomSlopes,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_posteriorRandomSlopes.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            if(length(unique(frame.legend)) != 1){
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin2.names$Internal,
                                            frame = frame.legend)
            }else{
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin2.names$Internal,
                                            frame = unique(as.character(mod.dat$frame)))
            }
            
            strata.weights$urban <- strata.weights$rural <- NA
            
            
            adj.idx <-  match(admin2.names$GADM[match(strata.weights$region,
                                                      admin2.names$Internal)],
                              poly.adm2@data$NAME_2)
            strata.weights$region.adj <- poly.adm2@data$NAME_1[adj.idx]
            
            for(area in unique(strata.weights$region.adj)){
              area <- as.character(area)
              # int <- c(as.character(admin2.names$Internal[match(area,
              #                                                   admin2.names$GADM)]),
              #          as.character(admin2.names$Internal[match(area,
              #                                                   admin2.names$GADM)]))
              # int <- int[!is.na(as.character(int))]
              prop <- prop.urban[prop.urban$admin == area,]
              prop <- prop[prop$year %in% beg.year:end.year,]
              prop.yrs <- sort(prop$year)
              min.yr <- min(prop.yrs)
              max.yr <- max(prop.yrs)
              strata.weights$urban[strata.weights$region.adj == area] <- c(rep(prop$urban_prop[1],
                                                                               min.yr - beg.year),
                                                                           prop$urban_prop,
                                                                           rep(prop$urban_prop[nrow(prop)],
                                                                               end.year - max.yr))
              
            }
            strata.weights$rural <- 1- strata.weights$urban
            
            res.frame.eff.admin2 <- getSmoothed(inla_mod = fit.frame.eff.admin2,
                                                Amat = admin2.mat,
                                                year_range = beg.year:end.year, 
                                                year_label = beg.year:end.year,
                                                nsim = 1000, save.draws.est = TRUE,
                                                weight.strata = strata.weights,
                                                weight.frame = NULL,
                                                draws = NULL, save.draws = TRUE)
            save(res.frame.eff.admin2, file = paste0(folder.name, '/',
                                                     country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench.rda'))
          }
          
          
          
        }else if(doBenchmark & useHIVAdj){
          
          message(paste0("Starting stratified random slopes AR1xICAR admin 1 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_frame_rw2main_randomSlopes_ar1xICAR_admin2.rda'), envir = .GlobalEnv)
            }
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            strata.weights.bench <- expand.grid(frame = frame.legend,
                                                years = beg.year:end.year)
            for(frm in frame.legend){
              prop <- prop.urban.natl[prop.urban.natl$year %in% beg.year:end.year,]
              prop <- prop[order(prop$year),]
              min.yr <- min(prop$year)
              max.yr <- max(prop$year)
              strata.weights.bench$urban[strata.weights.bench$frame == frm] <-
                #t(unpd.prop.urban[,as.character(beg.year:end.year)])
                c(rep(prop$urban_prop[1],
                      min.yr - beg.year),
                  prop$urban_prop,
                  rep(prop$urban_prop[nrow(prop)],
                      end.year - max.yr))
            }
            strata.weights.bench$rural <- 1 - strata.weights.bench$urban
            
            
            res.frame.eff.natl <- getSmoothed(inla_mod = fit.frame.eff.admin2, 
                                              year_range = beg.year:end.year, 
                                              year_label = beg.year:end.year,
                                              nsim = 1000, 
                                              Amat = admin2.mat,
                                              save.draws.est = TRUE,
                                              weight.strata = strata.weights.bench,
                                              weight.frame = NULL,
                                              include_subnational = FALSE,
                                              draws = NULL, save.draws = TRUE)
            
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year,
                                     frame = frame.legend)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              frm <- bench.adj$frame[i]
              bench.adj$est[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                      res.frame.eff.natl$overall$frame == frm]
              max.year <- max(mod.dat$years[mod.dat$frame == frm])
              if(yr %in% beg.year:max.year){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.frame.eff.natl$overall$median[res.frame.eff.natl$overall$years.num == yr&
                                                                         res.frame.eff.natl$overall$frame == frm]
              }          
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, 
                 file = paste0(folder.name, '/',
                               country,
                               '_frame_rw2main_randomSlopes_ar1xICAR_admin2Benchmarks.rda'))
            
            bench.adj <- merge(bench.adj, hiv.adj,
                               by = c('country', 'years'),
                               suffixes = c('.bench', '.hiv'))
            bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
            bench.adj$frame <- bench.adj$frame.bench
            
            fit.frame.eff.admin2 <- fitINLA2(data = mod.dat, 
                                             family = "betabinomial",
                                             Amat = admin2.mat, geo = poly.adm2,
                                             year_label = beg.year:end.year,
                                             rw = 2, ar = 1,
                                             pc.st.slope.u = 1, 
                                             pc.st.slope.alpha = 0.01,
                                             type.st = type.st,
                                             bias.adj = bench.adj, 
                                             overdisp.mean = -7.5,
                                             overdisp.prec = 0.39,
                                             bias.adj.by = c(adj.varnames, 'frame'),
                                             age.groups = levels(mod.dat$age),
                                             age.n = c(1,11,12,12,12,12),
                                             age.rw.group = c(1,2,3,3,3,3),
                                             verbose = FALSE,
                                             survey.effect = survey.effect,
                                             strata.time.effect = TRUE)
            save(fit.frame.eff.admin2,
                 file = paste0(folder.name, '/',
                               country,
                               '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench.rda'), envir = .GlobalEnv) 
          }
          
          hyperpar.table <- fit.frame.eff.admin2$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.frame.eff.admin2$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.frame.eff.admin2, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.frame.eff.admin2, field = "space",
                            Amat = admin2.mat)
          save(spaces, file = paste0(folder.name, '/',
                                     country, 
                                     '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.frame.eff.admin2, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin2.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_spatiotemporals.rda'))
          
          PosteriorInteractions <- fit.frame.eff.admin2$fit$summary.random$time.area
          save(PosteriorInteractions,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_PosteriorInteractions.rda'))
          
          posteriorRandomSlopes <- fit.frame.eff.admin2$fit$summary.random$st.slope
          save(posteriorRandomSlopes,
               file = paste0(folder.name, '/',
                             country,
                             '_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench_noStrata_posteriorRandomSlopes.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench = TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            if(length(unique(mod.dat$frame)) >1 ){
              frame.names <- fit.frame.eff.admin2$strata.base[grepl(tolower("urban"), fit.frame.eff.admin2$strata.base)]
              frame.legend <- unlist(lapply(frame.names, function(x){
                split <- strsplit(x, "-")
                return(split[[1]][1])
              }))
            }else{
              frame.legend <- as.character(unique(mod.dat$frame))
            }    
            
            if(length(unique(frame.legend)) != 1){
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin2.names$Internal,
                                            frame = frame.legend)
            }else{
              strata.weights <- expand.grid(years = beg.year:end.year,
                                            region = admin2.names$Internal,
                                            frame = unique(as.character(mod.dat$frame)))
            }
            
            strata.weights$urban <- strata.weights$rural <- NA
            
            adj.idx <-  match(admin2.names$GADM[match(strata.weights$region,
                                                      admin2.names$Internal)],
                              poly.adm2@data$NAME_2)
            strata.weights$region.adj <- poly.adm2@data$NAME_1[adj.idx]
            
            for(area in unique(strata.weights$region.adj)){
              area <- as.character(area)
              # int <- c(as.character(admin2.names$Internal[match(area,
              #                                                   admin2.names$GADM)]),
              #          as.character(admin2.names$Internal[match(area,
              #                                                   admin2.names$GADM)]))
              # int <- int[!is.na(as.character(int))]
              prop <- prop.urban[prop.urban$admin == area,]
              prop <- prop[prop$year %in% beg.year:end.year,]
              prop.yrs <- sort(prop$year)
              min.yr <- min(prop.yrs)
              max.yr <- max(prop.yrs)
              strata.weights$urban[strata.weights$region.adj == area] <- c(rep(prop$urban_prop[1],
                                                                               min.yr - beg.year),
                                                                           prop$urban_prop,
                                                                           rep(prop$urban_prop[nrow(prop)],
                                                                               end.year - max.yr))
            }
            strata.weights$rural <- 1- strata.weights$urban
            
            res.frame.eff.admin2 <- getSmoothed(inla_mod = fit.frame.eff.admin2, 
                                                year_range = beg.year:end.year, 
                                                year_label = beg.year:end.year,
                                                nsim = 1000, 
                                                weight.strata = strata.weights,
                                                Amat = admin2.mat,
                                                weight.frame = NULL, save.draws.est = TRUE,
                                                draws = NULL, save.draws = TRUE)
            save(res.frame.eff.admin2, 
                 file = paste0(folder.name, '/',
                               country, '_res_frame_rw2main_randomSlopes_ar1xICAR_admin2Bench.rda'))
          }
        }
        
      }
    }
  }
}


#### Libraries ####
#devtools::install_github("bryandmartin/SUMMER",
#                         build_vignettes = F, force = T)
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
INLA:::inla.dynload.workaround()
#####
# set number of theads for the cluster. For a custom value, replace NULL with number of 
# desired threads.  Starts at 32 for largemem and 12 for short/medium, although our code 
# defaults at a lower number to reduce memory use.
# DO NOT RUN THIS ON YOUR LAPTOP!
nthreads = NULL
if(is.null(nthreads)) {
  # figure out if we are in largemem queue or short/medium
  hostname = system("hostname", intern=TRUE)
  largemem = grepl(hostname, "newton")
  
  # set number of threads accordingly
  if(largemem)
    nthreads = 32 # this is for pure speed of finishing job, using max memory
  else
    nthreads = 8 # reduce memory by 50% from default of 12 threads. Hopefully keeps memory under 100GB
  inla.setOption(num.threads=nthreads)
}
#inla.setOption(pardiso.license = '/homes/jlg0003/pardisoUW.lic')
#inla.setOption(mkl = TRUE)
library(survey)
library(ggplot2)
library(gridExtra)
library(parallel)


#### Parameters ####
country <- "Malawi"
cluster <- FALSE
message("If have the same subfolder structure as 
        AfricaAdmin2Estimates/Data/countryDataFolders/. Do nothing!\n
        Otherwise, edit the following paths as needed.\n")

data.dir <- './toCluster'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '..'
ihme.dir.rel <- '..'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '..'

# data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
# code.dir.rel <- '../../Analysis/R'
# igme.dir.rel <- '../../Analysis/R'
# ihme.dir.rel <- '../../Analysis/R'
# shapes.sub.dir <- '/shapeFiles_gadm'
# hiv.dir.rel <- '../HIV/'


setwd(data.dir)

if(!exists("sheet_key", envir = .GlobalEnv)){
  # source(paste0(code.dir.rel,'/LoadCommandCenter.R'))
}
#CountryList <- sheets_read(sheet_key, sheet = "CountryList")
CountryList <- read.csv("CountryList.csv", header = T)
folder.name <- CountryList$folderName[CountryList$Country == country]

#hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
#                       gsub(" ", "", folder.name))
hand.dir.rel <- gsub(" ", "", folder.name)
pop.dir.rel <- folder.name

ptm <- proc.time()
fitBetabin(country, type.st = 4, 
           beg.year = 1990, 
           end.year = 2020, 
           doRandomSlopesRW1 = TRUE,
           doRandomSlopesAR1 = FALSE,
           frame.strata =TRUE, doBenchmark = TRUE, 
           doNatl = TRUE, doAdmin1= FALSE,
           doAdmin2 = TRUE,
           refit = TRUE, refitBench = TRUE,
           code.dir.rel = code.dir.rel,
           igme.dir.rel = igme.dir.rel,
           ihme.dir.rel = ihme.dir.rel,
           hand.dir.rel = hand.dir.rel,
           shapes.sub.dir = shapes.sub.dir,
           hiv.dir.rel = hiv.dir.rel,
           pop.dir.rel = pop.dir.rel)
proc.time()-ptm

#### RUN ####
# n.cores <- 4
# clus <- makeCluster(n.cores, outfile = 'Betabin_out.txt')
# valid.results <- parLapply(clus, countries[1], fitBetabin,
#                            beg.year = 1990,
#                            end.year = 2019,
#                            type.st = 1,
#                            fixed.strata = T,
#                            age.strata = F,
#                            frame.age.strata = F,
#                            cluster = T)
# 
# stopCluster(clus)



