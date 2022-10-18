#' filename: Betabinomial.R
#' author: Jessica Godwin
#'  edited: Austin Schumacher
#' sources: LoadCommandCenter.R

#'
#' loads: all files in /shapeFiles_gadm 
#'        CountryName_Amat_Names.rda
#'        CountryName_Amat.rda
#'        CountryName_cluster_dat.rda
#'        Data/HIV/HIVAdjustments.rda
#'        CountryName_HIVnames.key.rda (if exists)
#'        Results.csv
#'        



rm(list = ls())


fitBetabin <- function(country, type.st, beg.year,
                       end.year, doBenchmark=NULL,
                       doAdmin2 = FALSE, doRW = TRUE, doAR = TRUE,
                       doAdmin1 = TRUE, doNatl = FALSE,
                       no.strata, cluster,refit = FALSE,
                       refitBench = FALSE, loadSamples = TRUE,
                       code.dir.rel, igme.dir.rel,
                       ihme.dir.rel, hand.dir.rel,
                       shapes.sub.dir, hiv.dir.rel){
  
  #### Set directory ####
  if(cluster){
    setwd('./countryDataFolders')
    library(SUMMER)
    library(rgdal)
  }
  
  
  
  # CountryList <- sheets_read(sheet_key, sheet = "CountryList")
  CountryList <- read.csv('CountryList.csv', header = T)
  message(paste0("Starting function for ", country, ".\n"))
  
  folder.name <- CountryList$folderName[CountryList$Country == country]
  gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
  n.survey <- CountryList$nSurvey[CountryList$Country == country]
  
  #### Use HIV Adjusted data? ####
  #HIV.sheet <- gs_read(sheet_key, ws = "HIV")
  #HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
  HIV.sheet <- read.csv("HIV.csv", header = T)
  #  print(names(HIV.sheet)) 
  HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
  #useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
  #               unique(HIV.country$`UNAIDS data?`) == "Y")
  useHIVAdj <- (unique(HIV.country$MM.Adj.by.IGME) == "Y" & unique(HIV.country$UNAIDS.data.) == "Y")
   #### Get Survey years #### 
  
  #SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
  #SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
  SurveyInfo <- read.csv("SurveyInfo.csv")
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
  mod.dat$strata.orig <- mod.dat$strata
  mod.dat$strata <- mod.dat$urban
  mod.dat$country <- as.character(country)
  
  #### Load IGME data  ####
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
  
  #### No Strata ####
  if(no.strata){
    
    #### National ####
    
    if(doNatl){
      
      mod.dat$region <- "All"
      mod.dat$strata <- NA
      
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
      
      #### RW2 ####
      if(doRW){
        message(paste0("Starting no strata rw2 natl model for ", country, ".\n"))
        
        fit.natl <- fitINLA2(data = mod.dat, family = "betabinomial",
                             Amat = NULL, geo = NULL,
                             year_label = beg.year:end.year,
                             rw = 2, type.st = type.st,
                             bias.adj = adj.frame, overdisp.mean = -7.5,
                             overdisp.prec = 0.39,
                             bias.adj.by = adj.varnames,
                             age.groups = levels(mod.dat$age),
                             age.n = c(1,11,12,12,12,12),
                             age.rw.group = c(1,2,3,3,3,3),
                             verbose = FALSE,
                             survey.effect = TRUE)
        save(fit.natl, file = paste0(folder.name, '/',
                                     country, '_rw2_natl.rda'))
        
        res.natl <- getSmoothed(inla_mod = fit.natl, 
                                year_range = beg.year:end.year, 
                                year_label = beg.year:end.year, nsim = 1000, 
                                weight.strata = NULL, 
                                weight.frame = NULL,
                                draws = NULL, save.draws = TRUE)
        save(res.natl, file = paste0(folder.name, '/',
                                     country, '_res_rw2_natl.rda'))
        
        
        if(!dir.exists(paths = paste0(folder.name, '/Plots/', 'Betabinomial'))){
          dir.create(path = paste0(folder.name, '/Plots/', 'Betabinomial'))
        }
        
        
        if(doBenchmark & !useHIVAdj){
          
          message(paste0("Starting no strata rw2 national benchmarking model for ",
                         country, ".\n"))
          
          bench.adj <- expand.grid(country = country,
                                   years = beg.year:end.year)
          bench.adj$est <- bench.adj$igme <- NA
          for(i in 1:nrow(bench.adj)){
            yr <- bench.adj$years[i]
            bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            if(yr %in% beg.year:max(mod.dat$year)){
              bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
            }else{
              bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            }         
          }
          bench.adj$ratio <- bench.adj$est/bench.adj$igme
          save(bench.adj, file = paste0(folder.name, '/',
                                        country, '_rw2_natlBenchmarks.rda'))
          
          fit.natl <- fitINLA2(data = mod.dat, family = "betabinomial",
                               Amat = NULL, geo = NULL,
                               year_label = beg.year:end.year,
                               rw = 2, type.st = type.st,
                               bias.adj = bench.adj, overdisp.mean = -7.5,
                               overdisp.prec = 0.39,
                               bias.adj.by = adj.varnames,
                               age.groups = levels(mod.dat$age),
                               age.n = c(1,11,12,12,12,12),
                               age.rw.group = c(1,2,3,3,3,3),
                               verbose = FALSE,
                               survey.effect = TRUE)
          save(fit.natl, file = paste0(folder.name, '/',
                                       country, '_rw2_natlBench.rda'))
          
          res.natl <- getSmoothed(inla_mod = fit.natl, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = 1000, 
                                  weight.strata = NULL,
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
          save(res.natl, file = paste0(folder.name, '/',
                                       country, '_res_rw2_natlBench.rda'))
          
          
          
          
        }else{
          
          message(paste0("Starting no strata rw2 national benchmarking model for ",
                         country, ".\n"))
          
          bench.adj <- expand.grid(country = country,
                                   years = beg.year:end.year)
          bench.adj$est <- bench.adj$igme <- NA
          for(i in 1:nrow(bench.adj)){
            yr <- bench.adj$years[i]
            bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            if(yr %in% beg.year:max(mod.dat$year)){
              bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
            }else{
              bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            }          }
          bench.adj$ratio <- bench.adj$est/bench.adj$igme
          save(bench.adj, file = paste0(folder.name, '/',
                                        country, '_rw2_natlBenchmarks.rda'))
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
          
          fit.natl <- fitINLA2(data = mod.dat, family = "betabinomial",
                               Amat = NULL, geo = NULL,
                               year_label = beg.year:end.year,
                               rw = 2, type.st = type.st,
                               bias.adj = bench.adj,overdisp.mean = -7.5,
                               overdisp.prec = 0.39,
                               bias.adj.by = adj.varnames,
                               age.groups = levels(mod.dat$age),
                               age.n = c(1,11,12,12,12,12),
                               age.rw.group = c(1,2,3,3,3,3),
                               verbose = FALSE,
                               survey.effect = TRUE)
          save(fit.natl, file = paste0(folder.name, '/',
                                       country, '_rw2_natlBench.rda'))
          
          res.natl <- getSmoothed(inla_mod = fit.natl, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = 1000, 
                                  weight.strata = NULL, 
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
          save(res.natl, file = paste0(folder.name, '/',
                                       country, '_res_rw2_natlBench.rda'))
          
          
        }
        
      }
    }
     
    ####  Admin 1 ####
    
    if(doAdmin1){
      
      mod.dat$region <- mod.dat$admin1.char
      mod.dat$strata <- NA
      
      
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
      
      #### RW2 ####
      if(doRW){
        
        bench.adj <- adj.frame
        bench.adj$ratio <- 1.0
        if(!file.exists(paste0(folder.name, '/',
                               country, '_rw2_admin1.rda')) | refit){
          message(paste0("Starting no strata rw2 admin 1 model for ", country, ".\n"))
          
          fit.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial", 
                                 Amat = admin1.mat, geo = poly.adm1,
                                 year_label = beg.year:end.year, 
                                 rw = 2, ar = 0, type.st = type.st, 
                                 bias.adj = bench.adj,
                                 bias.adj.by = adj.varnames,
                                 age.groups = levels(mod.dat$age),
                                 age.n = c(1,11,12,12,12,12), 
                                 age.rw.group = c(1,2,3,3,3,3),
                                 verbose = FALSE, 
                                 survey.effect = TRUE)
          save(fit.admin1, file = paste0(folder.name, '/',
                                         country, '_rw2_admin1.rda'))
        }else{
          load(paste0(folder.name, '/', country,
                      '_rw2_admin1.rda'), envir = .GlobalEnv)
        }
        
        hyperpar.table <- fit.admin1$fit$summary.hyperpar
        save(hyperpar.table,
             file = paste0(folder.name, '/', 
                           country, 
                           '_rw2_admin1_noStrata_hyperpar.rda'))
        
        temporals <- getDiag(fit.admin1, field = "time",
                             year_label = beg.year:end.year)
        save(temporals,
             file = paste0(folder.name, '/',
                           country,
                           '_rw2_admin1_noStrata_temporals.rda'))
        spaces <- getDiag(fit.admin1, field = "space",
                          Amat = admin1.mat)
        save(spaces, 
             file = paste0(folder.name, '/',
                           country, '_rw2_admin1_noStrata_spatials.rda'))
        spacetimes <- getDiag(fit.admin1, field = "spacetime",
                                 year_label =  beg.year:end.year,
                                 Amat = admin1.mat)
        save(spacetimes,
             file = paste0(folder.name, '/',
                           country, 
                           '_rw2_admin1_noStrata_spatiotemporals.rda'))
        
        fixed.eff.table <- fit.admin1$fit$summary.fixed
        save(fixed.eff.table,
             file = paste0(folder.name, '/',
                           country,
                           '_rw2_admin1_noStrata_fixedeff.rda'))
        
        if(loadSamples & refit){
          stop('loadSamples cannot be TRUE while refit = TRUE')
        }else if(loadSamples & !refit){
          load(paste0(folder.name, '/',
                      country, '_res_rw2_admin1.rda'), envir = .GlobalEnv)
        }else if(!loadSamples){
          res.admin1 <- getSmoothed(inla_mod = fit.admin1, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin1.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    draws = NULL, save.draws = TRUE)
          save(res.admin1, file = paste0(folder.name, '/',
                                         country, '_res_rw2_admin1.rda'))
        }      
        
        #### Benchmark ####
        
        if(doBenchmark & !useHIVAdj){
          message(paste0("Starting no strata rw2 Admin1 benchmarking model for ",
                         country, ".\n"))
          
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_rw2_admin1.rda'), envir = .GlobalEnv)
            }
            res.natl <- getSmoothed(inla_mod = fit.admin1, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin1.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    include_subnational = FALSE,
                                    draws = NULL, save.draws = TRUE)
            
            
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              if(yr %in% beg.year:max(mod.dat$year)){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              }         
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, file = paste0(folder.name, '/',
                                          country, '_rw2_admin1Benchmarks.rda'))
            
            fit.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                   Amat = admin1.mat, geo = poly.adm1,
                                   year_label = beg.year:end.year,
                                   rw = 2, type.st = type.st,
                                   bias.adj = bench.adj, overdisp.mean = -7.5,
                                   overdisp.prec = 0.39,
                                   bias.adj.by = adj.varnames,
                                   age.groups = levels(mod.dat$age),
                                   age.n = c(1,11,12,12,12,12),
                                   age.rw.group = c(1,2,3,3,3,3),
                                   verbose = FALSE,
                                   survey.effect = TRUE)
            save(fit.admin1, file = paste0(folder.name, '/',
                                           country, '_rw2_admin1Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_rw2_admin1Bench.rda'), envir = .GlobalEnv)
          }
          
          
          hyperpar.table <- fit.admin1$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin1Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.admin1$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin1Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.admin1, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin1Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.admin1, field = "space",
                            Amat = admin1.mat)
          save(spaces, file = paste0(folder.name, '/',
                               country, '_rw2_admin1Bench_noStrata_spatials.rda'))
          spacetimes <- getDiag(fit.admin1, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin1.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_rw2_admin1Bench_noStrata_spatiotemporals.rda'))
          
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_rw2_admin1Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            res.admin1 <- getSmoothed(inla_mod = fit.admin1, Amat = admin1.mat,
                                      year_range = beg.year:end.year, 
                                      year_label = beg.year:end.year, nsim = 1000, 
                                      weight.strata = NULL,
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE)
            save(res.admin1, file = paste0(folder.name, '/',
                                           country, '_res_rw2_admin1Bench.rda'))
          }
          
          
          
        }else if(doBenchmark & useHIVAdj){
          
          message(paste0("Starting no strata rw2 Admin 1 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_rw2_admin1.rda'), envir = .GlobalEnv)
            }
            res.natl <- getSmoothed(inla_mod = fit.admin1, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin1.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    include_subnational = FALSE,
                                    draws = NULL, save.draws = TRUE)
            
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              if(yr %in% beg.year:max(mod.dat$year)){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              }          }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, file = paste0(folder.name, '/',
                                          country, '_rw2_admin1Benchmarks.rda'))
            bench.adj <- merge(bench.adj, hiv.adj,
                               by = c('country', 'years'),
                               suffixes = c('.bench', '.hiv'))
            bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
            
            fit.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                   Amat = admin1.mat, geo = poly.adm1,
                                   year_label = beg.year:end.year,
                                   rw = 2, type.st = type.st,
                                   bias.adj = bench.adj, overdisp.mean = -7.5,
                                   overdisp.prec = 0.39,
                                   bias.adj.by = adj.varnames,
                                   age.groups = levels(mod.dat$age),
                                   age.n = c(1,11,12,12,12,12),
                                   age.rw.group = c(1,2,3,3,3,3),
                                   verbose = FALSE,
                                   survey.effect = TRUE)
            save(fit.admin1, file = paste0(folder.name, '/',
                                           country, '_rw2_admin1Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_rw2_admin1Bench.rda'), envir = .GlobalEnv) 
          }
          
          hyperpar.table <- fit.admin1$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin1Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.admin1$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin1Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.admin1, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin1Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.admin1, field = "space",
                            Amat = admin1.mat)
          save(spaces, file = paste0(folder.name, '/',
                               country, 
                               '_rw2_admin1Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.admin1, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin1.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_rw2_admin1Bench_noStrata_spatiotemporals.rda'))
          
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench = TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_rw2_admin1Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            res.admin1 <- getSmoothed(inla_mod = fit.admin1, 
                                      year_range = beg.year:end.year, 
                                      year_label = beg.year:end.year, nsim = 1000, 
                                      weight.strata = NULL, Amat = admin1.mat,
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE)
            save(res.admin1, file = paste0(folder.name, '/',
                                           country, '_res_rw2_admin1Bench.rda'))
          }
        }
        
      }
      
      #### AR(1) ####
      if(doAR){
        bench.adj <- adj.frame
        bench.adj$ratio <- 1.0
        
        
        message(paste0("Starting no strata AR(1) admin 1 model for ", country, ".\n"))
        if(!file.exists(paste0(folder.name, '/',
                               country, '_ar1_admin1.rda')) | refit){
          fit.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial", 
                                 Amat = admin1.mat, geo = poly.adm1,
                                 year_label = beg.year:end.year, 
                                 rw = 2, ar = 1, type.st = type.st, 
                                 bias.adj = bench.adj,
                                 bias.adj.by = adj.varnames,
                                 age.groups = levels(mod.dat$age),
                                 age.n = c(1,11,12,12,12,12), 
                                 age.rw.group = c(1,2,3,3,3,3),
                                 verbose = TRUE, 
                                 survey.effect = TRUE)
          save(fit.admin1, file = paste0(folder.name, '/',
                                         country, '_ar1_admin1.rda'))
        }else{
          load(paste0(folder.name, '/', country,
                      '_ar1_admin1.rda'), envir = .GlobalEnv)
        }
        
        hyperpar.table <- fit.admin1$fit$summary.hyperpar
        save(hyperpar.table,
             file = paste0(folder.name, '/', 
                           country, 
                           '_ar1_admin1_noStrata_hyperpar.rda'))
        
        fixed.eff.table <- fit.admin1$fit$summary.fixed
        save(fixed.eff.table,
             file = paste0(folder.name, '/', 
                           country, 
                           '_ar1_admin1_noStrata_fixedeff.rda'))
        
        
        temporals <- getDiag(fit.admin1, field = "time",
                             year_label = beg.year:end.year)
        save(temporals,
             file = paste0(folder.name, '/', 
                           country, 
                           '_ar1_admin1_noStrata_temporals.rda'))
        spaces <- getDiag(fit.admin1, field = "space",
                          Amat = admin1.mat)
        save(spaces, file = paste0(folder.name, '/',
                             country,
                             '_ar1_admin1_noStrata_spatials.rda'))
        
        spacetimes <- getDiag(fit.admin1, field = "spacetime",
                              year_label =  beg.year:end.year,
                              Amat = admin1.mat)
        save(spacetimes,
             file = paste0(folder.name, '/',
                           country, 
                           '_ar1_admin1_noStrata_spatiotemporals.rda'))
        
        
        if(loadSamples & refit){
          stop('loadSamples cannot be TRUE while refit = TRUE')
        }else if(loadSamples & !refit){
          load(paste0(folder.name, '/',
                      country, '_res_ar1_admin1.rda'), envir = .GlobalEnv)
        }else if(!loadSamples){
          res.admin1 <- getSmoothed(inla_mod = fit.admin1, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin1.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    draws = NULL, save.draws = TRUE)
        
          save(res.admin1, file = paste0(folder.name, '/',
                                         country, '_res_ar1_admin1.rda'))
        }        
        
        
        #### Benchmark ####
        if(doBenchmark & !useHIVAdj){
          message(paste0("Starting no strata ar1 Admin1 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_ar1_admin1.rda'), envir = .GlobalEnv)
            }
            
            res.natl <- getSmoothed(inla_mod = fit.admin1, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin1.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    include_subnational = FALSE,
                                    draws = NULL, save.draws = TRUE)
          
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              if(yr %in% beg.year:max(mod.dat$year)){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              }         
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, file = paste0(folder.name, '/',
                                          country, '_ar1_admin1Benchmarks.rda'))
            
            fit.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                   Amat = admin1.mat, geo = poly.adm1,
                                   year_label = beg.year:end.year,
                                   rw = 2, ar = 1, type.st = type.st,
                                   bias.adj = bench.adj, overdisp.mean = -7.5,
                                   overdisp.prec = 0.39,
                                   bias.adj.by = adj.varnames,
                                   age.groups = levels(mod.dat$age),
                                   age.n = c(1,11,12,12,12,12),
                                   age.rw.group = c(1,2,3,3,3,3),
                                   verbose = FALSE,
                                   survey.effect = TRUE)
            save(fit.admin1, file = paste0(folder.name, '/',
                                           country, '_ar1_admin1Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_ar1_admin1Bench.rda'), envir = .GlobalEnv)
          }  
          
          hyperpar.table <- fit.admin1$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin1Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.admin1$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin1Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.admin1, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin1Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.admin1, field = "space",
                            Amat = admin1.mat)
          save(spaces, file = paste0(folder.name, '/',
                               country,
                               '_ar1_admin1Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.admin1, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin1.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_ar1_admin1Bench_noStrata_spatiotemporals.rda'))
          
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_ar1_admin1Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            res.admin1 <- getSmoothed(inla_mod = fit.admin1, 
                                      year_range = beg.year:end.year, 
                                      year_label = beg.year:end.year, nsim = 1000, 
                                      weight.strata = NULL,
                                      Amat = admin1.mat,
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE)
            save(res.admin1, file = paste0(folder.name, '/',
                                           country, '_res_ar1_admin1Bench.rda'))
          }          
        }else if(doBenchmark & useHIVAdj){
          
          message(paste0("Starting no strata ar1 Admin 1 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_ar1_admin1.rda'), envir = .GlobalEnv)
            }
            res.natl <- getSmoothed(inla_mod = fit.admin1, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin1.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    include_subnational = FALSE,
                                    draws = NULL, save.draws = TRUE)
            
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              if(yr %in% beg.year:max(mod.dat$year)){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              }          }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, file = paste0(folder.name, '/',
                                          country, '_ar1_admin1Benchmarks.rda'))
            bench.adj <- merge(bench.adj, hiv.adj,
                               by = c('country', 'years'),
                               suffixes = c('.bench', '.hiv'))
            bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
            
            fit.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                   Amat = admin1.mat, geo = poly.adm1,
                                   year_label = beg.year:end.year,
                                   rw = 2, ar = 1, type.st = type.st,
                                   bias.adj = bench.adj, overdisp.mean = -7.5,
                                   overdisp.prec = 0.39,
                                   bias.adj.by = adj.varnames,
                                   age.groups = levels(mod.dat$age),
                                   age.n = c(1,11,12,12,12,12),
                                   age.rw.group = c(1,2,3,3,3,3),
                                   verbose = FALSE,
                                   survey.effect = TRUE)
            save(fit.admin1, file = paste0(folder.name, '/',
                                           country, '_ar1_admin1Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_ar1_admin1Bench.rda'), envir = .GlobalEnv)
          }          
          
          hyperpar.table <- fit.admin1$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin1Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.admin1$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin1Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.admin1, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin1Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.admin1, field = "space",
                            Amat = admin1.mat)
          save(spaces, file = paste0(folder.name, '/',
                               country,
                               '_ar1_admin1Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.admin1, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin1.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_ar1_admin1Bench_noStrata_spatiotemporals.rda'))
          
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_ar1_admin1Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            res.admin1 <- getSmoothed(inla_mod = fit.admin1, 
                                      Amat = admin1.mat,
                                      year_range = beg.year:end.year, 
                                      year_label = beg.year:end.year, nsim = 1000, 
                                      weight.strata = NULL,
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE)
            save(res.admin1, file = paste0(folder.name, '/',
                                           country, '_res_ar1_admin1Bench.rda'))
          }
          
        }
      }
      rm(fit.admin1)
      
    }
    
    ####  Admin 2 ####
    
    if(doAdmin2){
      
      mod.dat$region <- mod.dat$admin2.char
      mod.dat$strata <- NA
      
      
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
      
      #### RW2 ####
      if(doRW){
        
        bench.adj <- adj.frame
        bench.adj$ratio <- 1.0
        if(!file.exists(paste0(folder.name, '/',
                               country, '_rw2_admin2.rda')) | refit){
          message(paste0("Starting no strata rw2 admin 2 model for ", country, ".\n"))
          
          fit.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial", 
                                 Amat = admin2.mat, geo = poly.adm2,
                                 year_label = beg.year:end.year, 
                                 rw = 2, ar = 0, type.st = type.st, 
                                 bias.adj = bench.adj,
                                 bias.adj.by = adj.varnames,
                                 age.groups = levels(mod.dat$age),
                                 age.n = c(1,11,12,12,12,12), 
                                 age.rw.group = c(1,2,3,3,3,3),
                                 verbose = FALSE, 
                                 survey.effect = TRUE)
          save(fit.admin2, file = paste0(folder.name, '/',
                                         country, '_rw2_admin2.rda'))
        }else{
          load(paste0(folder.name, '/', country,
                      '_rw2_admin2.rda'), envir = .GlobalEnv)
        }
        
        hyperpar.table <- fit.admin2$fit$summary.hyperpar
        save(hyperpar.table,
             file = paste0(folder.name, '/', 
                           country, 
                           '_rw2_admin2_noStrata_hyperpar.rda'))
        
        fixed.eff.table <- fit.admin2$fit$summary.fixed
        save(fixed.eff.table,
             file = paste0(folder.name, '/', 
                           country, 
                           '_rw2_admin2_noStrata_fixedeff.rda'))
        
        
        temporals <- getDiag(fit.admin2, field = "time",
                             year_label = beg.year:end.year)
        save(temporals,
             file = paste0(folder.name, '/', 
                           country, 
                           '_rw2_admin2_noStrata_temporals.rda'))
        spaces <- getDiag(fit.admin2, field = "space",
                          Amat = admin2.mat)
        save(spaces,
             file = paste0(folder.name, '/',
                             country,
                             '_rw2_admin2_noStrata_spatials.rda'))
        
        spacetimes <- getDiag(fit.admin2, field = "spacetime",
                              year_label =  beg.year:end.year,
                              Amat = admin2.mat)
        save(spacetimes,
             file = paste0(folder.name, '/',
                           country, 
                           '_rw2_admin2_noStrata_spatiotemporals.rda'))
        
        
        if(loadSamples & refit){
          stop('loadSamples cannot be TRUE while refit = TRUE')
        }else if(loadSamples & !refit){
          load(paste0(folder.name, '/',
                      country, '_res_rw2_admin2.rda'), envir = .GlobalEnv)
        }else if(!loadSamples){
          res.admin2 <- getSmoothed(inla_mod = fit.admin2, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin2.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    draws = NULL, save.draws = TRUE)
          save(res.admin2, file = paste0(folder.name, '/',
                                         country, '_res_rw2_admin2.rda'))
        }        
        
        #### Benchmark ####
        if(doBenchmark & !useHIVAdj){
          message(paste0("Starting no strata rw2 Admin 2 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_rw2_admin2.rda'), envir = .GlobalEnv)
            }
            res.natl <- getSmoothed(inla_mod = fit.admin2, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin2.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    include_subnational = FALSE,
                                    draws = NULL, save.draws = TRUE)
          
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              if(yr %in% beg.year:max(mod.dat$year)){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              }         
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, file = paste0(folder.name, '/',
                                          country, '_rw2_admin2Benchmarks.rda'))
            
            fit.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                   Amat = admin2.mat, geo = poly.adm2,
                                   year_label = beg.year:end.year,
                                   rw = 2, type.st = type.st,
                                   bias.adj = bench.adj, overdisp.mean = -7.5,
                                   overdisp.prec = 0.39,
                                   bias.adj.by = adj.varnames,
                                   age.groups = levels(mod.dat$age),
                                   age.n = c(1,11,12,12,12,12),
                                   age.rw.group = c(1,2,3,3,3,3),
                                   verbose = FALSE,
                                   survey.effect = TRUE)
            save(fit.admin2, file = paste0(folder.name, '/',
                                           country, '_rw2_admin2Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_rw2_admin2Bench.rda'), envir = .GlobalEnv)
          }          
          
          hyperpar.table <- fit.admin2$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin2Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.admin2$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin2Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.admin2, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin2Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.admin2, field = "space",
                            Amat = admin2.mat)
          save(spaces,
               file = paste0(folder.name, '/',
                               country,
                               '_rw2_admin2Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.admin2, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin2.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_rw2_admin2Bench_noStrata_spatiotemporals.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_rw2_admin2Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            res.admin2 <- getSmoothed(inla_mod = fit.admin2, 
                                      year_range = beg.year:end.year, 
                                      year_label = beg.year:end.year, nsim = 1000, 
                                      weight.strata = NULL,Amat = admin2.mat,
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE)
            save(res.admin2, file = paste0(folder.name, '/',
                                           country, '_res_rw2_admin2Bench.rda'))
          }          
          
          
          
        }else if(doBenchmark & useHIVAdj){
          
          message(paste0("Starting no strata rw2 Admin 2 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_rw2_admin2.rda'), envir = .GlobalEnv)
            }
            res.natl <- getSmoothed(inla_mod = fit.admin2, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin2.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    include_subnational = FALSE,
                                    draws = NULL, save.draws = TRUE)
          
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              if(yr %in% beg.year:max(mod.dat$year)){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              }          }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, file = paste0(folder.name, '/',
                                          country, '_rw2_admin2Benchmarks.rda'))
            bench.adj <- merge(bench.adj, hiv.adj,
                               by = c('country', 'years'),
                               suffixes = c('.bench', '.hiv'))
            bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
            
            fit.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                   Amat = admin2.mat, geo = poly.adm2,
                                   year_label = beg.year:end.year,
                                   rw = 2, type.st = type.st,
                                   bias.adj = bench.adj, overdisp.mean = -7.5,
                                   overdisp.prec = 0.39,
                                   bias.adj.by = adj.varnames,
                                   age.groups = levels(mod.dat$age),
                                   age.n = c(1,11,12,12,12,12),
                                   age.rw.group = c(1,2,3,3,3,3),
                                   verbose = FALSE,
                                   survey.effect = TRUE)
            save(fit.admin2, file = paste0(folder.name, '/',
                                           country, '_rw2_admin2Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_rw2_admin2Bench.rda'), envir = .GlobalEnv)
          }         
          
          hyperpar.table <- fit.admin2$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin2Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.admin2$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin2Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.admin2, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_rw2_admin2Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.admin2, field = "space",
                            Amat = admin2.mat)
          save(spaces, file = paste0(folder.name, '/',
                               country,
                               '_rw2_admin2Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.admin2, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin2.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_rw2_admin2Bench_noStrata_spatiotemporals.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_rw2_admin2Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            res.admin2 <- getSmoothed(inla_mod = fit.admin2, 
                                      year_range = beg.year:end.year, 
                                      year_label = beg.year:end.year, nsim = 1000, 
                                      weight.strata = NULL,
                                      Amat = admin2.mat,
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE)
            save(res.admin2, file = paste0(folder.name, '/',
                                           country, '_res_rw2_admin2Bench.rda'))
          }
      }
      }
      #### AR(1) ####
      if(doAR){
        bench.adj <- adj.frame
        bench.adj$ratio <- 1.0
        
        
        message(paste0("Starting no strata AR(1) admin 2 model for ", country, ".\n"))
        if(!file.exists(paste0(folder.name, '/',
                               country, '_ar1_admin2.rda')) | refit){
          fit.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial", 
                                 Amat = admin2.mat, geo = poly.adm2,
                                 year_label = beg.year:end.year, 
                                 rw = 2, ar = 1, type.st = type.st, 
                                 bias.adj = bench.adj,
                                 bias.adj.by = adj.varnames,
                                 age.groups = levels(mod.dat$age),
                                 age.n = c(1,11,12,12,12,12), 
                                 age.rw.group = c(1,2,3,3,3,3),
                                 verbose = TRUE, 
                                 survey.effect = TRUE)
          save(fit.admin2, file = paste0(folder.name, '/',
                                         country, '_ar1_admin2.rda'))
        }else{
          load(paste0(folder.name, '/', country,
                      '_ar1_admin2.rda'), envir = .GlobalEnv)
        }
        
        hyperpar.table <- fit.admin2$fit$summary.hyperpar
        save(hyperpar.table,
             file = paste0(folder.name, '/', 
                           country, 
                           '_ar1_admin2_noStrata_hyperpar.rda'))
        
        fixed.eff.table <- fit.admin2$fit$summary.fixed
        save(fixed.eff.table,
             file = paste0(folder.name, '/', 
                           country, 
                           '_ar1_admin2_noStrata_fixedeff.rda'))
        
        
        temporals <- getDiag(fit.admin2, field = "time",
                             year_label = beg.year:end.year)
        save(temporals,
             file = paste0(folder.name, '/', 
                           country, 
                           '_ar1_admin2_noStrata_temporals.rda'))
        spaces <- getDiag(fit.admin2, field = "space",
                          Amat = admin2.mat)
        save(spaces,file = paste0(folder.name, '/',
                             country,
                             '_ar1_admin2_noStrata_spatials.rda'))
        
        spacetimes <- getDiag(fit.admin2, field = "spacetime",
                              year_label =  beg.year:end.year,
                              Amat = admin2.mat)
        save(spacetimes,
             file = paste0(folder.name, '/',
                           country, 
                           '_ar1_admin2_noStrata_spatiotemporals.rda'))
        
        if(loadSamples & refit){
          stop('loadSamples cannot be TRUE while refit = TRUE')
        }else if(loadSamples & !refit){
          load(paste0(folder.name, '/',
                      country, '_ar1_admin2.rda'), envir = .GlobalEnv)
        }else if(!loadSamples){
          res.admin2 <- getSmoothed(inla_mod = fit.admin2, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin2.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    draws = NULL, save.draws = TRUE)
               
        
          save(res.admin2, file = paste0(folder.name, '/',
                                         country, '_res_ar1_admin2.rda'))
        }        
        
        #### Benchmark ####
        if(doBenchmark & !useHIVAdj){
          message(paste0("Starting no strata ar1 admin2 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_ar1_admin2.rda'), envir = .GlobalEnv)
            }
            res.natl <- getSmoothed(inla_mod = fit.admin2, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin2.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    include_subnational = FALSE,
                                    draws = NULL, save.draws = TRUE)
            
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              if(yr %in% beg.year:max(mod.dat$year)){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              }         
            }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, file = paste0(folder.name, '/',
                                          country, '_ar1_admin2Benchmarks.rda'))
          
            fit.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                   Amat = admin2.mat, geo = poly.adm2,
                                   year_label = beg.year:end.year,
                                   rw = 2, ar = 1, type.st = type.st,
                                   bias.adj = bench.adj, overdisp.mean = -7.5,
                                   overdisp.prec = 0.39,
                                   bias.adj.by = adj.varnames,
                                   age.groups = levels(mod.dat$age),
                                   age.n = c(1,11,12,12,12,12),
                                   age.rw.group = c(1,2,3,3,3,3),
                                   verbose = FALSE,
                                   survey.effect = TRUE)
            save(fit.admin2, file = paste0(folder.name, '/',
                                           country, '_ar1_admin2Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_ar1_admin2Bench.rda'), envir = .GlobalEnv)
          }          
          
          hyperpar.table <- fit.admin2$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin2Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.admin2$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin2Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.admin2, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin2Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.admin2, field = "space",
                            Amat = admin2.mat)
          save(spaces, file = paste0(folder.name, '/',
                               country,
                               '_ar1_admin2Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.admin2, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin2.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_ar1_admin2Bench_noStrata_spatiotemporals.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_ar1_admin2Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            res.admin2 <- getSmoothed(inla_mod = fit.admin2,
                                      Amat = admin2.mat,
                                      year_range = beg.year:end.year, 
                                      year_label = beg.year:end.year, nsim = 1000, 
                                      weight.strata = NULL,
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE)
            save(res.admin2, file = paste0(folder.name, '/',
                                           country, '_res_ar1_admin2Bench.rda'))
          }          
        }else if(doBenchmark & useHIVAdj){
          
          message(paste0("Starting no strata ar1 admin 2 benchmarking model for ",
                         country, ".\n"))
          if(refitBench){
            if(!refit){
              load(paste0(folder.name, '/',
                          country, '_ar1_admin2.rda'), envir = .GlobalEnv)
            }
            res.natl <- getSmoothed(inla_mod = fit.admin2, 
                                    year_range = beg.year:end.year, 
                                    year_label = beg.year:end.year, nsim = 1000, 
                                    Amat = admin2.mat, save.draws.est = TRUE,
                                    weight.strata = NULL,
                                    weight.frame = NULL,
                                    include_subnational = FALSE,
                                    draws = NULL, save.draws = TRUE)
            
            bench.adj <- expand.grid(country = country,
                                     years = beg.year:end.year)
            bench.adj$est <- bench.adj$igme <- NA
            for(i in 1:nrow(bench.adj)){
              yr <- bench.adj$years[i]
              bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              if(yr %in% beg.year:max(mod.dat$year)){
                bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
              }else{
                bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
              }          }
            bench.adj$ratio <- bench.adj$est/bench.adj$igme
            save(bench.adj, file = paste0(folder.name, '/',
                                          country, '_ar1_admin2Benchmarks.rda'))
            bench.adj <- merge(bench.adj, hiv.adj,
                               by = c('country', 'years'),
                               suffixes = c('.bench', '.hiv'))
            bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
            
            fit.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial",
                                   Amat = admin2.mat, geo = poly.adm2,
                                   year_label = beg.year:end.year,
                                   rw = 2, ar = 1, type.st = type.st,
                                   bias.adj = bench.adj, overdisp.mean = -7.5,
                                   overdisp.prec = 0.39,
                                   bias.adj.by = adj.varnames,
                                   age.groups = levels(mod.dat$age),
                                   age.n = c(1,11,12,12,12,12),
                                   age.rw.group = c(1,2,3,3,3,3),
                                   verbose = FALSE,
                                   survey.effect = TRUE)
            save(fit.admin2, file = paste0(folder.name, '/',
                                           country, '_ar1_admin2Bench.rda'))
          }else{
            load(paste0(folder.name, '/',
                        country, '_ar1_admin2Bench.rda'), envir = .GlobalEnv)
          } 
          
          hyperpar.table <- fit.admin2$fit$summary.hyperpar
          save(hyperpar.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin2Bench_noStrata_hyperpar.rda'))
          
          fixed.eff.table <- fit.admin2$fit$summary.fixed
          save(fixed.eff.table,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin2Bench_noStrata_fixedeff.rda'))
          
          
          temporals <- getDiag(fit.admin2, field = "time",
                               year_label = beg.year:end.year)
          save(temporals,
               file = paste0(folder.name, '/', 
                             country, 
                             '_ar1_admin2Bench_noStrata_temporals.rda'))
          spaces <- getDiag(fit.admin2, field = "space",
                            Amat = admin2.mat)
          save(spaces, file = paste0(folder.name, '/',
                               country,
                               '_ar1_admin2Bench_noStrata_spatials.rda'))
          
          spacetimes <- getDiag(fit.admin2, field = "spacetime",
                                year_label =  beg.year:end.year,
                                Amat = admin2.mat)
          save(spacetimes,
               file = paste0(folder.name, '/',
                             country, 
                             '_ar1_admin2Bench_noStrata_spatiotemporals.rda'))
          
          if(loadSamples & refitBench){
            stop('loadSamples cannot be TRUE while refitBench is TRUE')
          }else if(loadSamples & !refitBench){
            load(paste0(folder.name, '/',
                        country, '_res_ar1_admin2Bench.rda'), envir = .GlobalEnv)
          }else if(!loadSamples){
            res.admin2 <- getSmoothed(inla_mod = fit.admin2, 
                                      year_range = beg.year:end.year, 
                                      year_label = beg.year:end.year, nsim = 1000, 
                                      weight.strata = NULL,Amat = admin2.mat,
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE)
            save(res.admin2, file = paste0(folder.name, '/',
                                           country, '_res_ar1_admin2Bench.rda'))
          }
        }
        
      }
      rm(fit.admin2)
      
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
#inla.setOption(pardiso.license = '/homes/jlg0003/pardisoUW.lic')
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
# 
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

# hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
# gsub(" ", "", folder.name))
hand.dir.rel <- paste0(gsub(" ", "", folder.name))



ptm <- proc.time()
fitBetabin(country, type.st = 4, cluster = cluster,
           beg.year = 1990, end.year = 2020,
           doAR = FALSE, doRW = TRUE, refit = TRUE,
           refitBench = TRUE, loadSamples = FALSE,
           doBenchmark = TRUE, doNatl = TRUE, no.strata = TRUE,
           doAdmin1= FALSE, doAdmin2 = FALSE,
           code.dir.rel = code.dir.rel,
           igme.dir.rel = igme.dir.rel,
           ihme.dir.rel = ihme.dir.rel,
           hand.dir.rel = hand.dir.rel,
           shapes.sub.dir = shapes.sub.dir,
           hiv.dir.rel = hiv.dir.rel)
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



