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


fitBetabin <- function(country, type.st, beg.year,
                       end.year, doBenchmark=NULL,
                       doAdmin2 = TRUE, doRW = TRUE, doAR = TRUE,
                       doAdmin1 = TRUE, doNatl = TRUE,
                       no.strata, cluster,
                       code.dir.rel, igme.dir.rel,
                       ihme.dir.rel, hand.dir.rel,
                       shapes.sub.dir, hiv.dir.rel){
  
  #### Set directory ####
  if(cluster){
    setwd('./countryDataFolders')
    library(SUMMER)
    library(rgdal)
  }
  
  
  
  CountryList <- sheets_read(sheet_key, sheet = "CountryList")
  #CountryList <- read.csv('CountryList.csv', header = T)
  message(paste0("Starting function for ", country, ".\n"))
  
  folder.name <- CountryList$folderName[CountryList$Country == country]
  gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]
  n.survey <- CountryList$nSurvey[CountryList$Country == country]
  
  #### Use HIV Adjusted data? ####
  #HIV.sheet <- gs_read(sheet_key, ws = "HIV")
  HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
  #HIV.sheet <- read.csv("HIV.csv", header = T)
  
  HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
  useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                  unique(HIV.country$`UNAIDS data?`) == "Y")
  
  #### Get Survey years #### 
  
  #SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
  SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
  #SurveyInfo <- read.csv("SurveyInfo.csv", header = T)
  surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                        SurveyInfo$`GPS?` == "Y"]
  
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
  
  #### Load IGME data ####
  file.list <- list.files(igme.dir.rel)
  igme.file <- file.list[grepl("IGME", file.list)]
  igme.ests <- read.csv(paste0(igme.dir.rel,'/',igme.file),
                        header = T)
  igme.ests <- igme.ests[igme.ests$INDICATOR == "Under-five mortality rate" &
                           igme.ests$SEX == "Total" &
                           igme.ests$SERIES_YEAR == "2019" &
                           igme.ests$SERIES_NAME == "UN IGME estimate 2019",]
  igme.ests$year <- igme.ests$REF_DATE - 0.5
  if(country == "Cote dIvoire"){
    igme.ests <- igme.ests[igme.ests$REF_AREA == levels(igme.ests$REF_AREA)[45],]  
  }else{
    igme.ests <- igme.ests[igme.ests$REF_AREA == country, ]
  }
  igme.ests <- igme.ests[order(igme.ests$year),]
  igme.ests <- igme.ests[igme.ests$year %in% beg.year:end.year,]
  igme.ests
  
  
  #### Load IHME data ####
  file.list <- list.files(ihme.dir.rel)
  ihme.files <- file.list[grepl("IHME", file.list)]
  
  ihme.ests <- list()
  ihme.ests[['adm0']] <- read.csv( paste0(ihme.dir.rel,'/',
                                          ihme.files[grepl("ADM0", ihme.files)]),
                                   header = T)
  ihme.ests[['adm1']] <- read.csv( paste0(ihme.dir.rel,'/',
                                          ihme.files[grepl("ADMIN1", ihme.files)]),
                                   header = T)
  ihme.ests[['adm2']] <- read.csv( paste0(ihme.dir.rel,'/',
                                          ihme.files[grepl("ADMIN2", ihme.files)]),
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
  
  #stop("This part will be different for everyone!")
  source(paste0(hand.dir.rel,
                '/IHMEHand_', country, '.R'), local = T)
  
  
  #### No Strata ####
  if(no.strata){
    
    #### National ####
    
    if(doNatl){
      if(useHIVAdj){
        load(paste0(folder.name, '/',
                    country, '_directHIV_natl_yearly.rda'), envir = .GlobalEnv)
      }else{
        load(paste0(folder.name, '/',
                    country, '_direct_natl_yearly.rda'), envir = .GlobalEnv)
      }
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
          
          if(sum(grepl("HIVnames.key", list.files(folder.name))) != 0){
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
        load(paste0(folder.name, '/',
                    country, '_res_natl_yearly_SmoothedDirect.rda'), envir = .GlobalEnv)
        fit.natl <- fitINLA2(data = mod.dat, family = "betabinomial",
                             Amat = NULL, geo = NULL,
                             year_label = beg.year:end.year,
                             rw = 2, type.st = type.st,
                             bias.adj = adj.frame,
                             bias.adj.by = adj.varnames,
                             age.groups = levels(mod.dat$age),
                             age.n = c(1,11,12,12,12,12),
                             age.rw.group = c(1,2,3,3,3,3),
                             verbose = TRUE,
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
        
        pdf(paste0(folder.name, '/Plots/Betabinomial/',
                   country, '_rw2_natl_noStrata_spaghetti.pdf'), height = 4, width = 8)
        {
          tmp.area <- res.natl$overall
          tmp.area$width <- tmp.area$upper - tmp.area$lower
          tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
          tmp.area$cex2[tmp.area$cex2 > 6] <- 6
          tmp.ihme <- ihme.ests[[1]]
          cols <- rainbow(length(surveys)+1+1+1)
          plot.years <- beg.year:end.year
          
          par(mfrow = c(1,2))
          if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
            plot.max <- max(direct.natl.yearly$mean+.025, na.rm = T)
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
                       main =  paste0(admin1.names$GADM[area]))
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
            igme.years <- jitter(igme.ests$year)
            lines(igme.years, igme.ests$OBS_VALUE/1000,
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
              
              
              if(svy.idx== 1){
                if(dim(tmp)[1] != 0){
                  plot(NA,
                       xlab = "Year", ylab = "U5MR",
                       ylim = c(0, plot.max),
                       xlim = c(beg.year, end.year),
                       type = 'l', col = cols[svy.idx], lwd = 2,
                       main = country)
                  
                  
                  ihme.years <- jitter(tmp.ihme$year)
                  polygon(x = c(ihme.years, rev(ihme.years)),
                          y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
                          col = alpha(cols[length(surveys)+1], 0.25), border = cols[length(surveys)+1])
                  
                }else{
                  plot(NA,
                       xlab = "Year", ylab = "U5MR",
                       ylim = c(0, plot.max),
                       xlim = c(beg.year, end.year),
                       type = 'l', col = cols[svy.idx], lwd = 2,
                       main =  paste0(admin1.names$GADM[area]))
                }
              }
              
              
            }
            
            igme.years <- jitter(igme.ests$year)
            polygon(x = c(igme.years, rev(igme.years)),
                    y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
                    col = alpha(cols[length(surveys)+2], 0.25), border = cols[length(surveys)+2])
            res.tmp <- tmp.area
            res.tmp$years <- as.numeric(as.character(res.tmp$years))
            polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
                    y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                          rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
                    col = alpha(cols[length(surveys)+3], 0.25), border = cols[length(surveys)+3])
            polygon(x = c(res.tmp$years, rev(res.tmp$years)),
                    y = c(res.tmp$upper,
                          rev(res.tmp$lower)),
                    col = alpha('black', 0.25), border = 'black')
            legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                         'black'), .25),
                   border = c(cols[(length(surveys)+1):length(cols)],
                              'black'),
                   legend = c( 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
          }
          
          
        }
        dev.off()
        
        if(doBenchmark & !useHIVAdj){
          
          message(paste0("Starting no strata rw2 national benchmarking model for ",
                         country, ".\n"))
          load(paste0(folder.name, '/',
                      country, '_res_natlBench_yearly_SmoothedDirect.rda'), envir = .GlobalEnv)
          bench.adj <- expand.grid(country = country,
                                   years = beg.year:end.year)
          bench.adj$est <- bench.adj$igme <- NA
          for(i in 1:nrow(bench.adj)){
            yr <- bench.adj$years[i]
            bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            if(yr %in% igme.ests$year){
              bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
            }else{
              bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            }          }
          bench.adj$ratio <- bench.adj$est/bench.adj$igme
          save(bench.adj, file = paste0(folder.name, '/',
                                        country, '_rw2_natlBenchmarks.rda'))
          
          fit.natl <- fitINLA2(data = mod.dat, family = "betabinomial",
                               Amat = NULL, geo = NULL,
                               year_label = beg.year:end.year,
                               rw = 2, type.st = type.st,
                               bias.adj = bench.adj,
                               bias.adj.by = adj.varnames,
                               age.groups = levels(mod.dat$age),
                               age.n = c(1,11,12,12,12,12),
                               age.rw.group = c(1,2,3,3,3,3),
                               verbose = TRUE,
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
          
          pdf(paste0(folder.name, '/Plots/Betabinomial/',
                     country, '_rw2_natlBench_noStrata_spaghetti.pdf'), height = 4, width = 8)
          {
            tmp.area <- res.natl$overall
            tmp.area$width <- tmp.area$upper - tmp.area$lower
            tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
            tmp.area$cex2[tmp.area$cex2 > 6] <- 6
            tmp.ihme <- ihme.ests[[1]]
            cols <- rainbow(length(surveys)+1+1+1)
            plot.years <- beg.year:end.year
            
            par(mfrow = c(1,2))
            if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
              plot.max <- max(direct.natl.yearly$mean+.025, na.rm = T)
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
                         main =  paste0(admin1.names$GADM[area]))
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
              igme.years <- jitter(igme.ests$year)
              lines(igme.years, igme.ests$OBS_VALUE/1000,
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
                
                
                if(svy.idx== 1){
                  if(dim(tmp)[1] != 0){
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main = country)
                    
                    
                    ihme.years <- jitter(tmp.ihme$year)
                    polygon(x = c(ihme.years, rev(ihme.years)),
                            y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
                            col = alpha(cols[length(surveys)+1], 0.25), border = cols[length(surveys)+1])
                    
                  }else{
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main =  paste0(admin1.names$GADM[area]))
                  }
                }
                
                
              }
              
              igme.years <- jitter(igme.ests$year)
              polygon(x = c(igme.years, rev(igme.years)),
                      y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
                      col = alpha(cols[length(surveys)+2], 0.25), border = cols[length(surveys)+2])
              res.tmp <- tmp.area
              res.tmp$years <- as.numeric(as.character(res.tmp$years))
              polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
                      y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                            rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
                      col = alpha(cols[length(surveys)+3], 0.25), border = cols[length(surveys)+3])
              polygon(x = c(res.tmp$years, rev(res.tmp$years)),
                      y = c(res.tmp$upper,
                            rev(res.tmp$lower)),
                      col = alpha('black', 0.25), border = 'black')
              legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                           'black'), .25),
                     border = c(cols[(length(surveys)+1):length(cols)],
                                'black'),
                     legend = c( 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
            }
            
            
          }
          dev.off()
          
          
        }else{
          
          message(paste0("Starting no strata rw2 national benchmarking model for ",
                         country, ".\n"))
          load(paste0(folder.name, '/',
                      country, '_res_natlBench_yearly_SmoothedDirect.rda'), envir = .GlobalEnv)
          bench.adj <- expand.grid(country = country,
                                   years = beg.year:end.year)
          bench.adj$est <- bench.adj$igme <- NA
          for(i in 1:nrow(bench.adj)){
            yr <- bench.adj$years[i]
            bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            if(yr %in% igme.ests$year){
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
                               bias.adj = bench.adj,
                               bias.adj.by = adj.varnames,
                               age.groups = levels(mod.dat$age),
                               age.n = c(1,11,12,12,12,12),
                               age.rw.group = c(1,2,3,3,3,3),
                               verbose = TRUE,
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
          
          pdf(paste0(folder.name, '/Plots/Betabinomial/',
                     country, '_rw2_natlBench_noStrata_spaghetti.pdf'), height = 4, width = 8)
          {
            tmp.area <- res.natl$overall
            tmp.area$width <- tmp.area$upper - tmp.area$lower
            tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
            tmp.area$cex2[tmp.area$cex2 > 6] <- 6
            tmp.ihme <- ihme.ests[[1]]
            cols <- rainbow(length(surveys)+1+1+1)
            plot.years <- beg.year:end.year
            
            par(mfrow = c(1,2))
            if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
              plot.max <- max(direct.natl.yearly$mean+.025, na.rm = T)
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
                         main =  paste0(admin1.names$GADM[area]))
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
              igme.years <- jitter(igme.ests$year)
              lines(igme.years, igme.ests$OBS_VALUE/1000,
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
                
                
                if(svy.idx== 1){
                  if(dim(tmp)[1] != 0){
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main = country)
                    
                    
                    ihme.years <- jitter(tmp.ihme$year)
                    polygon(x = c(ihme.years, rev(ihme.years)),
                            y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
                            col = alpha(cols[length(surveys)+1], 0.25), border = cols[length(surveys)+1])
                    
                  }else{
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main =  paste0(admin1.names$GADM[area]))
                  }
                }
                
                
              }
              
              igme.years <- jitter(igme.ests$year)
              polygon(x = c(igme.years, rev(igme.years)),
                      y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
                      col = alpha(cols[length(surveys)+2], 0.25), border = cols[length(surveys)+2])
              res.tmp <- tmp.area
              res.tmp$years <- as.numeric(as.character(res.tmp$years))
              polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
                      y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                            rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
                      col = alpha(cols[length(surveys)+3], 0.25), border = cols[length(surveys)+3])
              polygon(x = c(res.tmp$years, rev(res.tmp$years)),
                      y = c(res.tmp$upper,
                            rev(res.tmp$lower)),
                      col = alpha('black', 0.25), border = 'black')
              legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                           'black'), .25),
                     border = c(cols[(length(surveys)+1):length(cols)],
                                'black'),
                     legend = c( 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
            }
            
            
          }
          dev.off()
          
          
        }
        
      }
      
      
      #### AR(1) ####
      if(doAR){
        message(paste0("Starting no strata ar1 natl model for ", country, ".\n"))
        load(paste0(folder.name, '/',
                    country, '_res_natl_yearly_SmoothedDirect.rda'), envir = .GlobalEnv)
        fit.natl <- fitINLA2(data = mod.dat, family = "betabinomial",
                             Amat = NULL, geo = NULL,
                             year_label = beg.year:end.year,
                             ar1 = TRUE, type.st = type.st,
                             bias.adj = adj.frame,
                             bias.adj.by = adj.varnames,
                             age.groups = levels(mod.dat$age),
                             age.n = c(1,11,12,12,12,12),
                             age.rw.group = c(1,2,3,3,3,3),
                             verbose = TRUE,
                             survey.effect = TRUE)
        save(fit.natl, file = paste0(folder.name, '/',
                                     country, '_ar1_natl.rda'))
        
        res.natl <- getSmoothed(inla_mod = fit.natl, 
                                year_range = beg.year:end.year, 
                                year_label = beg.year:end.year, nsim = 1000, 
                                weight.strata = NULL,
                                weight.frame = NULL,
                                draws = NULL, save.draws = TRUE)
        save(res.natl, file = paste0(folder.name, '/',
                                     country, '_res_ar1_natl.rda'))
        
        
        if(!dir.exists(paths = paste0(folder.name, '/Plots/', 'Betabinomial'))){
          dir.create(path = paste0(folder.name, '/Plots/', 'Betabinomial'))
        }
        
        pdf(paste0(folder.name, '/Plots/Betabinomial/',
                   country, '_ar1_natl_noStrata_spaghetti.pdf'), height = 4, width = 8)
        {
          tmp.area <- res.natl$overall
          tmp.area$width <- tmp.area$upper - tmp.area$lower
          tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
          tmp.area$cex2[tmp.area$cex2 > 6] <- 6
          tmp.ihme <- ihme.ests[[1]]
          cols <- rainbow(length(surveys)+1+1+1)
          plot.years <- beg.year:end.year
          
          par(mfrow = c(1,2))
          if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
            plot.max <- max(direct.natl.yearly$mean+.025, na.rm = T)
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
                       main =  paste0(admin1.names$GADM[area]))
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
            igme.years <- jitter(igme.ests$year)
            lines(igme.years, igme.ests$OBS_VALUE/1000,
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
              
              
              if(svy.idx== 1){
                if(dim(tmp)[1] != 0){
                  plot(NA,
                       xlab = "Year", ylab = "U5MR",
                       ylim = c(0, plot.max),
                       xlim = c(beg.year, end.year),
                       type = 'l', col = cols[svy.idx], lwd = 2,
                       main = country)
                  
                  
                  ihme.years <- jitter(tmp.ihme$year)
                  polygon(x = c(ihme.years, rev(ihme.years)),
                          y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
                          col = alpha(cols[length(surveys)+1], 0.25), border = cols[length(surveys)+1])
                  
                }else{
                  plot(NA,
                       xlab = "Year", ylab = "U5MR",
                       ylim = c(0, plot.max),
                       xlim = c(beg.year, end.year),
                       type = 'l', col = cols[svy.idx], lwd = 2,
                       main =  paste0(admin1.names$GADM[area]))
                }
              }
              
              
            }
            
            igme.years <- jitter(igme.ests$year)
            polygon(x = c(igme.years, rev(igme.years)),
                    y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
                    col = alpha(cols[length(surveys)+2], 0.25), border = cols[length(surveys)+2])
            res.tmp <- tmp.area
            res.tmp$years <- as.numeric(as.character(res.tmp$years))
            polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
                    y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                          rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
                    col = alpha(cols[length(surveys)+3], 0.25), border = cols[length(surveys)+3])
            polygon(x = c(res.tmp$years, rev(res.tmp$years)),
                    y = c(res.tmp$upper,
                          rev(res.tmp$lower)),
                    col = alpha('black', 0.25), border = 'black')
            legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                         'black'), .25),
                   border = c(cols[(length(surveys)+1):length(cols)],
                              'black'),
                   legend = c( 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
          }
          
          
        }
        dev.off()
        
        if(doBenchmark & !useHIVAdj){
          
          message(paste0("Starting no strata ar1 national benchmarking model for ",
                         country, ".\n"))
          load(paste0(folder.name, '/',
                      country, '_res_natlBench_yearly_SmoothedDirect.rda'), envir = .GlobalEnv)
          bench.adj <- expand.grid(country = country,
                                   years = beg.year:end.year)
          bench.adj$est <- bench.adj$igme <- NA
          for(i in 1:nrow(bench.adj)){
            yr <- bench.adj$years[i]
            bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            if(yr %in% igme.ests$year){
              bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
            }else{
              bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            }              }
          bench.adj$ratio <- bench.adj$est/bench.adj$igme
          save(bench.adj, file = paste0(folder.name, '/',
                                        country, '_ar1_natlBenchmarks.rda'))
          
          fit.natl <- fitINLA2(data = mod.dat, family = "betabinomial",
                               Amat = NULL, geo = NULL,
                               year_label = beg.year:end.year,
                               ar1 = TRUE, type.st = type.st,
                               bias.adj = bench.adj,
                               bias.adj.by = adj.varnames,
                               age.groups = levels(mod.dat$age),
                               age.n = c(1,11,12,12,12,12),
                               age.rw.group = c(1,2,3,3,3,3),
                               verbose = TRUE,
                               survey.effect = TRUE)
          save(fit.natl, file = paste0(folder.name, '/',
                                       country, '_ar1_natlBench.rda'))
          
          res.natl <- getSmoothed(inla_mod = fit.natl, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = 1000, 
                                  weight.strata = NULL, 
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
          save(res.natl, file = paste0(folder.name, '/',
                                       country, '_res_ar1_natlBench.rda'))
          
          pdf(paste0(folder.name, '/Plots/Betabinomial/',
                     country, '_ar1_natlBench_noStrata_spaghetti.pdf'), height = 4, width = 8)
          {
            tmp.area <- res.natl$overall
            tmp.area$width <- tmp.area$upper - tmp.area$lower
            tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
            tmp.area$cex2[tmp.area$cex2 > 6] <- 6
            tmp.ihme <- ihme.ests[[1]]
            cols <- rainbow(length(surveys)+1+1+1)
            plot.years <- beg.year:end.year
            
            par(mfrow = c(1,2))
            if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
              plot.max <- max(direct.natl.yearly$mean+.025, na.rm = T)
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
                         main =  paste0(admin1.names$GADM[area]))
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
              igme.years <- jitter(igme.ests$year)
              lines(igme.years, igme.ests$OBS_VALUE/1000,
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
                
                
                if(svy.idx== 1){
                  if(dim(tmp)[1] != 0){
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main = country)
                    
                    
                    ihme.years <- jitter(tmp.ihme$year)
                    polygon(x = c(ihme.years, rev(ihme.years)),
                            y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
                            col = alpha(cols[length(surveys)+1], 0.25), border = cols[length(surveys)+1])
                    
                  }else{
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main =  paste0(admin1.names$GADM[area]))
                  }
                }
                
                
              }
              
              igme.years <- jitter(igme.ests$year)
              polygon(x = c(igme.years, rev(igme.years)),
                      y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
                      col = alpha(cols[length(surveys)+2], 0.25), border = cols[length(surveys)+2])
              res.tmp <- tmp.area
              res.tmp$years <- as.numeric(as.character(res.tmp$years))
              polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
                      y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                            rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
                      col = alpha(cols[length(surveys)+3], 0.25), border = cols[length(surveys)+3])
              polygon(x = c(res.tmp$years, rev(res.tmp$years)),
                      y = c(res.tmp$upper,
                            rev(res.tmp$lower)),
                      col = alpha('black', 0.25), border = 'black')
              legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                           'black'), .25),
                     border = c(cols[(length(surveys)+1):length(cols)],
                                'black'),
                     legend = c( 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
            }
            
            
          }
          dev.off()
          
        }else{
          load(paste0(folder.name, '/',
                      country, '_res_natlBench_yearly_SmoothedDirect.rda'), envir = .GlobalEnv)
          bench.adj <- expand.grid(country = country,
                                   years = beg.year:end.year)
          bench.adj$est <- bench.adj$igme <- NA
          for(i in 1:nrow(bench.adj)){
            yr <- bench.adj$years[i]
            bench.adj$est[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            if(yr %in% igme.ests$year){
              bench.adj$igme[i] <- igme.ests$OBS_VALUE[igme.ests$year == yr]/1000
            }else{
              bench.adj$igme[i] <- res.natl$overall$median[res.natl$overall$years.num == yr]
            }          }
          bench.adj$ratio <- bench.adj$est/bench.adj$igme
          save(bench.adj, file = paste0(folder.name, '/',
                                        country, '_ar1_natlBenchmarks.rda'))
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
          
          fit.natl <- fitINLA2(data = mod.dat, family = "betabinomial",
                               Amat = NULL, geo = NULL,
                               year_label = beg.year:end.year,
                               ar1 = TRUE, type.st = type.st,
                               bias.adj = bench.adj,
                               bias.adj.by = adj.varnames,
                               age.groups = levels(mod.dat$age),
                               age.n = c(1,11,12,12,12,12),
                               age.rw.group = c(1,2,3,3,3,3),
                               verbose = TRUE,
                               survey.effect = TRUE)
          save(fit.natl, file = paste0(folder.name, '/',
                                       country, '_ar1_natlBench.rda'))
          
          res.natl <- getSmoothed(inla_mod = fit.natl, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = 1000, 
                                  weight.strata = NULL, 
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
          save(res.natl, file = paste0(folder.name, '/',
                                       country, '_res_ar1_natlBench.rda'))
          
          pdf(paste0(folder.name, '/Plots/Betabinomial/',
                     country, '_ar1_natlBench_noStrata_spaghetti.pdf'), height = 4, width = 8)
          {
            tmp.area <- res.natl$overall
            tmp.area$width <- tmp.area$upper - tmp.area$lower
            tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
            tmp.area$cex2[tmp.area$cex2 > 6] <- 6
            tmp.ihme <- ihme.ests[[1]]
            cols <- rainbow(length(surveys)+1+1+1)
            plot.years <- beg.year:end.year
            
            par(mfrow = c(1,2))
            if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
              plot.max <- max(direct.natl.yearly$mean+.025, na.rm = T)
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
                         main =  paste0(admin1.names$GADM[area]))
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
              igme.years <- jitter(igme.ests$year)
              lines(igme.years, igme.ests$OBS_VALUE/1000,
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
                
                
                if(svy.idx== 1){
                  if(dim(tmp)[1] != 0){
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main = country)
                    
                    
                    ihme.years <- jitter(tmp.ihme$year)
                    polygon(x = c(ihme.years, rev(ihme.years)),
                            y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
                            col = alpha(cols[length(surveys)+1], 0.25), border = cols[length(surveys)+1])
                    
                  }else{
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main =  paste0(admin1.names$GADM[area]))
                  }
                }
                
                
              }
              
              igme.years <- jitter(igme.ests$year)
              polygon(x = c(igme.years, rev(igme.years)),
                      y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
                      col = alpha(cols[length(surveys)+2], 0.25), border = cols[length(surveys)+2])
              res.tmp <- tmp.area
              res.tmp$years <- as.numeric(as.character(res.tmp$years))
              polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
                      y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                            rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
                      col = alpha(cols[length(surveys)+3], 0.25), border = cols[length(surveys)+3])
              polygon(x = c(res.tmp$years, rev(res.tmp$years)),
                      y = c(res.tmp$upper,
                            rev(res.tmp$lower)),
                      col = alpha('black', 0.25), border = 'black')
              legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                           'black'), .25),
                     border = c(cols[(length(surveys)+1):length(cols)],
                                'black'),
                     legend = c( 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
            }
            
            
          }
          dev.off()
        }
        
        
      }
    }
    
    ####  Admin 1 ####
    
    if(doAdmin1){
      if(useHIVAdj){
        load(paste0(folder.name, '/',
                    country, '_directHIV_admin1.rda'), envir = .GlobalEnv)
      }else{
        load(paste0(folder.name, '/',
                    country, '_direct_admin1.rda'), envir = .GlobalEnv)
      }
      
      load(paste0(folder.name, '/',
                  country, '_res_admin1_SmoothedDirect.rda'), envir = .GlobalEnv)
      res.smoothdir <- res.admin1
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
          
          if(sum(grepl("HIVnames.key", list.files(folder.name))) != 0){
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
      
      #### RW2 ####
      if(doRW){
        if(file.exists(paste0(folder.name, '/', country, '_rw2_natlBenchmarks.rda'))){
          load(paste0(folder.name, '/',
                      country, '_rw2_natlBenchmarks.rda'), envir = .GlobalEnv)
        }else{
          stop('No benchmarking file, run with doNatl = T and doBenchmark = T first.\n')
        }
        
        
        if(useHIVAdj){
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
        }
        
        message(paste0("Starting no strata rw2 admin 1 model for ", country, ".\n"))
        fit.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial", 
                               Amat = admin1.mat, geo = poly.adm1,
                               year_label = beg.year:end.year, 
                               rw = 2, type.st = type.st, 
                               bias.adj = bench.adj,
                               bias.adj.by = adj.varnames,
                               age.groups = levels(mod.dat$age),
                               age.n = c(1,11,12,12,12,12), 
                               age.rw.group = c(1,2,3,3,3,3),
                               verbose = TRUE, 
                               survey.effect = TRUE)
        save(fit.admin1, file = paste0(folder.name, '/',
                                       country, '_rw2_admin1.rda'))
        
        
        res.admin1 <- getSmoothed(inla_mod = fit.admin1, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = 1000, 
                                  Amat = admin1.mat,
                                  weight.strata = NULL, 
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
        save(res.admin1, file = paste0(folder.name, '/',
                                       country, '_res_rw2_admin1.rda'))
        
        
        pdf(paste0(folder.name, '/Plots/Betabinomial/',
                   country, '_rw2_admin1_noStrata_spaghetti.pdf'), height = 4, width = 8)
        {
          area.idx <- 0
          for(area in admin1.names$Internal){
            area.idx <- area.idx + 1
            tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
            tmp.area$width <- tmp.area$upper - tmp.area$lower
            tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
            tmp.area$cex2[tmp.area$cex2 > 6] <- 6
            tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
            cols <- rainbow(length(surveys)+1+1)
            plot.years <- beg.year:end.year
            
            par(mfrow = c(1,2))
            if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
              plot.max <- max(tmp.area$upper +.025, na.rm = T)
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
                         main =  paste0(admin1.names$GADM[area]))
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
              
              res.tmp <- res.smoothdir[res.smoothdir$region == as.character(admin1.names$Internal[area.idx]),]
              
              
              pane.years <- jitter(seq(beg.year+2, end.year-2, 5))
              est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
              lines(pane.years[est.ids],
                    res.tmp$median[est.ids], 
                    col = cols[length(surveys)+2],
                    lwd = 2, lty = 1)
              lines(pane.years[max(est.ids):length(pane.years)], 
                    res.tmp$median[max(est.ids):length(pane.years)], 
                    col = cols[length(surveys)+2], 
                    lwd = 2, lty = 2)
              lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
                    tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
                    lwd = 2, lty = 1)
              lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
                    tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
                    col = 'black', 
                    lwd = 2, lty = 2)
              
              legend('topright', bty = 'n', col = c(cols, 'black'),
                     lwd = 2, lty = c(rep(1, length(cols)+1)),
                     legend = c(surveys, 'IHME', 'Smoothed Direct', 'Betabinomial'),
                     cex = 0.6)
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
                    
                    
                    ihme.years <- tmp.ihme$year+2.5
                    polygon(x = c(ihme.years, rev(ihme.years)),
                            y = c(tmp.ihme$uci, rev(tmp.ihme$lci)),
                            col = alpha(cols[length(surveys)+1], 0.25),
                            border = cols[length(surveys)+1])
                    
                  }else{
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main =  paste0(admin1.names$GADM[area]))
                  }
                }
                
                
              }
              pane.years <- seq(beg.year+2.5,end.year-2.5,5)
              polygon(x = c(pane.years, rev(pane.years)),
                      y = c(res.tmp$upper[1:length(pane.years)],
                            rev(res.tmp$lower[1:length(pane.years)])),
                      col = alpha(cols[length(surveys)+2], 0.25),
                      border = cols[length(surveys)+2])
              polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
                      y = c(tmp.area$upper,
                            rev(tmp.area$lower)),
                      col = alpha('black', 0.25), border = 'black')
              legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                           'black'), .25),
                     border = c(cols[(length(surveys)+1):length(cols)],
                                'black'),
                     legend = c('IHME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
            }
            
            
          }
        }
        dev.off()
        rm(bench.adj)
      }
      
      #### AR(1) ####
      if(doAR){
        if(file.exists(paste0(folder.name, '/', country, '_ar1_natlBenchmarks.rda'))){
          load(paste0(folder.name, '/',
                      country, '_ar1_natlBenchmarks.rda'), envir = .GlobalEnv)
        }else{
          stop('No benchmarking file, run with doNatl = T and doBenchmark = T first.\n')
        }
        
        
        if(useHIVAdj){
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
        }
        
        message(paste0("Starting no strata AR(1) admin 1 model for ", country, ".\n"))
        
        fit.admin1 <- fitINLA2(data = mod.dat, family = "betabinomial", 
                               Amat = admin1.mat, geo = poly.adm1,
                               year_label = beg.year:end.year, 
                               ar1 = TRUE, type.st = type.st, 
                               bias.adj = bench.adj,
                               bias.adj.by = adj.varnames,
                               age.groups = levels(mod.dat$age),
                               age.n = c(1,11,12,12,12,12), 
                               age.rw.group = c(1,2,3,3,3,3),
                               verbose = TRUE, 
                               survey.effect = TRUE)
        save(fit.admin1, file = paste0(folder.name, '/',
                                       country, '_ar1_admin1.rda'))
        
        res.admin1 <- getSmoothed(inla_mod = fit.admin1, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = 1000, 
                                  Amat = admin1.mat,
                                  weight.strata = NULL, 
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
        save(res.admin1, file = paste0(folder.name, '/',
                                       country, '_res_ar1_admin1.rda'))
        
        
        pdf(paste0(folder.name, '/Plots/Betabinomial/',
                   country, '_ar1_admin1_noStrata_spaghetti.pdf'), height = 4, width = 8)
        {
          area.idx <- 0
          for(area in admin1.names$Internal){
            area.idx <- area.idx + 1
            tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
            tmp.area$width <- tmp.area$upper - tmp.area$lower
            tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
            tmp.area$cex2[tmp.area$cex2 > 6] <- 6
            tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
            cols <- rainbow(length(surveys)+1+1)
            plot.years <- beg.year:end.year
            
            par(mfrow = c(1,2))
            if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
              plot.max <- max(tmp.area$upper +.025, na.rm = T)
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
                         main =  paste0(admin1.names$GADM[area]))
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
              
              res.tmp <- res.smoothdir[res.smoothdir$region == as.character(admin1.names$Internal[area.idx]),]
              
              
              pane.years <- jitter(seq(beg.year+2, end.year-2, 5))
              est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
              lines(pane.years[est.ids],
                    res.tmp$median[est.ids], 
                    col = cols[length(surveys)+2],
                    lwd = 2, lty = 1)
              lines(pane.years[max(est.ids):length(pane.years)], 
                    res.tmp$median[max(est.ids):length(pane.years)], 
                    col = cols[length(surveys)+2], 
                    lwd = 2, lty = 2)
              lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
                    tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
                    lwd = 2, lty = 1)
              lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
                    tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
                    col = 'black', 
                    lwd = 2, lty = 2)
              
              legend('topright', bty = 'n', col = c(cols, 'black'),
                     lwd = 2, lty = c(rep(1, length(cols)+1)),
                     legend = c(surveys, 'IHME', 'Smoothed Direct', 'Betabinomial'),
                     cex = 0.6)
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
                    
                    
                    ihme.years <- tmp.ihme$year+2.5
                    polygon(x = c(ihme.years, rev(ihme.years)),
                            y = c(tmp.ihme$uci, rev(tmp.ihme$lci)),
                            col = alpha(cols[length(surveys)+1], 0.25),
                            border = cols[length(surveys)+1])
                    
                  }else{
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main =  paste0(admin1.names$GADM[area]))
                  }
                }
                
                
              }
              pane.years <- seq(beg.year+2.5,end.year-2.5,5)
              polygon(x = c(pane.years, rev(pane.years)),
                      y = c(res.tmp$upper[1:length(pane.years)],
                            rev(res.tmp$lower[1:length(pane.years)])),
                      col = alpha(cols[length(surveys)+2], 0.25),
                      border = cols[length(surveys)+2])
              polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
                      y = c(tmp.area$upper,
                            rev(tmp.area$lower)),
                      col = alpha('black', 0.25), border = 'black')
              legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                           'black'), .25),
                     border = c(cols[(length(surveys)+1):length(cols)],
                                'black'),
                     legend = c('IHME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
            }
            
            
          }
        }
        dev.off()
        
        rm(bench.adj)
      }
      rm(fit.admin1)
      
    }
    
    ####  Admin 2 ####
    
    if(doAdmin2){
      if(useHIVAdj){
        load(paste0(folder.name, '/',
                    country, '_directHIV_admin2.rda'), envir = .GlobalEnv)
      }else{
        load(paste0(folder.name, '/',
                    country, '_direct_admin2.rda'), envir = .GlobalEnv)
      }
      
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
          
          if(sum(grepl("HIVnames.key", list.files(folder.name))) != 0){
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
      
      #### RW2 ####
      if(doRW){
        if(file.exists(paste0(folder.name, '/', country, '_rw2_natlBenchmarks.rda'))){
          load(paste0(folder.name, '/',
                      country, '_rw2_natlBenchmarks.rda'), envir = .GlobalEnv)
        }else{
          stop('No benchmarking file, run with doNatl = T and doBenchmark = T first.\n')
        }
        
        
        if(useHIVAdj){
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
        }
        message(paste0("Starting no strata RW2 admin 2 model for ", country, ".\n"))
        
        
        fit.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial", 
                               Amat = admin2.mat, geo = poly.adm2,
                               year_label = beg.year:end.year, 
                               rw = 2, type.st = type.st, 
                               bias.adj = bench.adj,
                               bias.adj.by = adj.varnames,
                               age.groups = levels(mod.dat$age),
                               age.n = c(1,11,12,12,12,12), 
                               age.rw.group = c(1,2,3,3,3,3),
                               verbose = TRUE, 
                               survey.effect = TRUE)
        save(fit.admin2, file = paste0(folder.name, '/',
                                       country, '_rw2_admin2.rda'))
        
        res.admin2 <- getSmoothed(inla_mod = fit.admin2, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = 1000, 
                                  Amat = admin2.mat,
                                  weight.strata = NULL, 
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
        save(res.admin2, file = paste0(folder.name, '/',
                                       country, '_res_rw2_admin2.rda'))
        
        
        pdf(paste0(folder.name, '/Plots/Betabinomial/',
                   country, '_rw2_admin2_noStrata_spaghetti.pdf'), height = 4, width = 8)
        {
          area.idx <- 0
          for(area in admin2.names$Internal){
            area.idx <- area.idx + 1
            tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
            tmp.area$width <- tmp.area$upper - tmp.area$lower
            tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
            tmp.area$cex2[tmp.area$cex2 > 6] <- 6
            tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == as.character(admin2.names$GADM[area.idx]),]
            cols <- rainbow(length(surveys)+1)
            plot.years <- beg.year:end.year
            
            par(mfrow = c(1,2))
            if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
              plot.max <- max(tmp.area$upper +.025, na.rm = T)
            }else{
              plot.max <- 0.25
            }
            
            if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year + 1),
                   type = 'l', col = cols[1], lwd = 2,
                   main = admin2.names$GADM[area.idx])
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
                         main = admin2.names$GADM[area.idx])
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
                         main = admin2.names$GADM[area.idx])
                    
                    
                    ihme.years <- tmp.ihme$year+2.5
                    polygon(x = c(ihme.years, rev(ihme.years)),
                            y = c(tmp.ihme$uci, rev(tmp.ihme$lci)),
                            col = alpha(cols[length(surveys)+1], 0.25),
                            border = cols[length(surveys)+1])
                    
                  }else{
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main =  paste0(admin2.names$GADM[area]))
                  }
                }
                
                
              }
              
              polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
                      y = c(tmp.area$upper,
                            rev(tmp.area$lower)),
                      col = alpha('black', 0.25), border = 'black')
              legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                           'black'), .25),
                     border = c(cols[(length(surveys)+1):length(cols)],
                                'black'),
                     legend = c('IHME', 'Betabinomial'), cex = 0.75)
            }
            
            
          }
        }
        dev.off()
        
        rm(bench.adj)
      }
      
      #### AR(1) ####
      if(doAR){
        
        if(file.exists(paste0(folder.name, '/', country, '_ar1_natlBenchmarks.rda'))){
          load(paste0(folder.name, '/',
                      country, '_ar1_natlBenchmarks.rda'), envir = .GlobalEnv)
        }else{
          stop('No benchmarking file, run with doNatl = T and doBenchmark = T first.\n')
        }
        
        
        if(useHIVAdj){
          bench.adj <- merge(bench.adj, hiv.adj,
                             by = c('country', 'years'),
                             suffixes = c('.bench', '.hiv'))
          bench.adj$ratio <- bench.adj$ratio.bench*bench.adj$ratio.hiv
        }
        message(paste0("Starting no strata AR(1) admin 2 model for ", country, ".\n"))
        
        
        fit.fixed.eff.admin2 <- fitINLA2(data = mod.dat, family = "betabinomial", 
                                         Amat = admin2.mat, geo = poly.adm2,
                                         year_label = beg.year:end.year, 
                                         ar1 = TRUE, type.st = type.st, 
                                         bias.adj = bench.adj,
                                         bias.adj.by = adj.varnames,
                                         age.groups = levels(mod.dat$age),
                                         age.n = c(1,11,12,12,12,12), 
                                         age.rw.group = c(1,2,3,3,3,3),
                                         verbose = TRUE, 
                                         survey.effect = TRUE)
        save(fit.admin2, file = paste0(folder.name, '/',
                                       country, '_ar1_admin2.rda'))
        res.admin2 <- getSmoothed(inla_mod = fit.admin2, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = 1000, 
                                  Amat = admin2.mat,
                                  weight.strata = NULL, 
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
        save(res.admin2, file = paste0(folder.name, '/',
                                       country, '_res_ar1_admin2.rda'))
        
        
        pdf(paste0(folder.name, '/Plots/Betabinomial/',
                   country, '_ar1_admin2_noStrata_spaghetti.pdf'), height = 4, width = 8)
        {
          area.idx <- 0
          for(area in admin2.names$Internal){
            area.idx <- area.idx + 1
            tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
            tmp.area$width <- tmp.area$upper - tmp.area$lower
            tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
            tmp.area$cex2[tmp.area$cex2 > 6] <- 6
            tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == as.character(admin2.names$GADM[area.idx]),]
            cols <- rainbow(length(surveys)+1)
            plot.years <- beg.year:end.year
            
            par(mfrow = c(1,2))
            if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
              plot.max <- max(tmp.area$upper +.025, na.rm = T)
            }else{
              plot.max <- 0.25
            }
            
            if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year + 1),
                   type = 'l', col = cols[1], lwd = 2,
                   main = admin2.names$GADM[area.idx])
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
                         main = admin2.names$GADM[area.idx])
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
                         main = admin2.names$GADM[area.idx])
                    
                    
                    ihme.years <- tmp.ihme$year+2.5
                    polygon(x = c(ihme.years, rev(ihme.years)),
                            y = c(tmp.ihme$uci, rev(tmp.ihme$lci)),
                            col = alpha(cols[length(surveys)+1], 0.25),
                            border = cols[length(surveys)+1])
                    
                  }else{
                    plot(NA,
                         xlab = "Year", ylab = "U5MR",
                         ylim = c(0, plot.max),
                         xlim = c(beg.year, end.year),
                         type = 'l', col = cols[svy.idx], lwd = 2,
                         main =  paste0(admin2.names$GADM[area]))
                  }
                }
                
                
              }
              
              polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
                      y = c(tmp.area$upper,
                            rev(tmp.area$lower)),
                      col = alpha('black', 0.25), border = 'black')
              legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                           'black'), .25),
                     border = c(cols[(length(surveys)+1):length(cols)],
                                'black'),
                     legend = c('IHME', 'Betabinomial'), cex = 0.75)
            }
            
            
          }
        }
        dev.off()
        
        rm(bench.adj)
      }
      rm(fit.admin2)
    }
  }
  
  
  
}


#### Libraries ####
devtools::install_github("bryandmartin/SUMMER",
                         build_vignettes = F, force = T)
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
country <- "Senegal"
cluster <- FALSE
message("If have the same subfolder structure as 
        AfricaAdmin2Estimates/Data/countryDataFolders/. Do nothing!\n
        Otherwise, edit the following paths as needed.\n")

data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '../../Analysis/R'
ihme.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '../HIV'

setwd(data.dir)

if(!exists("sheet_key", envir = .GlobalEnv)){
  source(paste0(code.dir.rel,'/LoadCommandCenter.R'))
}
CountryList <- sheets_read(sheet_key, sheet = "CountryList")

#for(country in c("Ethiopia", "Malawi", "Zambia", "Ghana")){
folder.name <- CountryList$folderName[CountryList$Country == country]

hand.dir.rel <- paste0('../../Analysis/countryAnalysisFolders/',
                       gsub(" ", "", folder.name))


# re-running national and Admin1 AR1
ptm <- proc.time()
fitBetabin(country, type.st = 4, cluster = cluster,
           beg.year = 1990, end.year = 2020,
           doAR = TRUE, doRW = FALSE,
           doBenchmark = TRUE, doNatl = FALSE, no.strata = TRUE,
           doAdmin1= TRUE, doAdmin2 = FALSE,
           code.dir.rel = code.dir.rel,
           igme.dir.rel = igme.dir.rel,
           ihme.dir.rel = ihme.dir.rel,
           hand.dir.rel = hand.dir.rel,
           shapes.sub.dir = shapes.sub.dir,
           hiv.dir.rel = hiv.dir.rel)
proc.time()-ptm
# }
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



