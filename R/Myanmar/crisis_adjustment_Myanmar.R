rm(list =  ls())
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
country <- "Myanmar"
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
CountryList <- range_read(sheet_key, sheet = "CountryList")
#CountryList <- read.csv("CountryList.csv", header = T)

folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]

message('Where is IHMEHand_CountryName.rda?\n')
hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
                       gsub(" ", "", folder.name))
#hand.dir.rel <- paste0(gsub(" ", "", folder.name))
SurveyInfo <- range_read(sheet_key, sheet = "SurveyInfo")
#SurveyInfo <- read.csv("SurveyInfo.csv", header = T)
#surveys <- SurveyInfo$Survey.Year[SurveyInfo$Country == country &
#                                    SurveyInfo$`GPS.` == "Y"]

#SurveyInfo <- range_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]
survey.legends <- SurveyInfo$`OfficialSurveyName`[SurveyInfo$Country == country &
                                                    SurveyInfo$`GPS?` == "Y"]


#### More Params ####

beg.year <- 1990
end.year <- 2019
time.mod <- "rw2main_randomSlopes_rw1xICAR"

#### Load IGME data ####

file.list <- list.files(igme.dir.rel)
igme.file <- file.list[grepl("Results.csv", file.list)]
igme.ests <- read.csv(paste0(igme.dir.rel,'/',igme.file),
                      header = T)


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



################################################################################
#### ADMIN 2 EXCESS DEATH ADJUSTMENT                                        ####
################################################################################

# directory with data on excess deaths
ed_data_dir <- paste0(folder.name, "/excessDeathAdjustment/")
# affected admin 2 indices
affected_admin_2_ind <- c(1:5, 60:63)
affected_admin_2_names <- 
  read.csv(paste0(ed_data_dir, "Myanmar_admin2_U1_5.csv"))$names[affected_admin_2_ind] %>%
  as.character()

tbl_1_5 <- read.csv(paste0(ed_data_dir, "Myanmar_admin2_U1_5.csv")) %>%
  dplyr::select(-X) %>%
  mutate(region = paste0("admin2_", 1:length(unique(names)))) %>%
  pivot_longer(names_to = "year", values_to = "pop_1_5", -c(names, region)) %>%
  mutate(year = as.numeric(str_sub(year, 2, -1)))

tbl_0_1 <- read.csv(paste0(ed_data_dir, "Myanmar_admin2_U1.csv")) %>%
  dplyr::select(-X) %>%
  pivot_longer(names_to = "year", values_to = "pop_0_1", -c(names)) %>%
  mutate(year = as.numeric(str_sub(year, 2, -1)))

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

# Identify affected years and admin 2 areas
edmr_tbl <- edmr_tbl %>%
  mutate(crisis = (year == 2008) & (names %in% affected_admin_2_names))

# get national U5 excess deaths for 2008 cyclone
igme_crisis_df <- read.csv(paste0(ed_data_dir,
                                  "Crisis_Under5_deaths2020.csv"))
natl_u5_ed <-  igme_crisis_df$Crisis.d0.5[igme_crisis_df$Year == 2008.5 &
                                            igme_crisis_df$ISO3Code == "MMR"]

# KEY STEP:
# compute excess death mortality rates from 2008 cyclone
edmr_tbl <- edmr_tbl %>%
  mutate(ed_0_1 = (0.2) * natl_u5_ed * pop_0_1 * crisis / sum(pop_0_1 * crisis),
         ed_1_5 = (0.8) * natl_u5_ed * pop_1_5 * crisis / sum(pop_1_5 * crisis)) %>%
  mutate(ed_1m0 = ed_0_1 / pop_0_1,
         ed_4m1 = ed_1_5 / pop_1_5) %>%
  mutate(ed_1q0 = ed_1m0 / (1 + (1 - (0.3)) * ed_1m0),
         ed_4q1 = 4 * ed_4m1 / (1 + (4 - 4 * (0.4)) * ed_4m1)) %>%
  mutate(ed_5q0 = 1 - (1 - ed_1q0) * (1 - ed_4q1))

#### LOAD UNADJUSTED RESULTS ####
# load res.admin2 object from .rda (already run)
load(paste0(folder.name,'/', country,
            '_res_', time.mod, '_admin2.rda'))

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

# change draws.est slot to store adjusted results
for (i in 1:length(res.admin2$draws.est)) {
  yr <- res.admin2$draws.est[[i]]$years
  reg <- res.admin2$draws.est[[i]]$region
  res.admin2$draws.est[[i]]$draws <- (draws_tbl %>% filter(years == yr & region == reg))$adj_u5mr
}

# aggregate admin 2 U5MR to get national U5MR for all years
agg_natl_draws_tbl <- draws_tbl %>%
  group_by(years, draw) %>%
  dplyr::summarize(u5mr = sum(pop_0_5 * adj_u5mr) / sum(pop_0_5))

# compute median of national U5MR estimates
med_agg_natl_draws <- agg_natl_draws_tbl %>%
  group_by(years) %>%
  dplyr::summarize(u5mr = median(u5mr)) %>%
  mutate(bench = u5mr * 1000 / igme.ests$OBS_VALUE[match(beg.year:end.year,
                                                         igme.ests$year)])
# calculate post hoc benchmarks by comparing with IGME

# Smooth out benchmarks and forecast for 2016-2020 using RW2 model
smoothed_bench <- med_agg_natl_draws  
smoothed_bench$bench[smoothed_bench$years == 2008] <- NA
smoothed_bench$bench[(nrow(smoothed_bench) - 4):nrow(smoothed_bench)] <- NA
smoothed_res <- inla(bench ~ f(as.numeric(years), model = "rw2"), data = smoothed_bench,
                     control.predictor=list(compute=TRUE))
med_agg_natl_draws$bench = smoothed_res$summary.linear.predictor$mean
summary_adj_tbl <- draws_tbl %>%
  left_join(med_agg_natl_draws %>% dplyr::select(-u5mr), by = "years") %>%
  group_by(years, region) %>%
  dplyr::summarize(upper = quantile(adj_u5mr / bench, .95,
                                    na.rm = TRUE),
                   lower = quantile(adj_u5mr / bench, .05,
                                    na.rm = TRUE),
                   median = median(adj_u5mr / bench,
                                   na.rm = TRUE),
                   na.rm = TRUE)

res.admin2$overall <- res.admin2$overall %>%
  dplyr::select(-c(median, lower, upper)) %>%
  left_join(summary_adj_tbl, by = c("years", "region"))

#### SAVE ADJUSTED RESULTS ####
save(res.admin2, file = paste0(folder.name,'/',
                               country, '_res_',
                               time.mod, '_admin2EDBench_CI90.rda'))
write.csv(res.admin2$overall, row.names = FALSE,
          file = paste0(folder.name,'/',
                        country, '_res_',
                        time.mod, '_admin2EDBench_CI90.csv'))

summary_adj_tbl <- draws_tbl %>%
  left_join(med_agg_natl_draws %>% dplyr::select(-u5mr), by = "years") %>%
  group_by(years, region) %>%
  dplyr::summarize(upper = quantile(adj_u5mr / bench, .975,
                                    na.rm = TRUE),
                   lower = quantile(adj_u5mr / bench, .025,
                                    na.rm = TRUE),
                   median = median(adj_u5mr / bench,
                                   na.rm = TRUE),
                   na.rm = TRUE)

res.admin2$overall <- res.admin2$overall %>%
  dplyr::select(-c(median, lower, upper)) %>%
  left_join(summary_adj_tbl, by = c("years", "region"))
save(res.admin2, file = paste0(folder.name,'/',
                               country, '_res_',
                               time.mod, '_admin2EDBench.rda'))
bench.adj <- data.frame(year = beg.year:end.year,
                        igme = igme.ests$OBS_VALUE/1000)
bench.adj$est <-  med_agg_natl_draws$u5mr
bench.adj$ratio <-  med_agg_natl_draws$smoothed_bench
save(bench.adj, file = paste0(folder.name,'/', country, '_', 
                              time.mod, '_admin2EDBenchmarks.rda'))


################################################################################
#### ADMIN 1 EXCESS DEATH ADJUSTMENT                                        ####
################################################################################
ed_data_dir <- paste0(folder.name, "/excessDeathAdjustment/")


tbl_1_5 <- read.csv(paste0(ed_data_dir, "Myanmar_admin2_U1_5.csv")) %>%
  dplyr::select(-X) %>%
  mutate(region = paste0("admin2_", 1:length(unique(names)))) %>%
  pivot_longer(names_to = "year", values_to = "pop_1_5", -c(names, region)) %>%
  mutate(year = as.numeric(str_sub(year, 2, -1)))

tbl_0_1 <- read.csv(paste0(ed_data_dir, "Myanmar_admin2_U1.csv")) %>%
  dplyr::select(-X) %>%
  pivot_longer(names_to = "year", values_to = "pop_0_1", -c(names)) %>%
  mutate(year = as.numeric(str_sub(year, 2, -1)))

# excess death mortality rate -- to be computed!
edmr_tbl <- tbl_1_5 %>%
  left_join(tbl_0_1, by = c("names", "year")) %>%
  mutate(pop_0_5 = pop_1_5 + pop_0_1)


# match up admin 1 and admin 2 names
load(paste0(folder.name, "/Myanmar_cluster_dat.rda"))
load(paste0(folder.name, "/shapeFiles_GADM/Myanmar_Amat_names.rda"))
lookup_adm1_adm2 <- mod.dat %>%
  group_by(admin2.name, admin1.name) %>%
  summarise() %>%
  ungroup()

# affected admin 2 indices
affected_admin_1_ind <- c(1, 15)
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
  mutate(crisis = (year == 2008) & (admin1.name %in% affected_admin_1_names))

# get national U5 excess deaths for 2008 cyclone
igme_crisis_df <- read.csv(paste0(ed_data_dir,
                                  "Crisis_Under5_deaths2020.csv"))
natl_u5_ed <-  igme_crisis_df$Crisis.d0.5[igme_crisis_df$Year == 2008.5 &
                                            igme_crisis_df$ISO3Code == "MMR"]

# compute excess death mortality rates from 2008 cyclone
edmr_tbl <- edmr_tbl %>%
  mutate(ed_0_1 = (0.2) * natl_u5_ed * pop_0_1 * crisis / sum(pop_0_1 * crisis),
         ed_1_5 = (0.8) * natl_u5_ed * pop_1_5 * crisis / sum(pop_1_5 * crisis)) %>%
  mutate(ed_1m0 = ed_0_1 / pop_0_1,
         ed_4m1 = ed_1_5 / pop_1_5) %>%
  mutate(ed_1q0 = ed_1m0 / (1 + (1 - (0.3)) * ed_1m0),
         ed_4q1 = 4 * ed_4m1 / (1 + (4 - 4 * (0.4)) * ed_4m1)) %>%
  mutate(ed_5q0 = 1 - (1 - ed_1q0) * (1 - ed_4q1)) 
#### LOAD UNADJUSTED RESULTS ####
# load res.admin1 object from .rda (already run)
load(paste0(folder.name,'/', country,
            '_res_', time.mod, '_admin1.rda'))

# pull out all the unadjusted draws and reshape
draws_mat <- do.call(rbind, lapply(res.admin1$draws.est, function(x) x$draws))
colnames(draws_mat) <- paste0("draw_", 1:1000)
draws_tbl <- as_tibble(draws_mat) %>%
  mutate(years = as.factor(sapply(res.admin1$draws.est, 
                                  function(x) x$years)), 
         region = sapply(res.admin1$draws.est, function(x) x$region))

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

# change draws.est slot to store adjusted results
for (i in 1:length(res.admin1$draws.est)) {
  yr <- res.admin1$draws.est[[i]]$years
  reg <- res.admin1$draws.est[[i]]$region
  res.admin1$draws.est[[i]]$draws <- (draws_tbl %>% filter(years == yr & region == reg))$adj_u5mr
}

# aggregate admin 1 U5MR to get national U5MR for all years
agg_natl_draws_tbl <- draws_tbl %>%
  group_by(years, draw) %>%
  dplyr::summarize(u5mr = sum(pop_0_5 * adj_u5mr) / sum(pop_0_5))

# compute median of national U5MR estimates
med_agg_natl_draws <- agg_natl_draws_tbl %>%
  group_by(years) %>%
  dplyr::summarize(u5mr = median(u5mr)) %>%
  mutate(bench = u5mr * 1000 / igme.ests$OBS_VALUE[match(beg.year:end.year,
                                                         igme.ests$year)])
# calculate post hoc benchmarks by comparing with IGME

# Smooth out benchmarks and forecast for 2016-2020 using RW2 model
smoothed_bench <- med_agg_natl_draws  
smoothed_bench$bench[smoothed_bench$years == 2008] <- NA
smoothed_bench$bench[(nrow(smoothed_bench) - 4):nrow(smoothed_bench)] <- NA
smoothed_res <- inla(bench ~ f(as.numeric(years), model = "rw2"), data = smoothed_bench,
                     control.predictor=list(compute=TRUE))
med_agg_natl_draws$bench = smoothed_res$summary.linear.predictor$mean
summary_adj_tbl <- draws_tbl %>%
  left_join(med_agg_natl_draws %>% dplyr::select(-u5mr), by = "years") %>%
  group_by(years, region) %>%
  dplyr::summarize(upper = quantile(adj_u5mr / bench, .95,
                                    na.rm = TRUE),
                   lower = quantile(adj_u5mr / bench, .05,
                                    na.rm = TRUE),
                   median = median(adj_u5mr / bench,
                                   na.rm = TRUE))

res.admin1$overall <- res.admin1$overall %>%
  dplyr::select(-c(median, lower, upper)) %>%
  left_join(summary_adj_tbl, by = c("years", "region"))

#### SAVE ADJUSTED RESULTS ####
save(res.admin1, file = paste0(folder.name,'/', country,
                               '_res_', time.mod,
                               '_admin1EDBench_CI90.rda'))
write.csv(res.admin1$overall, row.names = FALSE,
          file = paste0(folder.name,'/', country,
                        '_res_', time.mod,
                        '_admin1EDBench_CI90.csv'))

summary_adj_tbl <- draws_tbl %>%
  left_join(med_agg_natl_draws %>% dplyr::select(-u5mr), by = "years") %>%
  group_by(years, region) %>%
  dplyr::summarize(upper = quantile(adj_u5mr / bench, .975,
                                    na.rm = TRUE),
                   lower = quantile(adj_u5mr / bench, .025,
                                    na.rm = TRUE),
                   median = median(adj_u5mr / bench,
                                   na.rm = TRUE))

res.admin1$overall <- res.admin1$overall %>%
  dplyr::select(-c(median, lower, upper)) %>%
  left_join(summary_adj_tbl, by = c("years", "region"))
save(res.admin1, file = paste0(folder.name,'/',
                               country, '_res_',
                               time.mod, '_admin1EDBench.rda'))
bench.adj$est <-  med_agg_natl_draws$u5mr
bench.adj$ratio <-  med_agg_natl_draws$bench
save(bench.adj, file = paste0(folder.name,'/', country, '_', 
                              time.mod, '_admin1EDBenchmarks.rda'))



################################################################################
#### NATIONAL EXCESS DEATH ADJUSTMENT                                       ####
################################################################################

load(paste0(folder.name,'/', country, '_res_rw2_natl.rda'))
ed_data_dir <- paste0(folder.name, "/excessDeathAdjustment/")
# get IGME estimate of excess death 5q0 for 2008 cyclone
igme_adj_df <- readxl::read_xlsx(paste0(ed_data_dir, 
                                        "Crisis adjustments_20200403.xlsx"),
                                 skip = 3, sheet = 2)
igme_crisis_5q0 <- igme_adj_df$`Crisis 5q0`[30]
adj_row <- (res.natl$overall$years == 2008)

res.natl$overall[adj_row, c("mean", "median", "upper", "lower")] <- 
  res.natl$overall[adj_row, c("mean", "median", "upper", "lower")] + igme_crisis_5q0
save(res.natl, file = paste0(folder.name,'/', country, '_res_rw2_natlED.rda'))


bench.adj$est <- res.natl$overall$median
smoothed_bench <- bench.adj
smoothed_bench$ratio <- smoothed_bench$est/smoothed_bench$igme
smoothed_bench$ratio[smoothed_bench$years == 2008] <- NA
smoothed_bench$ratio[(nrow(smoothed_bench) - 4):nrow(smoothed_bench)] <- NA
smoothed_res <- inla(ratio ~ f(as.numeric(year), model = "rw2"), data = smoothed_bench,
                     control.predictor=list(compute=TRUE))
bench.adj$ratio <- smoothed_res$summary.linear.predictor$mean


save(bench.adj, file = paste0(folder.name, '/',   country, '_rw2_natlEDBenchmarks.rda'))
res.natl$overall[, c("mean", "median", "upper", "lower")] <-
  res.natl$overall[, c("mean", "median", "upper", "lower")] / bench.adj$ratio
save(res.natl, file = paste0(folder.name,'/', country, '_res_rw2_natlEDBench.rda'))


