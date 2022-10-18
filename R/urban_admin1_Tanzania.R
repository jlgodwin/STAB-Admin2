##  author: Yunhan Wu
##
##  required objects and directories:
##  directories: 
##    - AfricaAdmin2Estimates/Data/countryDataFolders/CountryFolderName
##     objects needed:
##      - AfricaAdmin2Estimates/Data/countryDataFolders/CountryFolderName/CountryName_cluster_dat.rda
##
##     subdirectories needed:
##      - AfricaAdmin2Estimates/Data/countryDataFolders/CountryFolderName/dhsStata
##      - AfricaAdmin2Estimates/Data/countryDataFolders/CountryFolderName/Plots
## 
##  sourced scripts:
##   - AfricaAdmin2Estimates/Analysis/R/LoadCommandCenter.R



#### Load packages and data ####
library(haven)
library(survey)
library(ggplot2)
library(knitr)
library(kableExtra)
library(SUMMER)
library(dplyr)
library(INLA)
library(readr)
library(grid)
library(gridExtra)
library(readstata13)


#### Load functions ####


add_ghost<-function(o_data,year,i){
  ghost_sample<-o_data[1,]
  ghost_sample$v021<-0
  ghost_sample$v025<-levels(ghost_sample$v025)[i]
  ghost_sample$v022<-0
  ghost_sample$v005<-min(mean(o_data$v005),sum(o_data$v005)/1000)
  ghost_sample$v005<-max(ghost_sample$v005,sum(o_data$v005)/2000)
  
  ghost_sample$b3<-(year-1900-2)*12
  ghost_sample$b7<-NA
  o_data<-rbind(o_data,ghost_sample)
  
  return (o_data)
}

# Direct estimates for 1 year, 1 survey, 
# 1 region(national/admin1/admin2 etc.)
nat_1year <- function(svy_data,year,
                      ageseg=list(c(0, 60)),
                      cmc_adj=0){
  if (is.null(dim(svy_data)) | dim(svy_data)[1]==0) {
    return(NULL)
  }
  if(unique(tolower(as.character(svy_data$v025)))[1] == "rural" &
     length(unique(tolower(as.character(svy_data$v025)))) == 1){
    svy_data<-add_ghost(svy_data,year = year,1)
  }
  if(unique(tolower(as.character(svy_data$v025)))[1] == "urban" &
     length(unique(tolower(as.character(svy_data$v025)))) == 1){
    svy_data<-add_ghost(svy_data,year = year,2)
  }
  age_data<-data.frame(svy_data$b3,svy_data$b7,svy_data$v008,svy_data$v025,
                       svy_data$v005,svy_data$v021,svy_data$v022)
  
  colnames(age_data)<-c("b3","b7","v008","v025","v005",'v021','v022')
  age_data<-age_data[!is.na(age_data$v022),]
  
  age_data$b3<-age_data$b3+cmc_adj
  age_data$v008<-age_data$v008+cmc_adj
  
  start_ref<-((year-1900) * 12) +1
  end_ref<-((year-1900) * 12) +13
  
  age_data$b3<-age_data$b3+0.5
  age_data$terminate_date<-pmin(age_data$v008,
                                age_data$b3+age_data$b7,na.rm=TRUE)
  
  ## age of the child entering the year and leaving the year 
  age_data$s_age<-start_ref-age_data$b3
  age_data$e_age<-end_ref-age_data$b3
  
  urban_result<-data.frame()
  age_band<-vector()
  
  for (i in 1:length(ageseg)){
    
    age_band[i]<-paste(as.character(ageseg[[i]][1]),'-',
                       as.character(ageseg[[i]][2]),sep='')
    age_wid<-ageseg[[i]][2]-ageseg[[i]][1]
    age_data$exposure <- NA
    
    # first calculate assuming no death, adjust for death later
    
    ## when enter the year, the child is younger than the high cut
    ## before leaving the year, the child is older than the low cut
    temp_in<-age_data[age_data$s_age<ageseg[[i]][2]&
                        age_data$e_age>ageseg[[i]][1] ,]
    if (is.null(dim(temp_in)) | dim(temp_in)[1]==0) {
      return(NULL)
    }
    ## exposure for those included 
    temp_in$exposure<-pmin(ageseg[[i]][2]-temp_in$s_age,
                           temp_in$e_age-ageseg[[i]][1],age_wid,
                           12)/min(12,age_wid)
    
    age_data[age_data$s_age<ageseg[[i]][2]&
               age_data$e_age>ageseg[[i]][1] ,]$exposure<-temp_in$exposure
    
    # adjust for death
    
    ## child died/interviewed before the start of the year
    ## or before entering the age band
    age_data$terminate_age<-age_data$terminate_date-age_data$b3
    
    if(dim(age_data[age_data$terminate_date<=start_ref|
                    age_data$terminate_age<=ageseg[[i]][1],])[1]>0){
      age_data[age_data$terminate_date<=start_ref|
                 age_data$terminate_age<=ageseg[[i]][1],]$exposure<-NA
    }
    ## child died/interviewed in the year 
    death_ad<-age_data[!is.na(age_data$exposure)&
                         age_data$terminate_age<ageseg[[i]][2]&
                         age_data$terminate_age<age_data$e_age,]
    ### older children death age recorded as integer year, no adjustment 
    death_ad$exposure<-pmin(death_ad$terminate_age-death_ad$s_age,
                            death_ad$terminate_age-ageseg[[i]][1])/
      min(12,age_wid)
    
    age_data[!is.na(age_data$exposure)&
               age_data$terminate_age<ageseg[[i]][2]&
               age_data$terminate_age<age_data$e_age,]$exposure<-
      death_ad$exposure
    
    age_data$adj_wt<-age_data$exposure*age_data$v005/1000000
    
    age_data$urban_indicator<- if_else(tolower(as.character(age_data$v025)) == "urban",
                                       1,0)
    
    options(survey.lonely.psu = "adjust")
    dstrat <- survey::svydesign(id = ~ v021, strata = ~ v022, 
                                weights = ~ adj_wt, nest=TRUE,
                                data = age_data[!is.na(age_data$adj_wt),])
    
    svy_result <- svymean(~ urban_indicator,design = dstrat)
    urban_result<-rbind(urban_result,as.data.frame(svy_result))
    
  }
  urban_result$age_band<-age_band
  
  row.names(urban_result)<-NULL
  colnames(urban_result)<-c("urban_prop",'SE','age_band')
  urban_result$year<-year
  return(urban_result[,c(4,3,1,2)])
}


# direct estimates for multiple years, 1 survey,
# 1 region(national/admin1/admin2 etc.)
urb_nat <- function(svy_data,year,
                    ageseg=list(c(0, 60)),
                    cmc_adj=0){
  urban_result<-data.frame()
  
  for (i in 1:length(year)){
    urban_result<-rbind(urban_result,
                        nat_1year(svy_data,year[i],ageseg,cmc_adj))
    
  }
  return(urban_result)
}

# direct estimates for multiple years, multiple surveys,
# 1 region(national/admin1/admin2 etc.)
nat_multi_svy <- function(svy_data_list,
                          year_list=NULL,
                          ageseg=list(c(0, 60)),
                          cmc_adj=0, survey_year=NULL){
  urban_result<-data.frame()
  
  if(length(svy_data_list)==0){return (NULL)}
  
  if(is.null(survey_year)){
    survey_year<-vector()
    for (i in 1:length(svy_data_list)){
      survey_year[i]<-floor(max(svy_data_list[[i]]$v008+cmc_adj)/12)+1900-1
    }
  }
  
  if(is.null(year_list)){
    year_list<-list()
    for (i in 1:length(svy_data_list)){
      if(dim(svy_data_list[[i]])[1] != 0){
      real.year <- floor(max(svy_data_list[[i]]$v008+cmc_adj)/12)+1900-1
      year_list[[i]]<-c((real.year-16):real.year)
      }else{
        year_list[[i]] <- NULL
      }
    }
  }
  
  
  for (i in 1:length(svy_data_list)){
    if (is.null(dim(svy_data_list[[i]])) | dim(svy_data_list[[i]])[1]==0) {
      urban_result_1survey<-NULL
    }else {
      urban_result_1survey<-urb_nat(svy_data_list[[i]],
                                    year=year_list[[i]],
                                    ageseg=ageseg, 
                                    cmc_adj=cmc_adj)
      urban_result_1survey$survey_year<-survey_year[i]
    }
    urban_result<-rbind(urban_result,urban_result_1survey)
    
  }
  
  return(urban_result)
  
}


# transform estimates to logit scale, using delta method 
svy_process<-function(c_svy){
  
  c_svy$logit_prop<-
    logit(c_svy$urban_prop)
  
  c_svy$logit_se<-(1/c_svy$urban_prop+1/(1-c_svy$urban_prop))*c_svy$SE
  
  c_svy$lower<-expit(c_svy$logit_prop-1.96*c_svy$logit_se)
  
  c_svy$upper<-expit(c_svy$logit_prop+1.96*c_svy$logit_se)
  
  if(max(c_svy$urban_prop)==1){
    c_svy[c_svy$urban_prop==1,]$logit_prop<-NA
    c_svy[c_svy$urban_prop==1,]$logit_se<-NA
    c_svy[c_svy$urban_prop==1,]$lower<-1
    c_svy[c_svy$urban_prop==1,]$upper<-1
  }
  
  if(min(c_svy$urban_prop)==0){
    
    c_svy[c_svy$urban_prop==0,]$logit_prop<-NA
    c_svy[c_svy$urban_prop==0,]$logit_se<-NA
    c_svy[c_svy$urban_prop==0,]$lower<-0
    c_svy[c_svy$urban_prop==0,]$upper<-0
  }
  
  c_svy$survey_year<-as.factor(c_svy$survey_year)
  return(c_svy)
  
}

# meta-analysis, 1 year, 1 region
comb_1year_1age_band<-function(c_svy,year,age_band=NULL){
  
  if (is.null(age_band)){age_band=c_svy$age_band[1]}
  
  svy_year<-c_svy[c_svy$year==year&
                    c_svy$age_band==age_band ,]
  num_svy<-dim(svy_year)[1]
  if (min(svy_year$urban_prop)==0){
    logit_prop<-NA
    urban_prop<-0
    logit_se<-NA
    SE<-0
    lower<-0
    upper<-0
    
    urban_result<-data.frame(year,age_band,urban_prop,SE,logit_prop,
                             logit_se,lower,upper)
    urban_result$survey_year<-'combined'
    
    return(urban_result)
  }
  
  if (max(svy_year$urban_prop)==1){
    logit_prop<-NA
    urban_prop<-1
    logit_se<-NA
    SE<-0
    lower<-1
    upper<-1
    
    urban_result<-data.frame(year,age_band,urban_prop,SE,logit_prop,
                             logit_se,lower,upper)
    urban_result$survey_year<-'combined'
    
    return(urban_result)
  }
  
  
  if(num_svy>0){
    wt<-vector()
    sum_var<-sum(1/svy_year$logit_se^2)
    for (k in 1:num_svy){
      wt[k]<-1/svy_year$logit_se[k]^2/sum_var
    }
    
    logit_prop<-sum(svy_year$logit_prop*wt)
    urban_prop<-expit(logit_prop)
    logit_se<-sqrt(sum(svy_year$logit_se^2*wt))
    SE<-expit(logit_prop)*expit(-logit_prop)*logit_se
    lower<-expit(logit_prop-1.96*logit_se)
    upper<-expit(logit_prop+1.96*logit_se)
    
    urban_result<-data.frame(year,age_band,urban_prop,SE,logit_prop,
                             logit_se,lower,upper)
    urban_result$survey_year<-'combined'
    
    return(urban_result)
  }
  
}

# meta-analysis, multiple years, 1 region
comb_nat<-function(c_svy,year_list=NULL,age_band_list=NULL){
  if (is.null(year_list)){
    year_list<-
      c(min(c_svy$year):max(c_svy$year))
  }
  
  if (is.null(age_band_list)){
    age_band_list<-levels(as.factor(c_svy$age_band))
  }
  
  urban_result<-data.frame()
  for (i in 1:length(year_list)){
    for (k in 1:length(age_band_list)){
      urban_result<-rbind(urban_result,
                          comb_1year_1age_band(c_svy,year_list[i],age_band_list[k]) )
      
    }
  }
  return (urban_result)
}

# get combined national estimates 
comb_svy<-function(c_svy,year_list=NULL,age_band_list=list(c(0, 60)),
                   cmc_adj=0,survey_year=NULL){
  if(length(c_svy)==0){return (NULL)}
  urban_result<-nat_multi_svy(c_svy,year_list=year_list,
                              ageseg=age_band_list,
                              cmc_adj=cmc_adj,survey_year=survey_year)
  urban_result<-svy_process(urban_result)
  age_band<-vector()
  for (i in 1:length(age_band_list)){
    
    age_band[i]<-paste(as.character(age_band_list[[i]][1]),'-',
                       as.character(age_band_list[[i]][2]),sep='')
  }
  return(comb_nat(urban_result,year_list=year_list,
                  age_band_list=age_band))
}

# direct estimates for multiple years, multiple surveys,
# multiple regions (admin1/admin2 etc.)
admin_multi_svy<-function(svy_all,
                          year_list=NULL,
                          ageseg=list(c(0, 60)),
                          survey_year = NULL,
                          cmc_adj=0){
  
  admin_vec<-levels(as.factor(svy_all$admin))
  #survey_year<-as.numeric(levels(as.factor(svy_all$survey_year)))
  urban_result<-data.frame()
  
  for (j in 1:length(admin_vec)){
    
    admin_name<- admin_vec[j]
    svy_data_list<-list()
    for (i in 1:length(survey_year)){
      svy_data_list[[i]]<-svy_all[svy_all$survey_year==survey_year[i]&
                                    svy_all$admin==admin_name,]
    }
    
    temp_admin<-nat_multi_svy(svy_data_list, year_list=year_list,
                              ageseg=ageseg,
                              cmc_adj=cmc_adj, survey_year=survey_year)
    
    temp_admin$admin<-admin_name
    urban_result<-rbind(urban_result,temp_admin)
  }
  
  return(urban_result)
  
}


# process function, add admin information 
pre_prop<-function(svy_data_list,loop_up,svy_year_vec){
  BR_frame<-data.frame()
  
  for (i in 1:length(svy_data_list)){
    temp_fl<-svy_data_list[[i]]
    #levels(temp_fl$v025) <- tolower(levels(temp_fl$v025))
    temp_fl$v022 <- as.numeric(temp_fl$v022)
    br_temp<-data.frame(temp_fl$b3,temp_fl$b7,temp_fl$v008,
                        temp_fl$v025,temp_fl$v005,temp_fl$v021,
                        temp_fl$v022,temp_fl$v001)
    colnames(br_temp)<-c("b3","b7","v008","v025","v005",'v021','v022',
                         "v001")
    br_temp$survey_year<-svy_year_vec[i]
    br_temp<-merge(br_temp, loop_up, by=c("v001",'survey_year'))
    BR_frame<-rbind(BR_frame,br_temp)
  }
  
  return(BR_frame)
  
}

combine_all<-function(admin_ghost){
  
  admin_name<-levels(as.factor(admin_ghost$admin))
  
  comb_all<-data.frame()
  for (i in 1:length(admin_name)){
    temp_comb<-comb_nat(admin_ghost[admin_ghost$admin==admin_name[i],])
    temp_comb$admin<-admin_name[i]
    comb_all<-rbind(comb_all,temp_comb)
  }
  #comb_all$survey_year<-'combined'
  return(comb_all)
}
# rw2 smooth, 1 region
admin_rw2_smooth<-function(admin_name,admin_ghost,
  pcprec=list(theta=list(prior='pc.prec', param=c(1, 0.05))) ){
  
  
  admin_comb_ghost<-comb_nat(admin_ghost[admin_ghost$admin==
                                              admin_name,])
  admin_comb_ghost$survey_year<-'ghost'
  
  rw2_ghost<-admin_comb_ghost
  rw2_ghost$logit.prec<-(1/rw2_ghost$logit_se)^2
  rw2_ghost$time <- c(seq(1,length(rw2_ghost$logit.prec)))
  
  # pcprec <- list(theta=list(prior='pc.prec', param=c(1, 0.05))) 
  # 5% chance sigma>1
  
  rw2fit_no_se <- inla(logit_prop~f(time, model="rw2",scale.model=T,hyper=pcprec),
                       data=rw2_ghost, family="gaussian",control.predictor=list(compute=TRUE),
                       control.family = list( hyper = list(prec = list( initial = log(1), fixed=TRUE))), 
                       scale=logit.prec)
  
  admin_rw2<-admin_comb_ghost
  admin_rw2$urban_prop<-expit(rw2fit_no_se$summary.fitted.values$`0.5quant`)
  admin_rw2$lower<-expit(rw2fit_no_se$summary.fitted.values$`0.025quant`)
  admin_rw2$upper<-expit(rw2fit_no_se$summary.fitted.values$`0.975quant`)
  
  admin_rw2$survey_year<-'RW2 fitted'
  
  
  return(admin_rw2[,c(1:3,7:9)])
}


# rw2 smooth, all regions
rw2_smooth_all<-function(admin_ghost,
  pcprec=list(theta=list(prior='pc.prec', param=c(1, 0.05)))){
  
admin_name<-levels(as.factor(admin_ghost$admin))
ghost_rw2<-data.frame()
temp_rw2<-data.frame()
for (i in 1:length(admin_name)){
  temp_rw2<-admin_rw2_smooth(admin_name[i],admin_ghost,pcprec)
  temp_rw2$admin<-admin_name[i]
  ghost_rw2<-rbind(ghost_rw2,temp_rw2)
}

return(ghost_rw2)
}

admin_plot<-function(admin_name,ct_dat){
  
  admin_3<-ct_dat[ct_dat$admin==admin_name,]
  low_cut<-min(0.8*min(admin_3$urban_prop),0.95*min(admin_3$lower,na.rm=TRUE))
  high_cut<-max(1.2*max(admin_3$urban_prop),1.05*max(admin_3$upper,na.rm=TRUE))
  
  g<-ggplot(admin_3, aes(year, urban_prop, colour = survey_year)) +
    geom_path(size=1)+geom_point()+
    ylim(c(low_cut,high_cut))+
    ggtitle(paste(admin_name)) +geom_ribbon(aes(ymin=lower, ymax=upper, x=year, fill = "band",color=survey_year), alpha = 0.1)+
    theme(plot.title = element_text(hjust = 0.5))+ylab('urban proportion')
  return(g)
  
}



#### Load data ####

nation_name <- 'Tanzania'

## data_path should contain a director with the name CountryFolderName
data_path<- paste0('~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/')
setwd(data_path)
# set working directory
# data path

source('../../Analysis/R/LoadCommandCenter.R')

CountryList<- sheets_read(sheet_key, sheet = "CountryList")
folder.name <- CountryList$folderName[CountryList$Country == nation_name]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == nation_name]
n.survey <- CountryList$nSurvey[CountryList$Country == nation_name]

SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == nation_name &
                                      SurveyInfo$`GPS?` == "Y"]

filenames <- SurveyInfo$BirthsFile[SurveyInfo$Country == nation_name
                                   & SurveyInfo$`GPS?`== "Y"]

filenames <- filenames[order(surveys)]
surveys <- surveys[order(surveys)]

# save path, where to save the RW2 smoothed estimates 
data_path<- paste0(data_path, folder.name)
setwd(data_path)

save_path <- paste0(data_path)


## load BR recode survey data in FL format
## should match the surveys in CountryName_cluster_dat.rda

svy_list<-list()

svy.idx <- 0
for(survey in surveys){
  svy.idx <- svy.idx + 1
  dat.tmp <- suppressWarnings(readstata13::read.dta13(paste0(data_path, 
                                                             '/dhsStata/',
                                                             filenames[svy.idx])))
  svy_list[[svy.idx]] <- dat.tmp
}

#### Load country specific admin1 information ####

load(paste0(data_path, '/',
            nation_name, '_cluster_dat.rda'))

#### Analysis ####

# svy_list<-list(NGBR4BFL,NGBR53FL,NGBR6AFL,NGBR7AFL)


ct_cluster<-mod.dat
ct_dic<-distinct(ct_cluster, cluster, survey, admin1.name)
colnames(ct_dic)<-c('v001','survey_year','admin')

# set the data in right format
y_list<-as.numeric(levels(as.factor(ct_dic$survey_year))) ## year of the surveys 

# generate analysis dataset
ct_all<-pre_prop(svy_list,ct_dic,surveys)
head(ct_all)
tail(ct_all)


#### Direct Estimates ####

# national direct estimates
ct_nat<-nat_multi_svy(svy_list,
                      survey_year = surveys)
ct_nat$admin<-'National'

# admin level direct estimates 
ct_multi_svy_admin<-admin_multi_svy(svy_all = ct_all,
                                    survey_year = surveys)



#### Combine Estimates ####

# logit estimates 
ct_multi_svy_all<-rbind(ct_multi_svy_admin,ct_nat)
ct_multi_svy_all<-svy_process(ct_multi_svy_all)



# combine estimates from different surveys 
ct_comb_all<-combine_all(ct_multi_svy_all)




#### RW2 Smooth ####

# RW2 smoothing all
ct_rw2_smooth<-rw2_smooth_all(ct_multi_svy_all)




#### Save file ####

# save as csv
ct_to_plot<-rbind(ct_rw2_smooth,ct_comb_all[,c(1:3,7:10)])
setwd(save_path)
write_csv(ct_to_plot[ct_to_plot$admin != "National" &
                     ct_to_plot$survey_year == "RW2 fitted",],
          paste('RW2_',nation_name,'.csv',sep=''))
write_csv(ct_to_plot[ct_to_plot$admin == "National" &
                       ct_to_plot$survey_year == "RW2 fitted",],
          paste('RW2_',nation_name,'_national.csv',sep=''))



#### Plot RW2 ####

# plot national
ct_to_plot[ct_to_plot$survey_year=='combined',]$lower<-NA
ct_to_plot[ct_to_plot$survey_year=='combined',]$upper<-NA

if(!dir.exists(paste0(data_path, '/Plots/propUrban'))){
  dir.create(paste0(data_path, '/Plots/propUrban'))
}

pdf(paste0(data_path, '/Plots/propUrban/',
           nation_name, '_propUrban_natl.pdf'),
    height = 4, width = 4)
admin_plot('National',ct_to_plot)
dev.off()


#### Plot Admin1 ####

# plot subnational
admin_list<-levels(as.factor(ct_to_plot[ct_to_plot$admin!='National',]$admin))

n_panels<-floor(length(admin_list)/4)
pdf(paste0(data_path, '/Plots/propUrban/',
           nation_name, '_propUrban_admin1.pdf'),
    height = 8, width = 8)
if(n_panels>0){
for (i in 0:(n_panels-1)){
  grid.arrange(
admin_plot(admin_list[i*4+1],ct_to_plot), 
admin_plot(admin_list[i*4+2],ct_to_plot),
admin_plot(admin_list[i*4+3],ct_to_plot),
admin_plot(admin_list[i*4+4],ct_to_plot),
             nrow=2,ncol = 2)
}
}


n_left<-length(admin_list)-n_panels*4


if(n_left>0){
last_panel<-list()
for (i in 1:n_left){
  last_panel[[i]]<-admin_plot(admin_list[n_panels*4+i],ct_to_plot)
}

if(n_left==1){
 grid.arrange(
last_panel[[1]],
             nrow=2,ncol = 2)
  
}

if(n_left==2){
 grid.arrange(
last_panel[[1]],
last_panel[[2]],
             nrow=2,ncol = 2)
}
 
if(n_left==3){
 grid.arrange(
last_panel[[1]],
last_panel[[2]],
last_panel[[3]],
             nrow=2,ncol = 2)
}

}
dev.off()




