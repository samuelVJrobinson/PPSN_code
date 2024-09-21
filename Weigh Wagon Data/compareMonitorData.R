# Compare weigh wagon data with yield monitor data
library(tidyverse)
theme_set(theme_bw())

getFieldSummaries <- function(path){
  cClass <- c(rep('numeric',2),rep('character',7),rep('numeric',9),rep('logical',7))
  dat <- data.table::fread(path,sep=",",colClasses = cClass,select=c('Crop','Distance_m','SwathWidth_m','Yield_tha'))
  if(any(!complete.cases(dat))) warning('NAs found in harvest data')
  (hArea <- sum(with(dat,Distance_m*SwathWidth_m/10000),na.rm=TRUE)) #Harvested area (ha)
  (hMass <- sum(with(dat,Yield_tha*Distance_m*SwathWidth_m/10000),na.rm=TRUE)) #Mass (t)
  (avYield <- hMass/hArea) #Yield (t/ha)
  return(data.frame('Crop'=paste(unique(dat$Crop),collapse='/'),'Area_ha'=hArea,'Mass_t'=hMass,'Yield_tha'=avYield))
}

# getFieldSummaries("D:\\geoData\\SMSexport\\202204 SHANE STROEDER\\clean\\Allans east_2022.csv") #Works

# Get yield data from directories on Galpern01 -----------------

rootDirs <- c("D:\\geoData\\SMSexport\\202204 SHANE STROEDER\\clean",
              "D:\\geoData\\SMSexport\\202207 JVM VAN STAVEREN\\clean",
              "D:\\geoData\\SMSexport\\202215 ARM RIVER COLONY\\clean",
              "D:\\geoData\\SMSexport\\202221 ABERHART FARMS INC\\clean",
              "D:\\geoData\\SMSexport\\202231 HILLSBORO FARMS\\clean")

searchPatterns <- c("\\_2022.csv$","\\_2022.csv$","\\_2022.csv$",
                    "\\_202(1|2).csv$","\\_20(18|19|20|21|22).csv$")

yld <- vector('list',length(rootDirs)) #Empty list

for(i in 1:length(rootDirs)){
  paths <- dir(rootDirs[i],full.names = TRUE,pattern = searchPatterns[i])
  yld[[i]] <- lapply(paths,getFieldSummaries) %>% set_names(gsub('.csv','',basename(paths))) %>% 
    bind_rows(.id='FieldYear') %>% separate_wider_delim(FieldYear,names=c('Field','Year'),delim='_')  
  print(paste0('Finished ',i,'/',length(rootDirs)))
}

yld %>% set_names(sapply(strsplit(rootDirs,'\\\\'),function(x) x[4])) %>% 
  bind_rows(.id='Grower') %>% 
  write.csv("F://Weigh Wagon Data//monitorData.csv",row.names = FALSE)

# 

# Load in compiled weigh wagon/monitor data -----------------

bulkDens <- read.csv("C:\\Users\\Samuel\\Documents\\Projects\\UofC postdoc\\PPSN_code\\data\\cropBulkDensity.csv") %>%   filter(CropType!='Spring Wheat',CropType!='Winter Wheat') %>% 
  mutate(CropType=ifelse(CropType=='Durum','Wheat',CropType))

dat <- read.csv("C:\\Users\\Samuel\\Desktop\\Weigh Wagon Data\\weighWagon_monitorData.csv") %>% 
  select(Grower,Year,FieldName,Crop,Acres:Yield_buac,Crop_monitor:Yield_tha_monitor) %>% 
  mutate(Area_ha=Acres/2.471054,Mass_t=Weight_lbs/2204.623) %>% #Convert areas & weights
  select(-Weight_lbs,-Acres) %>% 

# Load in compiled weigh wagon/monitor data -----------------

bulkDens <- read.csv("C:\\Users\\Samuel\\Documents\\Projects\\UofC postdoc\\PPSN_code\\data\\cropBulkDensity.csv") %>%
# bulkDens <- read.csv("D:\\geoData/SMSexport/PPSN_code/data/cropBulkDensity.csv") %>%   
  filter(CropType!='Spring Wheat',CropType!='Winter Wheat') %>% 
  mutate(CropType=ifelse(CropType=='Durum','Wheat',CropType))

wDat <- read.csv("D:\\Weigh Wagon Data/wagonData.csv",strip.white = TRUE,sep=',') %>% filter(Use) %>% 
  group_by(Grower,Year,FieldName) %>% mutate(N=n()) %>% ungroup() %>% 
  filter(N==1) %>% select(-N,-FieldName_original) %>% 
  filter(Yield_buac>10) #Fields with very low yields tend to have mistakes
head(wDat)
mDat <- read.csv("D:\\Weigh Wagon Data/monitorData.csv",strip.white = TRUE,sep=',') %>% 
  group_by(Grower,Year,Field) %>% mutate(N=n()) %>% filter(N==1) %>% ungroup() %>% 
  select(-N,-Field_original) %>% ungroup() %>% 
  rename_with(.cols=c(-Grower:-Year),.fn=~paste0(.x,'_monitor')) 
head(mDat)

#Joined wagon/monitor data
dat <- wDat %>% inner_join(mDat,by=c('Grower'='Grower','Year'='Year','FieldName'='Field')) %>% ungroup()

# #Unmatched data
# noDat <- wDat %>% anti_join(mDat,by=c('Grower'='Grower','Year'='Year','FieldName'='Field')) %>% ungroup()
# select(noDat,Grower,Year,FieldName) %>% rename('FieldName_wagon'=FieldName) %>% data.frame()
# 
# #Full join
# wDat %>% full_join(mutate(mDat,Field_monitor=Field),
#                    by=c('Grower'='Grower','Year'='Year','FieldName'='Field')) %>% ungroup %>% 
#   select(Grower:Crop,Weight_lbs:Field_monitor) %>% data.frame() %>% 
#   write.csv("F:\\Weigh Wagon Data\\fullJoin.csv",row.names = FALSE)

## Hand-joined data  
# dat <- read.csv("C:\\Users\\Samuel\\Desktop\\Weigh Wagon Data\\weighWagon_monitorData.csv")

dat <- dat %>% select(Grower,Year,FieldName,Crop,Acres:Yield_buac,Crop_monitor:Yield_tha_monitor) %>%
  mutate(Area_ha=Acres/2.471054,Mass_t=Weight_lbs/2204.623) %>% #Convert areas & weights
  select(-Weight_lbs,-Acres) %>%

  mutate(Crop=case_when(
    Crop=="SpringWheat" ~ "Spring Wheat",
    Crop=="CWRS" ~ "Spring Wheat",
    Crop=="CPSR" ~ "Spring Wheat",
    Crop=="DurumWheat" ~ "Durum Wheat",
    Crop=="Durum" ~ "Durum Wheat",
    TRUE ~ Crop
<<<<<<< HEAD
  )) %>% 
  mutate(Crop=ifelse(grepl('Wheat',Crop),'Wheat',Crop)) %>% #Simplifies wheat types
  left_join(bulkDens,by=c('Crop'='CropType')) %>% #Joins in bulk density
  mutate(Mass_t=ifelse(is.na(Mass_t),Volume_bu/Bu_t,Mass_t)) %>% 
  mutate(Volume_bu=ifelse(is.na(Volume_bu),Mass_t*Bu_t,Volume_bu)) %>% 
  mutate(Yield_tha=Yield_buac*2.471054/Bu_t) %>% 
  select(-Bu_t,-DryMoisture_perc,-Yield_buac) %>% #filter(!is.na(Mass_t)) %>% 
  relocate(Grower:Volume_bu,Area_ha,Mass_t,Yield_tha,Crop_monitor:Yield_tha_monitor) %>% 
  mutate(MassErr_t=Mass_t_monitor-Mass_t,YieldErr_tha=Yield_tha_monitor-Yield_tha) %>% 
  mutate(MassErr_perc=100*MassErr_t/Mass_t,YieldErr_perc=100*YieldErr_tha/Yield_tha) %>% 
  mutate(Year=factor(Year)) %>% 
  group_by(Grower,Year,FieldName) %>% mutate(N=n()) %>% filter(N==1) %>% select(-N)
=======
  )) %>%
  mutate(Crop=ifelse(grepl('Wheat',Crop),'Wheat',Crop)) %>% #Simplifies wheat types
  left_join(bulkDens,by=c('Crop'='CropType')) %>% #Joins in bulk density
  mutate(Mass_t=ifelse(is.na(Mass_t),Volume_bu/Bu_t,Mass_t)) %>%
  mutate(Volume_bu=ifelse(is.na(Volume_bu),Mass_t*Bu_t,Volume_bu)) %>%
  mutate(Yield_tha=Yield_buac*2.471054/Bu_t) %>%
  select(-Bu_t,-DryMoisture_perc) %>% #filter(!is.na(Mass_t)) %>%
  relocate(Grower:Volume_bu,Area_ha,Mass_t,Yield_tha,Yield_buac,Crop_monitor:Yield_tha_monitor) %>%
  mutate(MassErr_t=Mass_t_monitor-Mass_t,YieldErr_tha=Yield_tha_monitor-Yield_tha) %>%
  mutate(MassErr_perc=100*MassErr_t/Mass_t,YieldErr_perc=100*YieldErr_tha/Yield_tha) %>%
  mutate(Year=factor(Year)) %>%
  group_by(Grower,Year,FieldName) %>% mutate(N=n()) %>% filter(N==1) %>% select(-N) %>% 
  ungroup()
>>>>>>> fcc3e7600f810614c47535139e7f3c894403444e

head(dat)
 
## Mass (t) ----------------------

dat %>% #Overall
  filter(!is.na(Mass_t_monitor),!is.na(Mass_t)) %>% 
  ggplot(aes(x=Mass_t_monitor,y=Mass_t))+
  geom_point()+
  geom_abline(intercept = 0,slope = 1,col='red')+
  labs(x='Monitor Mass (t)',y='Wagon Mass (t)')

dat %>% #By grower
  filter(!is.na(Mass_t_monitor),!is.na(Mass_t)) %>% 
  ggplot(aes(x=Mass_t_monitor,y=Mass_t))+
  geom_point()+
  facet_wrap(~Grower,scales = 'free')+
  geom_abline(intercept = 0,slope = 1,col='red')+
  labs(x='Monitor Mass (t)',y='Wagon Mass (t)')
  
#Grower/crop
dat %>% filter(Crop!='Mustard',Crop!='Oats') %>% 
  filter(!is.na(Mass_t_monitor),!is.na(Mass_t)) %>% 
  ggplot(aes(x=Mass_t_monitor,y=Mass_t,shape=Year))+
  geom_point()+
  facet_grid(Crop~Grower,scales = 'free')+
  geom_abline(intercept = 0,slope = 1,col='red')+
  labs(x='Monitor Mass (t)',y='Wagon Mass (t)')

# Yield (t/ha) -----------------------

dat %>% #Overall
  filter(!is.na(Yield_tha),!is.na(Yield_tha_monitor)) %>% 
  ggplot(aes(x=Yield_tha_monitor,y=Yield_tha))+
  geom_point()+
  geom_abline(intercept = 0,slope = 1,col='red')+
<<<<<<< HEAD
  labs(x='Monitor Yield (t/ha)',y='Wagon Mass (t/ha)')
=======
  geom_smooth(method='lm',formula = y ~ x)+
  facet_wrap(~Grower)+
  labs(x='Monitor Yield (t/ha)',y='Wagon Yield (t/ha)')

lm(Yield_tha~Yield_tha_monitor+0,data=dat) %>% summary
>>>>>>> fcc3e7600f810614c47535139e7f3c894403444e

dat %>% #Grower
  filter(!is.na(Yield_tha),!is.na(Yield_tha_monitor)) %>% 
  ggplot(aes(x=Yield_tha_monitor,y=Yield_tha))+
  geom_point()+
  facet_wrap(~Grower,scales = 'free')+
  geom_abline(intercept = 0,slope = 1,col='red')+
<<<<<<< HEAD
  labs(x='Monitor Yield (t/ha)',y='Wagon Mass (t/ha)')
=======
  labs(x='Monitor Yield (t/ha)',y='Wagon Yield (t/ha)')

dat %>% #Crop
  filter(!is.na(Yield_tha),!is.na(Yield_tha_monitor)) %>% 
  ggplot(aes(x=Yield_tha_monitor,y=Yield_tha,shape=Grower))+
  geom_point()+
  facet_wrap(~Crop)+
  geom_abline(intercept = 0,slope = 1,col='red')+
  labs(x='Monitor Yield (t/ha)',y='Wagon Yield (t/ha)')
>>>>>>> fcc3e7600f810614c47535139e7f3c894403444e

dat %>% #Grower/crop
  filter(!is.na(Yield_tha),!is.na(Yield_tha_monitor)) %>% 
  ggplot(aes(x=Yield_tha_monitor,y=Yield_tha,shape=Year))+
  geom_point()+
<<<<<<< HEAD
  facet_grid(Crop~Grower,scales = 'free')+
  geom_abline(intercept = 0,slope = 1,col='red')+
  labs(x='Monitor Yield (t/ha)',y='Wagon Mass (t/ha)')
=======
  facet_grid(Crop~Grower)+
  geom_abline(intercept = 0,slope = 1,col='red')+
  labs(x='Monitor Yield (t/ha)',y='Wagon Yield (t/ha)')
>>>>>>> fcc3e7600f810614c47535139e7f3c894403444e

# Percent error -----------------------------------------------------------

dat %>% 
<<<<<<< HEAD
  filter(!is.na(YieldErr_tha)) %>% 
  ggplot(aes(x=YieldErr_tha))+
=======
  filter(!is.na(YieldErr_tha)) %>%   ggplot(aes(x=YieldErr_tha))+
>>>>>>> fcc3e7600f810614c47535139e7f3c894403444e
  geom_histogram()+
  geom_vline(xintercept = 0)+
  labs(x='Error (t/ha)',title='Yield Error (t/ha)')

dat %>% 
<<<<<<< HEAD
  filter(!is.na(YieldErr_perc)) %>% 
  filter(abs(YieldErr_perc)<400) %>% 
  ggplot(aes(x=YieldErr_perc))+
  geom_histogram()+
  geom_vline(xintercept = 0)+
  labs(x='Error (%)',title='Yield Error (%)')

dat %>% 
  mutate(across(c(YieldErr_perc,MassErr_perc), ~ifelse(is.na(.x),FALSE,abs(.x)>100),.names = "{.col}_bad")) %>% 
  mutate(badPerc=YieldErr_perc_bad|MassErr_perc_bad) %>% select(-contains("_bad")) %>% 
  group_by(Grower,Year,FieldName) %>% mutate(N=n()) %>% 
  write.csv("C:\\Users\\Samuel\\Desktop\\Weigh Wagon Data\\badMonitorData.csv",row.names = FALSE)

# dat %>% 
#   filter(!is.na(Mass_t),!is.na(Mass_t_monitor)) %>% 
=======
  filter(!is.na(YieldErr_perc)) %>%
  ggplot(aes(x=Yield_tha,y=YieldErr_perc))+
  geom_point()+
  geom_hline(yintercept = 0)+
  facet_wrap(~Grower)+
  labs(x='Yield (t/ha)',y='Monitor Error (%)')

dat %>% 
  filter(!is.na(YieldErr_perc)) %>%
  ggplot(aes(x=Yield_tha,y=YieldErr_perc,shape=Grower))+
  geom_point()+
  geom_hline(yintercept = 0)+
  facet_wrap(~Crop)+
  labs(x='Yield (t/ha)',y='Monitor Error (%)')

dat %>% 
  filter(!is.na(YieldErr_perc)) %>%
  # filter(abs(YieldErr_perc)<400) %>%
  ggplot(aes(x=YieldErr_perc))+
  geom_histogram()+
  geom_vline(xintercept = 0)+
  # stat_summary(fun=mean,geom = 'vline')+
  labs(x='Error (%)',title='Yield Error (%)')

# dat %>% 
#   mutate(across(c(YieldErr_perc,MassErr_perc), ~ifelse(is.na(.x),FALSE,abs(.x)>100),.names = "{.col}_bad")) %>% 
#   mutate(badPerc=YieldErr_perc_bad|MassErr_perc_bad) %>% select(-contains("_bad")) %>% 
#   group_by(Grower,Year,FieldName) %>% mutate(N=n()) %>% 
#   write.csv("C:\\Users\\Samuel\\Desktop\\Weigh Wagon Data\\badMonitorData.csv",row.names = FALSE)

# dat %>%
#   filter(!is.na(Mass_t),!is.na(Mass_t_monitor)) %>%
>>>>>>> fcc3e7600f810614c47535139e7f3c894403444e
#   ggplot(aes(x=MassErr_t))+
#   geom_histogram()+
#   geom_vline(xintercept = 0)+
#   labs(x='Error (t)',title='Mass Error')

<<<<<<< HEAD
dat %>% 
  filter(!is.na(Yield_tha),!is.na(Yield_tha_monitor)) %>% 
  ggplot(aes(x=Yield_tha_monitor,y=YieldErr_perc))+
  geom_point()+
  # geom_abline(intercept = 0,slope = 1,col='red')+
  labs(x='Monitor Yield (t/ha)',y='Error (%)',title='Yield Error')

# dat %>% 
#   filter(!is.na(Mass_t),!is.na(Mass_t_monitor)) %>% 
=======
# dat %>%
#   filter(!is.na(Mass_t),!is.na(Mass_t_monitor)) %>%
>>>>>>> fcc3e7600f810614c47535139e7f3c894403444e
#   ggplot(aes(x=Mass_t_monitor,y=MassErr_perc))+
#   geom_point()+
#   labs(x='Monitor Yield (t)',y='Error (%)',title='Mass Error')

dat %>% 
  filter(!is.na(MassErr_perc)) %>% 
  ggplot(aes(x=Grower,y=MassErr_perc))+
  geom_boxplot()+
  geom_hline(yintercept = 0)+
  labs(x='Grower',y='Error (%)',title='Mass Error (%)')+
  ylim(NA,100)

dat %>% 
  filter(!is.na(Yield_tha),!is.na(Yield_tha_monitor)) %>% 
  ggplot(aes(x=Grower,y=YieldErr_perc))+
  geom_boxplot()+
  geom_hline(yintercept = 0)+
  labs(x='Grower',y='Error (%)',title='Yield Error (%)')+
  ylim(NA,100)

<<<<<<< HEAD
=======
# Other: Get yield data from directories on Galpern01 -----------------

getFieldSummaries <- function(path){
  cClass <- c(rep('numeric',2),rep('character',7),rep('numeric',9),rep('logical',7))
  dat <- data.table::fread(path,sep=",",colClasses = cClass,select=c('Crop','Distance_m','SwathWidth_m','Yield_tha'))
  if(any(!complete.cases(dat))) warning('NAs found in harvest data')
  (hArea <- sum(with(dat,Distance_m*SwathWidth_m/10000),na.rm=TRUE)) #Harvested area (ha)
  (hMass <- sum(with(dat,Yield_tha*Distance_m*SwathWidth_m/10000),na.rm=TRUE)) #Mass (t)
  (avYield <- hMass/hArea) #Yield (t/ha)
  return(data.frame('Crop'=paste(unique(dat$Crop),collapse='/'),'Area_ha'=hArea,'Mass_t'=hMass,'Yield_tha'=avYield))
}

# getFieldSummaries("D:\\geoData\\SMSexport\\202204 SHANE STROEDER\\clean\\Allans east_2022.csv") #Works


rootDirs <- c("D:\\geoData\\SMSexport\\202204 SHANE STROEDER\\clean",
              "D:\\geoData\\SMSexport\\202207 JVM VAN STAVEREN\\clean",
              "D:\\geoData\\SMSexport\\202215 ARM RIVER COLONY\\clean",
              "D:\\geoData\\SMSexport\\202221 ABERHART FARMS INC\\clean",
              "D:\\geoData\\SMSexport\\202231 HILLSBORO FARMS\\clean")

searchPatterns <- c("\\_2022.csv$","\\_2022.csv$","\\_2022.csv$",
                    "\\_202(1|2).csv$","\\_20(18|19|20|21|22).csv$")

yld <- vector('list',length(rootDirs)) #Empty list

for(i in 1:length(rootDirs)){
  paths <- dir(rootDirs[i],full.names = TRUE,pattern = searchPatterns[i])
  yld[[i]] <- lapply(paths,getFieldSummaries) %>% set_names(gsub('.csv','',basename(paths))) %>% 
    bind_rows(.id='FieldYear') %>% separate_wider_delim(FieldYear,names=c('Field','Year'),delim='_')  
  print(paste0('Finished ',i,'/',length(rootDirs)))
}

yld %>% set_names(sapply(strsplit(rootDirs,'\\\\'),function(x) x[4])) %>% 
  bind_rows(.id='Grower') %>% 
  write.csv("F://Weigh Wagon Data//monitorData.csv",row.names = FALSE)
>>>>>>> fcc3e7600f810614c47535139e7f3c894403444e
