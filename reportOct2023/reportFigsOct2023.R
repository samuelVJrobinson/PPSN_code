# Load everything --------------------

library(tidyverse)
library(sf)
library(stars)
library(ggpubr)
theme_set(theme_classic())

source("C:\\Users\\Samuel\\Documents\\Projects\\UofC postdoc\\PPSN_code\\helperFunctions.R")
load("C:\\Users\\Samuel\\Dropbox\\PPSN Cleaned Yield\\Rasterized yield\\canProf.Rdata")

canProf <- canProf[sapply(canProf,class)=='data.frame'] %>% 
  bind_rows(.id='Grower') %>% 
  filter(Yield_buAc!=0,!is.na(Yield_buAc))

canProf2 <- canProf %>% 
  mutate(Yield_buAc=round(Yield_buAc)) %>% 
  group_by(Grower,FieldYear,Yield_buAc) %>% 
  summarize(CropType=first(CropType),Profit_ac=mean(Profit_ac),N=n()) %>% 
  ungroup() 

#Overall acreage plot -----------------

canProf %>% filter(Yield_buAc<250) %>% 
  ggplot(aes(x=Yield_buAc,y=after_stat(count),weight=0.0988421526))+
  geom_histogram(binwidth = 5)+
  labs(x='Yield (bu/ac)',y='Acres')

# canProf2 %>% group_by(Yield_buAc) %>%
#   summarize(N=sum(N)*0.0988421526) %>% #Convert to acres
#   ungroup() %>%
#   mutate(cProp=cumsum(N)/sum(N)) %>%
#   filter(cProp<0.995) %>%
#   mutate(N=ifelse(Yield_buAc==max(Yield_buAc) & N[1]<last(N) ,first(N),N)) %>%
#   mutate(N=ifelse(Yield_buAc==min(Yield_buAc) & N[1]>last(N) ,last(N),N)) %>%
#   ggplot(aes(x=Yield_buAc,y=N))+
#   geom_polygon(alpha=0.3)+
#   geom_path()+
#   labs(x='Yield (bu/ac)',y='Acres')

#Overall profit plot -------------

canProf %>% ggplot(aes(x=Profit_ac,y=after_stat(count),weight=0.0988421526))+
  geom_text(aes(x=0,y=10000,label=paste0(round(100*mean(Profit_ac<0,na.rm=TRUE),2),'%\nunprofitable')),
                        col='red')+
  geom_histogram(breaks=seq(-200,2000,20),aes(fill=Profit_ac<0),show.legend = FALSE)+
  labs(x='Profit ($/ac)',y='Acres')+
  coord_cartesian(xlim=c(NA,2000))+
  scale_fill_manual(values=c('black','red'))

# canProf2 %>% filter(!is.na(Profit_ac)) %>% 
#   filter()
#   mutate(Profit_ac=cut(Profit_ac)) %>%
#   group_by(Profit_ac) %>% summarize(N=sum(N)*0.0988421526) %>%
#   ggplot(aes(x=Profit_ac,y=N))+
#   geom_col()+
#   # geom_polygon(alpha=0.3)+
#   # geom_path()+
#   labs(x='Profit ($/ac)',y='Acres')

#Acreage by crop (top 12, i.e. no corn)
# canProf2 %>% filter(CropType!='Corn') %>%
#   group_by(CropType,Yield_buAc) %>%
#   summarize(N=sum(N)*0.0988421526) %>% #Convert to acres
#   group_by(CropType) %>%
#   # filter(Yield_buAc<quantile(Yield_buAc,0.99)) %>%
#   mutate(cProp=cumsum(N)/sum(N)) %>%
#   filter(cProp<0.995) %>%
#   mutate(total=sum(N)) %>%
#   arrange(desc(total),desc(Yield_buAc)) %>%
#   mutate(N=ifelse(Yield_buAc==max(Yield_buAc) & N[1]<last(N) ,first(N),N)) %>%
#   mutate(N=ifelse(Yield_buAc==min(Yield_buAc) & N[1]>last(N) ,last(N),N)) %>%
#   ungroup() %>%
#   mutate(CropType=paste0(CropType,': ',round(total),' ac')) %>%
#   mutate(CropType=factor(CropType,levels=unique(CropType))) %>%
#   ggplot(aes(x=Yield_buAc,y=N))+
#   # geom_polygon(alpha=0.3)+geom_path()+
#   geom_col(fill='gray70')+
#   facet_wrap(~CropType,scales='free') +
#   labs(x='Yield (bu/ac)',y='Acres')

canProf %>% #slice_sample(n=20000) %>% 
  filter(CropType!='Corn') %>% 
  group_by(CropType) %>% 
  mutate(n=n()*0.0988421526) %>% 
  filter(Yield_buAc<quantile(Yield_buAc,0.99)) %>% 
  ungroup() %>% 
  mutate(CropType=paste0(CropType,': ',round(n),' ac')) %>% 
  arrange(desc(n)) %>% 
  mutate(CropType=factor(CropType,levels=unique(CropType))) %>% 
  ggplot(aes(x=Yield_buAc,y=after_stat(count),weight=0.0988421526))+
  geom_histogram(bins=30)+
  facet_wrap(~CropType,scales='free')+
  labs(x='Yield (bu/ac)',y='Acres')

canProf %>% filter(!is.na(Profit_ac)) %>% 
  #slice_sample(n=20000) %>% 
  filter(CropType!='Corn') %>% 
  group_by(CropType) %>% 
  mutate(n=n()*0.0988421526) %>% 
  filter(Yield_buAc<quantile(Yield_buAc,0.99)) %>% 
  ungroup() %>% 
  mutate(CropType=paste0(CropType,': ',round(n),' ac')) %>% 
  arrange(desc(n)) %>% 
  mutate(CropType=factor(CropType,levels=unique(CropType))) %>% 
  ggplot(aes(x=Profit_ac,y=after_stat(count),weight=0.0988421526))+
  geom_histogram(aes(fill=Profit_ac<0),show.legend = FALSE)+
  facet_wrap(~CropType,scales='free')+
  labs(x='Yield (bu/ac)',y='Acres')+
  scale_fill_manual(values=c('black','red'))

# Comparison of yield data + filled-in data -----------------

load("C:\\Users\\Samuel\\Documents\\Projects\\UofC postdoc\\PPSN_code\\carpFigures2023\\yieldHistDat.RData")

#Cells inside field
inField <- st_intersects(rast,fBound,sparse = FALSE,as_points = TRUE)

rast <- rast %>% #Changes NA cells within field to 0.01
  mutate(Yield_tha=ifelse(is.na(Yield_tha)&inField,0.01,Yield_tha)) %>%
  mutate(Profit_ha=Yield_tha*priceDat$CropPrice_t-priceDat$AvgCost_ha) %>% 
  mutate(Profit_ha_cat=cut(Profit_ha,breaks = c(min(Profit_ha,na.rm = TRUE),0,400,800,max(Profit_ha,na.rm=TRUE)),include.lowest=TRUE,
                           labels=c('<$0','$0-400','$400-800','>$800'))) %>% 
  mutate(Profit_ha_cat=factor(Profit_ha_cat,levels=rev(levels(Profit_ha_cat)))) %>% 
  mutate(Profit_ac=Profit_ha/2.47105) %>% 
  mutate(Profit_ac_cat=cut(Profit_ac,breaks = c(min(Profit_ac,na.rm = TRUE),0,100,200,300,400,max(Profit_ac,na.rm=TRUE)),include.lowest=TRUE,
                           labels=c('<$0','$0-100','$100-200','$200-300','$300-400','>$400'))) %>% 
  mutate(Profit_ac_cat=factor(Profit_ac_cat,levels=rev(levels(Profit_ac_cat))))

#Transform
epsgCode <- 2957 #Sask UTM 13
# epsgCode <- 3401 #AB 10 
fBound <- fBound %>% st_transform(epsgCode)
rast <- rast %>% st_transform(epsgCode)
badSpots <- badSpots %>% st_transform(epsgCode)

#Overall plot
ggplot(fBound)+
  geom_stars(data=rast,aes(fill=Profit_ac))+
  scale_fill_viridis_c(option='E',na.value = NA)+ #Cividis colour scheme
  theme_bw()+labs(x=NULL,y=NULL,fill='Dollars/ac')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank())

#Categorized plot
rastNA <- rast %>% 
  mutate(Profit_ac_cat=ifelse(Yield_tha==0.01,NA,Profit_ac_cat)) %>% 
  mutate(Profit_ac_cat=factor(Profit_ac_cat,labels=levels(rast$Profit_ac_cat)))

p1 <- ggplot()+
  geom_stars(data=rastNA,aes(fill=Profit_ac_cat),na.action = na.omit)+
  scale_fill_viridis_d(option='E',na.value = NA,direction = -1)+ #Cividis colour scheme
  theme_bw()+labs(x=NULL,y=NULL,fill='Dollars\nper acre')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank(),legend.position = 'right')

p2 <- ggplot()+
  geom_stars(data=rast,aes(fill=Profit_ac_cat),na.action = na.omit)+
  scale_fill_viridis_d(option='E',na.value = NA,direction = -1)+ #Cividis colour scheme
  theme_bw()+labs(x=NULL,y=NULL,fill='Dollars\nper acre')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank(),legend.position = 'right')

ggarrange(p1,p2,ncol=2,common.legend = TRUE, legend='right')

#Data for yield histograms
yieldHistDat <- data.frame(prof=as.vector(rast$Profit_ac),profCat=as.vector(rast$Profit_ac_cat),
                           fillin=as.vector(rast$Yield_tha)==0.01,
                           badSpot=factor(sapply(st_intersects(rast,badSpots,sparse = TRUE),function(x) if(length(x)==0) 0 else x))) %>%
  filter(!is.na(prof)) %>% 
  mutate(prof=ifelse(prof>-90,prof,-50)) %>%
  mutate(profCat=factor(profCat,levels=levels(rast$Profit_ac_cat)))

#Compare profit distributions
p1 <- yieldHistDat %>% filter(!fillin) %>% 
  ggplot()+
  geom_histogram(aes(x=prof,y=after_stat(count),weight=0.0988421526,fill=prof<0),
                 breaks=seq(-100,856,20),show.legend = FALSE)+
  labs(x='Profit ($/acre)',y='Number of acres')+
  geom_text(x=400,y=20,aes(label=paste0(round(100*mean(prof<0),2),'% Unprofitable')),col='red')+
  theme_classic()+
  scale_fill_manual(values=c('black','red'))

p2 <- yieldHistDat %>% ggplot()+
  geom_histogram(aes(x=prof,y=after_stat(count),weight=0.0988421526,fill=prof<0),
                 breaks=seq(-100,856,20),show.legend = FALSE)+
  labs(x='Profit ($/acre)',y='Number of acres')+
  geom_text(x=400,y=40,aes(label=paste0(round(100*mean(prof<0),2),'% Unprofitable')),col='red')+
  theme_classic()+
  scale_fill_manual(values=c('black','red'))

ggarrange(p1,p2,ncol=1)

# Comparison of overall raster acreage vs polygon area (i.e. % coverage) ------------
bPolyPath <- "C:\\Users\\Samuel\\Dropbox\\PPSN Cleaned Yield\\Field Boundaries"
sFiles <- dir(bPolyPath,"*.shp",full.names = TRUE)

fieldAreas <- lapply(sFiles,function(x){
  read_sf(x) %>% 
    transmute(Field=Field,area_m2=as.numeric(st_area(.))) %>% 
    st_drop_geometry()  
}) %>% setNames(nm=gsub('_poly.shp','',dir(bPolyPath,"*.shp"),fixed=TRUE)) %>% 
  bind_rows(.id='Grower')

rastCoverage <- canProf %>% 
  filter(!is.na(Yield_buAc)) %>% 
  count(Grower,FieldYear) %>% 
  ungroup() %>% 
  separate(FieldYear,c('Field','Year'),sep='_') %>% 
  left_join(fieldAreas,by=c('Grower'='Grower','Field'='Field')) %>% 
  mutate(prop=n*(20^2)/area_m2)

head(rastCoverage)

(p1 <- rastCoverage %>% filter(prop<1.1) %>% 
  ggplot(aes(x=prop))+geom_histogram()+
  geom_vline(xintercept = 1,col='blue')+
  geom_vline(xintercept=c(0.74,0.93),linetype='dashed',col='red')+
  annotate('text',x=c(0.74,0.93),y=1050,label=c('25th','50th'),size=3,col='red')+
  labs(x='Proportion raster coverage',y='Number of fields')+
  scale_x_continuous(breaks=seq(0,1,0.25)))

(p2 <- rastCoverage %>% filter(prop<1.1) %>% 
  ggplot(aes(x=prop))+stat_ecdf()+
  geom_vline(xintercept = 1,col='red')+
  geom_hline(yintercept = c(0.045,0.11,0.25),linetype='dashed')+
  annotate(geom='label',x=1.1,y=c(0.045,0.11,0.25),label=c('0.045','0.11','0.25'),size=3)+
  labs(x='Proportion raster coverage',y='Proportion of fields')+
  scale_x_continuous(breaks=seq(0,1,0.25)))

ggarrange(p1,p2,ncol=1)

# Map of coverage across provinces --------------

canProvinces <- read_sf('./Shapefiles/Canada provincial/gpr_000b11a_e.shp') %>% 
  filter(grepl("(Alberta|Saskatchewan)",PRENAME)) %>% 
  st_transform(3978)

bPolyPath <- "C:\\Users\\Samuel\\Dropbox\\PPSN Cleaned Yield\\Field Boundaries"
sFiles <- dir(bPolyPath,"*.shp",full.names = TRUE)

fieldCent <- lapply(sFiles,function(x){
  read_sf(x) %>% select(Field) %>% st_centroid() %>% st_transform(3978)
}) %>% bind_rows()

ggplot()+
  geom_sf(data=canProvinces)+
  geom_sf(data=fieldCent,size=0.5)




canProf %>% select(Grower,FieldYear) %>% distinct() %>% 
  separate(FieldYear,c('Field','Year'),sep='_') %>% pull(Year) %>%
  unique() %>% length
