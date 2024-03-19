library(tidyverse)
library(sf)
library(stars)

# #Yield raster from Mike & Annie 2022 (Clear Springs)
# rast <- read_stars('D:/geoData/SMSexport/202217 CLEAR SPRINGS COLONY/rasters/Mike & Annie_2022.tif') %>%
#   slice(band,which(st_get_dimension_values(.,3)=='median')) %>% #Get only median yield
#   setNames('Yield_tha')
# 
# #Field boundary
# fBound <- read_sf('D:/geoData/SMSexport/Field Boundaries/202217 CLEAR SPRINGS COLONY_poly.shp') %>%
#   filter(Field=='Mike & Annie') %>% st_geometry() %>%
#   st_transform(st_crs(rast))
# 
# #Potential seeding areas - Mike & Annie
# badSpots <- read_sf("yieldBoxes.shp")
# 
# #Econ data
# priceDat <- read.csv('D:/geoData/SMSexport/PPSN_code/data/cropPricesCSV.csv') %>%
#   filter(Prov=='Saskatchewan',CropType=='Canola',SoilZone=='Dark Brown')
# 
# save('badSpots','fBound','priceDat','rast',file = './carpFigures2023/yieldHistDat.RData')
# 
# #Alternate yield raster from Trent Clark
# rast <- read_stars('D:/geoData/SMSexport/202259 HAYWIRE HILL LTD/rasters/SW 02_2019.tif') %>%
#   slice(band,which(st_get_dimension_values(.,3)=='median')) %>% #Get only median yield
#   setNames('Yield_tha')
# 
# #Field boundary
# fBound <- read_sf('D:/geoData/SMSexport/Field Boundaries/202259 HAYWIRE HILL LTD_poly.shp') %>%
#   filter(Field=='SW 02') %>% st_geometry() %>%
#   st_transform(st_crs(rast))
# 
# #Econ data
# priceDat <- read.csv('D:/geoData/SMSexport/PPSN_code/data/cropPricesCSV.csv') %>%
#   filter(Prov=='Saskatchewan',CropType=='Canola',SoilZone=='Dark Brown')
# 
# save('rast','priceDat','fBound',file='./carpFigures2023/yieldHistDat2.Rdata')

load('./carpFigures2023/yieldHistDat.RData')
# load('./carpFigures2023/yieldHistDat2.RData')

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
  geom_sf(data=badSpots, col='red',fill=NA,size=2)+
  scale_fill_viridis_c(option='E',na.value = NA)+ #Cividis colour scheme
  theme_bw()+labs(x=NULL,y=NULL,fill='Dollars/ac')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank())

#Categorized plot
(p <- ggplot()+
  geom_stars(data=rast,aes(fill=Profit_ac_cat),na.action = na.omit)+
  scale_fill_viridis_d(option='E',na.value = NA,direction = -1)+ #Cividis colour scheme
  theme_bw()+labs(x=NULL,y=NULL,fill='Dollars\nper acre')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank(),legend.position = 'none'))
ggsave('profCatMap_noLab_noLeg.png',p,width=6,height=6)

ggsave('profCatMap_noLab_rLeg.png',p + theme(legend.position = 'right'),width=6,height=6)

ggsave('profCatMap_noLab_bLeg.png',p + theme(legend.position = 'bottom'),width=6,height=6)

(p <- p + geom_sf_text(data=st_centroid(badSpots),aes(label=FID),col='red',size=5) + 
  geom_sf(data=badSpots, col='red',fill=NA,size=1))
ggsave('profCatMap.png',p,width=6,height=6)

#Breaks for per-acre profit
acBreaks <- with(rast,c(-100,seq(0,800,100),max(Profit_ac,na.rm=TRUE)))

#Metrics
median(rast$Profit_ac,na.rm=TRUE) #Median profit for entire field
mean(rast$Profit_ac,na.rm=TRUE) #Mean profit
sum(rast$Profit_ac*0.0988421526,na.rm = TRUE) #Total $ profit for entire section (1 mi x 1 mi)

data.frame(prof=as.vector(rast$Profit_ac)) %>%  #Total unprofitable acres = 13.7%
  filter(!is.na(prof)) %>% 
  summarize(unprofitable=mean(prof<=0)*100)

#Data for yield histograms
yieldHistDat <- data.frame(prof=as.vector(rast$Profit_ac),profCat=as.vector(rast$Profit_ac_cat),
           badSpot=factor(sapply(st_intersects(rast,badSpots,sparse = TRUE),function(x) if(length(x)==0) 0 else x))) %>%
  filter(!is.na(prof)) %>% 
  mutate(prof=ifelse(prof>-90,prof,-50)) %>%
  mutate(profCat=factor(profCat,levels=levels(rast$Profit_ac_cat)))

#Overall histogram for entire field
(p <- yieldHistDat %>% 
  ggplot()+
  geom_histogram(aes(x=prof,y=after_stat(count),weight=0.0988421526,fill=profCat),
                 breaks=acBreaks,show.legend = FALSE)+
  labs(x='Profit ($/acre)',y='Number of acres')+
  scale_fill_viridis_d(direction = -1,option='E')+
  theme_classic())
ggsave('overallHist.png',p,width=4,height=4)

#Histograms for each "bad spot"
yieldHistDat %>% 
  filter(badSpot!=0) %>% 
  group_by(badSpot) %>% mutate(meanProf=paste0('Median profit: ',round(mean(prof)))) %>% 
  ggplot()+
  geom_histogram(aes(x=prof,y=after_stat(count),weight=0.0988421526,fill=profCat),breaks=acBreaks,show.legend = FALSE)+
  labs(x='Profit ($/acre)',y='Number of acres')+
  scale_fill_viridis_d(direction = -1,option='E')+
  theme_classic()+
  facet_wrap(~meanProf,ncol=2)

for(i in badSpots$FID){
  (p <- yieldHistDat %>% 
    filter(badSpot==i) %>% 
    ggplot()+
    geom_histogram(aes(x=prof,y=after_stat(count),weight=0.0988421526,fill=profCat),
                   breaks=acBreaks,show.legend = FALSE)+
    labs(x='Profit ($/acre)',y='Number of acres')+
    scale_fill_viridis_d(direction = -1,option='E')+
    theme_classic() )
  ggsave(paste0('badSpot',i,'.png'),p,width=3,height=3)
}

#Cumulative distribution

#This uses raw profit/acre. Discontinuity at lowest value
(p <- data.frame(prof=as.vector(rast$Profit_ac),profCat=as.vector(rast$Profit_ac_cat)) %>%
  filter(!is.na(prof)) %>% 
  mutate(profCat=factor(profCat,levels=levels(rast$Profit_ac_cat))) %>% 
  ggplot(aes(x=prof)) +
  stat_ecdf(geom = "step")+
  geom_vline(xintercept = 0,col='red')+
  coord_cartesian(xlim=c(NA,2000))+
  labs(x='Profit ($/acre)',y='Cumulative distribution')+
  theme_bw())
ggsave('cumDistProfit.png',p,width=6,height=6)

#Similar, but moves lowest value to next-lowest value, and adds random Norm(0,25) noise. 
#Looks "prettier", but is less conservative. Same number of unprofitable acres as above (13.7%)
(p <- data.frame(prof=as.vector(rast$Profit_ac),profCat=as.vector(rast$Profit_ac_cat)) %>%
  filter(!is.na(prof)) %>% 
  mutate(prof=ifelse(prof==min(prof),min(prof[prof!=min(prof)])+rnorm(n(),0,25),prof)) %>% #
  mutate(profCat=factor(profCat,levels=levels(rast$Profit_ac_cat))) %>% 
  ggplot(aes(x=prof)) +
  stat_ecdf(geom = "step")+
  geom_vline(xintercept = 0,col='red')+
  coord_cartesian(xlim=c(NA,2000))+
  labs(x='Profit ($/acre)',y='Cumulative distribution')+
  theme_bw())
ggsave('cumDistProfit_adjusted.png',p,width=6,height=6)

# Alternate colour scheme (R-G) -------------------------------------------

ggplot(fBound)+
  geom_stars(data=rast,aes(fill=Profit_ac))+
  scale_fill_distiller(palette = 'RdYlGn', na.value = NA,direction = 1)+ 
  theme_bw()+labs(x=NULL,y=NULL,fill='Dollars/ac')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank())

#Initial estimate had very high cost of canola (952/t), but what if it was lower? 500/t is about the average from 2010-2020
rast2 <- rast %>% #Changes NA cells within field to 0.01
  select(Yield_tha) %>% 
  mutate(Profit_ha=Yield_tha*500-priceDat$AvgCost_ha) %>% 
  mutate(Profit_ha_cat=cut(Profit_ha,breaks = c(min(Profit_ha,na.rm = TRUE),0,400,800,max(Profit_ha,na.rm=TRUE)),include.lowest=TRUE,
                           labels=c('<$0','$0-400','$400-800','>$800'))) %>% 
  mutate(Profit_ha_cat=factor(Profit_ha_cat,levels=rev(levels(Profit_ha_cat)))) %>% 
  mutate(Profit_ac=Profit_ha/2.47105) %>% 
  mutate(Profit_ac_cat=cut(Profit_ac,breaks = c(min(Profit_ac,na.rm = TRUE),0,100,200,300,400,max(Profit_ac,na.rm=TRUE)),include.lowest=TRUE,
                           labels=c('<$0','$0-100','$100-200','$200-300','$300-400','>$400'))) %>% 
  mutate(Profit_ac_cat=factor(Profit_ac_cat,levels=rev(levels(Profit_ac_cat))))

library(ggpubr)

#Continuous plot
p1 <- ggplot(fBound)+
  geom_stars(data=rast,aes(fill=Profit_ac))+
  scale_fill_distiller(palette = 'RdYlGn', na.value = NA,direction = 1,limits=c(-172,860))+ 
  theme_bw()+labs(x=NULL,y=NULL,fill='$/ac',title='Canola: $22/bu')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank())
p2 <- ggplot(fBound)+
  geom_stars(data=rast2,aes(fill=Profit_ac))+
  scale_fill_distiller(palette = 'RdYlGn', na.value = NA,direction = 1,limits=c(-172,860))+ 
  theme_bw()+labs(x=NULL,y=NULL,fill='$/ac',title='Canola: $12/bu')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid = element_blank())
ggarrange(p1,p2,common.legend = TRUE,legend='right')

#Categorized plot
p1 <- ggplot(fBound)+
  geom_stars(data=rast,aes(fill=Profit_ac_cat))+
  scale_fill_brewer(palette = 'RdYlGn',direction = -1)+ 
  theme_bw()+labs(x=NULL,y=NULL,fill='Dollars/ac',title='Canola: $22/bu (13.6% profitable)')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank())
p2 <- ggplot(fBound)+
  geom_stars(data=rast2,aes(fill=Profit_ac_cat))+
  scale_fill_brewer(palette = 'RdYlGn',direction = -1)+ 
  theme_bw()+labs(x=NULL,y=NULL,fill='$/ac',title='Canola: $12/bu (40.5% profitable)')+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid = element_blank())
(p <- ggarrange(p1,p2,common.legend = TRUE,legend='right'))
ggsave('./carpFigures2023/profCatMap_priceDiff.png',plot = p,bg = 'white',dpi = 400)
