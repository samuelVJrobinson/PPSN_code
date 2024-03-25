#Example profit map figures for general use

library(tidyverse)
library(sf)
library(stars)
library(ggpubr)
library(ggmap)

#API key for Google maps = AIzaSyBIf_mPDHW8WYilCC_t4q-Vs1BDalwWHH0
register_google(key = "AIzaSyBIf_mPDHW8WYilCC_t4q-Vs1BDalwWHH0") #Personal Google maps key

# Load in raster files
storageDir <- './carpFigures2023/Example yield maps/'
tifFiles <- list.files(storageDir,pattern = '.tif$',full.names = TRUE)

rastList <- lapply(tifFiles,function(x){
  brs <- c(0,0.08,0.25,0.5,0.75,1) #Quantile breaks
  read_stars(x) %>% 
    slice(band,which(st_get_dimension_values(.,3)=='median')) %>% #Get only median yield
    setNames('Yield_tha') %>% 
    mutate(Yield_cat=cut(Yield_tha,breaks = quantile(Yield_tha,brs,na.rm=TRUE))) %>% 
    st_transform(2957)
}) %>% setNames(nm=gsub('.tif$','',tifFiles))

# rast <- rastList[[1]] %>% 
#   mutate(Yield_cat=Yield_tha-quantile(Yield_tha,probs = 0.15,na.rm = TRUE)) %>%
#   # mutate(Yield_cat=cut(Yield_tha,breaks = 5))
#   
# ggplot()+geom_stars(data=rast,aes(fill=Yield_cat))

plotList <- lapply(rastList,function(x){
  # brs <- quantile(x$Yield_tha,seq(0,1,length.out=10),na.rm=TRUE)
  # brs <- quantile(x$Yield_tha,c(0,0.1,0.25,0.5,0.75,1),na.rm=TRUE)
  # x <- x %>% 
  #   # mutate(Yield_cat=cut(Yield_tha,breaks = 5))
  #   mutate(Yield_cat=cut(Yield_tha,breaks = brs)) %>% 
  #   st_transform(2957)
  
  p <- ggplot()+geom_stars(data=x,aes(fill=Yield_cat),show.legend=FALSE)+
    # scale_colour_brewer(aesthetics = 'fill',palette = 'YlGn')+
    scale_fill_manual(values = c('firebrick',RColorBrewer::brewer.pal(4,'YlGn')),na.value = NA)+
    theme_bw()+theme(axis.text = element_blank(),
                     axis.title = element_blank(),
                     axis.ticks = element_blank())
  return(p)
}) 

(p <- do.call(ggarrange,c(plotList,ncol=2,nrow=3,labels='auto',label.x=0.05,label.y=0.95)))
ggsave('./carpFigures2023/Example yield maps/candidateFields.png',height=12,width=8)

# Seems like D and E are the best - need to fill in the gaps
do.call(ggarrange,c(plotList[4:5]))

# Plots D and E ---------------------------

dRast <- rastList[[4]]
eRast <- rastList[[5]]

dBound <- read_sf('./carpFigures2023/shapefiles/202223 S AND T ACRES_poly.shp') %>% 
  filter(grepl('HIB.8.9',Field)) %>% st_transform(st_crs(dRast))
eBound <- read_sf('./carpFigures2023/shapefiles/202213 LAJORD COLONY_poly.shp') %>% 
  filter(grepl('Lefebvre Section',Field)) %>% st_transform(st_crs(eRast))

# brs <- c(0,0.1,0.25,0.5,0.75,1) #Quantile breaks

dRast <- dRast %>% 
  mutate(inField=st_intersects(dRast,dBound,sparse = FALSE,as_points = TRUE)) %>% 
  mutate(Yield_tha=ifelse(inField&is.na(Yield_tha),0.05,Yield_tha),
         Yield_cat=ifelse(inField&is.na(Yield_cat),dRast$Yield_cat[dRast$Yield_cat==levels(dRast$Yield_cat)[1]&!is.na(dRast$Yield_cat)][1],Yield_cat)) %>% 
  mutate(Yield_cat=factor(Yield_cat))

eRast <- eRast %>% 
  mutate(inField=st_intersects(eRast,eBound,sparse = FALSE,as_points = TRUE)) %>% 
  mutate(Yield_tha=ifelse(inField&is.na(Yield_tha),0.05,Yield_tha),
         Yield_cat=ifelse(inField&is.na(Yield_cat),eRast$Yield_cat[eRast$Yield_cat==levels(eRast$Yield_cat)[1]&!is.na(eRast$Yield_cat)][1],Yield_cat)) %>% 
  mutate(Yield_cat=factor(Yield_cat))

p1 <- ggplot()+
  geom_stars(data=dRast,aes(fill=Yield_cat),show.legend=FALSE)+
  # geom_sf(data=dBound,fill=NA)+
  scale_fill_manual(values = c('firebrick',RColorBrewer::brewer.pal(4,'YlGn')),na.value = NA)+
  theme_nothing()
  # theme_bw()+theme(axis.text = element_blank(),
  #                  axis.title = element_blank(),
  #                  axis.ticks = element_blank())

p2 <- ggplot()+
  geom_stars(data=eRast,aes(fill=Yield_cat),col=NULL,show.legend=FALSE)+
  # geom_sf(data=eBound,fill=NA)+
  scale_fill_manual(values = c('firebrick',RColorBrewer::brewer.pal(4,'YlGn')),na.value = NA)+
  theme_nothing()
  # theme_bw()+theme(axis.text = element_blank(),
  #                  axis.title = element_blank(),
  #                  axis.ticks = element_blank())

(p <- ggarrange(p1,p2,ncol = 2))
ggsave('./carpFigures2023/Example yield maps/d_e_noSat.png',p,height=8,width=12,bg = 'white',dpi = 400)

dRast_bbox <- st_as_sf(dRast) %>% st_centroid() %>% filter(!is.na(Yield_tha)) %>% 
  st_transform(4326) %>% st_bbox() #Bounding box for observations (buffered by 100 m)
dRast_center <- c(mean(dRast_bbox[c(1,3)]),mean(dRast_bbox[c(2,4)])) #Map center
dMapDat <- get_googlemap(dRast_center, zoom=15, maptype = "satellite")
# dRastVect <- dRast %>% st_as_sf() %>% st_transform(4326) %>% filter(inField) #Alternate version converting raster to vector

p1 <- ggmap(dMapDat,extent = 'device')+
  coord_sf(crs = st_crs(4326))+
  # geom_sf(data=dRastVect,aes(fill=Yield_cat),col=NA,inherit.aes = FALSE,show.legend=FALSE)+ 
  geom_stars(data=st_transform(dRast,4326),aes(fill=Yield_cat),col=NA,inherit.aes = FALSE,show.legend=FALSE,alpha=1)+
  scale_fill_manual(values = c('firebrick',RColorBrewer::brewer.pal(4,'YlGn')),na.value = NA)
  # geom_sf(data=st_transform(dBound,4326),inherit.aes = FALSE,col='black',fill=NA)
  # scale_x_continuous(limits = c(dRast_bbox[1], dRast_bbox[3]), expand = c(0, 0)) 
#Can clip, but cuts off Google logo (https://stackoverflow.com/questions/64425970/ggmap-in-r-keep-google-copyright-information-on-cropped-map)

eRast_bbox <- st_as_sf(eRast) %>% st_centroid() %>% filter(!is.na(Yield_tha)) %>% 
  st_transform(4326) %>% st_bbox() #Bounding box for observations (buffered by 100 m)
eRast_center <- c(mean(eRast_bbox[c(1,3)]),mean(eRast_bbox[c(2,4)])) #Map center
eMapDat <- get_googlemap(eRast_center, zoom=15, maptype = "satellite")

p2 <- ggmap(eMapDat,extent = 'device')+
  coord_sf(crs = st_crs(4326))+
  geom_stars(data=st_transform(eRast,4326),aes(fill=Yield_cat),col=NA,inherit.aes = FALSE,show.legend=FALSE,alpha=1)+
  scale_fill_manual(values = c('firebrick',RColorBrewer::brewer.pal(4,'YlGn')),na.value = NA)

(p <- ggarrange(p1,p2,ncol = 2))
ggsave('./carpFigures2023/Example yield maps/d_e_sat.png',p,height=8,width=12,bg = 'white',dpi = 400)
  
  













