#Example profit map figures for general use

library(tidyverse)
library(sf)
library(stars)
library(ggpubr)

# Load in raster files
storageDir <- './carpFigures2023/Example yield maps/'
tifFiles <- list.files(storageDir,pattern = '.tif$',full.names = TRUE)

rastList <- lapply(tifFiles,function(x){
  read_stars(x) %>% 
    slice(band,which(st_get_dimension_values(.,3)=='median')) %>% #Get only median yield
    setNames('Yield_tha') %>% st_transform(2957)
}) %>% setNames(nm=gsub('.tif$','',tifFiles))

# rast <- rastList[[1]] %>% 
#   mutate(Yield_cat=Yield_tha-quantile(Yield_tha,probs = 0.15,na.rm = TRUE)) %>%
#   # mutate(Yield_cat=cut(Yield_tha,breaks = 5))
#   
# ggplot()+geom_stars(data=rast,aes(fill=Yield_cat))

plotList <- lapply(rastList,function(x){
  # brs <- quantile(x$Yield_tha,seq(0,1,length.out=10),na.rm=TRUE)
  brs <- quantile(x$Yield_tha,c(0,0.1,0.25,0.5,0.75,1),na.rm=TRUE)
  x <- x %>% 
    # mutate(Yield_cat=cut(Yield_tha,breaks = 5))
    mutate(Yield_cat=cut(Yield_tha,breaks = brs)) %>% 
    st_transform(2957)
  
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





