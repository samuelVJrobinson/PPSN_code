#Written to convert custom shapefiles from growers

source("D:\\geoData\\SMSexport\\helperFunctions.R")
library(tidyverse)
library(sf)

#Template

# Longitude	Latitude Grower	#Field	#Date_ymd	#Year	#Crop	#Variety	CombineID	Distance_m	Track_deg	Duration_s	Elevation_m	SwathWidth_m	Moisture_perc	Yield_tha	Speed_kmh	Fuel_L
# -117.8436917	56.10191291	202236 REBELLION FARMS	44 acres	2021-09-21	2021	Canola	234	1H0S690SCD0765043	0.605	0.641	1	646.46	12.19	4.24	1.762	2.179	0
# -117.8436914	56.10191976	202236 REBELLION FARMS	44 acres	2021-09-21	2021	Canola	234	1H0S690SCD0765043	0.765	0.921	1	646.47	12.19	4.24	1.961	2.755	0
# -117.8436913	56.10192675	202236 REBELLION FARMS	44 acres	2021-09-21	2021	Canola	234	1H0S690SCD0765043	0.775	0.911	1	646.47	12.19	4.24	1.82	2.791	0

#202216 ZENNETH FAYE --------------------------


readDir <- "D:\\geoData\\YieldStorageRaw\\202216 Zenneth Faye\\Diamond F Harvest\\Harvest 2022 FE"
writeDir <- "D:\\geoData\\SMSexport\\202216 ZENNETH FAYE"
#Filepaths
fps <- list.files(readDir,pattern='*.shp$',recursive = TRUE,full.names = TRUE)

for(path in fps){
  fname <- strsplit(path,'/')[[1]]
  fname <- gsub("(^\\d+_|_rawYield_shp\\.shp$)","",fname[length(fname)])
  fname <- gsub('_','-',fname)
  
  if(!file.exists(paste0(writeDir,'\\',fname,'_2022.csv'))){
    read_sf(path) %>% 
      mutate(Longitude=st_coordinates(.)[,1],Latitude=st_coordinates(.)[,2]) %>% 
      st_drop_geometry() %>%
      mutate(Grower="202216 ZENNETH FAYE",Field=fname) %>%
      mutate(datetime=as.POSIXlt(datetime,format='%FT%H:%M:%S')) %>% 
      mutate(Date_ymd=format(datetime,format='%F'),Year=format(datetime,format='%Y')) %>% 
      transmute(Longitude,Latitude,Grower,Field,Date_ymd,Year,Crop=crop,Variety=variety,
                CombineID=paste(imp_make,imp_model,imp_name,sep='-'),
                Distance_m=interval_m,Track_deg=direction,Duration_s=interval_s,Elevation_m=elev_m,
                SwathWidth_m=width_m,Moisture_perc=`moisture_%`,Yield_tha=`rate_kg/ha`/1000) %>% 
      filter(!is.na(Yield_tha),Distance_m!=0,!is.na(Distance_m)) %>% 
      mutate(Yield_tha=ifelse(Yield_tha==0,0.01,Yield_tha)) %>% 
      write.csv(file=paste0(writeDir,'\\',fname,'_',unique(.$Year),'.csv'),row.names=FALSE)  
    print(paste0('Finished ',fname))
  } else print(paste0(fname,' already exists'))
}

#202230 Hansbrek farms inc ----------------------

readDir <- "D:\\geoData\\YieldStorageRaw\\202230 Hansbrek Farms Inc\\crop study"
writeDir <- "D:\\geoData\\SMSexport\\202230 HANSBREK FARMS LTD"
# unzipAll(readDir,rmOld = TRUE)

#Filepaths
fps <- list.files(readDir,pattern='*.shp$',recursive = TRUE,full.names = TRUE)
fps <- fps[grepl('20\\d{2} points/Hansbrek Farms Inc',fps)]
  
path <- fps[1]

fname <- strsplit(path,'/')[[1]]
fname <- gsub("(Hansbrek Farms Inc_|_corrected_points.shp$)","",fname[length(fname)])
fname <- gsub('_','-',fname)

# temp <- 
  read_sf(path) %>% 
  st_drop_geometry() %>%
  mutate(gps_time=as.POSIXct(gps_time)) %>% 
  arrange(gps_time) %>% 
  transmute(Longitude=longitude,Latitude=latitude,Grower="202230 HANSBREK FARMS INC",
            Field=fld_name,Date_ymd=format(gps_time,format='%F'),Year=format(gps_time,format='%Y'),
            Crop=crop,Variety=NA_character_,CombineID=combine,Distance_m=NA_real_,Track_deg=NA_real_,Duration_s=2,Elevation_m=NA_real_,
            SwathWidth_m=headerw_m,Moisture_perc=moisture,Yield_tha=yldg_m2/100,Speed_kmh=speedkmh) %>% 
    write.csv(file=paste0(writeDir,'\\',fname,'_',unique(.$Year),'.csv'),row.names=FALSE)  

# Longitude	Latitude Grower	Field	Date_ymd	Year	Crop	Variety	CombineID	Distance_m	Track_deg	Duration_s	Elevation_m	SwathWidth_m	Moisture_perc	Yield_tha	Speed_kmh	Fuel_L
# -117.8436917	56.10191291	202236 REBELLION FARMS	44 acres	2021-09-21	2021	Canola	234	1H0S690SCD0765043	0.605	0.641	1	646.46	12.19	4.24	1.762	2.179	0
# -117.8436914	56.10191976	202236 REBELLION FARMS	44 acres	2021-09-21	2021	Canola	234	1H0S690SCD0765043	0.765	0.921	1	646.47	12.19	4.24	1.961	2.755	0
# -117.8436913	56.10192675	202236 REBELLION FARMS	44 acres	2021-09-21	2021	Canola	234	1H0S690SCD0765043	0.775	0.911	1	646.47	12.19	4.24	1.82	2.791	0

