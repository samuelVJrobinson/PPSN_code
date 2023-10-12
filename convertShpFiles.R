#Written to convert custom shapefiles from growers
setwd("D:/geoData/SMSexport/PPSN_code")
source("./helperFunctions.R")
library(tidyverse)
library(sf)

#Template

# Longitude	Latitude Grower	#Field	#Date_ymd	#Year	#Crop	#Variety	CombineID	Distance_m	Track_deg	Duration_s	Elevation_m	SwathWidth_m	Moisture_perc	Yield_tha	Speed_kmh	Fuel_L

# 202216 ZENNETH FAYE - DONE --------------------------

readDir <- "D:\\geoData\\YieldStorageRaw\\202216 Zenneth Faye\\Diamond F Harvest\\Harvest 2022 FE"
writeDir <- "D:\\geoData\\SMSexport\\202216 ZENNETH FAYE"
#Filepaths
fps <- list.files(readDir,pattern='*.shp$',recursive = TRUE,full.names = TRUE)

for(path in fps){
  fname <- strsplit(path,'/')[[1]]
  fname <- gsub("(^\\d+_|_rawYield_shp\\.shp$)","",fname[length(fname)])
  fname <- gsub('_','-',fname)
  
  # if(!file.exists(paste0(writeDir,'\\',fname,'_2022.csv'))){
    read_sf(path) %>% 
      mutate(Longitude=st_coordinates(.)[,1],Latitude=st_coordinates(.)[,2]) %>% 
      st_drop_geometry() %>%
      mutate(Grower="202216 ZENNETH FAYE",Field=fname) %>%
      mutate(datetime=as.POSIXlt(datetime,format='%FT%H:%M:%S')) %>% 
      mutate(Date_ymd=format(datetime,format='%F'),Year=format(datetime,format='%Y')) %>% 
      transmute(Longitude,Latitude,Grower,Field,Date_ymd,Year,Crop=crop,Variety=variety,
                CombineID=paste(imp_make,imp_model,imp_name,sep='-'),
                Distance_m=interval_m,Track_deg=direction,Duration_s=interval_s,Elevation_m=elev_m,
                SwathWidth_m=width_m,Moisture_perc=`moisture_%`,
                Yield_tha=`rate_kg/ha`/1000,Speed_kmh = speed_kph,Fuel_L=`fuel_L/ha`) %>% 
      filter(!is.na(Yield_tha),Distance_m!=0,!is.na(Distance_m)) %>% 
      mutate(Yield_tha=ifelse(Yield_tha==0,0.01,Yield_tha)) %>% 
      write.csv(file=paste0(writeDir,'\\',fname,'_',unique(.$Year),'.csv'),row.names=FALSE)  
    print(paste0('Finished ',fname))
  # } else print(paste0(fname,' already exists'))
}

# 202230 HANSBREK FARMS - DONE ----------------------

readDir <- "D:\\geoData\\YieldStorageRaw\\202230 Hansbrek Farms Inc\\crop study"
writeDir <- "D:\\geoData\\SMSexport\\202230 HANSBREK FARMS LTD"
# unzipAll(readDir,rmOld = TRUE)

#Filepaths
fps <- list.files(readDir,pattern='*.shp$',recursive = TRUE,full.names = TRUE)
fps <- fps[grepl('20\\d{2} points/Hansbrek Farms Inc',fps)]
  
# path <- fps[1]
for(path in fps){
  fname <- strsplit(path,'/')[[1]]
  fname <- gsub("(Hansbrek Farms Inc_|_corrected_points.shp$)","",fname[length(fname)])
  fname <- gsub('_','-',fname)

  read_sf(path) %>% st_drop_geometry() %>%
  mutate(gps_time=as.POSIXct(gps_time)) %>% 
  arrange(gps_time) %>% 
  transmute(Longitude=longitude,Latitude=latitude,Grower="202230 HANSBREK FARMS INC",
            Field=fld_name,Date_ymd=format(gps_time,format='%F'),Year=format(gps_time,format='%Y'),
            Crop=crop,Variety=NA_character_,CombineID=combine,
            Distance_m=NA_real_,Track_deg=NA_real_,
            Duration_s=2,Elevation_m=NA_real_,
            SwathWidth_m=headerw_m,Moisture_perc=moisture,
            Yield_tha=yldg_m2/100,Speed_kmh=speedkmh,Fuel_L=NA_real_) %>%
    write.csv(file=paste0(writeDir,'\\',fname,'_',unique(.$Year),'.csv'),row.names=FALSE)
}

# 202244 MARC AND CHERYL NOREEN - DONE -------------------------------

readDir <- "D:\\geoData\\YieldStorageRaw\\202244 Marc and Cheryl Norleen\\2139926_canplug_shapefile_harvest_03_30_2023"
writeDir <- "D:\\geoData\\SMSexport\\202244 MARC AND CHERYL NOREEN"

#Filepaths
fps <- list.files(readDir,pattern='*.shp$',recursive = TRUE,full.names = TRUE)
# path <- fps[1]

for(path in fps){
  
  fname <- strsplit(path,'/')[[1]]
  fname <- gsub("([0-9]{7}_|_rawYield_shp.shp)","",fname[length(fname)])
  fname <- gsub('_','-',fname)
  
  read_sf(path) %>% mutate(datetime=as.POSIXct(datetime)) %>% 
    transmute(Longitude=st_coordinates(.)[,1],Latitude=st_coordinates(.)[,2],
              Grower="202244 MARC AND CHERYL NOREEN",Field=fname,
              Date_ymd=format(datetime,format='%F'),Year=format(datetime,format='%Y'),
              Crop=crop,Variety=variety,
              CombineID=paste(imp_make,imp_model,imp_name,sep='-'),
              Distance_m=interval_m,Track_deg=direction,Duration_s=interval_s,
              Elevation_m=elev_m,SwathWidth_m=width_m,
              Moisture_perc=`moisture_%`,Yield_tha=`rate_kg/ha`/1000,
              Speed_kmh = speed_kph,Fuel_L=`fuel_L/ha`) %>% 
    filter(!is.na(Yield_tha),Distance_m!=0,!is.na(Distance_m)) %>% 
    mutate(Yield_tha=ifelse(Yield_tha==0,0.01,Yield_tha)) %>% 
    write.csv(file=paste0(writeDir,'\\',fname,'_',unique(.$Year),'.csv'),row.names=FALSE)  
  
}

# 202261 DAVID FORSEILLE - DONE ------------------------

readDir <- "D:\\geoData\\YieldStorageRaw\\202261 David Forseille/202261 Yield Data Files 2021 2022/"
writeDir <- "D:\\geoData\\SMSexport\\202261 DAVID FORSEILLE"

lbsAc2tha <- 2.47105/2204.61 #Conversion factor for lbs per acre

fps <- list.files(readDir,pattern='*.shp$',recursive = TRUE,full.names = TRUE)
fps <- fps[grepl('Harvest',fps)]

cropBulkDens <- read.csv('./data/cropBulkDensity.csv') #Crop bulk density info

# path <- fps[1]

for(path in fps){
  
  fname <- strsplit(path,'/')[[1]]
  fname <- gsub("(Forseille Fa_Forseille Fa_|_Harvest.*shp$)","",fname[length(fname)])
  fname <- gsub('_','-',fname)
  
  # # Test read JSON files
  # lapply(list.files(readDir,pattern='*.json$',recursive = TRUE,full.names = TRUE),function(x){
  #   j <- readLines(x,warn=FALSE)
  #   j <- j[which(grepl('CropName',j))]
  #   j <- strsplit(j,'"')[[1]][4]
  #   j
  # })
  
  #Read JD JSON file to get crop type
  cropName <- readLines(gsub(".shp$","-Deere-Metadata.json",path),warn = FALSE)
  cropName <- cropName[which(grepl('CropName',cropName))]
  cropName <- strsplit(cropName,'"')[[1]][4]
  if(cropName=="Wheat (Hard Red Spring)") cropName <- 'Spring Wheat'
  
  #Lookup dry moisture%
  DryMoisture_perc <- cropBulkDens$DryMoisture_perc[cropBulkDens$CropType==cropName]
  
  read_sf(path) %>% 
    mutate(Longitude=st_coordinates(.)[,1],Latitude=st_coordinates(.)[,2]) %>% 
    # st_drop_geometry() %>%
    mutate(Grower="202262 DAVID FORSEILLE",Field=fname) %>%
    mutate(IsoTime =as.POSIXlt(IsoTime,format='%FT%H:%M:%S')) %>% 
    mutate(Date_ymd=format(IsoTime,format='%F'),Year=format(IsoTime,format='%Y')) %>% 
    mutate(Duration_s=as.numeric(difftime(IsoTime,lag(IsoTime),units = 'secs'))) %>% 
    mutate(Duration_s=ifelse((is.na(Duration_s)|Duration_s==0),mean(Duration_s,na.rm=TRUE),Duration_s)) %>% 
    mutate(speed=3.6*DISTANCE/Duration_s) %>% 
    transmute(Longitude,Latitude,Grower,Field,Date_ymd,Year,Crop=cropName,Variety=VARIETY,
              CombineID=Machine,
              Distance_m=DISTANCE ,Track_deg=Heading,
              Duration_s=Duration_s,
              Elevation_m=Elevation/3.28084, #Originally in feet
              SwathWidth_m=SWATHWIDTH,Moisture_perc=Moisture,
              Yield_tha=(WetMass*lbsAc2tha)*(1-Moisture_perc/100)/(1-DryMoisture_perc/100),
              Speed_kmh = speed, Fuel_L=NA) %>% 
    filter(!is.na(Yield_tha),Distance_m!=0,!is.na(Distance_m)) %>% 
    mutate(Yield_tha=ifelse(Yield_tha==0,0.01,Yield_tha)) %>% st_drop_geometry() %>% 
    write.csv(file=paste0(writeDir,'\\',fname,'_',unique(.$Year),'.csv'),row.names=FALSE)
}


# Longitude	Latitude Grower	Field	Date_ymd	Year	Crop	Variety	CombineID	Distance_m	Track_deg	Duration_s	Elevation_m	SwathWidth_m	Moisture_perc	Yield_tha	Speed_kmh	Fuel_L
# -117.8436917	56.10191291	202236 REBELLION FARMS	44 acres	2021-09-21	2021	Canola	234	1H0S690SCD0765043	0.605	0.641	1	646.46	12.19	4.24	1.762	2.179	0
# -117.8436914	56.10191976	202236 REBELLION FARMS	44 acres	2021-09-21	2021	Canola	234	1H0S690SCD0765043	0.765	0.921	1	646.47	12.19	4.24	1.961	2.755	0
# -117.8436913	56.10192675	202236 REBELLION FARMS	44 acres	2021-09-21	2021	Canola	234	1H0S690SCD0765043	0.775	0.911	1	646.47	12.19	4.24	1.82	2.791	0



# Process all dirs --------------------------

dirs <- dir('D:\\geoData\\SMSexport\\','(202216|202230|202244|202261)',include.dirs = TRUE,full.names = TRUE)

for(d in dirs){
  setwd(d)
  for(l in dir('.','*\\_20\\d{2}.csv',full.names = TRUE)){
    p1 <- gsub('./','./clean/',l,fixed = TRUE) #csv writing path
    p2 <- gsub('csv$','png',p1) #png writing path
    if(file.exists(p1)){
      print(paste0(gsub('./','',l),' already processed. Skipping.'))
    } else {
      try({
        clean_csv(l,p1,p2,useVega = TRUE,keepFiltCols = TRUE,ncore = 12,speedR2thresh = 0.9)
      },outFile =gsub('.csv','_ERROR.txt',basename(l)))
      gc(FALSE)
    } 
  }  
}
