source("D:\\geoData\\SMSexport\\helperFunctions.R")

library(sf)
library(tidyverse)
library(stars)

# Get crop type from ACI and add to boundary polygons -----------------------------

#Read in ACI data for field boundary polygons
bFiles <- list.files("D:\\geoData\\SMSexport\\Field Boundaries",pattern = '*.shp$',full.names = TRUE)

for(i in 1:length(bFiles)){
  cropTypeACI(bFiles[i])  
}

##Debugging
# d <- read_sf("D:\\geoData\\SMSexport\\Field Boundaries/202202 LAKELAND COLLEGE_poly.shp")
# d$y2022 <- NULL
# d <- st_write(d,"D:\\geoData\\SMSexport\\Field Boundaries/202202 LAKELAND COLLEGE_poly.shp",append = FALSE)
# cropTypeACI("D:\\geoData\\SMSexport\\Field Boundaries/202231 HILLSBORO FARMS_poly.shp")
# debugonce(cropTypeACI)


# Match crop type from polygons to yield data -----------------------------

getCropMismatch <- function(boundPath=NULL,yieldDatPath=NULL){
  library(sf)
  library(tidyverse)
  library(stars)
  
  #Read in boundary polygons
  bPoly <- read_sf(boundPath) %>% ungroup() %>% 
    pivot_longer(matches('^y2'),names_to='Year',values_to='cover') %>% 
    mutate(Year=gsub('^y','',Year)) %>% 
    mutate(cover1=gsub('_.*$','',cover),cover1Perc=as.numeric(gsub('(^\\D+_|,.*$)','',cover))) %>% 
    relocate(geometry,.after=cover1Perc)
  
  #Read in yield data
  yieldDatFiles <- data.frame(path=list.files(yieldDatPath,pattern='*.csv',full.names = TRUE)) %>% 
    mutate(fieldYear=gsub('.csv$','',basename(path))) %>% 
    separate(fieldYear,c('Field','Year'),sep='_')
  
  if(any(!unique(yieldDatFiles$Field) %in% unique(bPoly$Field))){
    stop("Field name mismatch:\n", 
         unique(yieldDatFiles$Field)[!unique(yieldDatFiles$Field) %in% unique(bPoly$Field)])
  } 
  #Join yield dat to boundary polygons
  yieldDatFiles <- left_join(yieldDatFiles,bPoly,by=c('Field','Year'))
  
  #Get yield data
  yieldDatFiles <- yieldDatFiles %>% 
    mutate(datCover=sapply(yieldDatFiles$path,function(p){
      u <- unique(data.table::fread(p,sep=",",select = c("Crop")))
      paste0(u,collapse='_')
    })) %>% 
    mutate(datCover=case_when(
      grepl('Spring Wheat',datCover) ~ datCover,
      TRUE ~ gsub('(Spring |Field | - Annual)','',datCover)
    )) 
  
  if(any(with(yieldDatFiles,cover1!=datCover))){
    print('Mismatched cover types:')
    ret <- yieldDatFiles %>% filter(cover1!=datCover) %>% 
      st_drop_geometry() %>% 
      separate(cover,paste0(rep('ACIcover',5),1:5),sep=',') %>% 
      select(-ACIcover4,-ACIcover5) %>% 
      select(Field,Year,ACIcover1:ACIcover3,datCover)
  } else {
    print('All cover types match')
    ret <- NA
  }
  return(ret)
}

# Debugging
bp <- "D:\\geoData\\SMSexport\\Field Boundaries/202201 CLINTON MONCHUK_poly.shp"
yp <- "D:\\geoData\\SMSexport\\202201 CLINTON MONCHUK\\clean"
getCropMismatch(bp,yp)

yieldDirs <- list.dirs("D:\\geoData\\SMSexport",recursive = FALSE)
yieldDirs <- normalizePath(paste0(yieldDirs[grepl('2022\\d{2}',yieldDirs)],'\\clean'))

# inputdf <- data.frame(bp=list.files("D:\\geoData\\SMSexport\\Field Boundaries",pattern = '.shp'),
#                       yp=yieldDirs)


# Function to change names of growers across all csvs ---------------------

changeGrowerName <- function(d=NULL,newName=NULL){
  # d <- "D:\\geoData\\SMSexport\\202223 S and T Acres"
  # newName <- '202223 S AND T ACRES'

  library(data.table)  
  if(is.null(d)) stop('Directory not named')
  if(is.null(newName)) stop('newName must be provided')
  #Files
  f <- dir(d,pattern='.csv',full.names = TRUE,recursive = TRUE)
  
  pb <- txtProgressBar(style=3)
  for(i in 1:length(f)){
    dat <- fread(f[i],sep=","
      #            ,colClasses = c(
      # rep('numeric',2),rep('character',7),rep('numeric',9))
      ) 
    dat$Grower <- rep(newName,nrow(dat))
    fwrite(dat,f[i],quote = TRUE)  
    setTxtProgressBar(pb,i/length(f))
  }
  close(pb)
}

# debugonce(changeGrowerName)
changeGrowerName("D:\\geoData\\SMSexport\\202224 LEWIS FARMS","202224 LEWIS FARMS")


# Get crop types within soil layers (for Sarah P) ---------------

#Get dominant crop cover for each year/polygon
bFiles <- list.files("D:\\geoData\\SMSexport\\Field Boundaries",
                     pattern = '*.shp$',full.names = TRUE)
bPoly <- lapply(bFiles,st_read,quiet=TRUE) %>%
  do.call('rbind',.) %>%
  mutate(across(matches('^y'),~gsub('_.*$','',.x))) %>%
  mutate(across(matches('^y'),~ifelse(.x=='NonCrop',NA,.x)))

bPoly %>% st_drop_geometry() %>% ungroup() %>%
  pivot_longer(-Field,names_to='year',values_to='cover') %>%
  mutate(cover=factor(gsub('_.*$','',cover)),year=factor(gsub('y','',year))) %>%
  filter(!is.na(cover)) %>% 
  count(cover) %>% arrange(desc(n))

#Crop types by year - lots of canola and spring wheat
bPoly %>% st_drop_geometry() %>% ungroup() %>%
  pivot_longer(-Field,names_to='year',values_to='cover') %>%
  mutate(year=factor(gsub('y','',year))) %>%
  filter(cover!='NonCrop',cover!='Other crops',!is.na(cover)) %>%
  count(cover,year,name='N') %>% group_by(cover) %>%
  mutate(r=sum(N)) %>% ungroup() %>% arrange(desc(r)) %>%
  mutate(cover=factor(cover,levels=unique(cover))) %>%
  mutate(r=as.numeric(factor(cover))) %>% filter(r<11) %>%
  ggplot(aes(x=as.numeric(as.character(year)),y=N,col=cover))+
  geom_point()+geom_line()+labs(x='Year',y='Number of fields')+
  scale_colour_brewer(type='qual',palette = 'Paired')+theme_bw()

#Get soil/PDQ layer
soil <- st_read("D:/geoData/Shapefiles/Soil Layers/PRV_SZ_PDQ_v6.shp")
soilProv <- soil %>% group_by(SoilZone,Prov) %>% summarize(do_union = TRUE)

ggplot()+geom_sf(data=soil,aes(fill=SoilZone))
ggplot()+geom_sf(data=soilProv,aes(fill=SoilZone))

#Unique crop types
cTypes <- bPoly %>% st_drop_geometry() %>% pivot_longer(-Field) %>%
  filter(!is.na(value)) %>% pull(value) %>% unique()

#String for each year/PDQ combo
bPolyInt <- st_intersects(soil,st_transform(bPoly,st_crs(soil))) #Intersections

soil_ctype <- lapply(bPolyInt,function(i){
  X <- apply(st_drop_geometry(bPoly[i,8:15]),2,function(x) paste(unique(na.omit(x)),collapse='; '))
  ifelse(nchar(X)==0,"NA",X)
}) %>% do.call('rbind',.) %>% data.frame() %>% bind_cols(soil,.)

#String for each year/soil combo
bPolyInt <- st_intersects(soilProv,st_transform(bPoly,st_crs(soil))) #Intersections

soilProv_ctype <- lapply(bPolyInt,function(i){
  X <- apply(st_drop_geometry(bPoly[i,8:15]),2,function(x) paste(unique(na.omit(x)),collapse='; '))
  ifelse(nchar(X)==0,"NA",X)
}) %>% do.call('rbind',.) %>% data.frame() %>% bind_cols(soilProv,.)

st_write(soil_ctype,"C:/Users/samuel.robinson/Desktop/CropTypes for Sarah P/pdq_cropType.shp")
st_write(soilProv_ctype,"C:/Users/samuel.robinson/Desktop/CropTypes for Sarah P/soilProv_cropType.shp")

#Crop types within soil types for each province
lapply(bPolyInt,function(i) data.frame(table(bPoly$y2022[i]))) %>% 
  bind_rows(.id='SoilProv') %>% 
  mutate(SoilProv=factor(SoilProv,labels=with(soilProv_ctype,paste(SoilZone,Prov,sep='_')))) %>% 
  separate(SoilProv,c('Soil','Prov'),sep='_') %>% 
  rename(CropType=Var1,Prop=Freq) %>% 
  ggplot()+geom_col(aes(x=Soil,y=Prop,fill=CropType),position = position_stack())+
  facet_wrap(~Prov)+
  labs(y='Count',x='Soil Type')
