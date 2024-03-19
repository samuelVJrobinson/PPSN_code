# Basic estimates of profitability using rasterized yield data
#Top 10 crop types from 2022:
# Canola, Spring Wheat, Peas, Barley, Lentils, Soybeans, Oats, Flaxseed, Corn, Chickpeas
library(sf)
library(tidyverse)
library(scales)
library(ggpubr)
library(rmarkdown)

# setwd("D:/geoData/SMSexport")
# setwd('./newsletter2023/')

ha2ac <- 2.47105 #Acres per hectare

#Get data from rasters
if(Sys.info()['nodename'] == 'BIO-RG-PG1'){ #Galpern machine
  setwd('D:/geoData/SMSexport/PPSN_code/newsletter2023/')
  rDirs <- list.dirs('D:/geoData/SMSexport',full.names = TRUE) #Yield directory
  rDirs <- rDirs[grepl('rasters$',rDirs)] #Raster directory
} else if(Sys.info()['nodename'] == 'MULTIVAC'){ #Multivac:
  setwd("C:/Users/Samuel/Documents/Projects/UofC postdoc/PPSN_code/newsletter2023/")
  rDirs <- list.dirs("C:\\Users\\Samuel\\Documents\\Shapefiles\\Yield Rasters")
  rDirs <- rDirs[grepl('/rasters$',rDirs)] #Raster directory  
}
source("../helperFunctions.R")

# canProf <- vector('list',length(rDirs))
# names(canProf) <- basename(gsub('/rasters','',rDirs))
# {pb <- txtProgressBar(style=3)
# for(i in 1:length(rDirs)){
#   if(is.null(canProf[[i]])){
#     if(Sys.info()['nodename'] == 'BIO-RG-PG1'){ #Galpern machine
#       # debugonce(profEstimates)
#       canProf[[i]] <- profEstimates(rDirs[i],excludeMissing = FALSE,includeYield = TRUE,useAcres = TRUE)
#     } else if(Sys.info()['nodename'] == 'MULTIVAC'){ #Multivac:
#       canProf[[i]] <- profEstimates(rDirs[i],
#                                     soilMapPath = "C:\\Users\\Samuel\\Dropbox\\PPSN Cleaned Yield\\Soil Layers\\PRV_SZ_PDQ_v6\\PRV_SZ_PDQ_v6.shp",
#                                     cropPrices = "./data/cropPricesCSV.csv",bulkDens = "./data/cropBulkDensity.csv",
#                                     boundDir = "C:\\Users\\Samuel\\Dropbox\\PPSN Cleaned Yield\\Field Boundaries",
#                                     excludeMissing = FALSE,includeYield = TRUE,useAcres = TRUE)
#     }
#   }
#   setTxtProgressBar(pb,i/length(rDirs))
# }
# close(pb)}
# save('canProf',file='./canProf.Rdata')
load('./canProf.Rdata')
  
# which(sapply(canProf,length)==1)

#Get summaries of each grower
sumList <- lapply(canProf[sapply(canProf,length)>1],function(x){
  if(class(x)=='logical') return(NA)
  x %>% group_by(FieldYear) %>% 
    summarize(CropType=first(CropType),Profit=mean(Profit_ac),Yield=mean(Yield_buAc)) %>% 
    ungroup() %>% separate(FieldYear,c('Field','Year'),sep='_')
})


# Report for growers ------------------------------------------------------

#Load list of growers
growerDat <- read.csv('../data/growerCSV.csv',strip.white = TRUE) %>% #Multivac
  rename_with(~gsub('.','',.x,fixed = TRUE)) %>% 
  select(GrowerID:BusinessName) %>% 
  #Amalgamate first and last name if businessname is blank
  mutate(BusinessName=ifelse(is.na(BusinessName)|BusinessName=='',paste(FirstName,LastName),BusinessName)) %>% 
  mutate(BusinessName=gsub('\\.$','',BusinessName)) #Remove trailing periods

#Get upper limits for different crops - using 99.9th percentile as overall upper bound
uprYieldLim <- canProf[sapply(canProf,length)==4] %>% 
  bind_rows(.id = 'grower') %>% 
  filter(!is.na(Yield_buAc)) %>% 
  group_by(CropType) %>% 
  summarize(upr=ceiling(quantile(Yield_buAc,0.999)))
            # upr1=quantile(Yield_buAc,0.99),
            # upr2=quantile(Yield_buAc,0.98))

# #Problem reports

#202217: chickpeas have poor profits, but grower says otherwise
# - Field =  M&M Wade: yield data says "Peas", Satellite data says "Chickpeas (66%) or Lentils (27%) ", Metadata says "Canola, Peas, Spring Wheat"

#202239: weird low yield spike for lentils - removed weird split fields
# canProf[[which(grepl('202239',names(canProf)))]] %>% 
#   separate_wider_delim('FieldYear',delim = '_',names=c('Field','Year')) %>% 
#   filter(Year=='2022',CropType=='Lentils') %>% 
#   filter(Yield_buAc<100) %>% 
#   # ggplot(aes(x=Field,y=Profit_ac))+geom_boxplot()
#   ggplot(aes(x=Yield_buAc))+geom_histogram()+
#   facet_wrap(~Field)


for(i in 1:length(canProf)){
  if(class(canProf[[i]])=='logical') next
  
  gID <- sapply(str_split(names(canProf)[i],' '),first) %>% gsub('-.*','',.)
  gName <- growerDat$FirstName[which(growerDat$GrowerID==gID)]
  fName <- growerDat$BusinessName[which(growerDat$GrowerID==gID)]
  
  #Skip existing reports (comment out to replace)
  if(file.exists(paste0('./reports/',gID,'-report.pdf'))){
    print(paste0('File ',paste0('./reports/',gID,'-report.pdf'),' already exists'))
    next
  }
  
  #Data for grower i
  temp <- canProf[[i]] %>% separate(FieldYear,c('Field','Year'),sep='_') %>%
    filter(!is.na(Yield_buAc)) %>% #Only non-NA yield
    arrange(desc(Year)) %>% 
    filter(Year %in% unique(Year)[1:pmin(5,length(unique(Year)))]) %>% #Chooses 5 most recent years
    left_join(uprYieldLim,by='CropType') %>%
    filter(Yield_buAc<upr) %>% select(-upr) #BS filter
  
  #Table of summary yields 
  
  #Overall top crops
  topCrops <- temp %>% group_by(Field,Year) %>%  #All crops harvested
    summarize(CropType=first(CropType),.groups = 'keep') %>% 
    ungroup() %>% count(CropType) %>% arrange(desc(n))
  
  #Only crops from 2022
  recentCrops <- temp %>% 
    filter(Year==2022,!is.na(Profit_ac)) %>% #Only 2022 crops with non-NA profit 
    group_by(Field,Year) %>% 
    summarize(CropType=first(CropType),.groups = 'keep') %>% 
    ungroup() %>% count(CropType) %>% arrange(desc(n))
  
  if(nrow(recentCrops)>=6){ #Table must have top-6 2022 crops, plus common non-2022 crops if <6 crops
    topCrops <- recentCrops %>% slice(1:6)
  } else if(nrow(recentCrops)<6){
    topCrops <- topCrops %>% filter(!CropType %in% recentCrops$CropType) %>% 
      slice(1:(6-nrow(recentCrops))) %>% 
      bind_rows(recentCrops) %>% arrange(desc(n))
  }
  
  yieldTable <- temp %>% group_by(CropType) %>% mutate(N=n()) %>% arrange(desc(N)) %>% 
    ungroup() %>% mutate(CropType=factor(CropType)) %>%
    group_by(Year,CropType,.drop = FALSE) %>% 
    summarize(medYield=round(median(Yield_buAc,na.rm=TRUE),0),.groups = 'keep') %>% 
    ungroup() %>% mutate(Year=factor(Year,levels=rev(unique(Year)))) %>%
    mutate(medYield=ifelse(is.na(medYield),'-',as.character(medYield))) %>%
    # droplevels() %>% 
    pivot_wider(names_from='Year',values_from='medYield',names_sort = TRUE) %>%
    arrange(CropType) %>%   
    # slice(1:pmin(6,nrow(.))) %>% 
    filter(CropType %in% topCrops$CropType) %>% 
    # select(1:pmin(6,ncol(.))) %>% 
    rename(`Crop Type`='CropType')
  
  #Figure of profit distributions
  profitDat <- temp %>% filter(!is.na(Profit_ac)) %>% 
    group_by(CropType) %>%
    filter(Profit_ac<quantile(Profit_ac,0.99)) %>% #Remove upper 1% profits
    mutate(CropType=paste0(toupper(CropType),', Yield: ',round(median(Yield_buAc)),' bu/ac\nProfit: $',round(median(Profit_ac)),'/ac, Marginal acres: ',round(100*mean(Profit_ac<0),1),'%')) 
  
  if(nrow(profitDat)==0){
    print(paste0('No profit data found for ',gID,'. Skipping.'))
    # profitFig <- ggplot()+theme_minimal()
    
    profitFig <- temp %>% filter(Year==max(Year)) %>% 
      ggplot(aes(x=Yield_buAc,y=after_stat(count),weight=0.0988421526))+
      geom_histogram()+
      facet_wrap(~CropType,scales='free')+
      scale_y_continuous(name = "Number of acres",breaks= pretty_breaks())+
      labs(x='Yield (bu/acre)',title=paste0('Yield distributions during ',max(temp$Year)))+
      theme_bw()
    
  } else {
    #Check for distributions with extreme differences in range
    checkXRange <- temp %>% filter(!is.na(Profit_ac)) %>% 
      group_by(CropType) %>% 
      summarize(lwr=min(Profit_ac),upr=max(Profit_ac),r=upr-lwr) %>% 
      ungroup() %>% mutate(percMaxR=r/max(r)) %>% pull(percMaxR)
    
    profitFig <- profitDat %>% #Create ggplot
      ggplot(aes(x=Profit_ac,y=after_stat(count),weight=0.0988421526)) 
    #note: weight = Acres per 20x20m cell
    
    if(any(checkXRange<0.1)){ #If range outliers exist
      profitFig <- profitFig + 
        geom_histogram(bins=20)+
        facet_wrap(~CropType,scales='free')
    } else {
      profitFig <- profitFig + 
        geom_histogram(binwidth=50)+
        facet_wrap(~CropType,scales='free_y')
    }
    
    profitFig <- profitFig + #Add extra elements
      geom_vline(xintercept = 0,col='red')+
      scale_y_continuous(name = "Number of acres",breaks= pretty_breaks())+
      labs(x='Profit ($/acre)',title='Predicted profit distribution during 2022')+
      theme_bw()+theme(strip.text = element_text(size=8))
    
    # profitFig <- profitDat %>%
    #   ggplot(aes(x=Profit_ac,y=after_stat(count),weight=0.0988421526))+
    #   geom_histogram(binwidth=50)+
    #   facet_wrap(~CropType,scales='free_y')+
    #   geom_vline(xintercept = 0,col='red')+
    #   scale_y_continuous(name = "Number of acres",breaks= pretty_breaks())+
    #   labs(x='Profit ($/acre)',title='Predicted profit distribution during 2022')+theme_bw()
    
    
    # #Older version
    # profitFig <- temp %>% filter(!is.na(Profit_ac)) %>% group_by(CropType) %>%
    #   mutate(CropType=paste0(toupper(CropType),', Yield: ',round(median(Yield_buAc)),' bu/ac\nProfit: $',round(median(Profit_ac)),'/ac, Marginal acres: ',round(100*mean(Profit_ac<0),1),'%')) %>% 
    #   ggplot(aes(x=Profit_ac))+
    #   geom_density(fill='grey90')+
    #   facet_wrap(~CropType)+
    #   geom_vline(xintercept = 0,col='red')+
    #   scale_y_continuous(labels = percent, name = "Percent of acres") +
    #   labs(x='Profit ($/acre)',title='Predicted profit distribution during 2022')+
    #   theme_bw()+
    #   coord_cartesian(xlim=c(NA,quantile(temp$Profit_ac,0.99,na.rm=TRUE))) #Show everything below 99th percentile
      
  }
  
  #Other values to replace
  numFieldYears <- length(unique(canProf[[i]]$FieldYear))
  numCropTypes <- length(unique(canProf[[i]]$CropType))
  numYears <- canProf[[i]] %>% separate(FieldYear,c('Field','Year'),sep='_') %>% 
    select(Year) %>% distinct() %>% nrow()
  percMargAc <- canProf[[i]] %>% filter(!is.na(Profit_ac)) %>% 
    mutate(NegProf=Profit_ac<0) %>% 
    pull(NegProf) %>% mean(.)*100
  percMargAc <- ifelse(percMargAc<1,'less than 1',as.character(round(percMargAc)))
  
  #Parameters for the report
  parList <- list(GROWERID=gID,
                  NUMFIELDYEARS=as.character(numFieldYears),
                  NUMCROPS=as.character(numCropTypes),NUMYEARS=as.character(numYears),
                  PERCMARGACRES=as.character(percMargAc),TABLEHERE=as.data.frame(yieldTable),
                  FIGUREHERE=profitFig)
  # if(any(sapply(parList,function(x) any(is.na(x))))){
  #   print('NAs found in parList for grower ',gID)
  # }
  
  render('grower-report.Rmd',params = parList,
         envir = new.env(),# output_dir = './reports',
         output_file = paste0('./reports/',gID,'-report.pdf'),
         clean = TRUE,quiet = TRUE)
  print(paste0('Finished report ',i,': grower ID ',gID))
}


# Overall yield ----------------------

canProf <- canProf[sapply(canProf,length)==4] %>% 
  bind_rows(.id = 'grower')

#Some weird outliers
canProf %>% filter(!is.na(Yield_buAc)) %>% 
  ggplot(aes(x=Yield_buAc))+
  geom_histogram()+facet_wrap(~CropType,scales='free')

canProf %>% filter(!is.na(Yield_buAc)) %>% group_by(CropType) %>% 
  summarize(upr=quantile(Yield_buAc,0.999),
            upr1=quantile(Yield_buAc,0.99),
            upr2=quantile(Yield_buAc,0.98))

top6crop <- canProf %>% count(CropType) %>% 
  arrange(desc(n)) %>% 
  slice(1:6) %>% pull(CropType)

canProf %>% filter(CropType %in% top6crop) %>% 
  ggplot(aes(x=Yield_buAc))+geom_histogram()+
  facet_wrap(~CropType,scales = 'free')

#Weirdly large outliers

canProf %>% filter(CropType=='Canola') %>% 
  ggplot(aes(x=Yield_buAc))+geom_histogram()+
  facet_wrap(~grower,scales = 'free')

canProf %>% filter(CropType=='Oats') %>% 
  ggplot(aes(x=Yield_buAc))+geom_histogram()+
  facet_wrap(~grower,scales = 'free')

canProf %>% filter(CropType=='Barley') %>% 
  ggplot(aes(x=Yield_buAc))+geom_histogram()+
  facet_wrap(~grower,scales = 'free')

canProf %>% filter(CropType %in% top6crop) %>% 
  group_by(grower,CropType) %>% summarize(upr=quantile(Yield_buAc,0.98)) %>% 
  ggplot(aes(x=upr))+geom_histogram()+facet_wrap(~CropType,scales='free')




# Overall profitability -----------------

canProf <- canProf[sapply(canProf,length)==4] %>% 
  bind_rows(.id = 'grower') %>% filter(!is.na(Profit_ac))

#Canola profitability
canProf %>% filter(CropType=='Canola',!is.na(Profit_ac)) %>% 
  filter(Yield_buAc<130) %>% 
  ggplot()+geom_histogram(aes(x=Profit_ac))+
  geom_vline(xintercept = 0,col='red')+
  labs(x='Profit ($/acre)',y='Count')+
  theme_classic()

#Cumulative distribution
p1 <- canProf%>% filter(CropType=='Canola') %>% 
  # slice_sample(n=1e6) %>% #Subsample to 1 mil
  ggplot(aes(x=Profit_ac)) +
  stat_ecdf(geom = "step")+
  geom_vline(xintercept = 0,col='red')+
  coord_cartesian(xlim=c(NA,2000))+
  labs(x='Profit ($/acre)',y='Cumulative distribution',title='Estimated canola profitability (2022)')+
  theme_bw()

p2 <- canProf %>% filter(CropType=='Canola') %>% 
  # slice_sample(n=1e6) %>% #Subsample to 1 mil
  ggplot(aes(x=Profit_ac)) +
  stat_ecdf(geom = "step")+
  geom_vline(xintercept = 0,col='red')+
  coord_cartesian(xlim=c(NA,150),ylim=c(0,0.05))+
  labs(x='Profit ($/acre)',y='Cumulative distribution',title='Estimated canola profitability (2022)')+
  scale_x_continuous(breaks = seq(-200,200,50))+
  scale_y_continuous(breaks = seq(0,1,0.005))+
  theme_bw()

p3 <- canProf %>% filter(CropType=='Canola') %>% 
  filter(Yield_buAc<130) %>% 
  slice_sample(n=1e6) %>% #Subsample to 1 mil
  ggplot(aes(x=Profit_ac,y=after_stat(count),weight=0.0988421526)) +
  geom_histogram(bins=100)+
  geom_vline(xintercept = 0,col='red')+
  coord_cartesian(xlim=c(NA,2100))+
  labs(x='Profit ($/acre)',y='Acres',title='Estimated canola profitability (2022)')+
  theme_bw()

ggsave("D:/geoData/SMSexport/PPSN_code/figures/canProf1.png",p1,height = 6, width = 8)
ggsave("D:/geoData/SMSexport/PPSN_code/figures/canProf2.png",p2,height = 6, width = 8)
ggsave("D:/geoData/SMSexport/PPSN_code/figures/canProf3.png",p3,height = 6, width = 8)

# Overall profitability
canProf %>% ggplot()+geom_histogram(aes(x=Profit_ac))+
  geom_vline(xintercept = 0,col='red')+
  labs(x='Profit ($/acre)',y='Count')+
  theme_classic()  

canProf %>% ggplot(aes(x=Profit_ac)) + 
  stat_ecdf(geom = "step")+
  geom_hline(yintercept = 0.1,col='red')+
  coord_cartesian(xlim=c(NA,1200))+
  labs(x='Profit ($/acre)',y='Cumulative distribution')+
  scale_x_continuous(breaks = seq(-200,1200,100))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme_bw()

#Broken up by crop type
top6crop <- canProf %>% count(CropType) %>% 
  arrange(desc(n)) %>% 
  slice(1:6) %>% pull(CropType)

canProf %>% filter(CropType %in% top6crop) %>% 
  group_by(CropType) %>% 
  mutate(CropType=paste0(toupper(CropType),', Yield: ',round(median(Yield_buAc)),' bu/ac\nProfit: $/ac ',round(median(Profit_ac)),', Unprofitable acres: ',round(100*mean(Profit_ac<0),1),'%')) %>% 
  ggplot()+geom_histogram(aes(x=Profit_ac),bins=30)+
  facet_wrap(~CropType,scales='free')+
  geom_vline(xintercept = 0,col='red')+
  # geom_vline(data=)
  labs(x='Profit ($/acre)',y='Count')+
  theme_classic()  

canProf %>% filter(CropType %in% top6crop) %>% 
  group_by(CropType) %>% 
  mutate(CropType=paste0(toupper(CropType),', Med $/ac: ',round(median(Profit_ac)),', % Unprofitable: ',round(100*mean(Profit_ac<0),2))) %>% 
  ggplot(aes(x=Profit_ac,col=CropType)) + 
  stat_ecdf(geom = "step")+
  geom_vline(xintercept = 0,col='red')+
  # coord_cartesian(xlim=c(NA,1200))+
  labs(x='Profit ($/acre)',y='Cumulative distribution')+
  coord_cartesian(xlim=c(NA,2000))+
  # scale_x_continuous(breaks = seq(-200,1200,100))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  scale_colour_brewer(type = 'qual')+
  theme_bw()




# Modality -----------------------

library(diptest) #Hartigans test for multimodality

# A bunch of individual fields have multi-modal profit distributions. I think this may be because of bad combine corrections. How to detect multimodality:
canProf[[1]] %>% 
  group_by(FieldYear) %>% 
  summarize(dipVal=dip.test(Yield_buAc)$p.value) %>% 
  data.frame

# Other --------------------------

# library(stars)
# pltTitle <- paste0('Median profit: $',
#                    round(median(profCalc),1),
#                    '/ac\n Unprofitable area: ',
#                    round(sum(profCalc<0)/length(profCalc)*100,1),'% of total acres')
# 
# density(profCalc) %>% 
#   plot(.,xlab='Profit ($/ac)',ylab='Density',main=pltTitle,xlim = quantile(profCalc,c(0.001,0.999)))
# abline(v=0,col='red')
# 
# for(i in 1:nrow(rastPaths)){
#   fieldRast <- read_stars(rastPaths$path[i]) #Read raster
#   # cellSize <- st_area(fieldRast[1,1,1])$area[1,1] #Get cell size (ha)
#   # if(any(!units(cellSize)$numerator=='m')){
#   #   stop('Cell size not in m^2')
#   # } else {
#   #   cellSize <- cellSize/10000 #Convert to ha
#   # }
#   profVals <- (pull(fieldRast)*rastPaths$Price_CADt[i]) - rastPaths$AvgCost_ha[i] #Profit ($/ha)
#   median(profVals,na.rm = TRUE)
# }