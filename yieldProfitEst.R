# Basic estimates of profitability using rasterized yield data
# source("D:\\geoData\\SMSexport\\helperFunctions.R")
setwd("C:/Users/Samuel/Documents/Projects/UofC postdoc/PPSN_code")
source("./helperFunctions.R")

#Top 10 crop types from 2022:
# Canola, Spring Wheat, Peas, Barley, Lentils, Soybeans, Oats, Flaxseed, Corn, Chickpeas
library(sf)
library(tidyverse)
library(scales)
library(stars)
library(ggpubr)
# library(officer)
library(rmarkdown)

# setwd("D:/geoData/SMSexport")

ha2ac <- 2.47105 #Acres per hectare

#Get data from rasters

# #Galpern:
# yDirs <- list.dirs('.',full.names = TRUE) #Yield directory
# yDirs <- yDirs[grepl('clean$',yDirs)]
# rDirs <- gsub('clean$','rasters',yDirs) #Raster directory
#Multivac:
rDirs <- list.dirs("C:\\Users\\Samuel\\Documents\\Shapefiles\\Yield Rasters")
rDirs <- rDirs[grepl('/rasters$',rDirs)] #Raster directory
canProf <- vector('list',length(rDirs))
names(canProf) <- basename(gsub('/rasters','',rDirs))

{pb <- txtProgressBar(style=3)
for(i in 1:length(rDirs)){
  if(is.null(canProf[[i]])){
    ##Galpern:
    # canProf[[i]] <- profEstimates(rDirs[i],excludeMissing = TRUE,includeYield = TRUE,useAcres = TRUE)
    
    canProf[[i]] <- profEstimates(rDirs[i],
                                  soilMapPath = "C:\\Users\\Samuel\\Dropbox\\PPSN Cleaned Yield\\Soil Layers\\PRV_SZ_PDQ_v6\\PRV_SZ_PDQ_v6.shp",
                                  cropPrices = "./data/cropPricesCSV.csv",
                                  bulkDens = "./data/cropBulkDensity.csv",
                                  boundDir = "C:\\Users\\Samuel\\Dropbox\\PPSN Cleaned Yield\\Field Boundaries",
                                  excludeMissing = FALSE,includeYield = TRUE,useAcres = TRUE)
    
  }
  setTxtProgressBar(pb,i/length(rDirs))
}
close(pb)}

# Single grower/year: Clinton Monchuk 2022 -----------------

temp <- canProf[[1]]
head(temp)

temp %>% group_by(CropType) %>% 
  mutate(CropType=paste0(toupper(CropType),', Yield: ',round(median(Yield_buAc)),' bu/ac\nProfit: $/ac ',round(median(Profit_ac)),', Unprofitable acres: ',round(100*mean(Profit_ac<0),1),'%')) %>% 
  ggplot(aes(x=Profit_ac))+
  geom_density(fill='grey90')+
  # geom_bar(stat="bin", aes(y=..density..)) +
  facet_wrap(~CropType)+
  geom_vline(xintercept = 0,col='red')+
  scale_y_continuous(labels = percent, name = "Percent of acres") +
  labs(x='Profit ($/ac)',title='Grower: Clinton Monchuk, Year: 2022')+theme_bw()#+
  # theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())

p1 <- temp %>% 
  group_by(CropType) %>% 
  mutate(CropType=paste0(toupper(CropType),'\nMedian b/Ac: ',round(median(Yield_buAc),2))) %>% 
  ggplot()+geom_histogram(aes(x=Yield_buAc))+
  facet_wrap(~CropType,scales='free',nrow=1)+
  labs(x='Yield (bu/ac)',y='Count')+theme_bw()

p2 <- temp %>% 
  group_by(CropType) %>% 
  mutate(CropType=paste0(toupper(CropType),'\nMedian $/ac: ',round(median(Profit_ac)),', % Unprofitable: ',round(100*mean(Profit_ac<0),2))) %>% 
  ggplot()+geom_histogram(aes(x=Profit_ac))+facet_wrap(~CropType,scales='free',nrow=1)+
  geom_vline(xintercept = 0,col='red')+
  labs(x='Profit ($/ac)',y='Count')+theme_bw()
ggarrange(p1,p2,ncol=1,nrow=2); rm(p1,p2)

#Distribution of crop types and avg yield per year

# debugonce(profEstimates)
# temp <- profEstimates("./202201 CLINTON MONCHUK/rasters",includeYield = TRUE)
temp <- profEstimates("C:\\Users\\Samuel\\Documents\\Shapefiles\\Yield Rasters\\202201 CLINTON MONCHUK\\rasters",
                      soilMapPath = "C:\\Users\\Samuel\\Dropbox\\PPSN Cleaned Yield\\Soil Layers\\PRV_SZ_PDQ_v6\\PRV_SZ_PDQ_v6.shp",
                      cropPrices = "./cropPricesCSV.csv",
                      bulkDens = "./cropBulkDensity.csv",
                      boundDir = "C:\\Users\\Samuel\\Dropbox\\PPSN Cleaned Yield\\Field Boundaries",
                      excludeMissing = FALSE,includeYield = TRUE,useAcres = TRUE)

# temp %>% separate(FieldYear,c('Field','Year'),sep='_') %>% 
#   # head
#   group_by(Year,CropType) %>% 
#   summarize(medYield=median(Yield_buAc)) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from='CropType',values_from='medYield')
  
# Overall profitability -----------------

canProf <- canProf[sapply(canProf,length)==4] %>% 
  bind_rows(.id = 'grower')

#Canola profitability
canProf %>% filter(CropType=='Canola') %>% 
  ggplot()+geom_histogram(aes(x=Profit_ac))+
  geom_vline(xintercept = 0,col='red')+
  labs(x='Profit ($/acre)',y='Count')+
  theme_classic()

canProf%>% filter(CropType=='Canola') %>% 
  ggplot(aes(x=Profit_ac)) +
  stat_ecdf(geom = "step")+
  # geom_hline(yintercept = 0.1,col='red')+
  # coord_cartesian(xlim=c(NA,200),ylim=c(0,0.2))+
  labs(x='Profit ($/acre)',y='Cumulative distribution')+
  # scale_x_continuous(breaks = seq(-200,1200,50))+
  # scale_y_continuous(breaks = seq(0,1,0.02))+
  theme_bw()

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
top6crop <- canProf %>% count(CropType) %>% arrange(desc(n)) %>% 
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


# Report for growers ------------------------------------------------------

temp <- canProf[[1]] #Single grower/year - Clinton Monchuck

#Table of summary yields
yieldTable <- temp %>% separate(FieldYear,c('Field','Year'),sep='_') %>% 
  group_by(CropType) %>% mutate(N=n()) %>% arrange(desc(N)) %>% 
  ungroup() %>% 
  mutate(CropType=factor(CropType,levels=unique(CropType))) %>% 
  group_by(Year,CropType,.drop = FALSE) %>% 
  summarize(medYield=round(median(Yield_buAc,na.rm=TRUE),0)) %>% 
  mutate(medYield=ifelse(is.na(medYield),'-',as.character(medYield))) %>%
  ungroup() %>% pivot_wider(names_from='CropType',values_from='medYield') %>%
  arrange(desc(Year)) %>%   
  slice(1:5) %>% #column_to_rownames('Year') %>% 
  select(1:7)

#Figure of profit distributions
profitFig <- temp %>% filter(!is.na(Profit_ac)) %>% group_by(CropType) %>%
  mutate(CropType=paste0(toupper(CropType),', Yield: ',round(median(Yield_buAc)),' bu/ac\nProfit: $',round(median(Profit_ac)),'/ac, Marginal acres: ',round(100*mean(Profit_ac<0),1),'%')) %>% 
  ggplot(aes(x=Profit_ac))+
  geom_density(fill='grey90')+
  facet_wrap(~CropType)+
  geom_vline(xintercept = 0,col='red')+
  scale_y_continuous(labels = percent, name = "Percent of acres") +
  labs(x='Profit ($/ac)',title='Grower: Clinton Monchuk, Year: 2022')+theme_bw()#+

#Other values to replace
numFieldYears <- length(unique(temp$FieldYear))
numCropTypes <- length(unique(temp$CropType))
numYears <- temp %>% separate(FieldYear,c('Field','Year'),sep='_') %>% 
  select(Year) %>% distinct() %>% nrow()
percMargAc <- temp %>% filter(!is.na(Profit_ac)) %>% 
  mutate(NegProf=Profit_ac<0) %>% 
  pull(NegProf) %>% mean(.)*100
percMargAc <- ifelse(percMargAc<1,'less than 1',as.character(round(percMargAc)))

##Using officer
# read_docx('./newsletter2023/fullTemplate.docx') %>% 
#   body_replace_all_text(old_value = 'GROWERNAME',new_value = 'Clinton') %>% 
#   body_replace_all_text(old_value = 'NUMFIELDYEARS',new_value = as.character(numFieldYears)) %>% 
#   body_replace_all_text(old_value = 'NUMCROPS',new_value = as.character(numCropTypes)) %>% 
#   body_replace_all_text(old_value = 'NUMYEARS',new_value = as.character(numYears)) %>% 
#   body_replace_all_text(old_value = 'PERCMARGACRES',new_value = as.character(percMargAc)) %>% 
#   #Add table 
#   body_add_table(value=yieldTable,style = 'Normal Table' ) %>%
#   body_add_par(value = '') %>% 
#   # Add figure
#   body_add_gg(value=profitFig,width = 7.5,height=4,scale = 1.2) %>%
#   print(target = './newsletter2023/templateTest.docx')

setwd('./newsletter2023/')
render('grower-report.Rmd',
       params = list(GROWERNAME='Clinton',
                       NUMFIELDYEARS=as.character(numFieldYears),
                       NUMCROPS=as.character(numCropTypes),
                       NUMYEARS=as.character(numYears),
                       PERCMARGACRES=as.character(percMargAc),
                       TABLEHERE=as.data.frame(yieldTable),
                       FIGUREHERE=profitFig),
       # output_dir = './reports',
       output_file = './reports/Clayton-Monchuk-test.pdf')


##All growers

#Load list of growers
growerDat <- read.csv('./data/growerCSV.csv',strip.white = TRUE) %>% 
  rename_with(~gsub('.','',.x,fixed = TRUE)) %>% 
  select(GrowerID:BusinessName) %>% 
  #Amalgamate first and last name if businessname is blank
  mutate(BusinessName=ifelse(is.na(BusinessName)|BusinessName=='',paste(FirstName,LastName),BusinessName)) %>% 
  mutate(BusinessName=gsub('\\.$','',BusinessName)) #Remove trailing periods

for(i in 1:3){
  gID <- sapply(str_split(names(canProf)[i],' '),first) %>% gsub('-.*','',.)
  gName <- growerDat$FirstName[which(growerDat$GrowerID==gID)]
  fName <- growerDat$BusinessName[which(growerDat$GrowerID==gID)]
  
  temp <- canProf[[i]] #Data for grower
  
  #Table of summary yields
  yieldTable <- temp %>% separate(FieldYear,c('Field','Year'),sep='_') %>% 
    group_by(CropType) %>% mutate(N=n()) %>% arrange(desc(N)) %>% 
    ungroup() %>% 
    mutate(CropType=factor(CropType,levels=unique(CropType))) %>% 
    group_by(Year,CropType,.drop = FALSE) %>% 
    summarize(medYield=round(median(Yield_buAc,na.rm=TRUE),0),.groups = 'keep') %>% 
    mutate(medYield=ifelse(is.na(medYield),'-',as.character(medYield))) %>%
    ungroup() %>% pivot_wider(names_from='CropType',values_from='medYield') %>%
    arrange(desc(Year)) %>%   
    slice(1:pmin(5,nrow(.))) %>% #column_to_rownames('Year') %>% 
    select(1:pmin(7,ncol(.)))
  
  #Figure of profit distributions
  profitFig <- temp %>% filter(!is.na(Profit_ac)) %>% 
    filter(Profit_ac<quantile(Profit_ac,0.99)) %>% #Removes top 1% yield outliers 
    group_by(CropType) %>%
    mutate(CropType=paste0(toupper(CropType),', Yield: ',round(median(Yield_buAc)),' bu/ac\nProfit: $',round(median(Profit_ac)),'/ac, Marginal acres: ',round(100*mean(Profit_ac<0),1),'%')) %>% 
    ggplot(aes(x=Profit_ac))+
    geom_density(fill='grey90')+
    facet_wrap(~CropType)+
    geom_vline(xintercept = 0,col='red')+
    scale_y_continuous(labels = percent, name = "Percent of acres") +
    labs(x='Profit ($/ac)',title=paste0('Farm: ',fName,', Year: 2022'))+theme_bw()#+
  
  #Other values to replace
  numFieldYears <- length(unique(temp$FieldYear))
  numCropTypes <- length(unique(temp$CropType))
  numYears <- temp %>% separate(FieldYear,c('Field','Year'),sep='_') %>% 
    select(Year) %>% distinct() %>% nrow()
  percMargAc <- temp %>% filter(!is.na(Profit_ac)) %>% 
    mutate(NegProf=Profit_ac<0) %>% 
    pull(NegProf) %>% mean(.)*100
  percMargAc <- ifelse(percMargAc<1,'less than 1',as.character(round(percMargAc)))
  
  read_docx('./newsletter2023/fullTemplate.docx') %>% 
    body_replace_all_text(old_value = 'GROWERNAME',new_value = gName) %>% 
    body_replace_all_text(old_value = 'NUMFIELDYEARS',new_value = as.character(numFieldYears)) %>% 
    body_replace_all_text(old_value = 'NUMCROPS',new_value = as.character(numCropTypes)) %>% 
    body_replace_all_text(old_value = 'NUMYEARS',new_value = as.character(numYears)) %>% 
    body_replace_all_text(old_value = 'PERCMARGACRES',new_value = as.character(percMargAc)) %>% 
    #Add table 
    body_add_table(value=yieldTable,
                   # layout=table_layout('fixed'),
                   # width=table_width(width=0.5,unit='pct'),
                   style = 'Table Grid',
                   # stylenames = prop_table(colwidths = table_colwidths(widths=0.5)),
                   align_table = 'right',
                   alignment = c('r',rep('l',ncol(yieldTable)-1)),first_row = FALSE) %>%
    body_add_par(value = '') %>% 
    # table_width(width = 1, unit = "pct") %>% 
    # Add figure
    body_add_gg(value=profitFig,width = 7.5,height=4,scale = 1) %>%
    print(target = paste0('./newsletter2023/reports/',gID,' ',fName,' 2023 report.docx'))
}


# Other --------------------------

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