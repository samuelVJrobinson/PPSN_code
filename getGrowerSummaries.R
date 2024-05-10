#Simple script to get summary information on number of files per grower

library(tidyverse)

setwd('D://geoData//SMSexport/')
getwd()

csvFiles <- list.files(path = './/202201 CLINTON MONCHUK/',pattern = '.csv')
# table(gsub('_\\d{4}.csv$','',csvFiles))

data.frame(fPath=csvFiles) %>% 
  mutate(fieldName=gsub('_\\d{4}.csv$','',fPath)) %>% 
  mutate(year=gsub('(^.+_|\\.csv$)','',fPath)) %>% 
  group_by(fieldName) %>% summarize(count=n())

growerNames <- dir('.',pattern = '^\\d{6}.*[^_ERROR\\.txt]$',full.names = TRUE)

#Get counts of associated files from each grower
resultList <- lapply(growerNames,function(x){
  data.frame(fPath=list.files(path = x,pattern = '.csv')) %>% 
    mutate(fieldName=gsub('_\\d{4}.csv$','',fPath)) %>% 
    mutate(year=gsub('(^.+_|\\.csv$)','',fPath)) %>% 
    group_by(fieldName) %>% summarize(count=n())
}) %>% setNames(nm = basename(growerNames))

length(resultList) #Number of growers
sapply(resultList,nrow) #Fields per grower
sum(sapply(resultList,nrow)) #Total number of fields

exportDat <- data.frame(totalFields=sapply(resultList,nrow),
           maxYears=sapply(resultList,function(x) max(x$count))) %>% 
  mutate(fieldYears=totalFields*maxYears,actualFieldYears=sapply(resultList,function(x) sum(x$count))) %>% 
  mutate(propFY=actualFieldYears/fieldYears)

write.csv(exportDat,file = './exportDat.csv')

