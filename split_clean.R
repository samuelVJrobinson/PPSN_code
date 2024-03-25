# source("/media/rsamuelStorage1/geoData/SMSexport/helperFunctions.R")
source("D:\\geoData\\SMSexport\\PPSN_code\\helperFunctions.R") #Load helper functions

# Basic operations - unzip and process csv ----------------------------------------

# Unzips all files within a directory
unzipAll("D:\\geoData\\YieldStorageRaw\\Fall 2023 Data\\202247 Yield Data Files\\202247 Yield Data Files 2019-2023\\",rmOld = TRUE)

dirPath <- "D:\\geoData\\SMSexport\\202213 LAJORD COLONY" #Directory path

#Renames default SMS filenames
rename_csv(dirPath) 

# #Splits large csvs into field-specific ones AND renames columns
# for(l in dir(dirPath,pattern = '*.csv',full.names = TRUE)){
#   split_csv(l,TRUE)
# }

#Merge fields that should be together - not really used
# merge_csv('./SE.Can Sec 1_2022.csv','./Pivot W of 844_2022.csv','./SE.Can Sec 1B_2022.csv')


# Clean and process yield data ---------------------------------

# #Single file
# fn <- 'Aaron east mini_2017'
# clean_csv('./Aaron east mini_2017.csv','./clean/Aaron east mini_2017.csv','./clean/Aaron east mini_2017.png',
#           useVega = TRUE,keepFiltCols = TRUE,ncore = 12)

#Process multiple directories

# dirPaths <- c("D:\\geoData\\SMSexport\\202204 SHANE STROEDER",
#               "D:\\geoData\\SMSexport\\202205 GJ C AND C")
dirPaths <- "D:\\geoData\\SMSexport\\202213 LAJORD COLONY"
# dirPaths <- dir('D:\\geoData\\SMSexport\\','.*2022[0-9]{2}\\s',include.dirs = TRUE,full.names = TRUE)

for(d in dirPaths){ #For each directory
  setwd(d) #Changes working directory to match
  try({rename_csv('.')},silent = TRUE)
  uprLimPath <- "D:\\geoData\\SMSexport\\PPSN_code\\data\\cropBulkDensity.csv" #Upper limit
  bPolyPath <- paste0("D:\\geoData\\SMSexport\\Field Boundaries\\",basename(d),"_poly.shp") #Field boundary files
  for(l in dir('.','*\\_(19|20)\\d{2}.csv$',full.names = TRUE)){ #Selects csv files with 1900-2000 years
    if(!dir.exists('./clean')) dir.create('./clean') #Creates "clean" directory if it doesn't already exist
    p1 <- gsub('./','./clean/',l,fixed = TRUE) #csv writing path
    p2 <- gsub('csv$','png',p1) #png writing path
    errFile <- gsub('.csv','_ERROR.txt',basename(l))
    if(file.exists(p1)){
      print(paste0(gsub('./','',l),' already processed. Skipping.'))
    } else if(file.exists(errFile)) {
      print(paste0(gsub('./','',l),' already processed (with error). Skipping.'))
    } else {
      split_csv(l,FALSE) #Split files
      try({
        # clean_csv(l,p1,p2,useVega = TRUE,keepFiltCols = TRUE,ncore = 12) #Old version
        clean_csv(path = l,newpath = p1, figpath = p2, upperYieldPath = uprLimPath,
                   boundaryPath = bPolyPath, useVega = TRUE, ncore = 12,fastRead = TRUE,
                   speedR2thresh=0.95,upperSpeed=15)
      },
      outFile =errFile)
      gc(FALSE)
    }
  }  
}
debugonce(clean_csv)

# #Files in directory with ERROR files
# setwd(dirPath)
# fp <- gsub('_ERROR.txt','.csv',list.files('.',pattern='*ERROR*',full.names = TRUE))
# for(l in fp){
#   p1 <- gsub('./','./clean/',l,fixed = TRUE) #csv writing path
#   p2 <- gsub('csv$','png',p1) #png writing path
#   split_csv(l,FALSE) #Split files
#   if(file.exists(p1)){
#     print(paste0(gsub('./','',l),' already processed. Skipping.'))
#   } else {
#     try({
#       clean_csv(l,p1,p2,useVega = TRUE,keepFiltCols = TRUE,ncore = 12)
#     },
#     outFile =gsub('.csv','_ERROR.txt',basename(l)))
#     gc(FALSE)
#   }
# }

# Save cleaned yield data into zipped directories (for export) -------------------

setwd('D:\\geoData\\SMSexport')

# #Subset of directories
# dirNums <- c(21,26,27,33,39,58) #Directories for export
# dirNums <- paste0('^(',paste0('2022',dirNums,collapse='|'),')')
# dirs <- list.files('.',dirNums,include.dirs = TRUE,full.names = TRUE)

#All directories
dirs <- list.files('.','^202\\d{3}',include.dirs = TRUE,full.names = TRUE)

dirs <- paste0(dirs,'/clean')
saveDir <- './Data for Export' #Directory to save zip files to

for(i in 1:length(dirs)){
  zipFileSave <- file.path(saveDir,paste0(strsplit(dirs[i],'/')[[1]][2],'.zip'))
  if(file.exists(zipFileSave)){
    print(paste0('Zipped directory ',basename(zipFileSave),' already exists'))
  } else {
    print(paste0('Zipping ',basename(zipFileSave)))
    filesToSave <- list.files(dirs[i],full.names = TRUE)
    filesToSave <- filesToSave[!grepl('ERROR',filesToSave)]
    if(length(filesToSave)==0){
      print(paste0('No files found in ',basename(zipFileSave)))
    } else {
      zip(zipFileSave,filesToSave)  
    }
  }
}


# Rasterize yield data ----------------------------------------

#Check all folders and rasterize where needed

if(Sys.info()['nodename'] == 'BIO-RG-PG1'){ #Galpern machine
  setwd("D:/geoData/SMSexport")
}

yDirs <- list.dirs('.',full.names = TRUE) #Yield directories
yDirs <- yDirs[grepl('clean$',yDirs)] #Anything with "clean" as ending characters
rDirs <- gsub('clean$','rasters',yDirs) #Raster directory

# debugonce(rasterizeYield)
# rasterizeYield(yieldDir = yDirs[3],
#                boundDir = "D:\\geoData\\SMSexport\\Field Boundaries",
#                # fieldFiltChar = "2022.csv$",
#                rastDir = rDirs[i], overwrite = FALSE)  

for(i in 1:length(yDirs)){
  try({
    rasterizeYield(yieldDir = yDirs[i],
                   boundDir = "D:\\geoData\\SMSexport\\Field Boundaries",
                   rastDir = rDirs[i])  
  },
  outFile =gsub('/rasters','_ERROR.txt',rDirs[1]))
  gc()
}

# #Same thing, but in parallel - need to do
# 
# library(parallel)
# cl <- makeCluster(6)
# 
# doRast <- function(inputs){
#   try({
#     rasterizeYield(yieldDir = inputs[1],
#                    boundDir = "D:\\geoData\\SMSexport\\Field Boundaries",
#                    rastDir = inputs[2])  
#   },
#   outFile =gsub('/rasters','_ERROR.txt',rDirs[2]))
#   gc()
# }



# Rasterize selected folders

yDirs <- c("D:\\geoData\\SMSexport\\202216 ZENNETH FAYE\\clean")
           # "D:\\geoData\\SMSexport\\202230 HANSBREK FARMS LTD\\clean",
           # "D:\\geoData\\SMSexport\\202244 MARC AND CHERYL NOREEN\\clean",
           # "D:\\geoData\\SMSexport\\202237 STUART LAWRENCE\\clean")
rDirs <- gsub('clean','rasters',yDirs)

for(i in 1:length(yDirs)){
  if(!dir.exists(rDirs[i])) dir.create(rDirs[i])
  try({
    rasterizeYield(yieldDir = yDirs[i],
                   boundDir = "D:\\geoData\\SMSexport\\Field Boundaries",
                   rastDir = rDirs[i], overwrite = FALSE)  
  },
  outFile =gsub('/rasters','_ERROR.txt',rDirs[1]))
  gc()
}

 
##Single dir
debugonce(rasterizeYield)
rasterizeYield(yieldDir = "D:\\geoData\\SMSexport\\202209 DOUBLE E AND STREAM STICK\\clean",
               boundDir = "D:\\geoData\\SMSexport\\Field Boundaries",
               # fieldFiltChar = "2022.csv$",
               rastDir = "D:\\geoData\\SMSexport\\202209 DOUBLE E AND STREAM STICK\\rasters", overwrite = FALSE)

# Other things ------------------------------------------------------------

#Which field has large proportion qFilt? (Extreme values)

library(tidyverse)
library(data.table)
searchDirs <- dir('..',include.dirs = TRUE,full.names = TRUE) #Get directories
searchDirs <- paste0(searchDirs[grepl('\\d{6}',searchDirs)],'/clean')
propFilter <- lapply(searchDirs,function(d){ #Takes quite a while to run this
  searchFiles <- dir(d,pattern = '\\.csv$',full.names = TRUE)
  lapply(searchFiles,function(f){
    apply(fread(f,header = TRUE,
                select=c('tooLarge','vegaFilt','qFilt','bFilt','speedFilt','dSpeedFilt','posFilt')),2,function(x) sum(!x)/length(x))
  }) %>% setNames(gsub('(\\.csv$|^.+\\/clean\\/)','',searchFiles)) %>% 
    bind_rows(.id = 'FieldName')
})
save('propFilter',file='./data/propFilter.Rdata')


# #Create boundary files for non-SMS directories
# 
# fName <- c("202230 HANSBREK FARMS LTD",
#            "202244 MARC AND CHERYL NOREEN",
#            "202261 DAVID FORSEILLE")
# 
# for(f in fName){
#   csv2Boundary(dirPath = paste0("D:\\geoData\\SMSexport\\",f,"\\clean"),
#                fileName = paste0("D:\\geoData\\SMSexport\\Field Boundaries\\",f,"_poly.shp"))  
# }
# 
# for(f in fName){
#   csv2Boundary(dirPath = paste0("D:\\geoData\\SMSexport\\",f,"\\clean"),
#                fileName = paste0("D:\\geoData\\SMSexport\\Field Boundaries\\",f,"_poly.shp"))  
# }




# debugonce(split_csv)
# split_csv("./Above Bees S.csv",FALSE)

# #Figure out what an appropriate cutoff for swath width is
# for(l in dir('/media/rsamuel/Storage1/geoData/SMSexport/Lakeland College','*20(19|20).csv',full.names = TRUE)){
#   dat <- read.csv(l)
#   library(ggplot2)
#   
#   # p <- ggplot(dat,aes(x=SwathWidth_m,y=Yield_tha))+geom_point()+ # Swath width
#   #   geom_vline(xintercept = max(dat$SwathWidth_m)*c(0.1,0.2,0.3,0.5),col='red',linetype='dashed')
#   # ggsave(gsub('\\.csv$','swathPlot.png',l),p)
#   
#   p <- ggplot(dat,aes(x=Distance_m,y=Yield_tha))+geom_point()+ # Swath length
#     geom_vline(xintercept = max(dat$Distance_m)*c(0.1,0.2,0.3,0.5),col='red',linetype='dashed')
#   ggsave(gsub('\\.csv$','distPlot.png',l),p)
#   
#   p <- ggplot(dat,aes(x=Distance_m*SwathWidth_m,y=Yield_tha))+geom_point()+ # Swath area
#     geom_vline(xintercept = max(dat$Distance_m*dat$SwathWidth_m)*c(0.1,0.2,0.3,0.5),col='red',linetype='dashed')
#   ggsave(gsub('\\.csv$','areaPlot.png',l),p)
# }
# 
# #Which 
# for(l in dir('.','*20(19|20).csv',full.names = TRUE)){
#   print(paste0(l,' : ',length(unique(read.csv(l)$CombineID))))
# }
# 
# charMatchFun <- function(ll,a){ #Function to return number of matching characters between strings. Returns 0 if length of strings not equal
#   if(length(ll)!=length(a)){
#     return(0)
#   } else {
#     yearmatch <- ll[(length(ll)-4):length(ll)]==a[(length(ll)-4):length(ll)]
#     fieldmatch <- ll[1:(length(ll)-5)]==a[1:(length(ll)-5)]
#     if(any(!yearmatch)) return(0) else return(sum(fieldmatch))
#   }
# }
# 
# #Renames files in clean Dir
# for(l in dir('.','*\\_20\\d{2}.csv')){
#   if(any(grepl(l,basename(cleanDir),fixed = TRUE))){
#     print(paste0(l, ' has same name in clean directory'))
#     cleanDir <- cleanDir[!grepl(l,basename(cleanDir))]
#   } else {
#     print(paste0('Fixing ',l))
#     strMatches <- sapply(strsplit(gsub('.csv$','',basename(cleanDir)),''),charMatchFun,
#                          a=strsplit(gsub('.csv','',basename(l)),'')[[1]])
#     if(sum(strMatches)==0){
#       print(paste0('No matching file found for ',l))
#     } else {
#       oldpath <- cleanDir[which.max(strMatches)]
#       newpath <- gsub(basename(cleanDir[which.max(strMatches)]),
#                       basename(l),cleanDir[which.max(strMatches)],fixed = TRUE)
#       
#       # Check characters being fixed
#       charMatches <- strsplit(gsub('_20[0-9]{2}.csv','',basename(oldpath)),'')[[1]]==
#         strsplit(gsub('_20[0-9]{2}.csv','',basename(newpath)),'')[[1]]
#       charMatches <- strsplit(gsub('_20[0-9]{2}.csv','',basename(newpath)),'')[[1]][!charMatches]
#       if(any(grepl('\\w',charMatches))) stop('Alphanumeric character found. Paths: ',basename(oldpath),' and ',basename(newpath))
#       
#       #Renaming
#       file.rename(oldpath,newpath) #csv file
#       file.rename(gsub('csv$','png',oldpath),gsub('csv$','png',newpath)) #png file
#       #Read in csv
#       # print('Reading in csv file')
#       dat <- read.csv(newpath)
#       lFieldName <- gsub('_20[0-9]{2}.csv$','',l)
#       
#       if(unique(dat$Field)!=lFieldName){
#         # print('Fixing field name in file')
#         dat$Field <- lFieldName
#       } else {
#         stop('Field name the same: ',lFieldName)
#       }
#       write.csv(dat,newpath,row.names = FALSE)
#       rm(dat); gc()
#     }
#   }
# }