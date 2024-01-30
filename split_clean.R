# source("/media/rsamuelStorage1/geoData/SMSexport/helperFunctions.R")
source("D:\\geoData\\SMSexport\\PPSN_code\\helperFunctions.R")

# Basic operations - unzip and process csv ----------------------------------------

unzipAll("D:\\geoData\\YieldStorageRaw\\Fall 2023 Data\\202218 Yield Data Files 2023\\Nunebor 2023 Yield Files",rmOld = TRUE)

dirPath <- "D:\\geoData\\SMSexport\\202231 HILLSBORO FARMS"

rename_csv(dirPath)
# debugonce(rename_csv)

#Split large csvs into field-specific ones AND rename columns
for(l in dir(dirPath,pattern = '*.csv',full.names = TRUE)){
  split_csv(l,TRUE)
}
debugonce(split_csv)
split_csv('./S1.2 20-28-3 W2M_2022.csv',TRUE)

#Merge fields that should be together
# merge_csv('./SE.Can Sec 1_2022.csv','./Pivot W of 844_2022.csv','./SE.Can Sec 1B_2022.csv')

#Clean and process 

# debugonce(clean_csv)
# debugonce(vegaFilter)

#Single file
fn <- 'Aaron east mini_2017'
clean_csv('./Aaron east mini_2017.csv','./clean/Aaron east mini_2017.csv','./clean/Aaron east mini_2017.png',
          useVega = TRUE,keepFiltCols = TRUE,ncore = 12)

#Process single/multiple directories

# dirPaths <- c("D:\\geoData\\SMSexport\\202216 ZENNETH FAYE",
#               "D:\\geoData\\SMSexport\\202230 HANSBREK FARMS LTD",
#               "D:\\geoData\\SMSexport\\202244 MARC AND CHERYL NOREEN",
#               "D:\\geoData\\SMSexport\\202237 STUART LAWRENCE")
dirPaths <- "D:\\geoData\\SMSexport\\202244 MARC AND CHERYL NOREEN"

for(d in dirPaths){
  setwd(d)
  for(l in dir('.','*\\_(19|20)\\d{2}.csv$',full.names = TRUE)){
    if(!dir.exists('./clean')) dir.create('./clean') #Creates "clean" directory if it doesn't already exist
    p1 <- gsub('./','./clean/',l,fixed = TRUE) #csv writing path
    p2 <- gsub('csv$','png',p1) #png writing path
    split_csv(l,FALSE) #Split files
    if(file.exists(p1)){
      print(paste0(gsub('./','',l),' already processed. Skipping.'))
    } else {
      try({
        clean_csv(l,p1,p2,useVega = TRUE,keepFiltCols = TRUE,ncore = 12)
      },
      outFile =gsub('.csv','_ERROR.txt',basename(l)))
      gc(FALSE)
    }
  }  
}
debugonce(clean_csv)

# #All directories
# dirs <- dir('D:\\geoData\\SMSexport\\','.*2022[0-9]{2}\\s',include.dirs = TRUE,full.names = TRUE)
# for(d in dirs){
#   setwd(d)
#   try({rename_csv('.')},silent = TRUE)
#   for(l in dir('.','*\\_20\\d{2}.csv',full.names = TRUE)){
#     p1 <- gsub('./','./clean/',l,fixed = TRUE) #csv writing path
#     p2 <- gsub('csv$','png',p1) #png writing path
#     if(file.exists(p1)){
#       print(paste0(gsub('./','',l),' already processed. Skipping.'))
#     } else {
#       # clean_csv(l,p1,p2,useVega = TRUE,keepFiltCols = TRUE,ncore = 12)
#       try({
#         # rename_csv(".") #Fix default file names names from SMS
#         split_csv(l,FALSE) #Split files
#         clean_csv(l,p1,p2,useVega = TRUE,keepFiltCols = TRUE,ncore = 12)
#         },outFile =gsub('.csv','_ERROR.txt',basename(l)))
#       gc(FALSE)
#     } 
#   }  
# }

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

# Next step - rasterize yield data ----------------------------------------

#Check all folders and rasterize where needed

if(Sys.info()['nodename'] == 'BIO-RG-PG1'){ #Galpern machine
  setwd("D:/geoData/SMSexport")
}

yDirs <- list.dirs('.',full.names = TRUE) #Yield directory
yDirs <- yDirs[grepl('clean$',yDirs)]
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
                   # fieldFiltChar = "2022.csv$",
                   rastDir = rDirs[i], overwrite = FALSE)  
  },
  outFile =gsub('/rasters','_ERROR.txt',rDirs[1]))
  gc()
}

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
                   # fieldFiltChar = "2022.csv$",
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