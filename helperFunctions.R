#Unzip all files within a given directory (including subdirectories)
unzipAll <- function(d,rmOld=FALSE){
  # d <- "D:\\geoData\\YieldStorageRaw\\202212 202 AgVentures"; rmOld=TRUE
  z <- list.files(d,pattern='\\.zip$',recursive = TRUE,full.names = TRUE)
  if(length(z)==0) stop(paste0('No files found in ',d))
  z <- normalizePath(z)
  print(paste0('Unzipping ',length(z),' directories within ',d))
  pb <- txtProgressBar(style=3)
  for(i in 1:length(z)){
    tryCatch(expr = {
      unzip(z[i],exdir=gsub('\\.zip$','',z[i]))
      if(rmOld) invisible(file.remove(z[i]))
    }, error = function(e){
      message('Error occurred: ')
      message(e)
    })
    setTxtProgressBar(pb,i/length(z))
  }
  close(pb)
}

#Break continuous vector into categories, using cut. b = vector of breaks to use
makeBreaks <- function(x,b){
  # x <- na.omit(x)
  # if(length(x)==0) stop('No non-NA values in x')
  brk <- b
  if(length(b)==0){
    stop('Length of b is 0')
  } else if(length(b)==1){
    ret <- cut(x,breaks=c(min(x,na.rm=TRUE),b,max(x,na.rm=TRUE)),
               labels = sapply(c('<','>'),function(x,b) paste0(x,b),b=b),
               include.lowest = TRUE)
  } else if(length(b)>1){
    lbs <- paste(b[1:(length(b)-1)],b[2:length(b)],sep='-')
    xRange <- range(x,na.rm=TRUE); bRange <- range(b,na.rm=TRUE)
    if(xRange[1]<bRange[1]){
      brk <- c(xRange[1],brk)
      lbs <- c(paste0('<',bRange[1]),lbs)
    }
    if(xRange[2]>bRange[2]){
      brk <- c(brk,xRange[2])
      lbs <- c(lbs,paste0('>',bRange[2]))
    }
    ret <- cut(x,breaks=brk,labels = lbs,include.lowest = TRUE)
  } else stop('Check breaks')
  return(ret)
}

# makeBreaks(x=runif(100,0,5),b=c(0.5))
# makeBreaks(x=runif(100,0,5),b=c(1,2,3))
# makeBreaks(x=runif(100,0,5),b=c(0.5,1,1.5))
# debugonce(makeBreaks)

#Splits single csv into multiple csvs, separating field by names/years
split_csv <- function(path,rmOld=FALSE,fastRead=TRUE){ 
  if(!file.exists(path)) stop('File ',path,' not found')
  
  #Get file/directory paths
  filename <- strsplit(path,'/')[[1]] 
  filename <- filename[length(filename)]
  dirname <- gsub(filename,'',path,fixed = TRUE)
  
  #Read in csv
  print(paste0('Reading in ',filename))
  
  cClass <- c(rep('numeric',2),rep('character',7),rep('numeric',9))
  if(fastRead){
    dat <- data.table::fread(path,sep=",",colClasses = cClass)
    dat <- data.frame(dat)  
  } else {
    dat <- read.csv(path,colClasses = cClass)
  }
  
  
  #Clean up column headers
  colnames(dat) <- gsub('^Date$','Date_ymd',colnames(dat))
  colnames(dat) <- gsub('.m.','_m',colnames(dat),fixed = TRUE)
  colnames(dat) <- gsub('.s.','_s',colnames(dat),fixed = TRUE)
  colnames(dat) <- gsub('.deg.','_deg',colnames(dat),fixed = TRUE)
  colnames(dat) <- gsub('...','_perc',colnames(dat),fixed = TRUE)
  colnames(dat) <- gsub('.tonne.ha.','_tha',colnames(dat),fixed = TRUE)
  colnames(dat) <- gsub('.km.h.','_kmh',colnames(dat),fixed = TRUE)
  colnames(dat) <- gsub('.L.','_L',colnames(dat),fixed = TRUE)
  
  cNames <- c('Longitude','Latitude','Grower','Field','Date_ymd','Year',
              'Crop','Variety','CombineID','Distance_m','Track_deg',
              'Duration_s','Elevation_m','SwathWidth_m','Moisture_perc',
              'Yield_tha','Speed_kmh','Fuel_L')
  
  if(any(colnames(dat)!=cNames)) stop('Column names incorrect. Check format')
  
  if(any(grepl('_',unique(dat$Field),fixed=TRUE))){
    print(paste0('Underscore replaced in field name: ',unique(dat$Field)))
    dat$Field <- gsub('_','.',dat$Field,fixed=TRUE) 
  } else if(any(grepl('/',unique(dat$Field),fixed=TRUE))){
    print(paste0('Forwardslash replaced in field name: ',unique(dat$Field)))
    dat$Field <- gsub('/','.',dat$Field,fixed=TRUE) 
  } else if(any(grepl('|',unique(dat$Field),fixed = TRUE))){
    print(paste0('Bar replaced in field name: ',unique(dat$Field)))
    dat$Field <- gsub('|','.',dat$Field,fixed = TRUE) 
  }
  
  # Fix Spring/Winter Wheat/Barley naming
  dat$Crop <- sub("(.*)\\s-\\s(Winter|Spring)", "\\2 \\1", dat$Crop)
  
  #Get number of splits needed
  splits <- with(dat,unique(data.frame(year=Year,field=Field,
                                       fy=paste(Field,Year,sep='_')
  ))) 
  if(nrow(splits)==1){
    print('Single field, year, and product. No splits needed')
    if(fastRead){
      data.table::fwrite(dat,paste0(dirname,splits$fy[1],'.csv'),sep=",",
                         row.names = FALSE)
    } else {
      write.csv(dat,paste0(dirname,splits$fy[1],'.csv'),row.names = FALSE)
    }
    
  } else {
    #SPLIT INTO SINGLE FIELD-YEAR CSV
    print(paste0('Splitting ',filename,' into ',nrow(splits),' files: ',
                 length(unique(splits$year)),' years, ',
                 length(unique(splits$field)),' fields'))
    pb <- txtProgressBar(style=3)
    for(i in 1:nrow(splits)){
      chooseThese <- which(dat$Year==splits$year[i] & dat$Field==splits$field[i])
      tempDat <- dat[chooseThese,]
      if(fastRead){
        data.table::fwrite(tempDat,paste0(dirname,splits$fy[i],'.csv'),sep=",",
                           row.names = FALSE)
      } else {
        write.csv(tempDat,paste0(dirname,splits$fy[i],'.csv'),row.names = FALSE)
      }
      setTxtProgressBar(pb,i/nrow(splits))
    }
    close(pb)
    if(rmOld & !(path %in% paste0(dirname,splits$fy,'.csv'))){
      print('Removing old file')
      file.remove(path)
    }
  }
  print('Done')
  rm(list = ls()); gc(FALSE) #Cleanup
}

#Renames single csv from default SMS filename
rename_csv <- function(dirname){
  # dirname <- "D:\\geoData\\SMSexport\\202208 Emde Land and Cattle Corp/" #Debugging
  if(!dir.exists(dirname)) stop('Directory not found')
  fullpath <- dir(path = dirname,pattern = '(19|20)[0-9]{4}\\s.*_.*_.*_(19|20)[0-9]{2}_.*\\.csv$',
                  full.names = TRUE) #Get full paths of files
  if(length(fullpath)==0) stop('No files matching SMS export pattern found')
  files <- basename(fullpath) #Get csv file name
  newnames <- sapply(strsplit(files,'_'),function(x){
    if(length(x)==6){
      paste0(x[3],'_',x[4],'.csv')
    } else {
      x <- x[c(-1,-2)]
      year_index <- which(grepl('^20[0-9]{2}$',x))
      paste0(paste0(x[1:year_index-1],collapse='.'),'_',x[year_index],'.csv')
    }
  }) #Create new names, accounting for potential extra underscores
  
  for(f in 1:length(files)){
    file.rename(fullpath[f],gsub(files[f],newnames[f],fullpath[f],fixed = TRUE)) #Rename files
  }
  print('Renamed default SMS names')
  badnames <- grepl("(\\||\\,|\\/)",newnames)
  if(any(badnames)){
    warning(paste0('Some files had (possibly) bad filenames:',newnames,collapse='\n'))
  } 
}

#Merges two csvs into a single one - used for cleanup
merge_csv <- function(path1,path2,newpath,rmOld=FALSE){
  
  if(!file.exists(path1)) stop('File ',path1,' not found')
  if(!file.exists(path2)) stop('File ',path2,' not found')
  
  print("Reading files")
  csv1 <- read.csv(path1)
  csv2 <- read.csv(path2)
  
  if(rmOld){
    if(rmOld){
      print('Removing old files')
      file.remove(path1)
      file.remove(path2)
    }
  }
  
  print("Writing merged files")
  newcsv <- rbind(csv1,csv2)
  write.csv(newcsv,newpath,row.names = FALSE)
}

#Function to iterate through field names/years, and choose crops/varieties interactively
fix_names <- function(filedir){
  # #Debugging
  # filedir <- "D://geoData//SMSexport//202201 Clayton Monchuk"
  
  paths <- dir(filedir,pattern = '*.csv',full.names = TRUE)
  
  #Figure out how many fields and years are present
  pathDat <- data.frame(paths)
  pathDat$fieldyear <- gsub('\\.csv$','',basename(paths))
  pathDat$field <- gsub('\\_20[0-9]{2}$','',pathDat$fieldyear)
  pathDat$year <- gsub('.*\\_','',pathDat$fieldyear)
  
  print(paste0(length(pathDat$fieldyear),' files found with ',length(unique(pathDat$field)),' unique fields'))
  
  for(f in 1:length(unique(pathDat$field))){
    print(paste0('Field: ',unique(pathDat$field)[f]))
    pathDat[pathDat$field==unique(pathDat$field)[f],]
  }
  
  #Load
  
  # fieldYears <- rev(with(splits,tapply(crop,paste(field,year,sep='_'),length)))
  # fieldYears <- data.frame(fy=names(fieldYears),n=unname(fieldYears))
  # if(any(fieldYears$n>1)){
  #   print('Some field-years have multiple products.')
  #   for(i in 1:nrow(fieldYears)){
  #     if(fieldYears$n[i]>1){
  #       opt <- splits[splits$fy==fieldYears$fy[i],c('crop','variety')]
  #       opt <- rbind(opt,data.frame(crop=unique(opt$crop),variety=NA))
  #       rownames(opt) <- 1:nrow(opt)
  #       cat(fieldYears$fy[i],':',sep='')
  #       print(opt)
  #       cat(paste0(nrow(opt)+1),' Ignore\n',paste0(nrow(opt)+2),' Abort',sep='')
  #       ans <- readline(paste0('Enter option (1-',nrow(opt)+2,'): '))
  #       if(ans<=nrow(opt)){
  #         #SET CROP/VARIETY TO USER CHOICE
  #         dat$Crop
  #         
  #       } else if(ans==nrow(opt)+1){ #Ignore
  #         #SAVE SINGLE FILE WITH INDIVIDUAL CROPS/VARIETIES 
  #         
  #         # #Save files with individual crop names - OLDER
  #         # splits$useCropName[splits$fy %in% fieldYears$fy[fieldYears$n>1]] <- TRUE
  #         # splits$fy[splits$useCropName] <- paste(splits$fy[splits$useCropName],splits$crop[splits$useCropName],sep='_')  
  #       } else { #Abort/other
  #         print('Exiting')
  #         return()
  #       }
  #     }
  #   }
  # }
  
}

#"Inlier" spatial filtering procedure from Vega et al 2019, https://doi.org/10.1007/s11119-018-09632-8 
#written to work with sf + dplyr. Returns boolean
vegaFilter <- function(dat,ycol,pvalCutoff=0.05,nDist=40,spDepInd=FALSE,cluster=NULL,chunksize=7500,exclude=NULL){
  library(spdep)
  library(tidyverse)
  
  if(!any(class(dat) %in% 'sf')) stop('Dataframe must be sf object')
  if(any(!is.null(exclude))){
    if(length(exclude)!=nrow(dat)) stop('exclude not the same length as dataframe')
    if(!is.logical(exclude)) stop('exclude must be a logical vector')
    dat <- dat %>% filter(!exclude) #Remove points to be excluded
  }
  
  #Get neighbourhood indices for each point (which other points are in this point's neighbourhood?)
  if(spDepInd){ #Original serial version from spdep - very slow  
    nIndices <- dnearneigh(dat,0,nDist) 
  } else if(!is.null(cluster) && length(cluster)>1) { # Parallel version 
    
    nChunks <- max(length(cluster),ceiling(nrow(dat)/chunksize)) #Divide data into chunks of chunksize
    #(If more clusters than chunks, nChunks = nCluster)
    
    #Split dataset into chunks
    splits <- cut(1:nrow(dat),nChunks,labels=1:nChunks)
    splits <- lapply(levels(splits),function(x) splits==x)
    
    #Function to split task across cluster
    clInd <- function(i,d,nd){ 
      sf::st_contains(sf::st_buffer(sf::st_geometry(d[i,]),nd),d)
    }
    nIndices <- parLapply(cluster,splits,clInd,d=dat,nd=nDist) #Run across clusters
    clusterCall(cluster,gc) #Cleanup
    nIndices <- do.call('c',nIndices) #Combine
    attr(nIndices,"class") <- 'nb' #Make list look like an nb object
  } else { #sf serial version - still takes quite a while  
    library(sf)
    nIndices <- st_contains(st_buffer(st_geometry(dat),nDist),dat) 
    # nIndices <- lapply(1:length(nIndices),function(x) nIndices[[x]][nIndices[[x]]!=x]) #Remove self-points from sets
    attr(nIndices,"class") <- 'nb' #Make list look like an nb object
  }
  
  if(any(sapply(nIndices,length)==1)) warning('Some points had no neighbours and were removed') 
  
  #Get neighbourhood weights from 0 to ndist meters    
  nWeights <- nb2listw(nIndices, style = "W",zero.policy = TRUE) 
  
  yield <- pull(dat,{{ycol}}) #Get yield data column
  
  #Local Moran's I
  LM <- localmoran(yield,nWeights,alternative ="less")
  
  results <- data.frame(LM) %>% #Converts to dataframe
    rename('pval'=contains('Pr.z.')) %>% 
    #Adjusts p-values for multiple comparisons
    mutate(pval=p.adjust(pval,method='bonferroni')) %>% 
    #Filter negative Ii and pvals < 0.05
    mutate(keepThese= Ii > 0 | pval > pvalCutoff) %>% 
    #NAs (points had no neighbours)
    mutate(keepThese=ifelse(is.na(keepThese),FALSE,keepThese)) 
  
  ret <- pull(results) #Convert to a vector
  
  if(any(!is.null(exclude))){ #If data were excluded
    temp <- rep(TRUE,length(exclude))
    temp[!exclude] <- ret
    ret <- temp
  }
  return(ret)
}

#Function to clean yield data

# newpath=NULL : path for cleaned csv to be written
# figpath=NULL : path for figures to be written
# upperYield=NULL : upper bound on yield (t/ha)
# boundaryPath=NULL : boundary shapefile (optional)
# useVega=TRUE : use Vega spatial inlier filter?
# keepFiltCols=FALSE : keep filter columns?
# ncore=1 : number of cores to use in processing
# fastRead=TRUE: use "fast" read/write csv commands?
# speedR2thresh = 0.95: R2 threshold for speed-distance correlation models used to fill in speed gaps (lower than this, and model will quit)
# upperSpeed = 15: upper limit for combine ground speed (km/hr); "usual" ground speed for actual harvest is <5 mph (8kph)

clean_csv <- function(path,newpath=NULL,figpath=NULL,upperYield=NULL,boundaryPath=NULL,useVega=TRUE,keepFiltCols=FALSE,ncore=1,fastRead=TRUE,
                      speedR2thresh=0.95,upperSpeed=15){
  library(tidyverse)
  library(sf)
  if(ncore>1){
    library(parallel)
    cl <- makeCluster(ncore)
    on.exit({stopCluster(cl); rm(list=ls());gc()}) #Cleanup on exit
    # print(paste0('Using ',ncore,' cores for parallel processing'))
  } else {
    cl <- NULL
    on.exit({rm(list=ls());gc()})
  }
  
  #FUNCTIONS
  
  #Function to filter anything above certain quantiles
  QuantileFilter <- function(x,quant=0.99){ 
    l <- c((1-quant)/2,1-(1-quant)/2) #Symmetric quantiles
    x>quantile(x,l[1]) & x<quantile(x,l[2]) 
  }
  
  #Filter differences in track angles 
  bearingFilter <- function(bearing,q=NULL,z=NULL,returnDiffs=FALSE){
    if(!xor(is.null(q),is.null(z))&!returnDiffs){
      stop('Input quantiles or Z-score')
    } else if(sum(is.na(bearing))==length(bearing)){
      warning('No bearings (track angles) found. Skipping filter')
      return(rep(TRUE,length(bearing)))
    }
    #Difference in compass bearings (in degrees)
    bearingDiff <- function(x1,x2){
      x <- x1-x2
      x <- ifelse(abs(x)>180,x-(360*sign(x)),x) #Angle differences can't be >180
      return(x)
    }
    
    #Looks 1 point ahead and behind
    bd <- cbind(bearingDiff(lag(bearing),bearing),
                bearingDiff(lead(bearing),bearing))
    #Maximum bearing difference ahead and behind
    bd <- apply(bd,1,function(x){
      max(abs(x),na.rm=TRUE)*sign(x[which.max(abs(x))])
    }) 
    
    if(returnDiffs) return(bd) #Return bearing differences only, without filtering
    
    if(!is.null(q)){
      ret <- QuantileFilter(bd,q=q)
    } else {
      ret <- ZscoreFilter(bd,z=z)
    }
    return(ret)
  }
  
  #Positional difference filter - filters out very distant and very close points
  posFilter <- function(data,q=NULL,returnDiffs=FALSE,upperOnly=FALSE){ 
    if(is.null(q)&!returnDiffs) stop('Input upper quantile')
    
    if(units(st_distance(data[1,],data[2,]))$numerator!='m'){
      warning('Position differences not in meters')
    } 
    coords <- st_coordinates(data) #Get coordinates
    pdiff <- sapply(1:(nrow(coords)-1),function(i){ #Distances between points
      as.numeric(dist(coords[i:(i+1),]))
    }) 
    pdiff <- cbind(c(pdiff,NA),c(NA,pdiff)) #Forward and backward lags
    pdiff <- apply(pdiff,1,max,na.rm=TRUE) #Maximum distance ahead and behind
    if(returnDiffs){
      return(pdiff)
    } else {
      if(upperOnly){
        return(quantile(pdiff,q)>pdiff) #Upper quantile  
      } else {
        return(QuantileFilter(pdiff,q)) #2-sided quantiles (upper and lower)  
      }
    }
  }
  
  #Filter for (forward and backward) lagged speed differences.
  dSpeedFilter <- function(speed,l=c(-1,1),perc=0.2){ 
    #Overloaded lag function that takes negative values
    lag2 <- function(x,n){
      if(n==0) {
        return(x) #No lag
      } else if(n>0){
        lag(x,n) #Positive lag
      } else {
        lead(x,abs(n)) #Negative lag
      } 
    }
    
    llist <- sapply(l,function(x) (lag2(speed,x)-speed)/lag2(speed,x)) #Matrix of % diffs
    
    #Are any lagged speed values > percent change threshold?
    ret <- !apply(llist,1,function(y) any(abs(y)[!is.na(y)]>perc))
    
    return(ret)
  }
  
  a <- Sys.time()
  
  #Read in data
  if(!file.exists(path)) stop('File ',path,' not found')
  filename <- strsplit(path,'/')[[1]] 
  filename <- filename[length(filename)]
  
  cClass <- c(rep('numeric',2),rep('character',7),rep('numeric',9))
  if(fastRead){
    dat <- data.table::fread(path,sep=",",colClasses = cClass)
    dat <- data.frame(dat)  
  } else {
    dat <- read.csv(path,colClasses = cClass)
  }
  print(paste0('Read in ',basename(getwd()),' / ',filename,' : ',nrow(dat),' data points -------------------------'))
  print(paste0('Time: ', Sys.time()))
  
  if(length(unique(dat$Field))>1) stop('Multiple field names: has this file been properly split?') 
  
  #Checks for multiple combine IDs, and performs simple K-W tests
  if(length(unique(dat$CombineID))>1){
    print(paste0(length(unique(dat$CombineID)),' combines used to harvest field. Differences in yield_tha:'))
    cdat <- with(dat,data.frame(N=tapply(Yield_tha,CombineID,length),
                                mean=tapply(Yield_tha,CombineID,mean),
                                sd=tapply(Yield_tha,CombineID,sd),
                                median=tapply(Yield_tha,CombineID,median)))
    rownames(cdat) <- names(with(dat,tapply(Yield_tha,CombineID,mean)))
    print(cdat)
    
    ktest <- kruskal.test(Yield_tha~CombineID,data=dat)
    atest <- summary(aov(Yield_tha~CombineID,data=dat))
    
    pfval <- function(i,d){ #Permanova f-values (i input ignored)
      d1 <- d
      d1$CombineID <- d1$CombineID[sample(1:nrow(d),replace = FALSE)]
      summary(aov(Yield_tha~CombineID,data=d1))[[1]]$F[1]
    }
    
    if(ncore>1){ #Parallel
      # clusterExport(cl,'pfval',envir = environment())
      patest <- parSapply(cl,1:500,pfval,d=dat)
    } else { #Serial
      # patest <- replicate(500,pfval(dat))
      patest <- sapply(1:500,pfval,d=dat)
    }
    
    patestPval <- sum(atest[[1]]$F[1] < patest)/200
    print(paste0('Kruskal-Wallis: ChiSq = ',round(unname(ktest$statistic),3),', pval = ',ifelse(ktest$p.value<2.2e-16,'<2.2e-16',ktest$p.value)))
    print(paste0('PermAnova: F = ',round(atest[[1]]$F[1],3),', pval = ',ifelse(patestPval<2.2e-16,'<2.2e-16',patestPval)))
    if(ktest$p.value<0.01|patestPval<0.01){
      print('*********')
      print('*** Large differences in yield between combines. Check output figures for combine-specific spatial patterns')  
      print('*********')
    }
  }
  
  cNames <- colnames(dat) #Column names to retain
  
  #Add spatial info
  dat <- dat %>% 
    st_as_sf(coords=c('Longitude','Latitude'),remove=FALSE) %>% #Add spatial feature info
    st_set_crs(4326) %>% #Lat-lon format 
    st_transform(3401) %>% #Transform to UTM
    mutate(E=st_coordinates(.)[,1],N=st_coordinates(.)[,2]) %>% #Create N and E column
    mutate(E=(E-mean(E)),N=(N-mean(N))) #Center N and E values
  
  #Points outside of field boundaries
  if(is.null(boundaryPath)){
    print('No field boundary provided. Using all data points')
    dat$inBoundary <- TRUE
  } else {
    if(!file.exists(boundaryPath)) stop('File ',path,' not found')
    fieldBoundary <- read_sf(boundaryPath) %>% st_transform(3401)
    dat$inBoundary <- apply(st_contains(fieldBoundary,dat,sparse = FALSE),2,any)
  }
  
  #Removes data points above upper limit thresholds
  if(is.null(upperYield)){
    print(paste0('Using default upper yield limit'))
    
    #These are rough estimates of "reasonable" maximum yield
    defaultLims <- data.frame(crop=c('wheat','barley','rye','canola','mustard','peas','flax','oats','lentil'),
                              upr=c(10.75,10.75,10.75,10,10,10,8,10,8),
                              uprPlot=c(6,6,6,6,6,6,6,6,6)) #Upper limits for plots
    
    cropTypes <- unique(dat$Crop) #Unique crop types
    
    #Matrix matching listed crop types (rows) to ones from data (columns)
    chooseCrops <- do.call('rbind',lapply(defaultLims$crop,function(x) grepl(x,cropTypes,ignore.case = TRUE)))
    colnames(chooseCrops) <- cropTypes
    rownames(chooseCrops) <- defaultLims$crop
    
    if(sum(chooseCrops)==0){
      warning(paste0('Crop type ',paste0(cropTypes,collapse=', '),' not listed in threshold table. Removing upper limit'))
      upperYield <- Inf
      uprLimPlot <- quantile(dat$Yield_tha,0.95) #Upper limit for filter/plots
    } else {
      if(any(apply(chooseCrops,2,sum)==0)){
        warning('Crop type ',paste0(cropTypes[apply(chooseCrops,2,sum)==0],collapse=', '),' removed: no matches.')
      }
      cropTypes <- defaultLims$crop[apply(chooseCrops,1,any)] #reset names of croptypes
      upperYield <- defaultLims$upr[apply(chooseCrops,1,any)] #Upper limit for filter
      uprLimPlot <- defaultLims$uprPlot[apply(chooseCrops,1,any)] #Upper limit for plotted yield
      if(length(cropTypes)>1){
        warning('Multiple crop types: ',paste0(cropTypes,collapse=', '),'. Using largest upper limits')
        upperYield <- max(upperYield)
        uprLimPlot <- max(uprLimPlot)
        if(is.na(upperYield)|is.na(uprLimPlot)) stop('NA crop type/upper limit')
      }
    }
  }
  
  #Check ground speed 
  nSpeedMiss <- with(dat,sum(is.na(Speed_kmh))) #Missing speed numbers
  if(nSpeedMiss>0){
    propSpeedMiss <- nSpeedMiss/nrow(dat)
    spdMsg <- paste0('Ground speed NA at ',nSpeedMiss,' points (',round(100*propSpeedMiss,1),'%)')
    if(propSpeedMiss>0.9){
      stop(spdMsg)
    } else {
      print(spdMsg) 
      
      #Fit simple LM to non-zero speed data
      tempDat <- dat %>% 
        filter(Speed_kmh!=0 & !is.na(Speed_kmh) & Speed_kmh<upperSpeed) %>% #Remove zeros, NAs, points above upper limit
        mutate(predSpeed=3.6*Distance_m/Duration_s)
      
      speedMod <- lm(Speed_kmh ~ predSpeed,data=tempDat)
      
      if(is.null(figpath)){
        figpath2 <- gsub('\\.csv$','_speedMod.png',newpath)
      } else {
        figpath2 <- gsub('\\.[a-z]{3}$','_speedMod.png',figpath)
      }
      
      #Make speed prediction figure
      png(figpath2,width=10, height=10, units = 'in', res = 200)
      plot(predSpeed~Speed_kmh,data=tempDat,xlab='Measured Ground Speed (km/h)',
           ylab='Predicted Ground Speed (km/h)',pch=19)
      abline(speedMod,col='red'); abline(0,1,col='cyan',lty=2)
      plotText <- paste0('y ~ ',round(coef(speedMod)[1],2),'+ ',
                         round(coef(speedMod)[2],2),'x\nR^2 = ', 
                         round(summary(speedMod)$r.squared,3),
                         '\nMAE = ',round(mean(abs(residuals(speedMod))),3),
                         '\nNumber of points filled in = ',nSpeedMiss)
      text(x=max(tempDat$Speed_kmh)*0.15,y=max(tempDat$predSpeed)*0.9,plotText)
      dev.off()
      figPath <- NULL
      
      if(summary(speedMod)$r.squared<speedR2thresh){
        warning(paste0('Bad ground speed predictions from Distance/Duration (R2<',speedR2thresh,'). Check output'))
      }
      
      #Fills in missing speed measurements
      dat <- dat %>% mutate(predSpeed=3.6*Distance_m/Duration_s) %>% 
        mutate(predSpeed2=predict(speedMod,newdata=.)) %>% 
        mutate(Speed_kmh = ifelse(is.na(Speed_kmh),predSpeed2,Speed_kmh)) %>% 
        select(-predSpeed,-predSpeed2)
      rm(tempDat,speedMod,plotText)
    }
  }
  
  #Run filters - TRUE indicates data point passed filtering process ("acceptable")
  print('Running filters')
  dat <- dat %>% 
    #Very large outliers. Exact thresholds should be determined by the combine operator or agronomist
    mutate(tooLarge = Yield_tha<upperYield) %>% 
    #Raw yield outliers
    mutate(qFilt = QuantileFilter(Yield_tha,q=0.98)) %>% 
    #Extreme bearing changes (turning)
    mutate(bFilt = bearingFilter(Track_deg,q=0.98)) %>% 
    #Absolute speed outliers
    mutate(speedFilt = QuantileFilter(Speed_kmh,q=0.98) & Speed_kmh<upperSpeed & 0<Speed_kmh) %>% 
    #Speed differences (>20% change 2 steps forward and backward, suggested by
    # Lyle et al 2014: https://doi.org/10.1007/s11119-013-9336-3)
    mutate(dSpeedFilt = dSpeedFilter(Speed_kmh,l=c(-2,-1,1,2),perc = 0.2)) %>% 
    #Points that are far away from eachother
    mutate(posFilt = posFilter(.,q=0.995,upperOnly = TRUE))  
  # #Swath widths smaller than 20% of the header bar - I think we can get around this by doing weighted sampling
  # mutate(swathFilt = SwathWidth_m>max(SwathWidth_m)*0.2) 
  
  #Combine filter criteria
  filtCrit <- with(dat, inBoundary & tooLarge & qFilt & bFilt & speedFilt & dSpeedFilt & posFilt)
  
  if(useVega){
    #Spatial "inliers"
    print('Running Vega filter')
    dat <- dat %>% 
      mutate(vegaFilt = vegaFilter(.,Yield_tha,nDist = 50,
                                   cluster = cl,exclude = !filtCrit))
  } else{
    dat <- dat %>% mutate(vegaFilt = TRUE)   
  }
  
  filtCrit <- filtCrit & dat$vegaFilt
  
  dat <- dat %>% 
    mutate(allFilt = filtCrit) %>% 
    mutate(Yield_tha_filt = ifelse(allFilt, Yield_tha, NA)) #Turns filtered values to NAs
  
  propFilt <- (sum(is.na(dat$Yield_tha_filt))/nrow(dat)) #Proportion of filtered data
  if(0.5<propFilt){
    filtCol <-  st_drop_geometry(dat) %>% #Proportion data dropped from each category
      select(inBoundary:allFilt) %>% as.matrix() %>%
      apply(.,2,function(x) round(mean(!x),2)) 
    msg <- paste0(round(propFilt*100,1),'% of the data have been filtered. Check filtering categories:\n',
                 paste(capture.output(print(filtCol)),collapse = "\n"))
    sapply(strsplit(msg,'\n')[[1]],print)
    # stop(msg)
    warning(msg)
  } else if(0.3<propFilt) {
    warning(paste0(round(propFilt*100,1),'% of the data have been filtered'))
  }
  
  #FILTERED DATA PLOTS
  print('Making figures')
  
  palFun <- function(x){ #Palette function for figure
    if(x==2) {
      return(c('#FF0000','#000000')) #2 categories = red + black
    } else {
      RColorBrewer::brewer.pal(x,'RdYlGn')
    }
  }
  
  #Break yield into bins
  
  #Figure out upper limits for plots and filters
  yldBreaks <- pretty(dat$Yield_tha_filt)[2:5] #"Pretty" breaks (without bottom and top)
  yldBreaks <- yldBreaks[yldBreaks<=upperYield]
  yldBreaks <- yldBreaks[!is.na(yldBreaks)]
  if(length(yldBreaks)<2) yldBreaks <- median(dat$Yield_tha_filt,na.rm = TRUE)
  
  dat$yield <- makeBreaks(dat$Yield_tha,b=yldBreaks) #Raw yield
  dat$yieldFilt <- makeBreaks(dat$Yield_tha_filt,b=yldBreaks) #Filtered yield
  
  #Columns to display
  dispCols <- c('vegaFilt','tooLarge','qFilt','bFilt','speedFilt','dSpeedFilt',
                'posFilt','allFilt','yield','yieldFilt')
  if(length(unique(dat$CombineID))>1) dispCols <- c(dispCols,'CombineID')
  if(is.null(figpath)) figpath <- gsub('\\.csv$','.png',newpath)
  #Make figure
  png(figpath,width=10, height=6, units = 'in', res = 200)
  plot(dat[dispCols],pch=19,cex=0.05,pal=palFun,max.plot=11)
  dev.off()
  
  b <- Sys.time()
  
  print(paste0('Filtered ',sum(is.na(dat$Yield_tha_filt)),' of ',nrow(dat),' records (',
               round(100*sum(is.na(dat$Yield_tha_filt))/nrow(dat),2),'%). Time: ',
               round(difftime(b,a,units = 'mins'),2),' mins'))
  
  #Organize data and write csv
  if(keepFiltCols){ #If filter columns are kept
    cNames <- c(cNames,'tooLarge','vegaFilt',
                'qFilt', 'bFilt', 'speedFilt','dSpeedFilt','posFilt')
  } else { 
    dat <- dat %>% filter(!is.na(Yield_tha_filt)) #Don't filter data if filter columns are retained
  }
  
  dat <- dat %>% st_drop_geometry() #Remove feature geometry
  dat <- dat[,cNames] #Remove columns not found in original
  
  if(is.null(newpath)){ #Overwrite original 
    newpath <- path
    print(paste0('Overwriting ',filename))
  } 
  
  print('Writing cleaned csv')
  if(fastRead){
    data.table::fwrite(dat,newpath,sep=",",row.names = FALSE)
  } else {
    write.csv(dat,newpath,row.names = FALSE) 
  }
  print(paste0('Finished. Time: ', Sys.time()))
}


#Function to clean yield data csvs and adjust for combine differences

# newpath=NULL : path for cleaned csv to be written
# figpath=NULL : path for figures to be written
# upperYield=NULL : upper bound on yield (t/ha)
# boundaryPath=NULL : boundary shapefile (optional)
# useVega=TRUE : use Vega spatial inlier filter?
# keepFiltCols=FALSE : keep filter columns?
# ncore=1 : number of cores to use in processing
# fastRead=TRUE: use "fast" read/write csv commands?
# speedR2thresh = 0.95: R2 threshold for speed-distance correlation models used to fill in speed gaps (lower than this, and model will quit)
# upperSpeed = 15: upper limit for combine ground speed (km/hr); "usual" ground speed for actual harvest is <5 mph (8kph)

clean_csv2 <- function(path,newpath=NULL,figpath=NULL,upperYield=NULL,
                       boundaryPath=NULL,useVega=TRUE,keepFiltCols=FALSE,
                       ncore=1,fastRead=TRUE,speedR2thresh=0.95,upperSpeed=15){
  library(tidyverse)
  library(sf)
  if(ncore>1){
    library(parallel)
    cl <- makeCluster(ncore)
    on.exit({stopCluster(cl); rm(list=ls());gc()}) #Cleanup on exit
    # print(paste0('Using ',ncore,' cores for parallel processing'))
  } else {
    cl <- NULL
    on.exit({rm(list=ls());gc()})
  }
  
  #FUNCTIONS
  
  #Function to filter anything above certain quantiles
  QuantileFilter <- function(x,quant=0.99){ 
    l <- c((1-quant)/2,1-(1-quant)/2) #Symmetric quantiles
    x>quantile(x,l[1]) & x<quantile(x,l[2]) 
  }
  
  #Filter differences in track angles 
  bearingFilter <- function(bearing,q=NULL,z=NULL,returnDiffs=FALSE){
    if(!xor(is.null(q),is.null(z))&!returnDiffs){
      stop('Input quantiles or Z-score')
    } else if(sum(is.na(bearing))==length(bearing)){
      warning('No bearings (track angles) found. Skipping filter')
      return(rep(TRUE,length(bearing)))
    }
    #Difference in compass bearings (in degrees)
    bearingDiff <- function(x1,x2){
      x <- x1-x2
      x <- ifelse(abs(x)>180,x-(360*sign(x)),x) #Angle differences can't be >180
      return(x)
    }
    
    #Looks 1 point ahead and behind
    bd <- cbind(bearingDiff(lag(bearing),bearing),
                bearingDiff(lead(bearing),bearing))
    #Maximum bearing difference ahead and behind
    bd <- apply(bd,1,function(x){
      max(abs(x),na.rm=TRUE)*sign(x[which.max(abs(x))])
    }) 
    
    if(returnDiffs) return(bd) #Return bearing differences only, without filtering
    
    if(!is.null(q)){
      ret <- QuantileFilter(bd,q=q)
    } else {
      ret <- ZscoreFilter(bd,z=z)
    }
    return(ret)
  }
  
  #Positional difference filter - filters out very distant and very close points
  posFilter <- function(data,q=NULL,returnDiffs=FALSE,upperOnly=FALSE){ 
    if(is.null(q)&!returnDiffs) stop('Input upper quantile')
    
    if(units(st_distance(data[1,],data[2,]))$numerator!='m'){
      warning('Position differences not in meters')
    } 
    coords <- st_coordinates(data) #Get coordinates
    pdiff <- sapply(1:(nrow(coords)-1),function(i){ #Distances between points
      as.numeric(dist(coords[i:(i+1),]))
    }) 
    pdiff <- cbind(c(pdiff,NA),c(NA,pdiff)) #Forward and backward lags
    pdiff <- apply(pdiff,1,max,na.rm=TRUE) #Maximum distance ahead and behind
    if(returnDiffs){
      return(pdiff)
    } else {
      if(upperOnly){
        return(quantile(pdiff,q)>pdiff) #Upper quantile  
      } else {
        return(QuantileFilter(pdiff,q)) #2-sided quantiles (upper and lower)  
      }
    }
  }
  
  #Filter for (forward and backward) lagged speed differences.
  dSpeedFilter <- function(speed,l=c(-1,1),perc=0.2){ 
    #Overloaded lag function that takes negative values
    lag2 <- function(x,n){
      if(n==0) {
        return(x) #No lag
      } else if(n>0){
        lag(x,n) #Positive lag
      } else {
        lead(x,abs(n)) #Negative lag
      } 
    }
    
    llist <- sapply(l,function(x) (lag2(speed,x)-speed)/lag2(speed,x)) #Matrix of % diffs
    
    #Are any lagged speed values > percent change threshold?
    ret <- !apply(llist,1,function(y) any(abs(y)[!is.na(y)]>perc))
    
    return(ret)
  }
  
  
  # Make polygons from width, dist, and angle measurements, centered on location from dat
  makePolys <- function(dat,width='w',dist='d',angle='a',backwards=FALSE){
    
    gType <- dat %>% st_geometry_type(FALSE) #Geometry type
    
    if(gType!='POINT') warning(paste('Input data type is',gType,'not POINT',sep=' '))
    
    rectFun <- function(x,y,w,d,a,b){
      #Function to create corners of rotated rectangle from:
      #   starting location (x,y),
      #   width (w), distance (d), and angle (a)
      #   rotate rectangles 180 degrees? (b)
      rotate <- ifelse(b,90,270)
      a <- (rotate-a)*pi/180 #90-angle in radians. 
      v <- c(x,y) #Starting point
      v1 <- c(d*cos(a),d*sin(a)) #Vectors to add together to get corners
      v2 <- c(-(w/2)*sin(a),(w/2)*cos(a))
      return(rbind(v+v2,v+v1+v2,v+v1-v2,v-v2,v+v2)) #Corners of rotated rectangle
    }
    datCRS <- st_crs(dat) #Coordinate system from dat
    #Apply rectFun to all rows in dat. Probably could be done through map or some other purrr related thing.
    xcoord <- st_coordinates(dat)[,1]
    ycoord  <- st_coordinates(dat)[,2]
    polys <- lapply(1:nrow(dat),function(i){
      r <- rectFun(x = xcoord[i], y = ycoord[i],  w = dat[[width]][i],  d = dat[[dist]][i],
                   a = dat[[angle]][i], b = backwards)
      st_polygon(list(r))
    })
    #Combine dat with new polygon geometry, and add original CRS
    dat2 <- st_sf(st_drop_geometry(dat),geometry=st_sfc(polys)) %>% st_set_crs(datCRS)
    return(dat2)
  }
  
  a <- Sys.time()
  
  #Read in data
  if(!file.exists(path)) stop('File ',path,' not found')
  filename <- strsplit(path,'/')[[1]] 
  filename <- filename[length(filename)]
  
  cClass <- c(rep('numeric',2),rep('character',7),rep('numeric',9))
  if(fastRead){
    dat <- data.table::fread(path,sep=",",colClasses = cClass)
    dat <- data.frame(dat)  
  } else {
    dat <- read.csv(path,colClasses = cClass)
  }
  print(paste0('Read in ',basename(getwd()),' / ',filename,' : ',nrow(dat),' data points -------------------------'))
  print(paste0('Time: ', Sys.time()))
  
  if(length(unique(dat$Field))>1) stop('Multiple field names: has this file been properly split?') 
  
  #Add spatial info
  dat <- dat %>% 
    st_as_sf(coords=c('Longitude','Latitude'),remove=FALSE) %>% #Add spatial feature info
    st_set_crs(4326) %>% #Lat-lon format 
    st_transform(3401) %>% #Transform to UTM
    mutate(E=st_coordinates(.)[,1],N=st_coordinates(.)[,2]) %>% #Create N and E column
    mutate(E=(E-mean(E)),N=(N-mean(N))) #Center N and E values
  
  fieldName <- unique(dat$Field)
  
  #Read in boundary around individual field
  if(file.exists(bPath)){
    boundPoly <- read_sf(bPath) %>%  
      filter(Field==fieldName) 
    if(nrow(boundPoly)==0) stop('No boundary found for Field ',fieldName)
  } else {
    stop('No field boundary shapefile provided")
  }
  
  
  cNames <- colnames(dat) #Column names to retain
  
  
  #Points outside of field boundaries
  if(is.null(boundaryPath)){
    print('No field boundary provided. Using all data points')
    dat$inBoundary <- TRUE
  } else {
    if(!file.exists(boundaryPath)) stop('File ',path,' not found')
    fieldBoundary <- read_sf(boundaryPath) %>% st_transform(3401)
    dat$inBoundary <- apply(st_contains(fieldBoundary,dat,sparse = FALSE),2,any)
  }
  
  #Check for point coverage across boundary
  
  yieldArea <- makePolys(dat, width='Width_m', dist = 'Distance_m',
                         angle = 'Bearing_deg')
  
  #If less than 50% of field area has data
  if(st_area(yieldArea)<st_area(boundPoly)*0.5){
    if(st_area(yieldArea)<st_area(boundPoly)*0.25){
      stop("Less than 25% of field boundary area has yield data. Skipping")
    } else {
      warning("Less than 50% of field boundary area has yield data.")
    }
  }
  
  #Removes data points above upper limit thresholds
  if(is.null(upperYield)){
    print(paste0('Using default upper yield limit'))
    
    #These are rough estimates of "reasonable" maximum yield
    defaultLims <- data.frame(crop=c('wheat','barley','rye','canola','mustard','peas','flax','oats','lentil'),
                              upr=c(10.75,10.75,10.75,10,10,10,8,10,8),
                              uprPlot=c(6,6,6,6,6,6,6,6,6)) #Upper limits for plots
    
    cropTypes <- unique(dat$Crop) #Unique crop types
    
    #Matrix matching listed crop types (rows) to ones from data (columns)
    chooseCrops <- do.call('rbind',lapply(defaultLims$crop,function(x) grepl(x,cropTypes,ignore.case = TRUE)))
    colnames(chooseCrops) <- cropTypes
    rownames(chooseCrops) <- defaultLims$crop
    
    if(sum(chooseCrops)==0){
      warning(paste0('Crop type ',paste0(cropTypes,collapse=', '),' not listed in threshold table. Removing upper limit'))
      upperYield <- Inf
      uprLimPlot <- quantile(dat$Yield_tha,0.95) #Upper limit for filter/plots
    } else {
      if(any(apply(chooseCrops,2,sum)==0)){
        warning('Crop type ',paste0(cropTypes[apply(chooseCrops,2,sum)==0],collapse=', '),' removed: no matches.')
      }
      cropTypes <- defaultLims$crop[apply(chooseCrops,1,any)] #reset names of croptypes
      upperYield <- defaultLims$upr[apply(chooseCrops,1,any)] #Upper limit for filter
      uprLimPlot <- defaultLims$uprPlot[apply(chooseCrops,1,any)] #Upper limit for plotted yield
      if(length(cropTypes)>1){
        warning('Multiple crop types: ',paste0(cropTypes,collapse=', '),'. Using largest upper limits')
        upperYield <- max(upperYield)
        uprLimPlot <- max(uprLimPlot)
        if(is.na(upperYield)|is.na(uprLimPlot)) stop('NA crop type/upper limit')
      }
    }
  }
  
  #Check ground speed 
  nSpeedMiss <- with(dat,sum(is.na(Speed_kmh))) #Missing speed numbers
  if(nSpeedMiss>0){
    propSpeedMiss <- nSpeedMiss/nrow(dat)
    spdMsg <- paste0('Ground speed NA at ',nSpeedMiss,' points (',round(100*propSpeedMiss,1),'%)')
    if(propSpeedMiss>0.9){
      stop(spdMsg)
    } else {
      print(spdMsg) 
      
      #Fit simple LM to non-zero speed data
      tempDat <- dat %>% 
        filter(Speed_kmh!=0 & !is.na(Speed_kmh) & Speed_kmh<upperSpeed) %>% #Remove zeros, NAs, points above upper limit
        mutate(predSpeed=3.6*Distance_m/Duration_s)
      
      speedMod <- lm(Speed_kmh ~ predSpeed,data=tempDat)
      
      if(is.null(figpath)){
        figpath2 <- gsub('\\.csv$','_speedMod.png',newpath)
      } else {
        figpath2 <- gsub('\\.[a-z]{3}$','_speedMod.png',figpath)
      }
      
      #Make speed prediction figure
      png(figpath2,width=10, height=10, units = 'in', res = 200)
      plot(predSpeed~Speed_kmh,data=tempDat,xlab='Measured Ground Speed (km/h)',
           ylab='Predicted Ground Speed (km/h)',pch=19)
      abline(speedMod,col='red'); abline(0,1,col='cyan',lty=2)
      plotText <- paste0('y ~ ',round(coef(speedMod)[1],2),'+ ',
                         round(coef(speedMod)[2],2),'x\nR^2 = ', 
                         round(summary(speedMod)$r.squared,3),
                         '\nMAE = ',round(mean(abs(residuals(speedMod))),3),
                         '\nNumber of points filled in = ',nSpeedMiss)
      text(x=max(tempDat$Speed_kmh)*0.15,y=max(tempDat$predSpeed)*0.9,plotText)
      dev.off()
      figPath <- NULL
      
      if(summary(speedMod)$r.squared<speedR2thresh){
        warning(paste0('Bad ground speed predictions from Distance/Duration (R2<',speedR2thresh,'). Check output'))
      }
      
      #Fills in missing speed measurements
      dat <- dat %>% mutate(predSpeed=3.6*Distance_m/Duration_s) %>% 
        mutate(predSpeed2=predict(speedMod,newdata=.)) %>% 
        mutate(Speed_kmh = ifelse(is.na(Speed_kmh),predSpeed2,Speed_kmh)) %>% 
        select(-predSpeed,-predSpeed2)
      rm(tempDat,speedMod,plotText)
    }
  }
  
  #Run filters - TRUE indicates data point passed filtering process ("acceptable")
  print('Running filters')
  dat <- dat %>% 
    #Very large outliers. Exact thresholds should be determined by the combine operator or agronomist
    mutate(tooLarge = Yield_tha<upperYield) %>% 
    #Raw yield outliers
    mutate(qFilt = QuantileFilter(Yield_tha,q=0.98)) %>% 
    #Extreme bearing changes (turning)
    mutate(bFilt = bearingFilter(Track_deg,q=0.98)) %>% 
    #Absolute speed outliers
    mutate(speedFilt = QuantileFilter(Speed_kmh,q=0.98) & Speed_kmh<upperSpeed & 0<Speed_kmh) %>% 
    #Speed differences (>20% change 2 steps forward and backward, suggested by
    # Lyle et al 2014: https://doi.org/10.1007/s11119-013-9336-3)
    mutate(dSpeedFilt = dSpeedFilter(Speed_kmh,l=c(-2,-1,1,2),perc = 0.2)) %>% 
    #Points that are far away from eachother
    mutate(posFilt = posFilter(.,q=0.995,upperOnly = TRUE))  
  # #Swath widths smaller than 20% of the header bar - I think we can get around this by doing weighted sampling
  # mutate(swathFilt = SwathWidth_m>max(SwathWidth_m)*0.2) 
  
  #Combine filter criteria
  filtCrit <- with(dat, inBoundary & tooLarge & qFilt & bFilt & speedFilt & dSpeedFilt & posFilt)
  
  if(useVega){
    #Spatial "inliers"
    print('Running Vega filter')
    dat <- dat %>% 
      mutate(vegaFilt = vegaFilter(.,Yield_tha,nDist = 50,
                                   cluster = cl,exclude = !filtCrit))
  } else{
    dat <- dat %>% mutate(vegaFilt = TRUE)   
  }
  
  filtCrit <- filtCrit & dat$vegaFilt
  
  dat <- dat %>% 
    mutate(allFilt = filtCrit) %>% 
    mutate(Yield_tha_filt = ifelse(allFilt, Yield_tha, NA)) #Turns filtered values to NAs
  
  propFilt <- (sum(is.na(dat$Yield_tha_filt))/nrow(dat)) #Proportion of filtered data
  if(0.5<propFilt){
    filtCol <-  st_drop_geometry(dat) %>% #Proportion data dropped from each category
      select(inBoundary:allFilt) %>% as.matrix() %>%
      apply(.,2,function(x) round(mean(!x),2)) 
    msg <- paste0(round(propFilt*100,1),'% of the data have been filtered. Check filtering categories:\n',
                  paste(capture.output(print(filtCol)),collapse = "\n"))
    sapply(strsplit(msg,'\n')[[1]],print)
    # stop(msg)
    warning(msg)
  } else if(0.3<propFilt) {
    warning(paste0(round(propFilt*100,1),'% of the data have been filtered'))
  }
  
  #Checks for multiple combine IDs, and adjusts using a spatial GAM
  
  dat <- dat %>% 
    bind_cols(st_coordinates(.)) %>% #Get x,y coords in meters
    mutate(across(c(X,Y),~.x-mean(.x))) %>%  #Center coordinates
    mutate(across(c(Grower:CombineID),factor)) %>% 
    mutate(Date_Combine=factor(paste(Date_ymd,CombineID,sep='_')))
  
  if(length(unique(dat$Date_Combine))>1){ #If multiple combines/dates are present
    
    
    errPath <- gsub('.tif','_ERROR.txt',fieldRastPath) #Error path
    
    #Fit combine-and-date yield model, with spatial smoother s(X,Y), then refit with autocorrelation term
    try({
      m1 <- bam(Yield_tha ~ Date_Combine + s(X,Y,k=200) + 0,data=dat,cluster=cl) #No rho term
      ar1 <- acf(resid(m1),type = 'partial',plot=FALSE)$acf[1,,] #Autocorrelation term
      m1 <- bam(Yield_tha ~ Date_Combine  + s(X,Y,k=200) + 0,data=dat,cluster=cl,rho=ar1) #Refit with rho
      
      #Back-correct estimated combine effects using AR1 model
      modMat <- model.matrix(~ Date_Combine + 0,data=dat) #Model matrix
      coefs <- coef(m1)[!grepl('s\\(',names(coef(m1)))] #Get coefficients
      
      #Maximum difference in size of coefficients
      maxDiff <- max(abs(sapply(1:length(coefs),function(i,vec){ vec[i]/vec[-i]},vec=coefs)))
      if(maxDiff>2){
        stop(paste0('Large differences in estimated combine yield differences (',
                    round(maxDiff,1),
                    ' times). Is this from a single field or crop type?'))
      }
      
    },outFile = errPath)  
    
    #If an error occurred during smoothing
    if(file.exists(errPath)){
      print(paste0('Error occurred during smoothing procedure for field ',
                   fieldName,'. Data (n = ',nrow(dat),') may be too sparse.'))
      next()
    } else {
      #Adjusted yield: subtracts combine/date, then adds back in an "average" combine/date effect
      dat$Yield_tha <- dat$Yield_tha - (modMat %*% coefs)[,1] + mean(modMat %*% coefs) 
      dat$Yield_tha[dat$Yield_tha<0] <- 0.0001 #Makes sure all yield values are non-zero   
    }
    
  } else {
    print('Single combine & harvest date, no adjustments needed')
  }
  
  
  #Filtered data plots
  print('Making figures')
  
  palFun <- function(x){ #Palette function for figure
    if(x==2) {
      return(c('#FF0000','#000000')) #2 categories = red + black
    } else {
      RColorBrewer::brewer.pal(x,'RdYlGn')
    }
  }
  
  #Break yield into bins
  
  #Figure out upper limits for plots and filters
  yldBreaks <- pretty(dat$Yield_tha_filt)[2:5] #"Pretty" breaks (without bottom and top)
  yldBreaks <- yldBreaks[yldBreaks<=upperYield]
  yldBreaks <- yldBreaks[!is.na(yldBreaks)]
  if(length(yldBreaks)<2) yldBreaks <- median(dat$Yield_tha_filt,na.rm = TRUE)
  
  dat$yield <- makeBreaks(dat$Yield_tha,b=yldBreaks) #Raw yield
  dat$yieldFilt <- makeBreaks(dat$Yield_tha_filt,b=yldBreaks) #Filtered yield
  
  #Columns to display
  dispCols <- c('vegaFilt','tooLarge','qFilt','bFilt','speedFilt','dSpeedFilt',
                'posFilt','allFilt','yield','yieldFilt')
  if(length(unique(dat$CombineID))>1) dispCols <- c(dispCols,'CombineID')
  if(is.null(figpath)) figpath <- gsub('\\.csv$','.png',newpath)
  #Make figure
  png(figpath,width=10, height=6, units = 'in', res = 200)
  plot(dat[dispCols],pch=19,cex=0.05,pal=palFun,max.plot=11)
  dev.off()
  
  b <- Sys.time()
  
  print(paste0('Filtered ',sum(is.na(dat$Yield_tha_filt)),' of ',nrow(dat),' records (',
               round(100*sum(is.na(dat$Yield_tha_filt))/nrow(dat),2),'%). Time: ',
               round(difftime(b,a,units = 'mins'),2),' mins'))
  
  #Organize data and write csv
  if(keepFiltCols){ #If filter columns are kept
    cNames <- c(cNames,'tooLarge','vegaFilt',
                'qFilt', 'bFilt', 'speedFilt','dSpeedFilt','posFilt')
  } else { 
    dat <- dat %>% filter(!is.na(Yield_tha_filt)) #Don't filter data if filter columns are retained
  }
  
  dat <- dat %>% st_drop_geometry() #Remove feature geometry
  dat <- dat[,cNames] #Remove columns not found in original
  
  if(is.null(newpath)){ #Overwrite original 
    newpath <- path
    print(paste0('Overwriting ',filename))
  } 
  
  print('Writing cleaned csv')
  if(fastRead){
    data.table::fwrite(dat,newpath,sep=",",row.names = FALSE)
  } else {
    write.csv(dat,newpath,row.names = FALSE) 
  }
  print(paste0('Finished. Time: ', Sys.time()))
}



#Function to clean yield data csvs and adjust for combine differences

# newpath=NULL : path for cleaned csv to be written
# figpath=NULL : path for figures to be written
# upperYield=NULL : upper bound on yield (t/ha)
# boundaryPath=NULL : boundary shapefile (optional)
# useVega=TRUE : use Vega spatial inlier filter?
# keepFiltCols=FALSE : keep filter columns?
# ncore=1 : number of cores to use in processing
# fastRead=TRUE: use "fast" read/write csv commands?
# speedR2thresh = 0.95: R2 threshold for speed-distance correlation models used to fill in speed gaps (lower than this, and model will quit)
# upperSpeed = 15: upper limit for combine ground speed (km/hr); "usual" ground speed for actual harvest is <5 mph (8kph)

clean_csv2 <- function(path,newpath=NULL,figpath=NULL,upperYield=NULL,
                       boundaryPath=NULL,useVega=TRUE,keepFiltCols=FALSE,
                       ncore=1,fastRead=TRUE,speedR2thresh=0.95,upperSpeed=15){
  library(tidyverse)
  library(sf)
  if(ncore>1){
    library(parallel)
    cl <- makeCluster(ncore)
    on.exit({stopCluster(cl); rm(list=ls());gc()}) #Cleanup on exit
    # print(paste0('Using ',ncore,' cores for parallel processing'))
  } else {
    cl <- NULL
    on.exit({rm(list=ls());gc()})
  }
  
  #FUNCTIONS
  
  #Function to filter anything above certain quantiles
  QuantileFilter <- function(x,quant=0.99){ 
    l <- c((1-quant)/2,1-(1-quant)/2) #Symmetric quantiles
    x>quantile(x,l[1]) & x<quantile(x,l[2]) 
  }
  
  #Filter differences in track angles 
  bearingFilter <- function(bearing,q=NULL,z=NULL,returnDiffs=FALSE){
    if(!xor(is.null(q),is.null(z))&!returnDiffs){
      stop('Input quantiles or Z-score')
    } else if(sum(is.na(bearing))==length(bearing)){
      warning('No bearings (track angles) found. Skipping filter')
      return(rep(TRUE,length(bearing)))
    }
    #Difference in compass bearings (in degrees)
    bearingDiff <- function(x1,x2){
      x <- x1-x2
      x <- ifelse(abs(x)>180,x-(360*sign(x)),x) #Angle differences can't be >180
      return(x)
    }
    
    #Looks 1 point ahead and behind
    bd <- cbind(bearingDiff(lag(bearing),bearing),
                bearingDiff(lead(bearing),bearing))
    #Maximum bearing difference ahead and behind
    bd <- apply(bd,1,function(x){
      max(abs(x),na.rm=TRUE)*sign(x[which.max(abs(x))])
    }) 
    
    if(returnDiffs) return(bd) #Return bearing differences only, without filtering
    
    if(!is.null(q)){
      ret <- QuantileFilter(bd,q=q)
    } else {
      ret <- ZscoreFilter(bd,z=z)
    }
    return(ret)
  }
  
  #Positional difference filter - filters out very distant and very close points
  posFilter <- function(data,q=NULL,returnDiffs=FALSE,upperOnly=FALSE){ 
    if(is.null(q)&!returnDiffs) stop('Input upper quantile')
    
    if(units(st_distance(data[1,],data[2,]))$numerator!='m'){
      warning('Position differences not in meters')
    } 
    coords <- st_coordinates(data) #Get coordinates
    pdiff <- sapply(1:(nrow(coords)-1),function(i){ #Distances between points
      as.numeric(dist(coords[i:(i+1),]))
    }) 
    pdiff <- cbind(c(pdiff,NA),c(NA,pdiff)) #Forward and backward lags
    pdiff <- apply(pdiff,1,max,na.rm=TRUE) #Maximum distance ahead and behind
    if(returnDiffs){
      return(pdiff)
    } else {
      if(upperOnly){
        return(quantile(pdiff,q)>pdiff) #Upper quantile  
      } else {
        return(QuantileFilter(pdiff,q)) #2-sided quantiles (upper and lower)  
      }
    }
  }
  
  #Filter for (forward and backward) lagged speed differences.
  dSpeedFilter <- function(speed,l=c(-1,1),perc=0.2){ 
    #Overloaded lag function that takes negative values
    lag2 <- function(x,n){
      if(n==0) {
        return(x) #No lag
      } else if(n>0){
        lag(x,n) #Positive lag
      } else {
        lead(x,abs(n)) #Negative lag
      } 
    }
    
    llist <- sapply(l,function(x) (lag2(speed,x)-speed)/lag2(speed,x)) #Matrix of % diffs
    
    #Are any lagged speed values > percent change threshold?
    ret <- !apply(llist,1,function(y) any(abs(y)[!is.na(y)]>perc))
    
    return(ret)
  }
  
  
  # Make polygons from width, dist, and angle measurements, centered on location from dat
  makePolys <- function(dat,width='w',dist='d',angle='a',backwards=FALSE){
    
    gType <- dat %>% st_geometry_type(FALSE) #Geometry type
    
    if(gType!='POINT') warning(paste('Input data type is',gType,'not POINT',sep=' '))
    
    rectFun <- function(x,y,w,d,a,b){
      #Function to create corners of rotated rectangle from:
      #   starting location (x,y),
      #   width (w), distance (d), and angle (a)
      #   rotate rectangles 180 degrees? (b)
      rotate <- ifelse(b,90,270)
      a <- (rotate-a)*pi/180 #90-angle in radians. 
      v <- c(x,y) #Starting point
      v1 <- c(d*cos(a),d*sin(a)) #Vectors to add together to get corners
      v2 <- c(-(w/2)*sin(a),(w/2)*cos(a))
      return(rbind(v+v2,v+v1+v2,v+v1-v2,v-v2,v+v2)) #Corners of rotated rectangle
    }
    datCRS <- st_crs(dat) #Coordinate system from dat
    #Apply rectFun to all rows in dat. Probably could be done through map or some other purrr related thing.
    xcoord <- st_coordinates(dat)[,1]
    ycoord  <- st_coordinates(dat)[,2]
    polys <- lapply(1:nrow(dat),function(i){
      r <- rectFun(x = xcoord[i], y = ycoord[i],  w = dat[[width]][i],  d = dat[[dist]][i],
                   a = dat[[angle]][i], b = backwards)
      st_polygon(list(r))
    })
    #Combine dat with new polygon geometry, and add original CRS
    dat2 <- st_sf(st_drop_geometry(dat),geometry=st_sfc(polys)) %>% st_set_crs(datCRS)
    return(dat2)
  }
  
  a <- Sys.time()
  
  #Read in data
  if(!file.exists(path)) stop('File ',path,' not found')
  filename <- strsplit(path,'/')[[1]] 
  filename <- filename[length(filename)]
  
  cClass <- c(rep('numeric',2),rep('character',7),rep('numeric',9))
  if(fastRead){
    dat <- data.table::fread(path,sep=",",colClasses = cClass)
    dat <- data.frame(dat)  
  } else {
    dat <- read.csv(path,colClasses = cClass)
  }
  print(paste0('Read in ',basename(getwd()),' / ',filename,' : ',nrow(dat),' data points -------------------------'))
  print(paste0('Time: ', Sys.time()))
  
  if(length(unique(dat$Field))>1) stop('Multiple field names: has this file been properly split?') 
  
  #Add spatial info
  dat <- dat %>% 
    st_as_sf(coords=c('Longitude','Latitude'),remove=FALSE) %>% #Add spatial feature info
    st_set_crs(4326) %>% #Lat-lon format 
    st_transform(3401) %>% #Transform to UTM
    mutate(E=st_coordinates(.)[,1],N=st_coordinates(.)[,2]) %>% #Create N and E column
    mutate(E=(E-mean(E)),N=(N-mean(N))) #Center N and E values
  
  fieldName <- unique(dat$Field)
  
  #Read in boundary around individual field
  if(file.exists(bPath)){
    boundPoly <- read_sf(bPath) %>%  
      filter(Field==fieldName) 
    if(nrow(boundPoly)==0) stop('No boundary found for Field ',fieldName)
  } else {
    stop('No field boundary shapefile provided')
  }
  
  cNames <- colnames(dat) #Column names to retain
  
  
  #Points outside of field boundaries
  if(is.null(boundaryPath)){
    print('No field boundary provided. Using all data points')
    dat$inBoundary <- TRUE
  } else {
    if(!file.exists(boundaryPath)) stop('File ',path,' not found')
    fieldBoundary <- read_sf(boundaryPath) %>% st_transform(3401)
    dat$inBoundary <- apply(st_contains(fieldBoundary,dat,sparse = FALSE),2,any)
  }
  
  #Check for point coverage across boundary
  
  yieldArea <- makePolys(dat, width='Width_m', dist = 'Distance_m',
                         angle = 'Bearing_deg')
  
  #If less than 50% of field area has data
  if(st_area(yieldArea)<st_area(boundPoly)*0.5){
    if(st_area(yieldArea)<st_area(boundPoly)*0.25){
      stop("Less than 25% of field boundary area has yield data. Skipping")
    } else {
      warning("Less than 50% of field boundary area has yield data.")
    }
  }
  
  #Removes data points above upper limit thresholds
  if(is.null(upperYield)){
    print(paste0('Using default upper yield limit'))
    
    #These are rough estimates of "reasonable" maximum yield
    defaultLims <- data.frame(crop=c('wheat','barley','rye','canola','mustard','peas','flax','oats','lentil'),
                              upr=c(10.75,10.75,10.75,10,10,10,8,10,8),
                              uprPlot=c(6,6,6,6,6,6,6,6,6)) #Upper limits for plots
    
    cropTypes <- unique(dat$Crop) #Unique crop types
    
    #Matrix matching listed crop types (rows) to ones from data (columns)
    chooseCrops <- do.call('rbind',lapply(defaultLims$crop,function(x) grepl(x,cropTypes,ignore.case = TRUE)))
    colnames(chooseCrops) <- cropTypes
    rownames(chooseCrops) <- defaultLims$crop
    
    if(sum(chooseCrops)==0){
      warning(paste0('Crop type ',paste0(cropTypes,collapse=', '),' not listed in threshold table. Removing upper limit'))
      upperYield <- Inf
      uprLimPlot <- quantile(dat$Yield_tha,0.95) #Upper limit for filter/plots
    } else {
      if(any(apply(chooseCrops,2,sum)==0)){
        warning('Crop type ',paste0(cropTypes[apply(chooseCrops,2,sum)==0],collapse=', '),' removed: no matches.')
      }
      cropTypes <- defaultLims$crop[apply(chooseCrops,1,any)] #reset names of croptypes
      upperYield <- defaultLims$upr[apply(chooseCrops,1,any)] #Upper limit for filter
      uprLimPlot <- defaultLims$uprPlot[apply(chooseCrops,1,any)] #Upper limit for plotted yield
      if(length(cropTypes)>1){
        warning('Multiple crop types: ',paste0(cropTypes,collapse=', '),'. Using largest upper limits')
        upperYield <- max(upperYield)
        uprLimPlot <- max(uprLimPlot)
        if(is.na(upperYield)|is.na(uprLimPlot)) stop('NA crop type/upper limit')
      }
    }
  }
  
  #Check ground speed 
  nSpeedMiss <- with(dat,sum(is.na(Speed_kmh))) #Missing speed numbers
  if(nSpeedMiss>0){
    propSpeedMiss <- nSpeedMiss/nrow(dat)
    spdMsg <- paste0('Ground speed NA at ',nSpeedMiss,' points (',round(100*propSpeedMiss,1),'%)')
    if(propSpeedMiss>0.9){
      stop(spdMsg)
    } else {
      print(spdMsg) 
      
      #Fit simple LM to non-zero speed data
      tempDat <- dat %>% 
        filter(Speed_kmh!=0 & !is.na(Speed_kmh) & Speed_kmh<upperSpeed) %>% #Remove zeros, NAs, points above upper limit
        mutate(predSpeed=3.6*Distance_m/Duration_s)
      
      speedMod <- lm(Speed_kmh ~ predSpeed,data=tempDat)
      
      if(is.null(figpath)){
        figpath2 <- gsub('\\.csv$','_speedMod.png',newpath)
      } else {
        figpath2 <- gsub('\\.[a-z]{3}$','_speedMod.png',figpath)
      }
      
      #Make speed prediction figure
      png(figpath2,width=10, height=10, units = 'in', res = 200)
      plot(predSpeed~Speed_kmh,data=tempDat,xlab='Measured Ground Speed (km/h)',
           ylab='Predicted Ground Speed (km/h)',pch=19)
      abline(speedMod,col='red'); abline(0,1,col='cyan',lty=2)
      plotText <- paste0('y ~ ',round(coef(speedMod)[1],2),'+ ',
                         round(coef(speedMod)[2],2),'x\nR^2 = ', 
                         round(summary(speedMod)$r.squared,3),
                         '\nMAE = ',round(mean(abs(residuals(speedMod))),3),
                         '\nNumber of points filled in = ',nSpeedMiss)
      text(x=max(tempDat$Speed_kmh)*0.15,y=max(tempDat$predSpeed)*0.9,plotText)
      dev.off()
      figPath <- NULL
      
      if(summary(speedMod)$r.squared<speedR2thresh){
        warning(paste0('Bad ground speed predictions from Distance/Duration (R2<',speedR2thresh,'). Check output'))
      }
      
      #Fills in missing speed measurements
      dat <- dat %>% mutate(predSpeed=3.6*Distance_m/Duration_s) %>% 
        mutate(predSpeed2=predict(speedMod,newdata=.)) %>% 
        mutate(Speed_kmh = ifelse(is.na(Speed_kmh),predSpeed2,Speed_kmh)) %>% 
        select(-predSpeed,-predSpeed2)
      rm(tempDat,speedMod,plotText)
    }
  }
  
  #Run filters - TRUE indicates data point passed filtering process ("acceptable")
  print('Running filters')
  dat <- dat %>% 
    #Very large outliers. Exact thresholds should be determined by the combine operator or agronomist
    mutate(tooLarge = Yield_tha<upperYield) %>% 
    #Raw yield outliers
    mutate(qFilt = QuantileFilter(Yield_tha,q=0.98)) %>% 
    #Extreme bearing changes (turning)
    mutate(bFilt = bearingFilter(Track_deg,q=0.98)) %>% 
    #Absolute speed outliers
    mutate(speedFilt = QuantileFilter(Speed_kmh,q=0.98) & Speed_kmh<upperSpeed & 0<Speed_kmh) %>% 
    #Speed differences (>20% change 2 steps forward and backward, suggested by
    # Lyle et al 2014: https://doi.org/10.1007/s11119-013-9336-3)
    mutate(dSpeedFilt = dSpeedFilter(Speed_kmh,l=c(-2,-1,1,2),perc = 0.2)) %>% 
    #Points that are far away from eachother
    mutate(posFilt = posFilter(.,q=0.995,upperOnly = TRUE))  
  # #Swath widths smaller than 20% of the header bar - I think we can get around this by doing weighted sampling
  # mutate(swathFilt = SwathWidth_m>max(SwathWidth_m)*0.2) 
  
  #Combine filter criteria
  filtCrit <- with(dat, inBoundary & tooLarge & qFilt & bFilt & speedFilt & dSpeedFilt & posFilt)
  
  if(useVega){
    #Spatial "inliers"
    print('Running Vega filter')
    dat <- dat %>% 
      mutate(vegaFilt = vegaFilter(.,Yield_tha,nDist = 50,
                                   cluster = cl,exclude = !filtCrit))
  } else{
    dat <- dat %>% mutate(vegaFilt = TRUE)   
  }
  
  filtCrit <- filtCrit & dat$vegaFilt
  
  dat <- dat %>% 
    mutate(allFilt = filtCrit) %>% 
    mutate(Yield_tha_filt = ifelse(allFilt, Yield_tha, NA)) #Turns filtered values to NAs
  
  propFilt <- (sum(is.na(dat$Yield_tha_filt))/nrow(dat)) #Proportion of filtered data
  if(0.5<propFilt){
    filtCol <-  st_drop_geometry(dat) %>% #Proportion data dropped from each category
      select(inBoundary:allFilt) %>% as.matrix() %>%
      apply(.,2,function(x) round(mean(!x),2)) 
    msg <- paste0(round(propFilt*100,1),'% of the data have been filtered. Check filtering categories:\n',
                  paste(capture.output(print(filtCol)),collapse = "\n"))
    sapply(strsplit(msg,'\n')[[1]],print)
    # stop(msg)
    warning(msg)
  } else if(0.3<propFilt) {
    warning(paste0(round(propFilt*100,1),'% of the data have been filtered'))
  }
  
  #Checks for multiple combine IDs, and adjusts using a spatial GAM
  
  dat <- dat %>% 
    bind_cols(st_coordinates(.)) %>% #Get x,y coords in meters
    mutate(across(c(X,Y),~.x-mean(.x))) %>%  #Center coordinates
    mutate(across(c(Grower:CombineID),factor)) %>% 
    mutate(Date_Combine=factor(paste(Date_ymd,CombineID,sep='_')))
  
  if(length(unique(dat$Date_Combine))>1){ #If multiple combines/dates are present
    
    errPath <- gsub('.tif','_ERROR.txt',fieldRastPath) #Error path
    
    #Fit combine-and-date yield model, with spatial smoother s(X,Y), then refit with autocorrelation term
    try({
      m1 <- bam(Yield_tha ~ Date_Combine + s(X,Y,k=200) + 0,data=dat,cluster=cl) #No rho term
      ar1 <- acf(resid(m1),type = 'partial',plot=FALSE)$acf[1,,] #Autocorrelation term
      m1 <- bam(Yield_tha ~ Date_Combine  + s(X,Y,k=200) + 0,data=dat,cluster=cl,rho=ar1) #Refit with rho
      
      #Back-correct estimated combine effects using AR1 model
      modMat <- model.matrix(~ Date_Combine + 0,data=dat) #Model matrix
      coefs <- coef(m1)[!grepl('s\\(',names(coef(m1)))] #Get coefficients
      
      #Maximum difference in size of coefficients
      maxDiff <- max(abs(sapply(1:length(coefs),function(i,vec){ vec[i]/vec[-i]},vec=coefs)))
      if(maxDiff>2){
        stop(paste0('Large differences in estimated combine yield differences (',
                    round(maxDiff,1),
                    ' times). Is this from a single field or crop type?'))
      }
      
    },outFile = errPath)  
    
    #If an error occurred during smoothing
    if(file.exists(errPath)){
      print(paste0('Error occurred during smoothing procedure for field ',
                   fieldName,'. Data (n = ',nrow(dat),') may be too sparse.'))
      next()
    } else {
      #Adjusted yield: subtracts combine/date, then adds back in an "average" combine/date effect
      dat$Yield_tha <- dat$Yield_tha - (modMat %*% coefs)[,1] + mean(modMat %*% coefs) 
      dat$Yield_tha[dat$Yield_tha<0] <- 0.0001 #Makes sure all yield values are non-zero   
    }
    
  } else {
    print('Single combine & harvest date, no adjustments needed')
  }
  
  #Filtered data plots
  print('Making figures')
  
  palFun <- function(x){ #Palette function for figure
    if(x==2) {
      return(c('#FF0000','#000000')) #2 categories = red + black
    } else {
      RColorBrewer::brewer.pal(x,'RdYlGn')
    }
  }

  #Break yield into bins
  
  #Figure out upper limits for plots and filters
  yldBreaks <- pretty(dat$Yield_tha_filt)[2:5] #"Pretty" breaks (without bottom and top)
  yldBreaks <- yldBreaks[yldBreaks<=upperYield]
  yldBreaks <- yldBreaks[!is.na(yldBreaks)]
  if(length(yldBreaks)<2) yldBreaks <- median(dat$Yield_tha_filt,na.rm = TRUE)
  
  dat$yield <- makeBreaks(dat$Yield_tha,b=yldBreaks) #Raw yield
  dat$yieldFilt <- makeBreaks(dat$Yield_tha_filt,b=yldBreaks) #Filtered yield
  
  #Columns to display
  dispCols <- c('vegaFilt','tooLarge','qFilt','bFilt','speedFilt','dSpeedFilt',
                'posFilt','allFilt','yield','yieldFilt')
  if(length(unique(dat$CombineID))>1) dispCols <- c(dispCols,'CombineID')
  if(is.null(figpath)) figpath <- gsub('\\.csv$','.png',newpath)
  #Make figure
  png(figpath,width=10, height=6, units = 'in', res = 200)
  plot(dat[dispCols],pch=19,cex=0.05,pal=palFun,max.plot=11)
  dev.off()
  
  b <- Sys.time()
  
  print(paste0('Filtered ',sum(is.na(dat$Yield_tha_filt)),' of ',nrow(dat),' records (',
               round(100*sum(is.na(dat$Yield_tha_filt))/nrow(dat),2),'%). Time: ',
               round(difftime(b,a,units = 'mins'),2),' mins'))
  
  #Organize data and write csv
  if(keepFiltCols){ #If filter columns are kept
    cNames <- c(cNames,'tooLarge','vegaFilt',
                'qFilt', 'bFilt', 'speedFilt','dSpeedFilt','posFilt')
  } else { 
    dat <- dat %>% filter(!is.na(Yield_tha_filt)) #Don't filter data if filter columns are retained
  }
  
  dat <- dat %>% st_drop_geometry() #Remove feature geometry
  dat <- dat[,cNames] #Remove columns not found in original
  
  if(is.null(newpath)){ #Overwrite original 
    newpath <- path
    print(paste0('Overwriting ',filename))
  } 
  
  print('Writing cleaned csv')
  if(fastRead){
    data.table::fwrite(dat,newpath,sep=",",row.names = FALSE)
  } else {
    write.csv(dat,newpath,row.names = FALSE) 
  }
  print(paste0('Finished. Time: ', Sys.time()))
}


#Function to create boundary multipolygon from crop csvs
# Output is same format as SMS output - needs to be further processed using cropTypeACI() to get satellite crop type metrics
# Boundaries are very simple 20m buffered convex hull; need to further 

csv2Boundary <- function(dirPath=NULL,fileName=NULL){
  library(tidyverse); library(sf)
  if(is.null(dirPath)) stop('Folder path must be provided')
  csvPaths <- dir(dirPath,'*\\_20\\d{2}.csv$',full.names = TRUE)
  if(length(csvPaths)==0) stop('No csvs found in folder')
  if(is.null(fileName)) stop('Output shapefile must be named')
  
  fldNames <- gsub('*\\_20\\d{2}.csv$','',basename(csvPaths)) #Get field names
  
  sapply(csvPaths,function(path){
    data.table::fread(path,sep=",") %>% data.frame() %>% #Get coordinates
      st_as_sf(coords=c('Longitude','Latitude')) %>%
      st_set_crs(4326) %>% st_transform(3401) %>% 
      st_union() %>% st_convex_hull() %>% st_buffer(20) %>% #Convex hull + Buffer
      st_transform(4326)}) %>% 
    st_as_sfc() %>% st_set_crs(4326) %>% st_as_sf() %>% #Convert to sf object
    mutate(Field=fldNames,Obj__id=1,Bnd_Name=fldNames) %>% 
    rename(geometry=x) %>% 
    write_sf(fileName)
}

# #Debugging
# csv2Boundary(dirPath = "D:\\geoData\\SMSexport\\202216 ZENNETH FAYE\\clean",
#              fileName = "D:\\geoData\\SMSexport\\Field Boundaries\\202216 ZENNETH FAYE_poly.shp")

#Get crop type info from ACI data, add as columns in field boundary shapefiles
cropTypeACI <- function(boundPath,invDir = "D:\\geoData\\Rasters\\croplandInventory",
                        addNewFields=NULL){
  #Debugging
  # #Creating new file from scratch - works
  # boundPath <- "D:\\geoData\\SMSexport\\Field Boundaries/202234 LEGUEE FARMS_poly.shp"
  # addNewFields <- NULL
  # invDir <- "D:\\geoData\\Rasters\\croplandInventory"
  
  # #More complicated: adding new data to existing raster
  # boundPath <- "D:\\geoData\\SMSexport\\Field Boundaries/202203 DAVE HOFER_poly.shp"
  # addNewFields <- "C:\\Users\\samuel.robinson\\Desktop\\202203 DAVE HOFER 2_poly.shp"
  # invDir <- "D:\\geoData\\Rasters\\croplandInventory"
  
  library(tidyverse)
  library(sf)
  library(stars)
  
  # sf_use_s2(FALSE) #Turn off spherical geometry
  
  # Read in boundary shp files for yield data 
  print(paste0('Reading boundary files: ',basename(boundPath)))
  
  fieldBoundaries <- read_sf(boundPath) 
  
  if(is.na(st_crs(fieldBoundaries))) stop('CRS for boundary polygons not defined')
  
  fieldBoundaries <- fieldBoundaries %>% 
    dplyr::select(matches('(Field|^y20\\d{2}$)')) %>% #Select only 
    mutate(Field=gsub('(/|_)','.',Field)) %>% #Replace forwardslash and underscore
    st_make_valid() %>% #Fix geometry if needed
    filter(!st_is_empty(.)) %>% #Remove empty geometries
    group_by(Field) %>% 
    suppressMessages(summarize(across(-geometry,first),do_union = TRUE)) %>% 
    ungroup()
  
  #Get ACI extent polygons, filter out non-overlapping polygons
  aciExtents <- st_read(file.path(invDir,'aciExtents.shp'),quiet = TRUE) %>% 
    filter(sapply(st_intersects(.,st_transform(fieldBoundaries,st_crs(.))),length)>0) #%>% 
  
  #Add columns to dataframe if not already present  
  newCols <- paste0('y',unique(aciExtents$year))[!paste0('y',unique(aciExtents$year)) %in% colnames(fieldBoundaries)]
  if(length(newCols)>0){
    fieldBoundaries <- matrix(NA_character_,nrow=nrow(fieldBoundaries),ncol=length(newCols)) %>% 
      data.frame() %>% setNames(newCols) %>% 
      bind_cols(fieldBoundaries,.) %>% relocate(Field,starts_with('y'),geometry)  
  }
  
  # Add new rows (fields) to dataframe if needed
  if(!is.null(addNewFields)){
    if(file.exists(addNewFields) && grep('.shp$',addNewFields)){
      #Load in new field boundaries
      fieldBoundaries <- read_sf(addNewFields) %>%
        dplyr::select(matches('(Field|^y20\\d{2}$)')) %>% #Select only
        mutate(Field=gsub('(/|_)','.',Field)) %>% #Replace forwardslash and underscore
        st_make_valid() %>% #Fix geometry if needed
        filter(!st_is_empty(.)) %>% #Remove empty geometries
        group_by(Field) %>%
        suppressMessages(summarize(across(everything(),first),do_union = TRUE)) %>% 
        filter(!Field %in% fieldBoundaries$Field) %>% 
        st_transform(crs=st_crs(fieldBoundaries)) %>% #Transform to fieldBoundary crs
        bind_rows(fieldBoundaries,.) %>% 
        ungroup()
    } else stop('Additional file ',basename(addNewFields),' not found, or not a shapefile')
  }
  
  #Transform boundaries to ACI extent crs
  fieldBoundaries <- st_transform(fieldBoundaries,st_crs(aciExtents))
  
  #Read in ACI feature table
  aciFT <- read.csv(file.path(invDir,'featureTableAAFC.csv')) %>% 
    mutate(Label=ifelse(isCrop,Label,'NonCrop'))
  
  #Get separate raster for each year/province combination
  aciRast <- lapply(aciExtents$path,function(x){
    suppressWarnings(read_stars(x)) %>% #Reads raster - weird CRS for ACI data
      # st_set_crs(NA_crs_) %>% st_set_crs(st_crs(aciExtents)) %>% #sets crs - takes too much time, easier to transform polygons
      # st_transform(st_crs(aciExtents)) %>% 
      set_names(nm='Code') %>% 
      mutate(Code=droplevels(factor(Code,level=aciFT$Code,labels = aciFT$Label)))
  })
  
  #Function for extracting crop names and proportion cover
  labFun <- function(x,N=5){
    a <- sort(table(x),TRUE)
    a <- 100*round(a/sum(a,na.rm = TRUE),3)
    if(length(a)>N){
      a <- a[1:N]
    } else if(length(a)<N){
      a <- c(a,rep(0,N-length(a)))
      names(a)[names(a)==''] <- 'NA'
    } 
    paste(names(a),unname(a),sep='_',collapse = ',')
  }
  
  #Function for running labFun across different fields and years
  getCover <- function(f,y,rasts=aciRast,extents=aciExtents,fBound=fieldBoundaries){
    x <- st_geometry(fBound)[f] #Boundary for field
    use <- which(extents$year==y & sapply(st_contains(extents,x),length)==1)
    if(length(use)==0){
      stop('Polygon does not overlap any raster.\nf=',f,', y=',y,'\n Polygon Field=',fBound$Field[f])
    } 
    tempFun <- function(u,xx){ #Transforms and gets codes from raster
      xx <- st_transform(xx,st_crs(rasts[[u]]))
      suppressWarnings(aggregate(rasts[[u]],xx,labFun,N=5)$Code) #Warnings from mismatched EPSG codes
    }
    ret <- sapply(use,tempFun,xx=x)
    if(length(ret)>1){
      ret <- ret[!grepl(paste0(rep('NA_0',5),collapse=','),ret)]
      if(length(ret)==0){
        stop('No info found in either raster. \nf=',f,', y=',y,'\n Polygon Field=',fBound$Field[f])
      } else if(length(ret)>1) ret <- ret[1]
    }
    return(ret)
  }
  
  #Number of year/field values needing updates
  nUpdates <- st_drop_geometry(fieldBoundaries) %>% select(starts_with('y')) %>% is.na() %>% sum
  
  if(nUpdates>0){
    print(paste0('Updating cover types for ',nUpdates, ' field boundary/year combinations'))
    
    #Get first 5 cover types for each year, saves as a new column in boundary sf
    pb <- txtProgressBar(style=3)
    
    yUpdates <- apply(select(st_drop_geometry(fieldBoundaries),starts_with('y')),2,function(x) sum(is.na(x)))
    yUpdates <- yUpdates[yUpdates!=0]
    
    #Iterates through years of data and gets cover data from each matched raster
    #Originally used only a single raster for each dataset, but some farms span multiple raster extents
    for(ii in 1:length(yUpdates)){ #For each year with NAs
      naRows <- which(is.na(pull(fieldBoundaries,names(yUpdates[ii])))) #Which rows in this year are NA?
      yNumber <- as.numeric(gsub('y','',names(yUpdates[ii]))) #Year to use
      
      #Gets cover from each field boundary/year, and writes to NAs in dataframe
      fieldBoundaries[naRows,names(yUpdates[ii])] <- sapply(naRows,getCover,y=yNumber) 
      setTxtProgressBar(pb,sum(yUpdates[1:ii])/sum(yUpdates))
    }
    close(pb)
  }
  
  #Check output
  chk <- apply(st_drop_geometry(fieldBoundaries)[,-1],2,function(x) sum(x==paste0(rep('NA_0',5),collapse=',')|is.na(x)))
  if(any(chk)>1){
    print('Some years had NA crop types:\n',chk)
  }
  
  #Writes to boundary path
  st_write(fieldBoundaries,boundPath,append = FALSE,quiet=TRUE)
}

#Rasterize yield data - back-corrects for combine ID and harvest dates
rasterizeYield <- function(yieldDir=NULL,boundDir="D:\\geoData\\SMSexport\\Field Boundaries",
                           fieldFiltChar = NULL,
                           rastDir=NULL,nClust=12,overwrite=FALSE){
  if(is.null(yieldDir)) stop('Yield file directory must be specified')
  if(is.null(rastDir)) rastDir <- gsub(basename(yieldDir),'rasters',yieldDir)
  if(!dir.exists(rastDir)) dir.create(rastDir)
  
  filePaths <- list.files(yieldDir,pattern="*\\.csv$",full.names = TRUE) #Get all csv paths
  if(length(filePaths)==0) stop('No files found in listed directory')
  if(!is.null(fieldFiltChar)){ #Select only fields matching listed regexp
    filePaths <- filePaths[grepl(fieldFiltChar,filePaths)]
    if(length(filePaths)==0) stop('No files matching listed regexp')
  }
  
  boundPaths <- list.files(boundDir,pattern="*\\.shp$",full.names = TRUE) #Get all shp paths
  fID <- regmatches(yieldDir,regexpr("2022[0-9]{2}",yieldDir)) #Farmer ID #
  if(length(fID)!=1) stop(paste0('No farmer IDs found matching ',yieldDir))
  bPath <- boundPaths[grepl(fID,boundPaths)] #Path for boundary shape files
  if(length(bPath)!=1) stop(paste0('No boundary paths found matching farmer ID ',fID))
  
  #Check if folder has already been processed
  if(!any(!file.exists(file.path(rastDir,gsub('.csv$','.tif',basename(filePaths)))))){
    print(paste0('All files in ',yieldDir,' already rasterized.'))
  } else {
    suppressMessages({
      library(tidyverse)
      library(sf)
      library(stars)
      library(mgcv)
      library(parallel)
    })
    
    boundPoly <- read_sf(bPath) #Boundary polygons for all fields
    
    fieldsFound <- gsub('\\_20[0-9]{2}.csv$','',basename(filePaths)) %in% boundPoly$Field
    if(any(!fieldsFound)){
      stop('No boundaries found for fields:\n',paste0(basename(filePaths)[!fieldsFound],collapse=', '))
    }
    
    print(paste('Rasterizing files in',yieldDir,'------------------------'))
    print(paste('Started at',Sys.time()))
    
    cl <- makeCluster(nClust) #Parallel processing cluster. 
    on.exit({stopCluster(cl); rm(list=ls());gc()}) #Cleanup on exit
    
    for(path in filePaths){
      fieldRastPath <- file.path(rastDir,gsub('.csv$','.tif',basename(path))) #Raster path
      
      if(file.exists(fieldRastPath) & !overwrite){
        print(paste0(basename(fieldRastPath),' already converted to raster'))
      } else {
        fieldName <- gsub('\\_20[0-9]{2}.csv$','',basename(path)) #Get field name from csv path
        fieldBoundPoly <- boundPoly %>% filter(Field==fieldName) #Boundary around individual field
        if(nrow(fieldBoundPoly)==0) stop('No boundary found for Field ',fieldName)
        
        #Read in data and turn into an sf object
        dat <- data.table::fread(path,sep=",") %>% data.frame() %>% #Faster than read.csv
          mutate(allFilt = tooLarge & vegaFilt & qFilt & bFilt & speedFilt & dSpeedFilt & posFilt) %>% 
          st_as_sf(coords=c('Longitude','Latitude'),remove=FALSE) %>% #Add spatial feature info
          st_set_crs(4326) %>% #Lat-lon format
          st_transform(st_crs(boundPoly)) %>%
          filter(allFilt) %>% 
          filter(sapply(st_within(.,fieldBoundPoly),length)==1) %>% 
          bind_cols(st_coordinates(.)) %>% #Get x,y coords in meters
          mutate(across(c(X,Y),~.x-mean(.x))) %>%  #Center coordinates
          mutate(across(c(Grower:CombineID),factor)) %>% 
          mutate(Date_Combine=factor(paste(Date_ymd,CombineID,sep='_')))
        
        if(length(unique(dat$Date_Combine))>1){ #If multiple combines/dates are present
          
          errPath <- gsub('.tif','_ERROR.txt',fieldRastPath) #Error path
          
          #Fit combine-and-date yield model, with spatial smoother s(X,Y), then refit with autocorrelation term
          try({
            m1 <- bam(Yield_tha ~ Date_Combine + s(X,Y,k=200) + 0,data=dat,cluster=cl) #No rho term
            ar1 <- acf(resid(m1),type = 'partial',plot=FALSE)$acf[1,,] #Autocorrelation term
            m1 <- bam(Yield_tha ~ Date_Combine  + s(X,Y,k=200) + 0,data=dat,cluster=cl,rho=ar1) #Refit with rho
            
            #Back-correct estimated combine effects using AR1 model
            modMat <- model.matrix(~ Date_Combine + 0,data=dat) #Model matrix
            coefs <- coef(m1)[!grepl('s\\(',names(coef(m1)))] #Get coefficients
            
            #Maximum difference in size of coefficients
            maxDiff <- max(abs(sapply(1:length(coefs),function(i,vec){ vec[i]/vec[-i]},vec=coefs)))
            if(maxDiff>2){
              stop(paste0('Large differences in estimated combine yield differences (',
                          round(maxDiff,1),
                          ' times). Is this from a single field or crop type?'))
            }
            
          },outFile = errPath)  
          
          #If an error occurred during smoothing
          if(file.exists(errPath)){
            print(paste0('Error occurred during smoothing procedure for field ',
                         fieldName,'. Data (n = ',nrow(dat),') may be too sparse.'))
            next()
          } else {
            #Adjusted yield: subtracts combine/date, then adds back in an "average" combine/date effect
            dat$Yield_tha <- dat$Yield_tha - (modMat %*% coefs)[,1] + mean(modMat %*% coefs) 
            dat$Yield_tha[dat$Yield_tha<0] <- 0.0001 #Makes sure all yield values are non-zero   
          }
          
        }
        
        #Aggregate data and write to raster
        rastTemplate <- st_as_stars(st_bbox(fieldBoundPoly), dx = 20, dy = 20, values=0)
        grd <- st_as_sf(rastTemplate) #Create grid (20x20 m cell size)
        #1st and 3rd quartile functions
        q1 <- function(x,na.rm=TRUE) quantile(x,0.25,na.rm)
        q3 <- function(x,na.rm=TRUE) quantile(x,0.75,na.rm)
        funs <- c('mean','sd','min','q1','median','q3','max') #Summary functions to apply
        agg <- lapply(funs,function(f){
          aggregate(select(dat,Yield_tha), grd, FUN = eval(parse(text=f)), na.rm=TRUE) %>% #Aggregate to grid
            st_rasterize(template = rastTemplate) #Convert to stars object  
        }) %>% set_names(nm=funs) %>% do.call('c',.) %>% merge()
        
        write_stars(agg,fieldRastPath) #Write to geoTiff
        print(paste('Converted',basename(fieldRastPath)))
      }
    }
    print(paste('Finished at',Sys.time()))
  }
}

# rasterizeYield(yieldDir = "D:\\geoData\\SMSexport\\202201 CLINTON MONCHUK\\clean",
#                boundDir = "D:\\geoData\\SMSexport\\Field Boundaries",
#                fieldFiltChar = "2022.csv$",
#                rastDir = "D:\\geoData\\SMSexport\\202201 CLINTON MONCHUK\\rasters",
#                overwrite = FALSE)

#Function to calculate profitability for rasterized yield data. 
#Finds all yield rasters in rastDir, matches to soil/province polygons, field boundaries, and crop prices
profEstimates <- function(rastDir = NULL,
                          soilMapPath = "D:\\geoData\\Shapefiles\\Soil Layers\\PRV_SZ_PDQ_v6.shp",
                          boundDir = "D:\\geoData\\SMSexport\\Field Boundaries",
                          cropPrices = "D:\\geoData\\SMSexport\\PPSN_code\\data\\cropPricesCSV.csv",
                          # retDat = FALSE, #Return dataframe or plot?
                          includeYield = FALSE, #Include yield data along with profit?
                          useAcres = FALSE, #Convert yield to bu/acre - requires bulkDens.csv if not listed in cropPrices
                          bulkDens = "D:\\geoData\\SMSexport\\PPSN_code\\data\\cropBulkDensity.csv",
                          excludeMissing = FALSE){ #Warn about missing econ data, or exclude?
  
  # #Debugging
  # rastDir="./202203 DAVE HOFER/rasters"
  # soilMapPath = "D:\\geoData\\Shapefiles\\Soil Layers\\PRV_SZ_PDQ_v6.shp"
  # boundDir = "D:\\geoData\\SMSexport\\Field Boundaries"
  # cropPrices = "D:\\geoData\\SMSexport\\cropPricesCSV.csv"
  # retDat = FALSE
  
  if(is.null(rastDir) || !dir.exists(rastDir)) stop('rastDir not found')
  if(!file.exists(soilMapPath)) stop('soilMapPath not found')
  if(!file.exists(boundDir)) stop('boundDir not found')
  
  library(sf)
  library(tidyverse)
  library(stars)
  
  ha2ac <- 2.47105 #Acres per hectare
  
  #Get yield rasters
  rastPaths <- data.frame(path=list.files(rastDir,pattern='*.tif$',full.names = TRUE)) %>% 
    mutate(fieldYr=gsub('.tif','',basename(path))) %>% 
    separate(fieldYr,c('Field','Year'),sep='_',remove = FALSE) %>% 
    mutate(Year=as.numeric(Year))
  
  if(nrow(rastPaths)==0){
    warning(paste0('No raster files found in ',rastDir))
    return(NA)
  }
  
  growerID <- basename(gsub(basename(rastDir),'',rastDir))
  
  #Get field boundary polygons with crop type
  bFiles <- list.files(boundDir,pattern = '*.shp$',full.names = TRUE)
  bFiles <- bFiles[grepl(growerID,bFiles)]
  if(length(bFiles)==0){
    stop('No matching boundary polygons found. growerID:',growerID)
  } else if(length(bFiles)>1){
    stop('Multiple matching boundary polygons found: ',basename(bFiles))
  }
  
  bPoly <- st_read(bFiles,quiet=TRUE) %>% 
    mutate(across(matches('^y'),~gsub('_.*$','',.x))) %>% #Gets most-dominant cover type
    mutate(across(matches('^y'),~ifelse(.x=='NonCrop',NA,.x)))
  
  #Get provincial/soil zones
  soilProv <- read_sf(soilMapPath) %>%  
    group_by(SoilZone,Prov) %>% summarize(do_union = TRUE,.groups = "keep") %>% 
    ungroup() %>% st_transform(st_crs(bPoly)) 
  
  bPoly <- bPoly %>% st_join(soilProv) 
  
  noSoil <- is.na(bPoly$SoilZone)|is.na(bPoly$Prov) #Fields not overlapping soil polygons
  
  if(any(noSoil)){
    badDat <- st_drop_geometry(bPoly) %>% filter(noSoil) %>% 
      transmute(Field,` `="...",y2021,y2022,SoilZone,Prov)
    warning('Some fields did not overlap soil polygons and were matched with nearby polygons: \n',
            paste(capture.output(print(badDat)), collapse = "\n"))
    # bPoly <- bPoly %>% filter(!noSoil)
    
    bPoly$SoilZone[noSoil] <- soilProv$SoilZone[
      st_nearest_feature(st_centroid(st_geometry(bPoly))[noSoil],soilProv)
    ]
    
    bPoly$Prov[noSoil] <- soilProv$Prov[
      st_nearest_feature(st_centroid(st_geometry(bPoly))[noSoil],soilProv)
    ]
  }
  
  bPoly <- bPoly %>% pivot_longer(matches('^y20'),names_to='Year',values_to='CropType') %>% 
    mutate(CropType=factor(gsub('_.*$','',CropType)),Year=as.numeric(gsub('y','',Year))) %>% 
    filter(Year %in% unique(rastPaths$Year)) #Strip out unused years
  
  #Get econ information for each region and time
  priceDat <- read.csv(cropPrices,strip.white = TRUE) %>% 
    mutate(across(c('AvgCost_ha','CropPrice_t','Bu_t','CropPrice_bu'),as.numeric))
  
  #Join econ data to boundary polygons
  bPoly <- bPoly %>% 
    left_join(priceDat,by=c('Prov','SoilZone','Year','CropType')) %>% 
    select(-CropUsed) %>% relocate(-geometry) 
  
  #Join econ/crop data to rastPaths
  rastPaths <- rastPaths %>% left_join(bPoly,by=c('Field','Year')) %>% 
    st_as_sf() %>% 
    mutate(SourceKnown=ifelse(is.na(SourceKnown),'Unknown',SourceKnown))
  
  
  if(any(is.na(rastPaths$CropType))){ #Missing crop types
    badDat <- st_drop_geometry(rastPaths) %>% filter(is.na(CropType)) %>% 
      select(-path)
    warning('Missing crop types for some fields:\n',
            paste(capture.output(print(badDat)), collapse = "\n"))
    rastPaths <- rastPaths %>% filter(!is.na(CropType))
  } 
  
  if(any(!rastPaths$SourceKnown=='Known')){ #Source is not "known"
    warning(paste0('Price data for some fields is listed as not known.',ifelse(excludeMissing,' Removing fields.','')))
    if(excludeMissing){
      rastPaths <- rastPaths %>% filter(SourceKnown=='Known')
    }
  } 
  
  #Bulk density for conversion
  if(useAcres & any(is.na(rastPaths$Bu_t))){
    warning('Bulk density not listed for some crops. Using average bushels/tonne')
    if(!file.exists(bulkDens)) stop('Bulk density csv file not found')
    bd <- read.csv(bulkDens) %>% rename('Bu_t2'='Bu_t')
    missingBDcrops <- rastPaths$CropType[is.na(rastPaths$Bu_t)] 
    if(any(!missingBDcrops %in% bd$CropType)){
      badCrops <- paste0(unique(missingBDcrops[!missingBDcrops %in% bd$CropType]),collapse=', ')
      warning(paste0('Some crop types were not found in bulk density csv:\n',badCrops
      ))
    }
    rastPaths <- left_join(rastPaths,bd,by='CropType') %>% 
      mutate(Bu_t=ifelse(is.na(Bu_t),Bu_t2,Bu_t)) %>% 
      select(-Bu_t2)
  }
  
  if(any(!complete.cases(st_drop_geometry(rastPaths)))){ #Any other missing values
    badDat <- st_drop_geometry(rastPaths)[!complete.cases(st_drop_geometry(rastPaths)),-1]
    warning('Price data incomplete or missing for some fields:\n',
            paste(capture.output(print(badDat)), collapse = "\n"))
  }
  
  if(nrow(rastPaths)==0){
    warning('No rasters with matching econ data. Exiting')
    return(NA)
  }
  
  #Function to get profit (and yield) from rasters
  profFun <- function(i,rp,inclYld=FALSE,acres=FALSE){
    fieldRast <- read_stars(rp$path[i]) %>%  #Read raster
      split() %>% pull(median) #Use median yield_tha for now
    fieldRast <- as.vector(fieldRast) #Convert to vector
    fieldRast <- fieldRast[!is.na(fieldRast)] #Remove NAs
    if(length(fieldRast)==0) stop('All yield data are NA. Field: ',rp$Field[i],' Year: ',rp$Year[i])
    profVals <- rp$CropPrice_t[i]*fieldRast - rp$AvgCost_ha[i] #Profit calculations ($/ha)
    d <- data.frame(CropType=rp$CropType[i], Profit_ha = profVals) #Convert to dataframe
    if(acres){ #If using acres
      names(d)[2] <- 'Profit_ac'
      d$Profit_ac <- d$Profit_ac/ha2ac
    }
    if(inclYld){ #If including yield data
      if(acres){
        d$Yield_buAc <- fieldRast*rp$Bu_t[i]/ha2ac #Include yield (bu/ac)  
      } else {
        d$Yield_tha <- fieldRast #Include yield (t/ha)  
      }
    } 
    return(d)
  }
  
  profCalc <- lapply(1:nrow(rastPaths),profFun,
                     rp=rastPaths,inclYld=includeYield,acres=useAcres) %>%
    set_names(nm = rastPaths$fieldYr) %>% 
    bind_rows(.id='FieldYear')
}



