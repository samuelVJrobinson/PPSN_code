#Function to clean yield data csvs and adjust for combine differences

# newpath=NULL : path for cleaned csv to be written
# figpath=NULL : path for figures to be written
# upperYield=NULL : upper bound on yield (t/ha)
# boundaryPath=NULL : boundary shapefile (optional)
# useVega=TRUE : use Vega spatial inlier filter?
# ncore=1 : number of cores to use in processing
# fastRead=TRUE: use "fast" read/write csv commands?
# speedR2thresh = 0.95: R2 threshold for speed-distance correlation models used to fill in speed gaps (lower than this, and model will quit)
# upperSpeed = 15: upper limit for combine ground speed (km/hr); "usual" ground speed for actual harvest is <5 mph (8kph)

clean_csv2 <- function(path,newpath=NULL,figpath=NULL,upperYieldPath=NULL,
                       boundaryPath=NULL,useVega=TRUE,
                       ncore=1,fastRead=TRUE,speedR2thresh=0.95,upperSpeed=15){
  #Debugging/testing
  
  #Test field 1 - clustered combines, no striping
  path="D:\\geoData\\SMSexport\\202201 CLINTON MONCHUK\\ANDERSON_2022.csv"
  newpath="D:\\geoData\\SMSexport\\202201 CLINTON MONCHUK\\clean\\ANDERSON_2022.csv"
  figpath="D:\\geoData\\SMSexport\\202201 CLINTON MONCHUK\\clean\\ANDERSON_2022.png"
  boundaryPath="D:\\geoData\\SMSexport\\Field Boundaries\\202201 CLINTON MONCHUK_poly.shp"
  
  #Test field 2 - many combines, lots of striping
  path="D:\\geoData\\SMSexport\\202213 LAJORD COLONY\\Alain Section 19_2022.csv"
  newpath="D:\\geoData\\SMSexport\\202213 LAJORD COLONY\\clean\\Alain Section 19_2022.csv"
  figpath="D:\\geoData\\SMSexport\\202213 LAJORD COLONY\\clean\\Alain Section 19_2022.png"
  upperYieldPath="D:\\geoData\\SMSexport\\PPSN_code\\data\\cropBulkDensity.csv"
  boundaryPath="D:\\geoData\\SMSexport\\Field Boundaries\\202213 LAJORD COLONY_poly.shp"
  
  upperYieldPath="D:\\geoData\\SMSexport\\PPSN_code\\data\\cropBulkDensity.csv"
  useVega=TRUE
  ncore=4
  fastRead=TRUE
  speedR2thresh=0.95
  upperSpeed=15
  
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
  
  cNames <- colnames(dat) #Column names to retain
  
  #Add spatial info
  dat <- dat %>% 
    st_as_sf(coords=c('Longitude','Latitude'),remove=FALSE) %>% #Add spatial feature info
    st_set_crs(4326) %>% #Lat-lon format 
    st_transform(3401) %>% #Transform to UTM
    mutate(E=st_coordinates(.)[,1],N=st_coordinates(.)[,2]) %>% #Create N and E column
    mutate(E=(E-mean(E)),N=(N-mean(N))) #Center N and E values
  
  fieldName <- unique(dat$Field)
  
  #Read in boundary around individual field
  if(file.exists(boundaryPath)){
    fieldBoundary <- read_sf(boundaryPath) %>%  
      filter(Field==fieldName) %>% st_transform(st_crs(dat))
    if(nrow(fieldBoundary)==0) stop('No boundary found for Field ',fieldName)
  } else {
    stop('No field boundary shapefile provided')
  }
  
  #Points outside of field boundaries
  inBoundary <- apply(st_contains(fieldBoundary,dat,sparse = FALSE),2,any)
  if(any(!inBoundary)){
    dat <- dat %>% filter(inBoundary)
    print(paste0(sum(!inBoundary),' points outside field boundary removed'))
  } 
  
  #Create harvest polygons from point data
  yieldArea <- makePolys(dat, width='SwathWidth_m', dist = 'Distance_m',angle = 'Track_deg')
  
  #Check for point coverage across boundary
  propCoverage <- as.numeric(sum(st_area(yieldArea))/sum(st_area(fieldBoundary)))
  if(propCoverage<0.5){ #if less than 50% of field area has data
    if(propCoverage<0.25){
      stop("Less than 25% of field boundary area has yield data. Skipping")
    } else {
      warning("Less than 50% of field boundary area has yield data.")
    }
  } else {
    print(paste0(round(100*propCoverage),'% of field boundary has yield data'))
  }
  
  #Removes data points above upper limit thresholds
  
  # #Rough estimates of "reasonable" maximum yield (t/ha)
  # defaultLims <- data.frame(crop=c('wheat','barley','rye','canola','mustard','peas','flax','oats',
  #                                  'lentil','chickpea','fababean','canaryseed','durum','corn',),
  #                           upr=c(10,11,11,8,8,10,8,11,8,8,10,8,10,8)) %>% 
  #   mutate(uprPlot=6) #Upper limits for plots
  
  #Reads in a csv of maximum yield data
  defaultLims <- read.csv("D:\\geoData\\SMSexport\\PPSN_code\\data\\cropBulkDensity.csv") %>%
    select(CropType,contains("UprLim_tha")) %>% 
    filter(CropType!='Blow Out') %>% 
    mutate(CropType=gsub('(winter|spring)\\s','',tolower(CropType))) %>%
    mutate(CropType=gsub('s$','',CropType)) %>% 
    distinct() %>% mutate(uprPlot=6) %>% 
    rename(crop=CropType)
  
  cropTypes <- unique(dat$Crop) #Unique crop types from yield data
  
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
    upperYield <- defaultLims$UprLim_tha[apply(chooseCrops,1,any)] #Upper limit for filter
    uprLimPlot <- defaultLims$uprPlot[apply(chooseCrops,1,any)] #Upper limit for plotted yield
    if(length(cropTypes)>1){
      warning('Multiple crop types: ',paste0(cropTypes,collapse=', '),'. Using largest upper limits')
      upperYield <- max(upperYield)
      uprLimPlot <- max(uprLimPlot)
      if(is.na(upperYield)|is.na(uprLimPlot)) stop('NA crop type/upper limit')
    }
  }
  
  #Check ground speed 
  nSpeedMiss <- with(dat,sum(is.na(Speed_kmh))) #Missing speed numbers
  if(nSpeedMiss>0){ 
    propSpeedMiss <- nSpeedMiss/nrow(dat)
    spdMsg <- paste0('Ground speed NA at ',nSpeedMiss,' points (',round(100*propSpeedMiss,1),'%)')
    if(propSpeedMiss>0.9){ #If speed is missing at >90% of points
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
  
  #Run filters - TRUE indicates data point passed filtering process (i.e. "acceptable")
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
  
  #Combine filtering criteria
  filtCrit <- with(dat, inBoundary & tooLarge & qFilt & bFilt & speedFilt & dSpeedFilt & posFilt)
  
  if((1-mean(filtCrit))>0.15){
    warning(paste0(round((1-mean(filtCrit))*100),'% of data filtered in first stage.'))
    if((1-mean(filtCrit))>0.5){
      stop(paste0(round((1-mean(filtCrit))*100),'% of data filtered in first stage. Check data quality'))
    }
  }
  
  #Turns filtered values to NAs
  dat$Yield_tha_filt <- ifelse(filtCrit, dat$Yield_tha, NA) 
  
  #Checks for multiple combine IDs & harvest dates, and adjusts using a spatial GAM
  # NOTE: needs to happen before spatial inlier filtering, otherwise combine differences may cause large variance at short spatial ranges
  dat <- dat %>% 
    mutate(across(c(Grower:CombineID),factor)) %>% #Turn Grower -> CombineID to factors
    mutate(Date_Combine=factor(paste(Date_ymd,CombineID,sep='_'))) #Create "Date/CombineID" factor
  
  #Which data points are in which chunk of the field? (If field is a multipolygon)
  if(st_geometry_type(fieldBoundary)[1]=='MULTIPOLYGON'){
    inFieldPart <- fieldBoundary %>% st_cast('POLYGON',warn = FALSE) %>% select(Field) %>% 
      mutate(FieldPart=paste(Field,1:n(),sep='.')) %>% 
      st_within(dat,.) %>% sapply(.,first)
  } else {
    inFieldPart <- rep(1,nrow(dat))
  }
   
  #If multiple combines/dates are present in different sub-polygons of the field, adjust yield 
  # NOTE: could do some kind of cross-correction using the same combine/date if present in different field parts, but this isn't necessarily any better, due to "field part" being a separate (unestimated) factor
  if(any(apply(table(inFieldPart,dat$Date_Combine),1,function(x) sum(x>0))>1)){
    library(mgcv)
    # origYield <- dat$Yield_tha #Save original yield
    isAdjusted <- rep(FALSE,length(unique(inFieldPart))) #Has this field part been adjusted?
    
    for(part in 1:length(unique(inFieldPart))){ #For each field part
      errPath <- gsub('.png',paste0('_part',part,'ERROR.txt'),figpath) #Error path
      sDat <- dat %>% mutate(rID=1:n()) %>% #Row ID
        filter(inFieldPart==part) %>% #Data for field part i
        mutate(Date_Combine=droplevels(Date_Combine)) %>% 
        filter(!is.na(Yield_tha_filt)) #Removes filtered data
      
      if(length(levels(sDat$Date_Combine))==1) next() #If only a single Date/Combine for this field part, skip
      
      #Step 1: are combine/date records spatially clustered? If so, skip GAM correction
      # Reason: if combine/date are spatially confounded, no point in adjusting
      DC_centroids <- sDat %>% group_by(Date_Combine) %>%  #Centroids for each Date/Combine
        summarize(do_union = TRUE) %>% st_centroid()
      
      #Average distance from centroids
      centDist <- lapply(1:nrow(DC_centroids),function(x){
        sDat %>% filter(Date_Combine==DC_centroids$Date_Combine[x]) %>% 
          st_distance(DC_centroids[x,])
      }) %>% unlist() %>% mean
      
      #Generate distances from random points among sDat assigned to different Date_combine groupings
      genDists <- function(i){ #Input not used
        fakeCentroids <- sDat %>% select(Date_Combine) %>% 
          slice_sample(n=2) %>% 
          mutate(Date_Combine=DC_centroids$Date_Combine)
        ret <- lapply(1:nrow(DC_centroids),function(x){ #Get distances from points, and take mean
          sDat %>% filter(Date_Combine==fakeCentroids$Date_Combine[x]) %>% 
            st_distance(fakeCentroids[x,])}) %>% unlist() %>% mean
        return(ret)
      }
      
      if(ncore>1){ #Generate random distances from "fake centroids"
        clusterEvalQ(cl,{library(sf);library(dplyr)})
        clusterExport(cl,varlist=c('sDat','DC_centroids'))
        ranDists <- parSapply(cl,1:300,genDists)
      } else {
        ranDists <- replicate(300,genDists())
      }
      
      if(!(centDist>quantile(ranDists,c(0.025)) & centDist<quantile(ranDists,c(0.975)))){
        print(paste0('Combine ID/Date data spatially clustered in field part ',part,'. Skipping...'))
      } else {
        #Step 2: if combine/date records aren't spatially clustered, do GAM correction
        try({
          m1 <- bam(Yield_tha ~ Date_Combine + s(E,N,k=50) + 0,data=sDat,cluster=cl) #No rho term
          ar1 <- acf(resid(m1),type = 'partial',plot=FALSE)$acf[1,,] #Autocorrelation term
          m1 <- bam(Yield_tha ~ Date_Combine  + s(E,N,k=50) + 0,data=sDat,cluster=cl,rho=ar1) #Refit with rho
          
          #Back-correct estimated combine effects using AR1 model
          modMat <- model.matrix(~ Date_Combine + 0,data=sDat) #Model matrix
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
                       fieldName,' - part ',part,'. Data (n = ',nrow(sDat),') may be too sparse.'))
          next()
        } else {
          #Adjusted yield: subtracts combine/date, then adds back in an "average" combine/date effect
          sDat$Yield_tha <- sDat$Yield_tha - (modMat %*% coefs)[,1] + mean(modMat %*% coefs) 
          sDat$Yield_tha[sDat$Yield_tha<0] <- 0.0001 #Makes sure all yield values are non-zero   
          dat$Yield_tha_filt[sDat$rID] <- sDat$Yield_tha #Replace original data
          isAdjusted[part] <- TRUE
        }
        print(paste0('Finished part ',part,' of ',length(unique(inFieldPart))))
      }
    }
    if(any(isAdjusted)){
      propChange <- (1-(dat$Yield_tha_filt/dat$Yield_tha))*100 #Yield adjustment
      print(paste0('Adjusted yields for ',length(levels(dat$Combine)),' combine and ',
                   length(levels(dat$Date_ymd)),' harvest dates across ',
                   length(unique(inFieldPart)),' field parts. Mean (SD) yield adjustment = ',
                   round(mean(propChange,na.rm = TRUE),2),' (',round(sd(propChange,na.rm = TRUE),2),')%'))
    }
  } else {
    print('Single combine & harvest date, no adjustments needed')
  }  
  
  #Run Vega filter if needed
  if(useVega){
    #Spatial "inliers"
    print('Running Vega filter')
    dat <- dat %>% 
      mutate(vegaFilt = vegaFilter(.,Yield_tha_filt,nDist = 50,
                                   cluster = cl,exclude = !filtCrit))
  } else{
    dat <- dat %>% mutate(vegaFilt = TRUE)   
  }
  
  if((1-mean(dat$vegaFilt[filtCrit]))>0.15){
    warning(paste0(round((1-mean(dat$vegaFilt[filtCrit]))*100),'% of data filtered in Vega filter.'))
    if((1-mean(dat$vegaFilt))>0.5){
      stop(paste0(round((1-mean(dat$vegaFilt[filtCrit]))*100),'% of data filtered in Vega filter. Check data quality'))
    }
  }
  filtCrit <- filtCrit & dat$vegaFilt #Adds vegaFilt to overall filtCrit
  
  #Turns filtered values to NAs
  dat <- dat %>% 
    mutate(allFilt = filtCrit) %>% 
    mutate(Yield_tha_filt = ifelse(allFilt, Yield_tha_filt, NA))
  
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
  plot(dat[,dispCols],pch=19,cex=0.05,pal=palFun,max.plot=11)
  dev.off()
  
  b <- Sys.time()
  
  print(paste0('Filtered ',sum(is.na(dat$Yield_tha_filt)),' of ',nrow(dat),' records (',
               round(100*sum(is.na(dat$Yield_tha_filt))/nrow(dat),2),'%). Time: ',
               round(difftime(b,a,units = 'mins'),2),' mins'))
  
  #Organize data and write csv

  cNames <- c(cNames,'tooLarge','vegaFilt','qFilt', 'bFilt',
              'speedFilt','dSpeedFilt','posFilt','Yield_tha_filt')
  
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