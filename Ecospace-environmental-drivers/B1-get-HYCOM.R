library(terra)

## -----------------------------------------------------------------------------
## Set-up
bbox.gom = c(-98, -80.5, 24, 31) ## xmin, xmax, ymin, ymax  
bbox.wfs = c(-88, -80.5, 25, 30.5)
bbox = bbox.gom
dir.hycom = "D:/HYCOM satelite imagery/"
##--> Sets download to external hard drive due to space requirements

## -----------------------------------------------------------------------------
##
## 1) Get list of hycom files on ftp server

## experiment                   dates available             dates used (complete months)
## ------------------           ---------------             ---------------
## GOMu0.04/expt_50.1           1993.01.01-2012.12.31       1993.01-2012.12
## GOMl0.04/expt_31.0           2009.04-2014.07.31          2013.01-2014.03  *overlap with 32.5
## GOMl0.04/expt_32.5           2014.04.01-2019.02.03       2014.04-2016.12
## GOMu0.04/expt_90.1m000       2017.01-2021 current        2017.01-current

## expt_50.1..............................................
nc.files = character()
for(y in 1993:2012){
  # for(y in 1993:1998){ ## Can download in chunks due to space requirements
  ftp_base = paste0("ftp://ftp.hycom.org/datasets/GOMu0.04/expt_50.1/data/netcdf/",y,"/")
  hycom.files = curl::new_handle()
  curl::handle_setopt(hycom.files,ftp_use_epsv=T,dirlistonly=T)
  con = curl::curl(url=ftp_base,"r",handle=hycom.files)
  tmp.files = readLines(con)
  close(con)
  tmp.files = paste0(ftp_base,tmp.files)
  nc.files = c(nc.files,tmp.files)
  rm(tmp.files,hycom.files,con,ftp_base);gc()
}


nc.files = nc.files[grep('t000',nc.files,value=F)]
nc.df.50 = data.frame(filename=nc.files,
                      year = as.numeric(substr(basename(nc.files),16,19)),
                      month = as.numeric(substr(basename(nc.files),20,21)),
                      day = as.numeric(substr(basename(nc.files),22,23)),
                      exp = 50.1)

## expt_31.0.............................................
nc.files = character()
for(y in 2013:2014){
  #y=1993
  ftp_base = paste0("ftp://ftp.hycom.org/datasets/GOMl0.04/expt_31.0/data/",y,"/")
  hycom.files = curl::new_handle()
  curl::handle_setopt(hycom.files,ftp_use_epsv=T,dirlistonly=T)
  con = curl::curl(url=ftp_base,"r",handle=hycom.files)
  tmp.files = readLines(con)
  close(con)
  tmp.files = paste0(ftp_base,tmp.files)
  nc.files = c(nc.files,tmp.files)
  rm(tmp.files,hycom.files,con,ftp_base);gc()
}

nc.files = nc.files[grep('_00_',nc.files,value=F)]
nc.df.31 = data.frame(filename=nc.files,
                      year = as.numeric(substr(basename(nc.files),7,10)))
nc.df.31$month = as.numeric(format(as.Date(as.numeric(substr(basename(nc.files),12,14))-1,origin=paste0(nc.df.31$year,"-01-01")),"%m"))
nc.df.31$day = as.numeric(format(as.Date(as.numeric(substr(basename(nc.files),12,14))-1,origin=paste0(nc.df.31$year,"-01-01")),"%d"))
nc.df.31$exp = 31.0

## Get rid of those that overlap with expt 32.5
nc.df.31 = nc.df.31[-which(nc.df.31$year>=2014 & nc.df.31$month>=4),]

## expt_32.5................................................
nc.files = character()
for(y in 2014:2016){
  #y=1993
  ftp_base = paste0("ftp://ftp.hycom.org/datasets/GOMl0.04/expt_32.5/data/",y,"/")
  hycom.files = curl::new_handle()
  curl::handle_setopt(hycom.files,ftp_use_epsv=T,dirlistonly=T)
  con = curl::curl(url=ftp_base,"r",handle=hycom.files)
  tmp.files = readLines(con)
  close(con)
  tmp.files = paste0(ftp_base,tmp.files)
  nc.files = c(nc.files,tmp.files)
  rm(tmp.files,hycom.files,con,ftp_base);gc()
}
nc.files = nc.files[grep('_00_',nc.files,value=F)]
nc.df.32 = data.frame(filename=nc.files,
                      year = as.numeric(substr(basename(nc.files),7,10)))
nc.df.32$month = as.numeric(format(as.Date(as.numeric(substr(basename(nc.files),12,14))-1,origin=paste0(nc.df.32$year,"-01-01")),"%m"))
nc.df.32$day = as.numeric(format(as.Date(as.numeric(substr(basename(nc.files),12,14))-1,origin=paste0(nc.df.32$year,"-01-01")),"%d"))
nc.df.32$exp = 32.5

## expt_90.1...............................................
nc.files = character()
for(y in 2017:2020){
  ftp_base = paste0("ftp://ftp.hycom.org/datasets/GOMu0.04/expt_90.1m000/data/hindcasts/",y,"/")
  hycom.files = curl::new_handle()
  curl::handle_setopt(hycom.files,ftp_use_epsv=T,dirlistonly=T)
  con = curl::curl(url=ftp_base,"r",handle=hycom.files)
  tmp.files = readLines(con)
  close(con)
  tmp.files = paste0(ftp_base,tmp.files)
  nc.files = c(nc.files,tmp.files)
  rm(tmp.files,hycom.files,con,ftp_base);gc()
}
nc.files = nc.files[grep('t000',nc.files,value=F)]
nc.df.90 = data.frame(filename=nc.files,
                      year = as.numeric(substr(basename(nc.files),20,23)),
                      month = as.numeric(substr(basename(nc.files),24,25)),
                      day = as.numeric(substr(basename(nc.files),26,27)),
                      exp = 90.1)

## Combine all experiments into one
nc.df = rbind(nc.df.50,nc.df.31,nc.df.32,nc.df.90)
nc.df$date = as.Date(paste(nc.df$year,nc.df$month,nc.df$day,sep="-"))
nc.df$yrmon = paste0(nc.df$year,formatC(nc.df$month,width = 2,flag="0"))#nc.df$year+(nc.df$month-1)/12
nc.df = nc.df[order(nc.df$date),]
yrmon.uni = unique(nc.df$yrmon)
nc.df$dup = duplicated(nc.df$date)

## Figure out what files are needed --------------------------------------------
dir.asc.out = "./data extraction/HYCOM/SST/" ## Set directories for environmental drivers

## Get list of existing ascii files
asc.files = list.files(dir.asc.out, pattern="*.asc")
asc.dates = data.frame(file=nc.df$filename, year=nc.df$year, month=nc.df$month, yrmo = nc.df$yrmon)
asc.need = yrmon.uni

## -----------------------------------------------------------------------------
##
## 2) Download HYCOM files

## This does one month at a time
t.surf.m = t.bot.m = t.avg.m = s.surf.m = s.bot.m = s.avg.m = stack()
time.st = Sys.time(); print(time.st)

## Make nc.files.sub without downloading data
for(i in 1:length(yrmon.uni)){#length(yrmon.uni)){
  #i=1
  #subset for month
  nc.files.sub = nc.df$filename[nc.df$yrmon==yrmon.uni[i]]
  nc.files.sub = nc.df$filename[nc.df$yrmon%in%yrmon.uni]
}

for(i in 1:length(yrmon.uni)){#length(yrmon.uni)){
  #i=1
  print(paste(Sys.time(),yrmon.uni[i]));flush.console()
  #subset for month
  nc.files.sub = nc.df$filename[nc.df$yrmon==yrmon.uni[i]]
  nc.files.sub = nc.df$filename[nc.df$yrmon%in%yrmon.uni]
  
  
  ## Set up parallel processing to download the daily netcdf files for month 
  ## USE NO MORE THAN 10 CORES OR HYCOM WILL BLOCK YOU!!!
  cl <- makeSOCKcluster(7)
  clusterExport(cl,"nc.files.sub")
  registerDoSNOW(cl)
  
  pbar <- winProgressBar(paste("Getting HYCOM data from ftp..."), label=paste("N files ",length(nc.files.sub),sep=""),max=100)
  progress<-function(n) setWinProgressBar(pbar,(n/length(nc.files.sub)*100),label=paste("File", n,"of", length(nc.files.sub),"Completed"))
  opts<-list(progress=progress)
  
  
  hcom = foreach(i=4150:length(nc.files.sub),.packages='curl',.options.snow=opts) %dopar% {
    curl::curl_download(nc.files.sub[i], destfile = paste0(dir.hycom, basename(nc.files.sub[i])))
  }

  close(pbar)
  stopCluster(cl)
  
  t.surf.d = t.bot.d = t.avg.d = s.surf.d = s.bot.d = s.avg.d = stack()#(replicate(length(nc.files.sub),r))
}  
  
## -----------------------------------------------------------------------------
##
## 3) Build temp and salinity raster bricks from HYCOM nc files

## Set up dataframe
dfhycom = data.frame(file = nc.files.sub)
dfhycom$datedig = 0

## Extract date from file name
for (i in 1:nrow(dfhycom)){
  string = dfhycom$file[i]
  if(grepl("hycom_gomu_501", string, fixed=TRUE) == TRUE) {
    dfhycom$datedig[i] = substr(substring(string, regexpr("hycom_gomu_501", string) + 15), 1, 8)
  } else if(grepl("hycom_gomu_901", string, fixed=TRUE) == TRUE) {
    dfhycom$datedig[i] = substr(substring(string, regexpr("hycom_gomu_901", string) + 19), 1, 8)
  } else if(grepl("archv", string, fixed=TRUE) == TRUE) {
    dfhycom$datedig[i] = substr(substring(string, regexpr("hycom_gomu_901", string) + 66), 1, 8)
  } 
}

dfhycom$year = substr(dfhycom$datedig, 1, 4)
dfhycom$month = substr(dfhycom$datedig, 5, 6)
dfhycom$day = substr(dfhycom$datedig, 7, 8)

## Some dates are Year-Day so convert to Year-Month-Day
library(lubridate)

## Build monthly rasters from daily rasters ------------------------------------
for (i in 1:nrow(dfhycom)){
  #i = 7399
  string = dfhycom$datedig[i]
  #string = "2013_010"
  if(grepl("_", string, fixed=TRUE) == TRUE) {
    mo = month(as.Date(as.numeric(substr(string, 6, 8)) - 1, origin = paste0(dfhycom$year[i], "-01-01")))
    da = day  (as.Date(as.numeric(substr(string, 6, 8)) - 1, origin = paste0(dfhycom$year[i], "-01-01")))
    dfhycom$month[i] = sprintf("%02d", mo)
    dfhycom$day[i]   = sprintf("%02d", da)
  }
}  

dfhycom$ym = paste0(dfhycom$year, "-", dfhycom$month)


## -----------------------------------------------------------------------------
##
## Loop through year-months to make rasters

ymuni = unique(dfhycom$ym)
t.surf.m = t.bot.m = t.avg.m = s.surf.m = s.bot.m = s.avg.m = brick()  

## Remove failed openings-------------------------------------------------------
dfhycom$datedig = as.numeric(dfhycom$datedig)
bad = 20011009; which(dfhycom$datedig == bad); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad) ## Check that date is present; subset bad date out; then check to make it's gone
bad = 20040507; which(dfhycom$datedig == bad); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad)
bad = 20040511; which(dfhycom$datedig == bad); nrow(dfhycom); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad); nrow(dfhycom)
bad = 20040512; which(dfhycom$datedig == bad); nrow(dfhycom); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad); nrow(dfhycom)
bad = 20040513; which(dfhycom$datedig == bad); nrow(dfhycom); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad); nrow(dfhycom)
bad = 20040514; which(dfhycom$datedig == bad); nrow(dfhycom); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad); nrow(dfhycom)
bad = 20041017; which(dfhycom$datedig == bad); nrow(dfhycom); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad); nrow(dfhycom)
bad = 20050921; which(dfhycom$datedig == bad); nrow(dfhycom); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad); nrow(dfhycom)
bad = 20050922; which(dfhycom$datedig == bad); nrow(dfhycom); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad); nrow(dfhycom)
bad = 20050924; which(dfhycom$datedig == bad); nrow(dfhycom); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad); nrow(dfhycom)
bad = 20100102; which(dfhycom$datedig == bad); nrow(dfhycom); dfhycom = subset(dfhycom, dfhycom$datedig != bad); which(dfhycom$datedig == bad); nrow(dfhycom)


## Execute loop ----------------------------------------------------------------
for (i in 1:length(ymuni)){
  #i = 241
  ym = ymuni[i]
  mofiles = dfhycom[dfhycom$ym == ym, ]
  print(paste0("Batch ", i, " of ", length(ymuni), " | ", ym, ", #days = ", nrow(mofiles), ", ", Sys.time()))
  
  ## Initialize blank daily and monthly bricks 
  t.surf.d = t.bot.d = t.avg.d = s.surf.d = s.bot.d = s.avg.d = brick()  
  
  ## Loop to make daily rasters
  for(j in 1:nrow(mofiles)){
    #j = 1
    
    ## Convert netcdf to raster brick
    file  = paste0(dir.hycom, basename(mofiles$file[j]))
    yr = as.numeric(substr(ym, 1, 4))
    if(yr == 2013 | yr == 2014 | yr == 2015 | yr == 2016) {
      t.brick  = brick(file, varname='temperature', stopIfNotEqualSpaced = FALSE)
      t.brick  = crop(t.brick, t.surf.m)
      t.brick  = resample(t.brick, t.surf.m)

      s.brick  = brick(file, varname='salinity', stopIfNotEqualSpaced = FALSE)
      s.brick  = crop(s.brick, s.surf.m)
      s.brick  = resample(s.brick, s.surf.m)
      
    } else {
      t.brick  = brick(file, varname='water_temp')
      s.brick  = brick(file, varname='salinity')
    }
    
    ## Fill down to last level, so easier to extract bottom values
    t.brick2 = approxNA(t.brick, method='constant', rule=2)
    s.brick2 = approxNA(s.brick, method='constant', rule=2)
    
    ## Get surface, bottom, and average (temp and sal)
    t.surf = raster(t.brick,layer=1)
    t.bot  = raster(t.brick2,layer=nlayers(t.brick))
    t.avg  = calc(t.brick,mean,na.rm=T)
    s.surf = raster(s.brick,layer=1)
    s.bot  = raster(s.brick2,layer=nlayers(t.brick))
    s.avg  = calc(s.brick,mean,na.rm=T)
    
    ## Add to daily stack
    t.surf.d = addLayer(t.surf.d, t.surf)
    t.bot.d  = addLayer(t.bot.d,t.bot)
    t.avg.d  = addLayer(t.avg.d,t.avg)
    s.surf.d = addLayer(s.surf.d,s.surf)
    s.bot.d  = addLayer(s.bot.d,s.bot)
    s.avg.d  = addLayer(s.avg.d,s.avg)
    
    #housekeeping
    rm(t.brick, t.brick2, t.surf, t.bot, t.avg, s.brick, s.brick2, s.surf, s.bot, s.avg)
    gc(); flush.console()
  } ## Finish building daily brick for that ym
  
  ## Monthly avg ---------------------------------------------------------------
  t.surf.m = addLayer(t.surf.m, calc(t.surf.d, mean))
  t.bot.m  = addLayer(t.bot.m, calc(t.bot.d, mean))
  t.avg.m  = addLayer(t.avg.m,calc(t.avg.d, mean))
  s.surf.m = addLayer(s.surf.m,calc(s.surf.d,mean))
  s.bot.m  = addLayer(s.bot.m,calc(s.bot.d,mean))
  s.avg.m  = addLayer(s.avg.m,calc(s.avg.d,mean))
  
  names(t.surf.m)[i] = names(t.bot.m)[i] = names(t.avg.m)[i] = ym
  names(s.surf.m)[i] = names(s.bot.m)[i] = names(s.avg.m)[i] = ym
  
  ## Housekeeping
  rm(t.surf.d, t.bot.d, t.avg.d, s.surf.d, s.avg.d, s.bot.d)
}   

## Plot check
plot(t.surf.m[[1:12]], col = terrain.colors(100), ylim = c(15,32))


## -----------------------------------------------------------------------------
##
## 5) Write out rasterstacks 

dir.out = "./maps/HYCOM/Bricks/"

## Write full GoM raster stacks to file
datelabel = "1993-01 to 2020-12"

## Write out monthly stacks
writeRaster(t.surf.m, paste0(dir.out, 'HYCOM GOM temp surface ', datelabel), overwrite=TRUE)
writeRaster(t.bot.m,  paste0(dir.out, 'HYCOM GOM temp bottom ', datelabel), overwrite=TRUE)
writeRaster(t.avg.m,  paste0(dir.out, 'HYCOM GOM temp avg ', datelabel), overwrite=TRUE)
writeRaster(s.surf.m, paste0(dir.out, 'HYCOM GOM salinity surface ', datelabel), overwrite=TRUE)
writeRaster(s.bot.m,  paste0(dir.out, 'HYCOM GOM salinity bottom ', datelabel), overwrite=TRUE)
writeRaster(s.avg.m,  paste0(dir.out, 'HYCOM GOM salinity avg ', datelabel), overwrite=TRUE)