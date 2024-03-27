rm(list=ls());gc()

library(curl)
library(raster)
library(rerddap)
library(ncdf4)

##------------------------------------------------------------------------------
## Set-up variable list to query ERDDAP

bbox.gom = c(-98, -80.5, 24, 31)
bbox = bbox.gom

dir.modis = "./Ecospace-environmental-drivers/MODIS/"
vars = c("cfl", "chla", "PIC", "POC")
overwrite = 'n' ## Do we want to over write?

for (i in 1:length(vars)) {
  dir = paste0(dir.modis, vars[i])
  if(file.exists(dir) & overwrite == 'n') print(paste("Already exists: ", dir))
  else {
    unlink(dir, recursive = TRUE)
    dir.create(dir)
    print(paste("Make file: ", dir))
   }
 }

dir.vars  = list.dirs(dir.modis, recursive=F)
varlist   = data.frame(var = basename (dir.vars), dir=dir.vars)
print(varlist$var)

varlist$datasetID_month = c('erdMH1chlamday', 'erdMH1cflhmday', 'erdMPICmday', 'erdMPOCmday')
varlist$datasetID_day =   c('erdMH1chla1day', 'erdMH1cflh1day', 'erdMPIC1day', 'erdMPOC1day')

#varlist$datasetID_month = c('erdMH1cflhmday', 'erdMH1chlamday', 'erdMPICmday')
#varlist$datasetID_day =   c('erdMH1cflh1day', 'erdMH1chla1day', 'erdMPIC1day')
#varlist = varlist[-nrow(varlist),] 
print(varlist)


##------------------------------------------------------------------------------
## PROCESS MODIS DATA FROM NOAA ERDDAP SERVER 
## https://coastwatch.pfeg.noaa.gov/erddap/index.html

for(i in 1:nrow(varlist)){
  #  i=1
  varname = varlist$var[i]
  print(paste("ERRDAP pull: ", varname, "| Start time: ", format(Sys.time(), "%H:%M")))
  
  dir.out = varlist$dir[i]
  dat.id.m = varlist$datasetID_month[i]
  dat.id.d = varlist$datasetID_day[i]
  #setwd(dir.out)
  
  ## Get monthly composite imagery----------------------------------------------
  erd.info1 <- griddap(info(dat.id.m),
                       latitude = c('last','last'),
                       longitude = c('last','last'))
  erd.time1 = data.frame(time=erd.info1$data$time)
  erd.time1$month = substr(erd.time1$time,6,7)
  erd.time1$yrmo = gsub("-","",substr(erd.time1$time,1,7))

  ## GOM grid (big pull takes awhile)
  erd1 <- griddap(info(dat.id.m),
                  latitude = bbox[3:4],
                  longitude = bbox[1:2],
                  time = erd.time1$time[c(1,nrow(erd.time1))])
  
  ## Monthly raster stack
  s.m = stack(erd1$summary$filename,quick=T)#,varname='chlorophyll'
  names(s.m) = erd.time1$yrmo
  head(erd.time1$yrmo); tail(erd.time1$yrmo) ## Check dates
  
  ## Daily imagery---------------------------------------------------------------
  erd.info2 <- griddap(info(dat.id.d),
                       latitude = c('last','last'),
                       longitude = c('last','last'))
  erd.time2 = data.frame(time=erd.info2$data$time)
  erd.time2$month = substr(erd.time2$time,6,7)
  erd.time2$yrmo = gsub("-","",substr(erd.time2$time,1,7))
  erd.time2 = erd.time2[which(!erd.time2$yrmo %in% erd.time1$yrmo),]
  
  ## GOM grid (big pulls take awhile)
  erd2 <- griddap(info(dat.id.d),
                  latitude = bbox[3:4],
                  longitude = bbox[1:2],
                  time = erd.time2$time[c(1,nrow(erd.time2))])
  
  ## Daily raster stack
  s.d = stack(erd2$summary$filename,quick=T)#,varname='chlorophyll'
  
  ## Monthly average
  s.m2 = stackApply(s.d,indices=erd.time2$yrmo,fun=mean,na.rm=T)
  names(s.m2) = unique(erd.time2$yrmo)
  
  ## Add to monthly stack
  s.m3 = stack(s.m,s.m2)
  names(s.m3)
  
  ## Write raster
  writeRaster(s.m3,overwrite=T,
              paste0(dir.out,"/",varname,'_',paste(bbox,collapse="_"),"_",gsub("X","",names(s.m3)[1]),"-",gsub("X","",names(s.m3)[nlayers(s.m3)])))
  
  ## Integrate surface cholorphyll over euphotic depth--------------------------
  if(varname == 'chla'){
    Zd.m = calc(s.m3, function(x) 34*(x^-0.39)) #Lee (2007) z1%
    
    ## Total cholorphyll in euphotic zone from surface chlorophyll: 
    ## equations 3b and 3c in Morel and Berthon (1989)
    Ctot = calc(s.m3,function(x) ifelse(x<=1,38*x^0.423,40.3*x^0.505))  
    
    ## Euphotic depth calculated from total chlorophyll: 
    ## Equation 6 in Morel and Maritorena (2001), updated equations from M&B 1989 
    Zehat1 = 912.5*Ctot^-0.839  #for Ze<102m
    Zehat2 = 426.3*Ctot^-0.547  #for Ze>102m
    
    ## If Ze is > 102, replace with second equation
    Zehat = Zehat1
    Zehat[] = ifelse(Zehat1[]>102,Zehat2[],Zehat[])
    
    ## Mean concentration in euphotic depth
    Cze = Ctot/Zehat
    
    ## Integrated over true euphotic depth from satellite
    Cint = Cze*Zd.m
    names(Cint) = names(s.m3)
    
    ## Write raster
    writeRaster(Cint,overwrite=T,
                paste0(dir.out,"/chlaxZe_",paste(bbox,collapse="_"),"_",gsub("X","",names(s.m3)[1]),"-",gsub("X","",names(s.m3)[nlayers(s.m3)])))
  }
  #rm(s.d,s.m,s.m2,s.m3,erd1,erd2,Zd.m,Ctot,Zehat1,Zehat2,Zehat,Cze,Cint,erd.info1,erd.info2,erd.time1,erd.time2);gc()
}

## References ------------------------------------------------------------------
## Lee et al., 2007
## Z.P. Lee, A. Weideman, J. Kindle, R. Arnone, K. Carder, C. Davis
## Euphotic zone depth: its derivation and implication to ocean-color remote sensing
## Journal of Geophysical Research, 112 (2007), p. C03009
## https://doi.org/10.1029/2006JC003802

## Morel, A., and J. F. Berthon (1989), 
## Surface pigments, algal biomass profiles, and potential production of the euphotic layer: relationships reinvestigated in review of remote-sensing applications, Limnol. Oceanogr., 34, 1545–1562.
## https://doi.org/10.4319/lo.1989.34.8.1545

## Morel, A. and Maritorena, S., 2001. 
## Bio‐optical properties of oceanic waters: A reappraisal. Journal of Geophysical Research: Oceans, 106(C4), pp.7163-7180.
## https://doi.org/10.1029/2000JC000319