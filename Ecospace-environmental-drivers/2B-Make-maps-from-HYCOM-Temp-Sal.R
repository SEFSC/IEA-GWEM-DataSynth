## -----------------------------------------------------------------------------
##
## Write out ASCII files for Ecospace 

rm(list=ls()); rm(.SavedPlots); graphics.off(); gc(); windows(record=T)
library(terra)
library(viridis)

datelabel = "1993-01 to 2020-12"
dir.in = "./Ecospace-environmental-drivers/HYCOM/"

## HIRES monthly stacks
t.surf.hycom = stack( paste0(dir.in, 'HYCOM GOM temp surface ', datelabel))
t.bot.hycom  = stack( paste0(dir.in, 'HYCOM GOM temp bottom ', datelabel))
t.avg.hycom  = stack( paste0(dir.in, 'HYCOM GOM temp avg ', datelabel))
s.surf.hycom = stack( paste0(dir.in, 'HYCOM GOM salinity surface ', datelabel))
s.bot.hycom  = stack( paste0(dir.in, 'HYCOM GOM salinity bottom ', datelabel))
s.avg.hycom  = stack( paste0(dir.in, 'HYCOM GOM salinity avg ', datelabel))

## Resampled and smoothed stacks
t.surf.smoo  = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM temp surface 1993-01 to 2020-12.grd"))
t.bot.smoo   = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM temp bottom 1993-01 to 2020-12.grd"))
t.avg.smoo   = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM temp avg 1993-01 to 2020-12.grd"))
s.surf.smoo  = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM salinity surface 1993-01 to 2020-12.grd"))
s.bot.smoo   = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM salinity bottom 1993-01 to 2020-12.grd"))
s.avg.smoo   = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM salinity avg 1993-01 to 2020-12.grd"))

## Calculate average to intialize Ecospace -------------------------------------
avg.t.surf = calc(t.surf.smoo, mean)
avg.t.bot  = calc(t.bot.smoo,  mean)
avg.t.avg  = calc(t.avg.smoo,  mean)
avg.s.surf = calc(s.surf.smoo, mean)
avg.s.bot  = calc(s.bot.smoo,  mean)
avg.s.avg  = calc(s.avg.smoo,  mean)
#avg.t.avg = stackApply(t.avg.smoo, indices =  rep(1, nlayers(t.avg.smoo)), fun = "mean", na.rm = T)

## Plot check
par(mfrow=c(3,2))
plot(avg.t.surf, colNA='black', main = "Temp surf")
plot(avg.t.bot,  colNA='black',  main = "Temp bot")
plot(avg.t.avg,  colNA='black',  main = "Temp avg")
plot(avg.s.surf, colNA='black', main = "Sal surf")
plot(avg.s.bot,  colNA='black',  main = "Sal bot")
plot(avg.s.avg,  colNA='black',  main = "Sal avg")
par(mfrow=c(1,1)) 

## Save average GOM ascii layers for Ecospace ----------------------------------
dir.asci = "./maps/HYCOM/ASCII/"
writeRaster(avg.t.surf, paste0(dir.asci,"Averaged//avg_temp_surf"), format='ascii', overwrite=T)
writeRaster(avg.t.bot,  paste0(dir.asci,"Averaged//avg_temp_bot"),  format='ascii', overwrite=T)
writeRaster(avg.t.avg,  paste0(dir.asci,"Averaged//avg_temp_avg"),  format='ascii', overwrite=T)
writeRaster(avg.s.surf, paste0(dir.asci,"Averaged//avg_saln_surf"), format='ascii', overwrite=T)
writeRaster(avg.s.bot,  paste0(dir.asci,"Averaged//avg_saln_bot"),  format='ascii', overwrite=T)
writeRaster(avg.s.avg,  paste0(dir.asci,"Averaged//avg_saln_avg"),  format='ascii', overwrite=T)

## -----------------------------------------------------------------------------
##
## Make ASCII files (with replicates of monthly avg before data starts) 
## for the Ecospace
## HYCOM data start in 1993, so we need to make dummy copies per month from 
## Jan 1980 to Dec 1993

dir.out.brick = "./maps/HYCOM/Bricks/3 Final stacks/"

## -----------------------------------------------------------------------------
## 1) Surface temperature

## Input parameters-------------------------------------
ras   = t.surf.smoo
hires = t.surf.hycom
dir.asc.out = "./maps/HYCOM/ASCII/Temperature surface/"
env_driver = "Temp_surface"
## -----------------------------------------------------

## Make dataframe of dates from raster layers
ras.dates = data.frame(year=substr(names(hires),2,5),month=substr(names(hires),7,8))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
mo = unique(ras.dates$month)

## Make dataframe of year months before HYCOM data starts
enddummy = min(as.numeric(ras.dates$year))-1
yr = 1980:enddummy
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")
head(dummy.dates); tail(dummy.dates)

## Get monthly averages -------------------------------------------------------
nlayers(month.stack) ## Should be 28 or so
month.stack = stack()
for (month in mo){
  #month = "01"
  subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
  month.avg = calc(subset.month, mean)
  names(month.avg) = paste0("Mo", month, "_avg", nlayers(subset.month),"y")
  month.stack = addLayer(month.stack, month.avg)
}  

## Plot check
par(mfrow=c(3,4))
plot(month.stack, colNA = 'black', 
     zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
)
par(mfrow=c(1,1))
names(ras.comb)

## Write out raster
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))
writeRaster(ras.comb, paste0(dir.out.brick, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)

## Write out ASCII files
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), format = 'ascii', overwrite=T)

## -----------------------------------------------------------------------------
## 2) Bottom temperature

## Input parameters-------------------------------------
ras   = t.bot.smoo
hires = t.bot.hycom
dir.asc.out = "./maps/HYCOM/ASCII/Temperature bottom/"
env_driver = "Temp_bottom"
## -----------------------------------------------------

## Make dataframe of dates from raster layers
ras.dates = data.frame(year=substr(names(hires),2,5),month=substr(names(hires),7,8))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
mo = unique(ras.dates$month)

## Make dataframe of year months before HYCOM data starts
enddummy = min(as.numeric(ras.dates$year))-1
yr = 1980:enddummy
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")
head(dummy.dates); tail(dummy.dates)

## Get monthly averages -------------------------------------------------------
nlayers(month.stack) ## Should be 28 or so
month.stack = stack()
for (month in mo){
  #month = "01"
  subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
  month.avg = calc(subset.month, mean)
  names(month.avg) = paste0("Mo", month, "_avg", nlayers(subset.month),"y")
  month.stack = addLayer(month.stack, month.avg)
}  

## Plot check
par(mfrow=c(3,4))
plot(month.stack, colNA = 'black', 
     zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
)

## Combine dummy raster set (1980-1992) and data (1993-2022)
rep.stack = stack()
for (year in yr){
  #year = 1980
  xx = month.stack
  names(xx) = paste0(year, "_", substr(labels(month.stack), 3, 8))
  rep.stack = addLayer(rep.stack, xx)
}

ras.comb = stack(rep.stack, ras)

## Write out raster
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))
writeRaster(ras.comb, paste0(dir.out.brick, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)

## Write out ASCII files
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, 
            suffix = names(ras.comb), format = 'ascii', overwrite=T)



## -----------------------------------------------------------------------------
## 3) Average temperature

## Input parameters-------------------------------------
ras   = t.avg.smoo
hires = t.avg.hycom
dir.asc.out = "./maps/HYCOM/ASCII/Temperature average/"
env_driver = "Temp_avg"
## -----------------------------------------------------

## Make dataframe of dates from raster layers
ras.dates = data.frame(year=substr(names(hires),2,5),month=substr(names(hires),7,8))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
mo = unique(ras.dates$month)

## Make dataframe of year months before HYCOM data starts
enddummy = min(as.numeric(ras.dates$year))-1
yr = 1980:enddummy
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")
head(dummy.dates); tail(dummy.dates)

## Get monthly averages -------------------------------------------------------
nlayers(month.stack) ## Should be 28 or so
month.stack = stack()
for (month in mo){
  #month = "01"
  subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
  month.avg = calc(subset.month, mean)
  names(month.avg) = paste0("Mo", month, "_avg", nlayers(subset.month),"y")
  month.stack = addLayer(month.stack, month.avg)
}  

## Plot check
par(mfrow=c(3,4))
plot(month.stack, colNA = 'black', 
     zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
)

## Combine dummy raster set (1980-1992) and data (1993-2022)
rep.stack = stack()
for (year in yr){
  #year = 1980
  xx = month.stack
  names(xx) = paste0(year, "_", substr(labels(month.stack), 3, 8))
  rep.stack = addLayer(rep.stack, xx)
}

ras.comb = stack(rep.stack, ras)

## Write out raster
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))
writeRaster(ras.comb, paste0(dir.out.brick, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)


## Write out ASCII files
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, 
            suffix = names(ras.comb), format = 'ascii', overwrite=T)

## -----------------------------------------------------------------------------
## 4) Surface salinity

## Input parameters-------------------------------------
ras   = s.surf.smoo
hires = s.surf.hycom
dir.asc.out = "./maps/HYCOM/ASCII/Salinity surface/"
env_driver = "Sal_surface"
## -----------------------------------------------------

## Make dataframe of dates from raster layers
ras.dates = data.frame(year=substr(names(hires),2,5),month=substr(names(hires),7,8))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
mo = unique(ras.dates$month)

## Make dataframe of year months before HYCOM data starts
enddummy = min(as.numeric(ras.dates$year))-1
yr = 1980:enddummy
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")
head(dummy.dates); tail(dummy.dates)

## Get monthly averages -------------------------------------------------------
nlayers(month.stack) ## Should be 28 or so
month.stack = stack()
for (month in mo){
  #month = "01"
  subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
  month.avg = calc(subset.month, mean)
  names(month.avg) = paste0("Mo", month, "_avg", nlayers(subset.month),"y")
  month.stack = addLayer(month.stack, month.avg)
}  

## Plot check
par(mfrow=c(3,4))
plot(month.stack, colNA = 'black', 
     zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
)

## Combine dummy raster set (1980-1992) and data (1993-2022)
rep.stack = stack()
for (year in yr){
  #year = 1980
  xx = month.stack
  names(xx) = paste0(year, "_", substr(labels(month.stack), 3, 8))
  rep.stack = addLayer(rep.stack, xx)
}

ras.comb = stack(rep.stack, ras)

## Write out raster
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))
writeRaster(ras.comb, paste0(dir.out.brick, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)


## Write out ASCII files
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, 
            suffix = names(ras.comb), format = 'ascii', overwrite=T)

## -----------------------------------------------------------------------------
## 5) Bottom salinity

## Input parameters-------------------------------------
ras   = s.bot.smoo
hires = s.bot.hycom
dir.asc.out = "./maps/HYCOM/ASCII/Salinity bottom/"
env_driver = "Sal_bottom"
## -----------------------------------------------------

## Make dataframe of dates from raster layers
ras.dates = data.frame(year=substr(names(hires),2,5),month=substr(names(hires),7,8))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
mo = unique(ras.dates$month)

## Make dataframe of year months before HYCOM data starts
enddummy = min(as.numeric(ras.dates$year))-1
yr = 1980:enddummy
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")
head(dummy.dates); tail(dummy.dates)

## Get monthly averages -------------------------------------------------------
nlayers(month.stack) ## Should be 28 or so
month.stack = stack()
for (month in mo){
  #month = "01"
  subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
  month.avg = calc(subset.month, mean)
  names(month.avg) = paste0("Mo", month, "_avg", nlayers(subset.month),"y")
  month.stack = addLayer(month.stack, month.avg)
}  

## Plot check
par(mfrow=c(3,4))
plot(month.stack, colNA = 'black', 
     zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
)

## Combine dummy raster set (1980-1992) and data (1993-2022)
rep.stack = stack()
for (year in yr){
  #year = 1980
  xx = month.stack
  names(xx) = paste0(year, "_", substr(labels(month.stack), 3, 8))
  rep.stack = addLayer(rep.stack, xx)
}

ras.comb = stack(rep.stack, ras)

## Write out raster
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))
writeRaster(ras.comb, paste0(dir.out.brick, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)


## Write out ASCII files
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, 
            suffix = names(ras.comb), format = 'ascii', overwrite=T)

## -----------------------------------------------------------------------------
## 6) Average salinity

## Input parameters-------------------------------------
ras   = s.avg.smoo
hires = s.avg.hycom
dir.asc.out = "./maps/HYCOM/ASCII/Salinity average/"
env_driver = "Sal_average"
## -----------------------------------------------------

## Make dataframe of dates from raster layers
ras.dates = data.frame(year=substr(names(hires),2,5),month=substr(names(hires),7,8))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
mo = unique(ras.dates$month)

## Make dataframe of year months before HYCOM data starts
enddummy = min(as.numeric(ras.dates$year))-1
yr = 1980:enddummy
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")
head(dummy.dates); tail(dummy.dates)

## Get monthly averages -------------------------------------------------------
nlayers(month.stack) ## Should be 28 or so
month.stack = stack()
for (month in mo){
  #month = "01"
  subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
  month.avg = calc(subset.month, mean)
  names(month.avg) = paste0("Mo", month, "_avg", nlayers(subset.month),"y")
  month.stack = addLayer(month.stack, month.avg)
}  

## Plot check
par(mfrow=c(3,4))
plot(month.stack, colNA = 'black', 
     zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
)

## Combine dummy raster set (1980-1992) and data (1993-2022)
rep.stack = stack()
for (year in yr){
  #year = 1980
  xx = month.stack
  names(xx) = paste0(year, "_", substr(labels(month.stack), 3, 8))
  rep.stack = addLayer(rep.stack, xx)
}

ras.comb = stack(rep.stack, ras)

## Write out raster
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))
writeRaster(ras.comb, paste0(dir.out.brick, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)


## Write out ASCII files
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, 
            suffix = names(ras.comb), format = 'ascii', overwrite=T)


################################################################################
##
## MAKE PDF MAPS

pdf_map(ras.comb, colscheme = 'virid', dir = dir.out, env_name = env_driver, 
        modtype = "HYCOM", strt=1980, stop=2020)
