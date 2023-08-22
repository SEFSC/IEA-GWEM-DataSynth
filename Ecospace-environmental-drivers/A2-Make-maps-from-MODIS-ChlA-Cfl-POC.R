rm(list=ls());rm(.SavedPlots);graphics.off();gc();windows(record=T)

library(terra)
library(stringr)
source("./Ecospace-environmental-drivers/0-Make-PDF-maps-function.R") ## Call pdf_map function

##------------------------------------------------------------------------------
##
## Set up directory paths
overwrite = 'n'
dir.ras.in   <- "./Ecospace-environmental-drivers/MODIS/"
dir.ras.out  <- "./Ecospace-environmental-drivers/Outputs/Bricks/"
fld.asc.out  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/"
dir.asc.avg  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/Averages/"
dir.pdf.out  <- "./Ecospace-environmental-drivers/Outputs/PDF-maps/"
depth08min   <- raster("./global-data/shorelinecorrected-basemap-depth-131x53-08 min-14sqkm.asc")
date_coord_range <- gsub("^[^_]*_|\\.gri$", "", list.files(paste0(dir.ras.in, "/chla"), pattern = "\\.gri$")[1])

## Make directories
dirs = c(dir.ras.out, fld.asc.out, dir.asc.avg, dir.pdf.out)
for (i in 1:length(dirs)){
  dir = dirs[i]
  if(overwrite == 'y') unlink(dir, recursive = TRUE) ## Overwrite if 'y'
  dir.create(dir)
}

##------------------------------------------------------------------------------
##
## Make Ecospace environmental-drivers files for ChlA integrated euphotic depth
## https://modis.gsfc.nasa.gov/data/dataprod/chlor_a.php
## ChlA represents near-surface concentration of chlorophyll-a (chlor_a) in mg m-3, calculated using an empirical relationship derived from in situ measurements of chlor_a and blue-to-green band ratios of in situ remote sensing reflectances (Rrs).

env_driver = "ChlA"
overwrite  = 'y'
dir.asc.out = paste0(fld.asc.out, env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 

## Read in stack
ras_hires = stack(paste0(dir.ras.in, tolower(env_driver), "/", tolower(env_driver), "xZe", "_", date_coord_range))

## First, crop and resample to basemap grid 
ras = crop(ras_hires, depth08min)
ras = resample(ras, depth08min)
dim(depth08min); dim(ras) ## Check that dimensions should match but with different number of layers

## Plotcheck
n_months = dim(ras)[3]
par(mfrow=c(2,2))
plot(depth08min, colNA = 'black', main = "Depth")
plot(ras[[1]], colNA = 'black', main = paste("First month", names(ras[[1]])))
plot(ras[[100]], colNA = 'black', main = paste("100th month", names(ras[[100]])))
plot(ras[[n_months]], colNA = 'black', main = paste("Last month", names(ras[[n_months]])))
par(mfrow=c(1,1))

## Determine which ones need to be written out
ras.dates = data.frame(year=substr(names(ras),2,5),month=substr(names(ras),6,7))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
asc.need = ras.dates$yrmo

## Average for intial map 
mean <- stackApply(ras, indices =  rep(1,nlayers(ras)), fun = "mean", na.rm = T)

## Plot global average 
par(mfrow=c(2,1)) 
plot(mean, colNA = "black", main = "Averaged Chla Euphotic Depth")
plot(log10(mean), colNA = "black", main = "Log10 Chla Euphotic Depth")
par(mfrow=c(1,1))

## Create dummy months to be filled later by monthly means: 
## copy mean by month for months before data -----------------------------------
mo = unique(ras.dates$month)
enddummy = min(as.numeric(ras.dates$year))-1
yr = 1980:enddummy

## Make dataframe of year months before MODIS data starts
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$ym = paste(dummy.dates$year, dummy.dates$month, sep="-")
head(dummy.dates); tail(dummy.dates)

## Replicate average for months Jan 1980 until MODIS data starts in 2003
dummy.ras = brick(replicate(nrow(dummy.dates), mean))
names(dummy.ras) = dummy.dates$ym

## Rename layers: e.g., "X200301" to "X2003.01" to extract months
layers = names(ras_hires)
names(ras) = sub("(.{5})(.*)", "\\1.\\2", layers) ## Capture the first 5 characters as a group ((.{5})) followed by one or more characters in another capture group ((.*)) and then replace with the backreference of first group (\\1) followed by a . followed by second backreference (\\2).
names(ras)

## Get monthly averages --------------------------------------------------------
month.stack = stack()
for (month in mo){
  #month = "01"
  subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
  names(subset.month)
  month.avg = stackApply(subset.month, indices =  rep(1,nlayers(subset.month)), fun = "mean", na.rm = T)
  names(month.avg) = paste0("Mo", month, "_avg", nlayers(subset.month),"y")
  month.stack = addLayer(month.stack, month.avg)
}  

## Plot check
par(mfrow=c(3,4))
plot(month.stack, colNA = 'black', 
     zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
)
par(mfrow=c(1,1))

## Combine dummy raster set (1980-1992) and data (1993-2022) -------------------
rep.stack = stack()
for (year in yr){
  #year = 1980
  xx = month.stack
  names(xx) = paste0(year, "_", substr(labels(month.stack), 3, 8))
  rep.stack = addLayer(rep.stack, xx)
}

## Combine stacks: monthly averages (rep.stack) and monthy data (ras)
ras.comb = stack(rep.stack, ras)
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))

## Write out files -------------------------------------------------------------
## Save raster
writeRaster(ras.comb, paste0(dir.ras.out, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)

## Make PDF of plots
## Set plotting maximum to maximum of 99th percentile by month
pdf_map(ras.comb, colscheme = 'virid', dir = dir.pdf.out, 
        env_name = env_driver, mintile = 'zero', maxtile = 0.99, modtype = "MODIS")

## ASCII files by month
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
            format = 'ascii', overwrite=T)

## ASCII global average
writeRaster(mean, paste0(dir.asc.avg, 'Avg_', env_driver),
            bylayer=F, format='ascii', overwrite=T)


## -----------------------------------------------------------------------------
##
## Cfl: carbon florescence
## Fluorescence Line Height, Aqua MODIS

env_driver = "Cfl"
overwrite  = 'n'
dir.asc.out = paste0(fld.asc.out, env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 

## Read in stack
ras_hires = stack(paste0(dir.ras.in, tolower(env_driver), "/", tolower(env_driver), "_", date_coord_range))

## First, crop and resample to basemap grid 
ras = crop(ras_hires, depth08min)
ras = resample(ras, depth08min)
dim(depth08min); dim(ras) ## Dimensions should match

## Determine which ones need to be written out
ras.dates = data.frame(year=substr(names(ras),2,5),month=substr(names(ras),6,7))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
asc.need = ras.dates$yrmo

## Average for intial map 
mean <- stackApply(ras, indices =  rep(1,nlayers(ras)), fun = "mean", na.rm = T)

## Plotcheck
par(mfrow=c(2,2))
plot(depth08min, colNA = 'black', main="Base/depth map")
plot(ras[[1]], colNA = 'black', main = paste(env_driver, "first month"))
plot(mean, colNA = "black", main = paste(env_driver, "global average"))
par(mfrow=c(1,1))

## Create dummy months to be filled later by monthly means: 
## copy mean by month for months before data -----------------------------------
mo = unique(ras.dates$month)
enddummy = min(as.numeric(ras.dates$year))-1
yr = 1980:enddummy

## Make dataframe of year months before MODIS data starts
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$ym = paste(dummy.dates$year, dummy.dates$month, sep="-")
head(dummy.dates); tail(dummy.dates)

## Replicate average for months Jan 1980 until MODIS data starts in 2003
dummy.ras = brick(replicate(nrow(dummy.dates), mean))
names(dummy.ras) = dummy.dates$ym

## Rename layers: e.g., "X200301" to "X2003.01" to extract months
layers = names(ras_hires)
names(ras) = sub("(.{5})(.*)", "\\1.\\2", layers) ## Capture the first 5 characters as a group ((.{5})) followed by one or more characters in another capture group ((.*)) and then replace with the backreference of first group (\\1) followed by a . followed by second backreference (\\2).
names(ras)

## Get monthly averages --------------------------------------------------------
month.stack = stack()
for (month in mo){
  #month = "01"
  subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
  names(subset.month)
  month.avg = stackApply(subset.month, indices =  rep(1,nlayers(subset.month)), fun = "mean", na.rm = T)
  names(month.avg) = paste0("Mo", month, "_avg", nlayers(subset.month),"y")
  month.stack = addLayer(month.stack, month.avg)
}  

## Plot check
par(mfrow=c(3,4))
plot(month.stack, colNA = 'black', 
     zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
)
par(mfrow=c(1,1))

## Combine dummy raster set (1980-2002) and data (2003-2022) -------------------
rep.stack = stack()
for (year in yr){
  #year = 1980
  xx = month.stack
  names(xx) = paste0(year, "_", substr(labels(month.stack), 3, 8))
  rep.stack = addLayer(rep.stack, xx)
}

## Combine stacks: monthly averages (rep.stack) and monthy data (ras)
ras.comb = stack(rep.stack, ras)
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))

## Write out files -------------------------------------------------------------
## Make PDF of plots
## Set plotting maximum to maximum of 99th percentile by month
pdf_map(ras.comb, colscheme = 'virid', dir = dir.pdf.out, 
        env_name = env_driver, mintile = 0.01, maxtile = 0.99, modtype = "MODIS")


## Save raster
writeRaster(ras.comb, paste0(dir.ras.out, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)

## ASCII files by month
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
            format = 'ascii', overwrite=T)

## ASCII global average
writeRaster(mean, paste0(dir.asc.avg, 'Avg_', env_driver),
            bylayer=F, format='ascii', overwrite=T)


## -----------------------------------------------------------------------------
##
## POC: Particulate organic carbon concentration
## Concentration of particulate organic carbon (POC) in mg m-3, calculated using an empirical relationship derived from in situ measurements of POC and blue-to-green band ratios of remote sensing reflectances (Rrs).
## https://modis.gsfc.nasa.gov/data/dataprod/poc.php

env_driver = "POC"
overwrite  = 'y'
dir.asc.out = paste0(fld.asc.out, env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 

## Read in stack
ras_hires = stack(paste0(dir.ras.in, tolower(env_driver), "/", tolower(env_driver), "_", date_coord_range))

## First, crop and resample to basemap grid 
ras = crop(ras_hires, depth08min)
ras = resample(ras, depth08min)
dim(depth08min); dim(ras) ## Dimensions should match

## Determine which ones need to be written out
ras.dates = data.frame(year=substr(names(ras),2,5),month=substr(names(ras),6,7))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
asc.need = ras.dates$yrmo

## Average for intial map 
mean <- stackApply(ras, indices =  rep(1,nlayers(ras)), fun = "mean", na.rm = T)

## Plotcheck
par(mfrow=c(2,2))
plot(depth08min, colNA = 'black', main="Base/depth map")
plot(ras[[1]], colNA = 'black', main = paste(env_driver, "first month"))
plot(mean, colNA = "black", main = paste(env_driver, "global average"))
par(mfrow=c(1,1))

## Create dummy months to be filled later by monthly means: 
## copy mean by month for months before data -----------------------------------
mo = unique(ras.dates$month)
enddummy = min(as.numeric(ras.dates$year))-1
yr = 1980:enddummy

## Make dataframe of year months before MODIS data starts
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$ym = paste(dummy.dates$year, dummy.dates$month, sep="-")
head(dummy.dates); tail(dummy.dates)

## Replicate average for months Jan 1980 until MODIS data starts in 2003
dummy.ras = brick(replicate(nrow(dummy.dates), mean))
names(dummy.ras) = dummy.dates$ym

## Rename layers: e.g., "X200301" to "X2003.01" to extract months
layers = names(ras_hires)
names(ras) = sub("(.{5})(.*)", "\\1.\\2", layers) ## Capture the first 5 characters as a group ((.{5})) followed by one or more characters in another capture group ((.*)) and then replace with the backreference of first group (\\1) followed by a . followed by second backreference (\\2).
names(ras)

## Get monthly averages --------------------------------------------------------
month.stack = stack()
for (month in mo){
  #month = "01"
  subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
  names(subset.month)
  month.avg = stackApply(subset.month, indices =  rep(1,nlayers(subset.month)), fun = "mean", na.rm = T)
  names(month.avg) = paste0("Mo", month, "_avg", nlayers(subset.month),"y")
  month.stack = addLayer(month.stack, month.avg)
}  

## Plot check
par(mfrow=c(3,4))
plot(month.stack, colNA = 'black', 
     zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
)
par(mfrow=c(1,1))

## Combine dummy raster set (1980-2002) and data (2003-2022) -------------------
rep.stack = stack()
for (year in yr){
  #year = 1980
  xx = month.stack
  names(xx) = paste0(year, "_", substr(labels(month.stack), 3, 8))
  rep.stack = addLayer(rep.stack, xx)
}

## Combine stacks: monthly averages (rep.stack) and monthy data (ras)
ras.comb = stack(rep.stack, ras)
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))

## Write out files -------------------------------------------------------------
## Save raster
writeRaster(ras.comb, paste0(dir.ras.out, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)

## Make PDF of plots
## Set plotting maximum to maximum of 99th percentile by month
pdf_map(ras.comb, colscheme = 'virid', dir = dir.pdf.out, 
        env_name = env_driver, mintile = 0.01, maxtile = 0.99, modtype = "MODIS")

## ASCII files by month
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
            format = 'ascii', overwrite=T)

## ASCII global average
writeRaster(mean, paste0(dir.asc.avg, 'Avg_', env_driver),
            bylayer=F, format='ascii', overwrite=T)


