rm(list=ls());rm(.SavedPlots);graphics.off();gc();windows(record=T)

library('terra')
library('marmap')
library('stringr')
library('viridis')


## -----------------------------------------------------------------------------
##
## FUNCTION TO MAKE PDFS

## Function to make file name---------------------------------------------------
outname = function(stack, dir, env_name){
  #  stack = ras
  #  dir = dir.asc.out
  #  env_name = "CHLA"
  
  strt = str_sub(names(stack)[1], -7)
  stop = str_sub(names(stack)[nlayers(stack)], -7)
  out = gsub("X","", paste0(dir, env_name, "_", strt,"-", stop))
  return(out)
}

## Function to make PDF of maps by year-----------------------------------------
pdf_map = function(plt.stack, colscheme = 'brks', dir = './', env_name = '', modtype = '', 
                   mintile = 'zero', maxtile = 0.99){
  #  plt.stack = t.surf
  #  colscheme = 'turbo'
  #  dir = dir.out
  #  env_name = env_driver
  #  modtype = "MODIS"
  #  mintile = 0.01; maxtile = 0.99
  
  maxval = as.integer(round(quantile(values(plt.stack), maxtile, na.rm=T), 0)) 
  minval = ifelse (mintile == 'zero', 0,
                   as.integer(round(quantile(values(plt.stack), mintile, na.rm=T), 0))
                   )
  print(paste(env_name, "plotting range:", minval, "-", maxval))
  #maxval = 40
  
  ## Determine color scheme
  brks = seq(0, maxval, maxval/50)
  if(colscheme == 'turbo'){
    color   = viridis(min(length(brks),100), option = "H")
    colid   = "col-turbo"
  } else if (colscheme == 'virid'){
    color   = viridis(min(length(brks),100), option = "D")
    colid   = "col-viridis"
  } else if (colscheme == 'rev-virid'){
    color   = rev(viridis(min(length(brks),100), option = "D"))
    colid   = "col-viridis"
  } else if (colscheme == 'brks') {
    colv    = c("purple4","purple", "blue", "darkblue", "cyan", "green","darkgreen", "yellow", "orange", "red", "darkred")
    funpal  = colorRampPalette(colv,bias=2)
    nbcols  = length(brks)-1
    color   = funpal(nbcols) 
    colid   = "col-brks"
  }
  
  ## Determine years
  start = as.numeric(str_sub(names(plt.stack)[1], 2, 5))
  stop  = as.numeric(str_sub(names(plt.stack)[nlayers(plt.stack)], 2, 5))
  yrs = seq(start, stop, 1)
  
  ## Make PDF 
  namepdf = paste0(dir, env_name, "_", modtype, "_", start, "-", stop, ".pdf")
  pdf(namepdf, onefile = T)
  for(y in yrs){
    #  y = 1980
    print(paste("Plotting", y))
    plt.yr = raster::subset(plt.stack, grep(paste0('X', y), names(plt.stack), value = T, fixed = T))
    
    ## Plot 12 months
    par(mfrow=c(4,3),mar=c(1,1,2,0), oma=c(2,2,0,5))
    for(i  in 1:nlayers(plt.yr)){
      plot(plt.yr[[i]], legend=F, col=color, colNA='darkgray', zlim=c(minval, maxval), breaks=brks,
           main = names(plt.yr)[i])
    }
    ##Add legend
    par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,1))
    fields::image.plot(plt.yr,legend.only=T, zlim=c(minval, maxval),col=color,add=T,legend.width=1,
                       legend.mar=4,legend.line=3,
                       legend.lab = env_name)
  }
  dev.off()
}





##------------------------------------------------------------------------------
##
## Set up directory paths

dir.ras.in   <- "./Ecospace-environmental-drivers/MODIS/"
dir.ras.out  <- "./Ecospace-environmental-drivers/Outputs/Bricks/"
fld.asc.out  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/"
dir.asc.avg  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/Averages/"
dir.pdf.out  <- "./Ecospace-environmental-drivers/Outputs/PDF-maps/"
depth08min   <- raster("./global-data/shorelinecorrected-basemap-depth-131x53-08 min-14sqkm.asc")
date_coord_range <- "-98_-80.5_24_31_200301-202207"


##------------------------------------------------------------------------------
##
## Make Ecospace environmental-drivers files for ChlA integrated euphotic depth

env_driver = "ChlA"
overwrite  = 'y'
dir.asc.out = paste0(fld.asc.out, env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 

## Read in stack
ras_hires = stack(paste0(dir.ras.in, tolower(env_driver), "/", tolower(env_driver), "xZe", "_", date_coord_range))

## First, crop and resample to basemap grid 
ras = crop(ras_hires, depth08min)
ras = resample(ras, depth08min)
dim(depth08min); dim(ras) ## Dimensions should match

## Plotcheck
par(mfrow=c(2,1))
plot(depth08min, colNA = 'black')
plot(ras[[1]], colNA = 'black')
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
        env_name = env_driver, mintile = 'zero', maxtile = 0.99, modtype = "MODIS")

## ASCII files by month
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
            format = 'ascii', overwrite=T)

## ASCII global average
writeRaster(mean, paste0(dir.asc.avg, 'Avg_', env_driver),
            bylayer=F, format='ascii', overwrite=T)


## -----------------------------------------------------------------------------
##
## Cfl

env_driver = "Cfl"
overwrite  = 'y'
dir.asc.out = paste0(fld.asc.out, env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 
ras_hires = stack(paste0(dir.ras.in, tolower(env_driver), "/", tolower(env_driver), "_", date_coord_range))

## First, crop and resample to basemap grid 
ras = crop(ras_hires, depth08min)
ras = resample(ras, depth08min)
dim(depth08min); dim(ras) ## Dimensions should match

## Plotcheck
par(mfrow=c(2,1))
plot(depth08min, colNA = 'black')
plot(ras[[1]], colNA = 'black')
par(mfrow=c(1,1))

## Determine which ones need to be written out
ras.dates = data.frame(year=substr(names(ras),2,5),month=substr(names(ras),6,7))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
asc.need = ras.dates$yrmo

## Average for intial map 
mean <- stackApply(ras, indices =  rep(1,nlayers(ras)), fun = "mean", na.rm = T)
plot(mean, colNA = "black", main = paste0( "Averaged ", env_driver))

## Copy mean by month for months before data -----------------------------------
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
dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")
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

ras.comb = stack(rep.stack, ras) ## Combine stacks

start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))

## Write out files -------------------------------------------------------------
## Save raster
writeRaster(ras.comb, paste0(dir.ras.out, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)

## Make PDF of plots
pdf_map(ras, dir = dir.pdf.out, env_name = env_driver, maxval = max(maxValue(ras.comb)))

## ASCII global average
writeRaster(mean, paste0(dir.asc.avg, 'Avg_', env_driver),
            bylayer=F, format='ascii', overwrite=T)


## ASCII files by month
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
            format = 'ascii', overwrite=T)



## -----------------------------------------------------------------------------
##
## POC

env_driver = "POC"
overwrite  = 'y'
dir.asc.out = paste0(fld.asc.out, env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 
ras_hires = stack(paste0(dir.ras.in, tolower(env_driver), "/", tolower(env_driver), "_", date_coord_range))

## First, crop and resample to basemap grid 
ras = crop(ras_hires, depth08min)
ras = resample(ras, depth08min)
dim(depth08min); dim(ras) ## Dimensions should match

## Plotcheck
par(mfrow=c(2,1))
plot(depth08min, colNA = 'black')
plot(ras[[1]], colNA = 'black')
par(mfrow=c(1,1))

## Determine which ones need to be written out
ras.dates = data.frame(year=substr(names(ras),2,5),month=substr(names(ras),6,7))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
head(ras.dates); tail(ras.dates)
asc.need = ras.dates$yrmo

## Average for intial map 
mean <- stackApply(ras, indices =  rep(1,nlayers(ras)), fun = "mean", na.rm = T)
plot(mean, colNA = "black", main = paste0( "Averaged ", env_driver))

## Copy mean by month for months before data -----------------------------------
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
dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")
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

ras.comb = stack(rep.stack, ras) ## Combine stacks

start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))

## Write out files -------------------------------------------------------------
## Save raster
writeRaster(ras.comb, paste0(dir.ras.out, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)

## Make PDF of plots
pdf_map(ras, dir = dir.pdf.out, env_name = env_driver, maxval = max(maxValue(ras.comb)))

## ASCII global average
writeRaster(mean, paste0(dir.asc.avg, 'Avg_', env_driver),
            bylayer=F, format='ascii', overwrite=T)


## ASCII files by month
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
            format = 'ascii', overwrite=T)
