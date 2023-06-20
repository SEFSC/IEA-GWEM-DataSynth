rm(list=ls());rm(.SavedPlots);graphics.off();gc();windows(record=T)

library('terra')
library('marmap')
library('stringr')
library('viridis')

dir.ras.in   <- "./Ecospace-environmental-drivers/MODIS/"
dir.ras.out  <- "./Ecospace-environmental-drivers/Outputs/Bricks/"
fld.asc.out  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/"
dir.asc.avg  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/Averages/"
dir.pdf.out  <- "./Ecospace-environmental-drivers/Outputs/PDF-maps/"
depth08min   <- raster("./global-data/shorelinecorrected-basemap-depth-131x53-08 min-14sqkm.asc")


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
pdf_map = function(plt.stack, colscheme = "virid", dir, env_name, maxval){
  
  #  plt.stack = ras
  #  colscheme = 'virid'
  #  dir = "./maps/"
  #  env_name = "Nutrients"
  #  maxval = 380
  
  ## Determine color scheme
  brks = seq(0, maxval, maxval/50)
  if(colscheme == 'turbo'){
    color   = viridis(min(length(brks),100), option = "H")
    colid   = "col-turbo"
  } else if (colscheme == 'brks') {
    colv    = c("purple4","purple", "blue", "darkblue", "cyan", "green","darkgreen", "yellow", "orange", "red", "darkred")
    funpal  = colorRampPalette(colv,bias=2)
    nbcols  = length(brks)-1
    color   = funpal(nbcols) 
    colid   = "col-brks"
  } else {
    color   = viridis(min(length(brks),100), option = "D")
    colid   = "col-viridis"
  }
  
  ## Make PDF  
  pdf(paste0(outname(plt.stack, dir, env_name), ".pdf"), onefile = T)
  
  ras.dates = data.frame(year=substr(names(plt.stack),2,5),month=substr(names(ras),6,7))
  raster_years = unique(ras.dates$year)
  
  for(y in raster_years){
    #y = raster_years[1]
    print(paste("Plotting",y))
    yr.sub  = substr(names(plt.stack),2,5)
    yr.idx  = which(yr.sub == y)
    plt.yr  = plt.stack[[yr.idx]] 
    par(mfrow=c(4,3),mar=c(1,1,2,0),oma=c(2,2,0,6))
    ## Plot 12 months
    for(i  in 1:nlayers(plt.yr)){
      mo.sub = paste0(y, "-", substr(names(plt.yr)[[i]],6,7))
      plot(plt.yr[[i]], legend=F, col=color, colNA='darkgray', zlim=c(0, maxval),breaks=brks,
           main = mo.sub)
    }
    ##Add legend
    par(mfrow=c(1,1),mar=c(0,0,0,0),oma=c(0,0,0,1))
    fields::image.plot(plt.yr,legend.only=T,zlim=c(0,maxval),col=color,add=T,legend.width=1,
                       legend.mar=4,legend.line=3,
                       legend.lab = env_name)
  }
  dev.off()
}


##------------------------------------------------------------------------------
##
## Make Ecospace env ASCII files for ChlZ - ChlA integrated euphotic depth

env_driver = "ChlA"
overwrite  = 'y'
dir.asc.out = paste0(fld.asc.out, env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 
ras_hires = stack(paste0(dir.ras.in, env_driver, "/chlaxZe_-98_-80.5_24_31_200301-202207"))


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

## Plot average 
png(paste0("./maps/Mean_ChlZ.png"), width = 6, height = 8.5, units = "in", res = 1500)
par(mfrow=c(2,1))
plot(mean, colNA = "black", main = "Averaged Chla Euphotic Depth")
plot(log10(mean), colNA = "black", main = "Log10 Chla Euphotic Depth")
par(mfrow=c(1,1))
dev.off()


################################################################################
## Copy mean raster for months before data -------------------------------
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

ras.comb = stack(rep.stack, ras)

#env_driver = "ChlA_Depth-integrated_MODIS"
start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))
writeRaster(ras.comb, paste0(dir.ras.out, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)

## Write out files -------------------------------------------------------------

## ASCII
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
            format = 'ascii', overwrite=T)

## Make PDF of plots
pdf_map(ras, dir = dir.pdf.out, env_name = "ChlA", maxval = 200)
pdf_map(log10(ras), dir = dir.pdf.out, env_name = "ChlA_log10", maxval = log10(390))


## Average for intial map 
mean <- stackApply(ras, indices =  rep(1,nlayers(ras)), fun = "mean", na.rm = T)
writeRaster(mean, paste0(dir.asc.avg, 'avg_chla_integrated_euphotic_depth'),
            bylayer=F, format='ascii', overwrite=T)






















################################################################################
##
## Cfl

env_driver = "cfl"
overwrite  = 'y'
ras_hires = stack('./data extraction/MODIS/cfl/cfl_-98_-80.5_24_31_200301-202207')


## Set up folder 
dir.asc.out = paste0("./maps/", env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 

## First, crop and resample to basemap grid 
ras = crop(ras_hires, depth08min)
ras = resample(ras, depth08min)

## Check that dimensions are the same
dim(depth08min); dim(ras) 
par(mfrow=c(2,1))
plot(depth08min, colNA = 'black')
plot(ras[[1]], colNA = 'black')
par(mfrow=c(1,1))

#which ones need to be written out
ras.dates = data.frame(year=substr(names(ras),2,5),month=substr(names(ras),6,7))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month); head(ras.dates)
asc.need = ras.dates$yrmo

## Write out ASCII files
sufx = paste0("y",substr(asc.need,1,4),"_m", substr(asc.need,5,7)); head(sufx)
writeRaster(ras, paste0(dir.asc.out, env_driver), bylayer=T, suffix=sufx,format='ascii', overwrite=T)

## Make PDF of plots
pdf_map(ras, dir = "./maps/Plots/", env_name = paste0(env_driver, "_08min"), maxval = 0.9 * max(values(ras), na.rm=T))
pdf_map(log10(ras+1), dir = "./maps/Plots/", env_name = paste0(env_driver, "_08min_log10"), maxval = log10(max(values(ras+1), na.rm=T)))

## Average for intial map ------------------------------------------------
mean <- stackApply(ras, indices = rep(1,nlayers(ras)), fun = "mean", na.rm = T)

## Plot average 
png(paste0("./maps/Plots/Mean_", env_driver, ".png"), width = 6, height = 8.5, units = "in", res = 1500)
par(mfrow=c(2,1))
plot(mean, colNA = "black", main = paste("Averaged", env_driver))
plot(log10(mean), colNA = "black", main = paste("Log10", env_driver))
par(mfrow=c(1,1))
dev.off()



################################################################################
##
## PIC

env_driver = "PIC"
overwrite  = 'y'
ras_hires = stack('./data extraction/MODIS/PIC/PIC_-98_-80.5_24_31_200301-202207')


## Set up folder 
dir.asc.out = paste0("./maps/", env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 

## First, crop and resample to basemap grid 
ras = crop(ras_hires, depth08min)
ras = resample(ras, depth08min)

## Check that dimensions are the same
dim(depth08min); dim(ras) 
par(mfrow=c(2,1))
plot(depth08min, colNA = 'black')
plot(ras[[1]], colNA = 'black')
par(mfrow=c(1,1))

#which ones need to be written out
ras.dates = data.frame(year=substr(names(ras),2,5),month=substr(names(ras),6,7))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month); head(ras.dates)
asc.need = ras.dates$yrmo


## Average for intial map ------------------------------------------------
mean <- stackApply(ras, indices = rep(1,nlayers(ras)), fun = "mean", na.rm = T)

## Copy mean raster for months before data -------------------------------
mo = unique(ras.dates$month)
yr = 1980:1992
dummy.dates = data.frame(year = character(), month = character())
for(y in yr){
  for(m in mo){
    dummy.dates = rbind(dummy.dates, c(y,m))
  }
}
colnames(dummy.dates) = c("year", "month")
dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")

dummy.ras = brick(replicate(nrow(dummy.dates), mean))
names(dummy.ras) = dummy.dates$ym

## Combine dummy raster set (1980-1992) and data (1993-2022)
dates.comb = rbind(dummy.dates, ras.dates)
ras.comb = stack(dummy.ras, ras)

## Write out ASCII files
#sufx = paste0("y",substr(asc.need,1,4),"_m", substr(asc.need,5,7)); head(sufx)
dir.asc.out = "./maps/Chl-A Euphotic Depth/08 min Chl-A Euphotic Depth/"
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, 
            suffix=dates.comb$yrmo,format='ascii', overwrite=T)


## Make PDF of plots
pdf_map(ras, dir = "./maps/Plots/", env_name = paste0(env_driver, "_08min"), maxval = 0.9 * max(values(ras), na.rm=T))
pdf_map(log10(ras+1), dir = "./maps/Plots/", env_name = paste0(env_driver, "_08min_log10"), maxval = log10(max(values(ras+1), na.rm=T)))

## Plot average 
print(paste0("./maps/Plots/Mean_", env_driver, ".png"))
png(paste0("./maps/Plots/Mean_", env_driver, ".png"), width = 6, height = 8.5, units = "in", res = 1500)
par(mfrow=c(2,1))
plot(mean, colNA = "black", main = paste("Averaged", env_driver))
plot(log10(mean), colNA = "black", main = paste("Log10", env_driver))
par(mfrow=c(1,1))
dev.off()

################################################################################
##
## POC

env_driver = "POC"
overwrite  = 'y'
ras_hires = stack('./data extraction/MODIS/POC/POC_-98_-80.5_24_31_200301-202207')

## Set up folder 
dir.asc.out = paste0("./maps/", env_driver, "/")
if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 

## First, crop and resample to basemap grid 
ras = crop(ras_hires, depth08min)
ras = resample(ras, depth08min)

## Check that dimensions are the same
dim(depth08min); dim(ras) 
par(mfrow=c(2,1))
plot(depth08min, colNA = 'black')
plot(ras[[1]], colNA = 'black')
par(mfrow=c(1,1))

#which ones need to be written out
ras.dates = data.frame(year=substr(names(ras),2,5),month=substr(names(ras),6,7))
ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month); head(ras.dates)
asc.need = ras.dates$yrmo

## Write out ASCII files
sufx = paste0("y",substr(asc.need,1,4),"_m", substr(asc.need,5,7)); head(sufx)
writeRaster(ras, paste0(dir.asc.out, env_driver), bylayer=T, suffix=sufx,format='ascii', overwrite=T)

## Make PDF of plots
pdf_map(ras, dir = "./maps/Plots/", env_name = paste0(env_driver, "_08min"), maxval = 0.9 * max(values(ras), na.rm=T))
pdf_map(log10(ras+1), dir = "./maps/Plots/", env_name = paste0(env_driver, "_08min_log10"), maxval = log10(max(values(ras+1), na.rm=T)))

## Average for intial map ------------------------------------------------
mean <- stackApply(ras, indices = rep(1,nlayers(ras)), fun = "mean", na.rm = T)

## Plot average 
print(paste0("./maps/Plots/Mean_", env_driver, ".png"))
png(paste0("./maps/Plots/Mean_", env_driver, ".png"), width = 6, height = 8.5, units = "in", res = 1500)
par(mfrow=c(2,1))
plot(mean, colNA = "black", main = paste("Averaged", env_driver))
plot(log10(mean), colNA = "black", main = paste("Log10", env_driver))
par(mfrow=c(1,1))
dev.off()

