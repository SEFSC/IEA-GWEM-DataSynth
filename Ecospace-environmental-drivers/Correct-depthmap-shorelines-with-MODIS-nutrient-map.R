rm(list=ls());rm(.SavedPlots);graphics.off();gc();windows(record=T)

library(terra)
library(dplyr)
library(viridis)
library(stringr)

##------------------------------------------------------------------------------
##
## Re-project chla stack to match resolution of depth basemap

dir.ras.in = "./Ecospace-environmental-drivers/MODIS/chla/"

## Get depths and make rasters
bbox.gom = c(-98,-80.5,24,31)
bbox = bbox.gom
resolutions = c(4, 8, 16, 32)

depth04 = marmap::as.raster(getNOAA.bathy(bbox[1], bbox[2], bbox[3], bbox[4], resolution = resolutions[1])) ## Get data from NOAA server and put into raster
depth04 = depth04 * -1 ## Make depth positive
depth = depth04
crs(depth) = "+proj=robin"
proj4string(depth) = CRS("+proj=longlat +datum=WGS84")

## Uncorrected depth map
uncor = depth
uncor[uncor < 0] = NA ##replace land with NA 
plot(uncor, colNA = 'black')

## Read chla raster
ras_hires = stack(paste0(dir.ras.in,"chlaxZe_-98_-80.5_24_31_200301-202207"))

## Average over stack
maxchlz = stackApply(ras_hires, indices =  rep(1, nlayers(ras_hires)), fun = "max", na.rm = T)
plot(maxchlz,colNA = "black")

# Crop and resample to depth grid ---------------------------------------
maxchlz = crop(maxchlz, depth)
extent(depth); extent(ras_hires)
maxchlz = resample(maxchlz, depth, method = "ngb")
#maxchlz = projectRaster(maxchlz, depth, method = 'ngb', crs= "+proj=utm +zone=16 +datum=WGS84 +units=km")

dim(depth); dim(maxchlz) ## Should be the same

## Set 0 to land and 1 is water
maxchlz[is.na(maxchlz)] = 0
maxchlz[maxchlz != 0] = 1
plot(maxchlz)

## Correct depth map
cordepth = depth
cordepth[cordepth < 1] = 1
cordepth = cordepth * maxchlz 
cordepth[cordepth < 1] = NA

## Plotcheck
par(mfrow=c(2,1))
plot(uncor, colNA = 'black'); plot(cordepth, colNA = "black") ## plotcheck
par(mfrow=c(1,1))

## Aggregate to lower resolution
## reduces by a factor of 4; from 27.5k cells to 6.9k cells
depth08min = aggregate(cordepth, fact = 2)
plot(depth08min, colNA='black')

cellarea.km2 = getValues(area(depth08min))
cellsize = paste0(round(sqrt(mean(cellarea.km2))),' km2'); cellsize
min = paste0(round(res(depth08min)[1]*60,0),' min'); min
grid = paste0(dim(depth08min)[1],' x ',dim(depth08min)[2]); grid


################################################################################
## Plot depth corrections
visaid = depth; visaid[visaid < 1] = NA

png(paste0("./maps/Depth_corrections.png"), width = 6, height = 8.5, units = "in", res = 1500)
par(mfrow=c(3,2))
plot(visaid, colNA = "black", main = "Depth (raw 04 min)")
plot(maxchlz, colNA = "black", main = "Max chla (binary)")
plot(cordepth, colNA = "black", main = "Corrected depth (04 min)")
plot(mean, colNA = "black", main = "Mean Chla (08 min)")
plot(depth08min, colNA = "black", main = "Corrected depth (08 min)")
plot(log10(depth08min), colNA = "black", main = "Log 10 corrected depth")
par(mfrow=c(1,1))
dev.off()


## Write out depth
#writeRaster(depth08min, paste0(dir.depth, "depth ", min, " ", cellsize, " GWEM"), format='ascii', NAflag=0, overwrite=F)
#writeRaster(log10(depth08min), paste0(dir.depth, "log10 depth ", min, " ", cellsize, " GWEM"), format='ascii', NAflag=0, overwrite=F)

## Plot average 
#png(paste0("./maps/Depth/Depth.png"), width = 6, height = 8.5, units = "in", res = 1500)
#par(mfrow=c(2,1))
#plot(depth08min, colNA = "black", main = "Depth (08 min)")
#plot(log10(depth08min), colNA = "black", main = "Log10 Depth")
#par(mfrow=c(1,1))
#dev.off()
