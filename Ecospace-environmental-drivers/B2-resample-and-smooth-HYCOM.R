## ---------------------------------setup---------------------------------------
rm(list=ls());graphics.off();rm(.SavedPlots);gc();windows(record=T)
library(raster)

datelabel <-  "1993-01 to 2020-12"
dir.in    <-  "./Ecospace-environmental-drivers/HYCOM/"
dir.out   <-  dir.in
depth     <- raster("./global-data/shorelinecorrected-basemap-depth-131x53-08 min-14sqkm.asc")

## Read in monthly stacks
t.surf.hycom = stack( paste0(dir.in, 'HYCOM GOM temp surface ', datelabel))
t.bot.hycom  = stack( paste0(dir.in, 'HYCOM GOM temp bottom ' , datelabel))
t.avg.hycom  = stack( paste0(dir.in, 'HYCOM GOM temp avg '    , datelabel))
s.surf.hycom = stack( paste0(dir.in, 'HYCOM GOM salinity surface ', datelabel))
s.bot.hycom  = stack( paste0(dir.in, 'HYCOM GOM salinity bottom ' , datelabel))
s.avg.hycom  = stack( paste0(dir.in, 'HYCOM GOM salinity avg '    , datelabel))
print("nrow, ncol, nlayers"); dim(t.surf.hycom); extent(t.surf.hycom); dim(depth); extent(depth) ## Check dimensions and extent; Needs cropping.


## -----------------------------------------------------------------------------
## Crop and resample to GOM

## Make 0s NAs and check
depthcheck = depth
depthcheck[depthcheck == 0] = NA
plot(depthcheck, colNA = 'black', main = "depth/base map") ## Looks good

## First, crop to match extent of EwE base/depth map
t.surf.crop = crop(t.surf.hycom,depth)
t.bot.crop  = crop(t.bot.hycom, depth)
t.avg.crop  = crop(t.avg.hycom, depth)
s.surf.crop = crop(s.surf.hycom,depth)
s.bot.crop  = crop(s.bot.hycom, depth)
s.avg.crop  = crop(s.avg.hycom, depth)
crs(depth); crs(t.surf.crop)

## Second, resample to match cell size and resolution
t.surf.rs = resample(t.surf.crop, depth)
t.bot.rs  = resample(t.bot.crop,  depth)
t.avg.rs  = resample(t.avg.crop,  depth)
s.surf.rs = resample(s.surf.crop, depth)
s.bot.rs  = resample(s.bot.crop,  depth)
s.avg.rs  = resample(s.avg.crop,  depth)

par(mfrow=c(2,2))
plot(depthcheck, colNA = 'black'); plot(t.avg.rs[[1]], colNA = 'black'); plot(s.avg.rs[[1]], colNA = 'black')
## PROBEM! --> The coastlines don't align

## -----------------------------------------------------------------------------
##
## Build and run fuction to smooth NAs along the coast 

## Function to smooth coastline ------------------------------------------------
## Smoother based on x-y grid size
## Inputs include stack and pixel size of smoother. 
## Note that size must be an odd number: e.g., 3x3, 5x5, 7x7
smooth.na <- function(s, size = 3){
  #s = s.surf.rs
  #size = 11
  middlecell = ceiling(size^2 / 2)
  
  ## Internal function: 
  ## If cell is NA, fill with mean of surrounding grid
  fill.na <- function(x, i = middlecell) {
    if(is.na(x)[i] ) {
      return(mean(x, na.rm=TRUE))
    } else {
      return(x[i])
    }
  }  
  
  ## Loop to make new raster
  newstack = s
  r = raster()
  #for (i in 1:2){
  for (i in 1:nlayers(s)){
    newstack[[i]] = focal(s[[i]], w = matrix(1, size, size), fun = fill.na, 
                          pad = TRUE, na.rm = FALSE)
  }
  return(newstack)
}

## Iteratively run smooth.na function ------------------------------------------
## Smooths coastline given that some coastal cells don't align.
## These each takes a few minutes 

smoothsize = 3
t.surf.smoo = smooth.na(t.surf.rs, smoothsize)
t.bot.smoo  = smooth.na(t.bot.rs,  smoothsize)
t.avg.smoo  = smooth.na(t.avg.rs,  smoothsize)
s.surf.smoo = smooth.na(s.surf.rs, smoothsize)
s.bot.smoo  = smooth.na(s.bot.rs,  smoothsize)
s.avg.smoo  = smooth.na(s.avg.rs,  smoothsize)

## Initialize
t.surf.smoo = t.surf.rs
t.bot.smoo  = t.bot.rs
t.avg.smoo  = t.avg.rs
s.surf.smoo = s.surf.rs
s.bot.smoo  = s.bot.rs
s.avg.smoo  = s.avg.rs

## This needs to be done iteratively to use nearest neigbor information
## Takes a few minutes 

stepsneeded = 6
for (j in 1:stepsneeded){
  print(paste("Step", j, "- t.surf -", Sys.time()))
  t.surf.smoo = smooth.na(t.surf.smoo, smoothsize)
  print(paste("Step", j, "- t.bot -", Sys.time()))
  t.bot.smoo  = smooth.na(t.bot.smoo,  smoothsize)
  print(paste("Step", j, "- t.avg -", Sys.time()))
  t.avg.smoo  = smooth.na(t.avg.smoo,  smoothsize)
  print(paste("Step", j, "- s.surf -", Sys.time()))
  s.surf.smoo = smooth.na(s.surf.smoo, smoothsize)
  print(paste("Step", j, "- s.bot -", Sys.time()))
  s.bot.smoo  = smooth.na(s.bot.smoo,  smoothsize)
  print(paste("Step", j, "- s.avg -", Sys.time()))
  s.avg.smoo  = smooth.na(s.avg.smoo,  smoothsize)
}

## Mask smoothed rasters with depth map ----------------------------------------
depthNA = depth
depthNA[depthNA==0] = NA
plot(depthNA, colNA='black')

t.surf.msk = mask(t.surf.smoo, depthNA)
t.bot.msk  = mask(t.bot.smoo, depthNA)
t.avg.msk  = mask(t.avg.smoo, depthNA)
s.surf.msk = mask(s.surf.smoo, depthNA)
s.bot.msk  = mask(s.bot.smoo, depthNA)
s.avg.msk  = mask(s.bot.smoo, depthNA)

## Plotcheck
par(mfrow=c(2,1))
plot(t.avg.smoo[[1]]); plot(t.avg.msk[[1]], colNA = 'black')

## Rename layers with names from original stack --------------------------------
names(t.surf.msk) = labels(t.surf.hycom)
names(t.bot.msk) = labels(t.bot.hycom)
names(t.avg.msk) = labels(t.avg.hycom)
names(s.surf.msk) = labels(s.surf.hycom)
names(s.bot.msk) = labels(s.bot.hycom)
names(s.avg.msk) = labels(s.avg.hycom)

## Write out smoothed stacks ---------------------------------------------------
writeRaster(t.surf.msk, paste0(dir.out, 'Resamp-smoothed HYCOM GOM temp surface ', datelabel), overwrite=TRUE)
writeRaster(t.bot.msk,  paste0(dir.out, 'Resamp-smoothed HYCOM GOM temp bottom ', datelabel), overwrite=TRUE)
writeRaster(t.avg.msk,  paste0(dir.out, 'Resamp-smoothed HYCOM GOM temp avg ', datelabel), overwrite=TRUE)
writeRaster(s.surf.msk, paste0(dir.out, 'Resamp-smoothed HYCOM GOM salinity surface ', datelabel), overwrite=TRUE)
writeRaster(s.bot.msk,  paste0(dir.out, 'Resamp-smoothed HYCOM GOM salinity bottom ', datelabel), overwrite=TRUE)
writeRaster(s.avg.msk,  paste0(dir.out, 'Resamp-smoothed HYCOM GOM salinity avg ', datelabel), overwrite=TRUE)
