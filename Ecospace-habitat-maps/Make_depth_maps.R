##------------------------------------------------------------------------------
##
## Org:     NOAA / SEFSC / GoM IEA
## Project: Gulf-wide Ecospace Model
## Contact: Holden Earl Harris | holden.harris@noaa.gov
## Code:    Make depth maps for Ecospace model
##
##------------------------------------------------------------------------------

## --Reference for MARMAP--
## Pante E, Simon-Bouhet B (2013) marmap: A Package for Importing, Plotting and 
## Analyzing Bathymetric and Topographic Data in R. PLoS ONE 8(9): e73051. 
## doi:10.1371/journal.pone.0073051

rm(list=ls()); graphics.off()
library('marmap')
library('raster')

dir_depth = "./Ecospace-habitat-maps/Depth_maps/"


## Parameters: bounded area and resoluations
bbox = c(-98,-80.5, 24,31)    ## xmin, xmax, ymin, ymax
resolutions = c(4, 8, 16, 32) ## In lat. minutes

## Get depths and make rasters with different grid-cell resolutions
depth04 = marmap::as.raster(getNOAA.bathy(bbox[1], bbox[2], bbox[3], bbox[4], resolution = resolutions[1])) #get data from NOAA server and put into raster
depth04[depth04 > 0] = NA; depth04 = depth04 * -1 # replace land with NA and make depth positive

depth08 = marmap::as.raster(getNOAA.bathy(bbox[1], bbox[2], bbox[3], bbox[4], resolution = resolutions[2]))
depth08[depth08 > 0] = NA; depth08 = depth08 * -1 # replace land with NA and make depth positive

depth16 = marmap::as.raster(getNOAA.bathy(bbox[1], bbox[2], bbox[3], bbox[4], resolution = resolutions[3]))
depth16[depth16 > 0] = NA; depth16 = depth16 * -1 # replace land with NA and make depth positive

depth32 = marmap::as.raster(getNOAA.bathy(bbox[1], bbox[2], bbox[3], bbox[4], resolution = resolutions[4]))
depth32[depth32 > 0] = NA; depth32 = depth32 * -1 # replace land with NA and make depth positive

depth_list = list(depth04, depth08, depth16, depth32)


## Make figures of different resolutions ---------------------------------------
png(paste0(dir_depth, "4depthmaps-res-04-08-16-32-min.png"), width = 9.5, height = 6, units = "in", res = 1600)
par(mfrow=c(2,2))
for (depth in depth_list){
  ## Get paramaters of depth map
  min         = paste0(round(res(depth)[1]*60,0),' min.') 
  dims         = paste0(dim(depth)[1],'x',dim(depth)[2])
  cellarea_km2 = paste0('~', round(sqrt(mean(getValues(area(depth))))), ' sq.km') ## Get surface area of each cell in km2
  
  ## Plot
  plot(depth,colNA='black', main =paste(min, cellarea_km2, dims, sep = ' / '),
       col = topo.colors(30, rev=T))
}
dev.off()

## Export maps as ascii files---------------------------------------------------
for (depth in depth_list){
  ## Get paramaters of depth map
  min          = paste0(round(res(depth)[1]*60,0),'min') 
  dims         = paste0(dim(depth)[1],'x',dim(depth)[2])
  cellarea_km2 = paste0(round(sqrt(mean(getValues(area(depth))))), 'sqkm') ## Get surface area of each cell in km2
  map_params   = paste(min, cellarea_km2, dims, sep='-'); map_params
  
  ## Write out ASCII
  writeRaster(depth, paste0(dir_depth, '/ASCII/depthmap_', map_params), format='ascii', NAflag=0, overwrite=T)
}


## -----------------------------------------------------------------------------
## Use 08 min resolution for Ecospace
depth = depth08
min          = paste0(round(res(depth)[1]*60,0),'min') 
dims         = paste0(dim(depth)[1],'x',dim(depth)[2])
cellarea_km2 = paste0(round(sqrt(mean(getValues(area(depth))))), 'sqkm') ## Get surface area of each cell in km2
map_params   = paste(min, cellarea_km2, dims, sep='-'); map_params

## Plot map and log 10 depth
png(paste0(dir_depth, "log10_depthmap_", map_params, ".png"), width = 7, height = 8.5, units = "in", res = 1600)
par(mfrow = c(2,1))
plot(depth, colNA='black', col = topo.colors(30, rev=T))
plot(log10(depth), colNA='black', col = topo.colors(30, rev=T))
dev.off()

## Write out ASCII
writeRaster(depth, paste0(dir_depth, '/ecospace_basemap_', map_params), format='ascii', NAflag=0, overwrite=T)