##------------------------------------------------------------------------------
##
## Org:     NOAA / SEFSC / GoM IEA
## Project: Gulf-wide Ecospace Model
## Contact: Holden Earl Harris | holden.harris@noaa.gov
## Code:    Make hardbottom reef map 
##          ouputs of AR and NR habitat maps for Ecospace model


rm(list=ls()); graphics.off(); windows()

library(sf)
library(ggplot2)
library(raster)
library(cowplot)

dir_in = "./Ecospace-habitat-maps/Data-inputs/GRSC-hardbottom-maps/"
dir_out = "./Ecospace-habitat-maps/Output-for-ecospace/"
dir_fig = "./Ecospace-habitat-maps/Figures/"


## Shape files
hb  = st_read(paste0(dir_in, "hardbottom_proj.shp"))
ars = st_read(paste0(dir_in, "Artificial_Reefs.shp"))
wrecks = st_read(paste0(dir_in, "AWOIS_Wrecks.shp"))
platforms = st_read(paste0(dir_in, "Working_Platforms.shp"))

## Plot shape files ------------------------------------------------------------
plot_hb <- 
  hb %>% ggplot() + 
  geom_sf(color="black", fill="gray") +
  ggtitle("Hardbottom Projection") 

plot_ars <- 
  ars %>% ggplot() + 
  geom_sf() +
  ggtitle("Artificial Reefs")

plot_wrecks <- 
  wrecks %>% ggplot() + 
  geom_sf() +
  ggtitle("AWOIS Wrecks")

plot_platforms <- 
  platforms %>% ggplot() + 
  geom_sf() +
  ggtitle("Working Platforms")


png(paste0(dir_fig, "Plots-HB-ARs-wrecks-platforms.png"), 
    width = 12, height = 8, units = "in", res=1000)
plot_grid(plot_hb, plot_ars, plot_wrecks, plot_platforms, nrow = 2)
dev.off(); windows()


##------------------------------------------------------------------------------
## Distance rasters

## Read in rasters
dist_to_AR = raster(paste0(dir_in, "dist_to_AR"))
dist_to_HB = raster(paste0(dir_in, "dist_to_HB"))

## Read in depth
depth = raster("./global-data/basemap_depth_8min-14sqkm-52x131.asc")
dim(depth);extent(depth)

## Take inverse distance
inv_AR = calc(dist_to_AR, fun = function(x) {1 / x}) 
inv_HB = calc(dist_to_HB, fun = function(x) {1 / sqrt(x)})
##--> ARs use 1/x proxy
##--> HBs use 1/sqrt(x) proxy

## Crop to match extent of EwE base/depth map
crop_AR = crop(inv_AR, depth)
crop_HB = crop(inv_HB, depth)

## Resample to match cell size and resolution
resamp_AR = resample(crop_AR, depth) ## takes a few minutes due to raster size
resamp_HB = resample(crop_HB, depth) ## takes a few minutes due to raster size

par(mfrow=c(2,1))
plot(resamp_AR,  main = "ARs (cropped and resampled)", colNA = 'gray')
plot(resamp_HB,  main = "HB (cropped and resampled)", colNA = 'gray')


## Scale to one
scaled_AR = calc(resamp_AR, fun = function(x) {
  x / max(values(resamp_AR), na.rm=TRUE)
})

scaled_HB = calc(resamp_HB, fun = function(x) {
  x / max(values(resamp_HB), na.rm=TRUE)
})


## Write out habitat maps------------------------------------------------------
dir_out = "./Ecospace-habitat-maps/Output-for-ecospace/"
writeRaster(scaled_AR, paste0(dir_out, "scaled-inv-dist-AR_div-x"), format = "ascii", overwrite=T)
writeRaster(scaled_HB, paste0(dir_out, "scaled-inv-dist-HB_div-sqrt-x"), format = "ascii", overwrite=T)


## Plots ------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(dist_to_AR,  main = "Distance from ARs", colNA = 'gray')
plot(dist_to_HB,  main = "Distance from HB", colNA = 'gray')
plot(scaled_AR,  main = "Artificial reefs (scaled inverse distance)", colNA = 'gray')
plot(scaled_HB,  main = "Hard bottom (scaled inverse squared distance)", colNA = 'gray')

## Write out plots
png(paste0(dir_fig, "Ecospace-hardbottom-ARs.png"), 
    width = 8.5, height = 11, units = "in", res=1200)
par(mfrow=c(2,1))
plot(scaled_AR, main = "Artificial reefs (scaled inverse distance)", colNA = 'gray')
plot(scaled_HB, main = "Hard bottom (scaled inverse distance squared)", colNA = 'gray')
dev.off()


