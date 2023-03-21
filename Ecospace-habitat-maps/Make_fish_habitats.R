##------------------------------------------------------------------------------
##
## Org:     NOAA / SEFSC / GoM IEA
## Project: Gulf-wide Ecospace Model
## Contact: Holden Earl Harris | holden.harris@noaa.gov
## Code:    


rm(list=ls()); graphics.off(); windows()

library(sf)
library(ggplot2)
library(raster)
library(cowplot)

dir_in  = "./Ecospace-habitat-maps/Data-inputs/NCEI-GOM-data-atlas/"
dir_out = "./Ecospace-habitat-maps/Output-for-ecospace/"
dir_fig = "./Ecospace-habitat-maps/Figures/"

depth = raster("./global-data/basemap_depth_8min-14sqkm-52x131.asc")

## Seabed sediment values ------------------------------------------------------
seabed  = st_read(paste0(dir_in, "Sediments/usSEABED_GOM_Sediments.shp"))

## The attributes are denoted by the grid filenames: 
## gma_rckv - gridded values substrate rock presence (% exposure); computed with CS Interpolator; max 5km search radius 
## gma_rcku - gridded uncertainties on substrate rock presence (% exposure); 
## gma_gvlv - gridded values sediment gravel fraction content (% weight); computed with CS Interpolator; max 20km search radius 
## gma_gvlu - gridded uncertainties on sediment gravel contents (% weight); computed with CS Interpolator; max 20km search radius 
## gma_sndv - gridded values sediment sand fraction content (% weight); computed with CS Interpolator; max 20km search radius 
## gma_sndu - gridded uncertainties on sediment sand contents (% weight); computed with CS Interpolator; max 20km search radius 
## gma_mudv - gridded values sediment mud fraction content (% weight); computed with CS Interpolator; max 20km search radius 
## gma_mudu - gridded uncertainties on sediment mud contents (% weight); computed with CS Interpolator; max 20km search radius 
## gma_folk - gridded codes for sediment Folk Codes; the 0 (or " "),1,2,3 indicate negligible-, slightly-, x-ly-, or major components 
##            of mud, sand or gravel in code positions 1,2,3 respectively; they can be converted to "(x)xX" types codes; see the dbSEABED web sites for a suitable ESRI legend; 
##            computed from the foregoing mud, sand, gravel griddings. 
## gma_domnc - gridded codes for dominant types of substrates; 
##            the 0 (or " "),2,3 indicate negligible-, subdominant-, or dominant- components for rock, gravel, sand, mud 
##            in code positions 1,2,3,4 respectively; see the dbSEABED web sites for a suitable ESRI legend. 


rckv <- rasterize(seabed, depth, field = "gom_rckv") ## Takes a few min due to size
rcku <- rasterize(seabed, depth, field = "gom_rcku") ## Takes a few min due to size
sndv <- rasterize(seabed, depth, field = "gom_sndv") ## Takes a few min due to size
sndu <- rasterize(seabed, depth, field = "gom_sndu") ## Takes a few min due to size
mudv <- rasterize(seabed, depth, field = "gom_mudv") ## Takes a few min due to size
mudu <- rasterize(seabed, depth, field = "gom_mudu") ## Takes a few min due to size
gvlv <- rasterize(seabed, depth, field = "gom_gvlv") ## Takes a few min due to size
gvlu <- rasterize(seabed, depth, field = "gom_gvlu") ## Takes a few min due to size


## Rock
rck_prd <- overlay(rckv, rcku, fun=function(x,y){(x*y)})
rck_max <- overlay(rckv, rcku, fun = max)
par(mfrow=c(1,2))
plot(rckv, main = "rckv") 
plot(rck_prd, main = "prd")
writeRaster(rckv, paste0(dir_in, "Intermed-rasters/rckv"), overwrite=TRUE)
writeRaster(rcku, paste0(dir_in, "Intermed-rasters/rcku"), overwrite=TRUE)

## Sand
sndv[sndv == -99] <- NA; sndu[sndu == -99] <- NA
snd_sum <- overlay(sndv, sndu, fun = sum)
snd_max <- overlay(sndv, sndu, fun = max)
par(mfrow=c(2,2))
plot(sndv, main = "sndv"); plot(sndu, main = "sndu")
plot(snd_sum, main = "sum"); plot(snd_max, main = "max") 
writeRaster(sndv, paste0(dir_in, "Intermed-rasters/sndv"), overwrite=TRUE)
writeRaster(sndu, paste0(dir_in, "Intermed-rasters/sndu"), overwrite=TRUE)

## Mud
mudv[mudv == -99] <- NA; mudu[mudu == -99] <- NA
mud_sum <- overlay(mudv, mudu, fun = sum)
mud_max <- overlay(mudv, mudu, fun = max)
par(mfrow=c(2,2))
plot(mudv, main = "mudv"); plot(mudu, main = "mudu")
plot(mud_sum, main = "sum"); plot(mud_max, main = "max") 
writeRaster(mudv, paste0(dir_in, "Intermed-rasters/mudv"), overwrite=TRUE)
writeRaster(mudu, paste0(dir_in, "Intermed-rasters/mudu"), overwrite=TRUE)

## Gravel
gvlv[gvlv == -99] <- NA; gvlu[gvlu == -99] <- NA
gvl_sum <- overlay(gvlv, gvlu, fun = sum)
gvl_max <- overlay(gvlv, gvlu, fun = max)
par(mfrow=c(2,2))
plot(gvlv, main = "gvlv"); plot(gvlu, main = "gvlu")
plot(gvl_sum, main = "sum"); plot(gvl_max, main = "max") 
writeRaster(gvlv, paste0(dir_in, "Intermed-rasters/gvlv"), overwrite=TRUE)
writeRaster(gvlu, paste0(dir_in, "Intermed-rasters/gvlu"), overwrite=TRUE)

## Write out ASCII files for ecospace
writeRaster(rck)

## Figure
png(paste0(dir_fig, "Ecospace-seabed-types.png"), 
    width = 9, height = 6, units = "in", res=1200)
par(mfrow=c(2,2))
plot(mudv, colNA = "gray", main = "Mud");  plot(sndv, colNA = "gray", main = "Sand")
plot(rckv, colNA = "gray", main = "Rock"); plot(gvlv, colNA = "gray", main = "Gravel")
dev.off()
