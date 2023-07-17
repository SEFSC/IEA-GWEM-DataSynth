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

depth = raster("./global-data/shorelinecorrected-basemap-depth-131x53-08 min-14sqkm.asc")

## Seabed sediment values ------------------------------------------------------
seabed  = sf::st_read(paste0(dir_in, "Sediments/usSEABED_GOM_Sediments.shp"))

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

## Rasterize polygons
rckv_rawdat <- raster::rasterize(seabed, depth, field = "gom_rckv") ## Takes awhile
gvlv_rawdat <- raster::rasterize(seabed, depth, field = "gom_gvlv") ## Takes awhile
sndv_rawdat <- raster::rasterize(seabed, depth, field = "gom_sndv") ## Takes awhile
mudv_rawdat <- raster::rasterize(seabed, depth, field = "gom_mudv") ## Takes awhile

## Write out rasters and read back (rasterizing is slow)
raster::writeRaster(rckv_rawdat, paste0(dir_in, "Intermed-rasters/rckv"), overwrite=TRUE)
raster::writeRaster(gvlv_rawdat, paste0(dir_in, "Intermed-rasters/gvlv"), overwrite=TRUE)
raster::writeRaster(sndv_rawdat, paste0(dir_in, "Intermed-rasters/sndv"), overwrite=TRUE)
raster::writeRaster(mudv_rawdat, paste0(dir_in, "Intermed-rasters/mudv"), overwrite=TRUE)

rckv <- raster::raster(paste0(dir_in, "Intermed-rasters/rckv"))
gvlv <- raster::raster(paste0(dir_in, "Intermed-rasters/gvlv"))
sndv <- raster::raster(paste0(dir_in, "Intermed-rasters/sndv"))
mudv <- raster::raster(paste0(dir_in, "Intermed-rasters/mudv"))

## Change -99 to NA
rckv[rckv < 0] <- NA
gvlv[gvlv < 0] <- NA
sndv[sndv < 0] <- NA
mudv[mudv < 0] <- NA

## Scale composition from 100% to 0-1
rckv <- rckv / max(values(rckv), na.rm=T)
gvlv <- gvlv / max(values(gvlv), na.rm=T)
sndv <- sndv / max(values(sndv), na.rm=T)
mudv <- mudv / max(values(mudv), na.rm=T)

## Write out ASCII files for ecospace
raster::writeRaster(rckv, paste0(dir_out, "/seabed-sedcomp-rock"),   format = 'ascii', overwrite=TRUE)
raster::writeRaster(gvlv, paste0(dir_out, "/seabed-sedcomp-gravel"), format = 'ascii', overwrite=TRUE)
raster::writeRaster(sndv, paste0(dir_out, "/seabed-sedcomp-sand"),   format = 'ascii', overwrite=TRUE)
raster::writeRaster(mudv, paste0(dir_out, "/seabed-sedcomp-mud"),    format = 'ascii', overwrite=TRUE)

## Figure
png(paste0(dir_fig, "Ecospace-seabed-types.png"), 
    width = 9, height = 6, units = "in", res=1200)
par(mfrow=c(2,2))
plot(rckv, colNA = "gray", main = "Rock"); plot(gvlv, colNA = "gray", main = "Gravel")
plot(sndv, colNA = "gray", main = "Sand");  plot(mudv, colNA = "gray", main = "Mud");  
dev.off()

################################################################################
## 
## Corals Essential Fish Habitat

shp_corals <- sf::st_read("./Ecospace-habitat-maps/Data-inputs/NCEI-GOM-data-atlas/Corals-EFH/Coral_EFH_GOM.shp")
str(shp_corals)
shp_corals$const = 1
coral_ras <- raster::rasterize(shp_corals, depth, field = "const")

## Write out ASCII files for ecospace
raster::writeRaster(coral_ras, paste0(dir_out, "/coral-hab"),   format = 'ascii', overwrite=TRUE)

## Figure
png(paste0(dir_fig, "Ecospace-coral-hab.png"), 
    width = 6, height = 4, units = "in", res=1200)
plot(coral_ras, main="Coral habitat")
dev.off()

################################################################################
## 
## Seagrass
shp_sav <- sf::st_read("./Ecospace-habitat-maps/Data-inputs/NCEI-GOM-data-atlas/SAV/Seagrass_ALFLMSTX.shp")
shp_sav$const = 1
shp_sav2 <- shp_sav[!st_is_empty(shp_sav),,drop=FALSE] ## need to remove NAs
sav_ras <- terra::rasterize(shp_sav2, depth, field = "const")
plot(sav_ras, main="Seagrass habitat")

## Write out ASCII files for ecospace
raster::writeRaster(sav_ras, paste0(dir_out, "/seagrass-hab"),   
                    format = 'ascii', overwrite=TRUE)

## Figure
png(paste0(dir_fig, "Ecospace-seagrass-hab.png"), 
    width = 6, height = 4, units = "in", res=1200)
plot(sav_ras, main="Seagrass habitat")
dev.off()

################################################################################
## 
## HMS
shp_hms <- sf::st_read("./Ecospace-habitat-maps/Data-inputs/NCEI-GOM-data-atlas/HMS-EFH/ahms_efh.shp")
shp_hms$const = 1
plot(shp_hms["DDBOX"])
shp_hms$DDBOX <- as.factor(shp_hms$DDBOX)
bluefin <- subset(shp_hms, shp_hms$DDBOX == "Bluefin Tuna")
plot(bluefin["DDBOX"])
unique(shp_hms$DDBOX)
