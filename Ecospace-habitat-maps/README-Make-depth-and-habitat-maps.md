# Make_depth_maps.R

The script fetches bathymetric data from the NOAA server, converts it into rasters, and generates depth maps with different grid-cell resolutions. The bounding box and resolutions are parameters that can be customized to fit different regions and scale requirements.

```R
## Parameters: bounded area and resoluations
bbox = c(-98,-80.5, 24,31)    ## xmin, xmax, ymin, ymax
resolutions = c(4, 8, 16, 32) ## In lat. minutes
```

e.g., 4-minute resolution
```R
## Get depths and make rasters with different grid-cell resolutions,
depth04 = marmap::as.raster(getNOAA.bathy(bbox[1], bbox[2], bbox[3], bbox[4], resolution = resolutions[1])) #get data from NOAA server and put into raster
depth04[depth04 > 0] = NA; depth04 = depth04 * -1 # replace land with NA and make depth positive
. . .
depth_list = list(depth04, depth08, depth16, depth32)
```

We generate figures for each resolution and export the maps as ASCII files. 
```R
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
```
Different resolutions appear as such:
![Plot](./Depth_maps/4depthmaps-res-04-08-16-32-min.png)
For the USGWEM Ecospace, we use 08 minute resolution and a maximum depth of 400 m, making all cells over 400m deep equal to 400m.
``` R
## -----------------------------------------------------------------------------
## Use 08 min resolution for Ecospace
depth = depth08
min          = paste0(round(res(depth)[1]*60,0),'min') 
dims         = paste0(dim(depth)[1],'x',dim(depth)[2])
cellarea_km2 = paste0(round(sqrt(mean(getValues(area(depth))))), 'sqkm') ## Get surface area of each cell in km2
map_params   = paste(min, cellarea_km2, dims, sep='-'); map_params
```
The final depth map appears as such:
![Plot!](./Depth_maps/Depthmap_8min-14sqkm-53x131.png)

# Make_reef_maps.R
Generates hardbottom reef maps for the US Gulf-wide Ecospace model. Required libraries include `sf`, `ggplot2`, `raster`, `cowplot`.

## Make scaled hardbottom (AR and NR) maps for Ecospace
1. **Calculate habitat poxy** The scaled hardbottom maps are developed from the rasters identifying distances to artificial reefs `dist_to_AR` and natural hardbottom `dist_to_HB`. We make the habitat map by taking the inverse distance. Specifically, proxy distances to ARs are taken as the inverse distance and proxy distances to natural hardbottom use the inverse squared distance. This estimates a faster habitat decline from artificial reef and a longer, more gradual decline away from a natural reef area.

```R
## Take inverse distance
inv_AR = calc(dist_to_AR, fun = function(x) {1 / x}) 
inv_HB = calc(dist_to_HB, fun = function(x) {1 / sqrt(x)})
```
2. **Crop, resample, and scale**. These are then cropped and resampled match the extent, cell size, and resolution of the Ecopath base/depth map. Lastly, we scale all values between [0 , 1]. E.g., for hard bottom:
```R
crop_AR = crop(inv_AR, depth) ## Crop to match extent of EwE base/depth map
resamp_AR = resample(crop_AR, depth) ## Resample to match cell size and resolution
scaled_HB = calc(resamp_HB, fun = function(x) { ## Scale to one
  x / max(values(resamp_HB), na.rm=TRUE)
})
```
Finally, these are written out as ASCII files for Ecospoace and PNG images. The maps appear as follows:
![Plot!](./Figures/Ecospace-hardbottom-ARs.png)

# Make_fish_habitats.R
We use data collated by the [NOAA Gulf of Mexico Data Atlas](https://www.ncei.noaa.gov/products/gulf-mexico-data-atlas) This data atlas provides information about the physical environment, marine resources, socioeconomic activity, and other aspects of the Gulf of Mexico that can be used to identify baseline conditions, status, and trends of the ecosystem. It includes data from all five Gulf States (Alabama, Florida, Louisiana, Mississippi, and Texas), and its seaward boundaries extend to the Yucatan Channel and the Straits of Florida. We generate habitat maps for **rock**, **gravel**, **sand**, **mud**, **coral**, and **seagrasses**. 

## Seabed composition
A seabed composition shape file is used to generate ASCII habitat maps for rock, gravel, sand, and mud. Sediment characteristics here are described as follows: if the most abundant of the seabed-sized fractions of rock, gravel, sand, or mud is >66%, then it is said to be dominant. If the most abundant of these is >33%, then it is subdominant. Rock and gravel provide microhabitats for organisms and favor attached epibenthos (e.g., suspension feeders). However, rock is more stable. Sand is the most mobile of substrates and encourages vagrant and active burrowing forms. Mud is usually stable on the scale of burrows because of its cohesiveness and favors infauna, and also tends to have the highest organic carbon contents.

- Seabed Mud Content: Mud is the sediment-size fraction finer than 63 µm, with abundance expressed as percentage by weight. Computing separate maps for silt (coarse mud) and clay (mud finer than 2 µm) contents is not possible because their routine measurement is unreliable. Nevertheless, clay content is a strong determinant of how cohesive (sticky) the bottom is.
- Seabed Sand Content: Sand is the sediment-size fraction coarser than 63 µm but finer than 2 mm. The sand percentages are by weight.
- Seabed Gravel Content: Gravel is the sediment-size fraction coarser than 2 mm. At the coarser end gravel includes cobbles and boulders. In this database, objects larger than 256 mm are regarded as rock, beyond gravel. The gravel percentages are by weight.
- Seabed Rock Content: Rock occurs as crusts and pavements, pinnacles, and bedrock outcrops. Areas of hard biological framework, such as coral-algal reef fronts, can also be classified as rock. Rock is difficult to map by direct sampling or even visual observations, so results from geophysical techniques, such as sidescan sonar, sonar acoustic classification, and seismic wave analysis, were included in the mapping. Rock percent is in terms of estimated areal exposure, thus allowing for sediment veneers that may be present.

More info: Jenkins C. Dominant Bottom Types and Habitats In Gulf of Mexico Data Atlas [Internet]. Stennis Space Center (MS): National Centers for Environmental Information; 2011. [5 screens]. Available from: https://gulfatlas.noaa.gov/.

Creating the Ecospace habitat map for rock, for example, is done with the following code: 
```R
## Rasterize polygons
rckv_rawdat <- raster::rasterize(seabed, depth, field = "gom_rckv")
## Write out rasters and read back (rasterizing is slow)
raster::writeRaster(rckv_rawdat, paste0(dir_in, "Intermed-rasters/rckv"), overwrite=TRUE)
rckv <- raster::raster(paste0(dir_in, "Intermed-rasters/rckv"))
## Change -99 to NA
rckv[rckv < 0] <- NA
## Scale composition from 100% to 0-1
rckv <- rckv / max(values(rckv), na.rm=T)
## Write out ASCII files for ecospace
raster::writeRaster(rckv, paste0(dir_out, "/seabed-sedcomp-rock"),   format = 'ascii', overwrite=TRUE)
```

## Coral fish habitat

## Seagrasses
Submerged aquatic vegetation (SAV or seagrasses) in the Gulf of Mexico region are crucial benthic habitats that sustain a wide range of ecological functions and human activities. Seagrass ecosystems provide critical habitat for finfish, shellfish, and crustaceans, as well as numerous threatened and endangered species such as sea turtles and bottlenose dolphins. The root and rhizome system of seagrass forms an intertwined mat underground, which stabilizes the seabed. It also helps in maintaining water clarity and reducing wave energy by trapping sediment and reducing the overgrowth of algae, thereby enhancing water quality. The health and productivity of seagrass habitats directly impact key industries in the Gulf region, such as commercial and recreational fisheries and tourism. These industries contribute hundreds of millions of dollars to local communities, thereby underlining the economic significance of seagrass ecosystems.

More info: Handley L. Submerged Aquatic Vegetation In Gulf of Mexico Data Atlas [Internet]. Stennis Space Center (MS): National Centers for Environmental Information; 2011. [1 screen]. Available from: https://gulfatlas.noaa.gov/.

The code to create the ASCII habitat file from the NCEI shape file is relatively staightfoward: 

