## -----------------------------------------------------------------------------
##
## Write out ASCII files for Ecospace 

rm(list=ls()); rm(.SavedPlots); graphics.off(); gc(); windows(record=T)
library(raster)
library(viridis)

##------------------------------------------------------------------------------
##
## Set up directory paths
model     = "MOM6"
datelabel = "1960-01 to 2010-12"
dir.in   <- "./Ecospace-environmental-drivers/MOM6/"
dir.ras.out  <- "./Ecospace-environmental-drivers/Outputs/Bricks/"
fld.asc.out  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/"
dir.asc.avg  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/Averages/"
dir.pdf.out  <- "./Ecospace-environmental-drivers/Outputs/PDF-maps/"
source("./Ecospace-environmental-drivers/0-Make-PDF-maps-function.R") ## Call PDF-map function

## Read in monthly stack
ras.orig = stack(paste0(dir.in, "MOM6_chl_1961-01_2010-12"))

## First, crop and resample to basemap grid 
ras = crop(chl.mom, depth08min)
ras = resample(ras, depth08min)
dim(depth08min); dim(ras) ## Check that dimensions should match but with different number of layers
plot(ras[[1]])

## Plotchecks
n_months = dim(ras)[3]
par(mfrow=c(3,2))
plot(chl.mom[[1]], colNA = 'black')
plot(ras[[1]], colNA = 'black', main = paste("First month", names(ras[[1]])))
plot(chl.mom[[n_months]], colNA = 'black')
plot(ras[[n_months]], colNA = 'black', main = paste("Last month", names(ras[[n_months]])))
plot(depth08min)

## Check with basemap
## Make 0s NAs and check
depthcheck = depth08min
depthcheck[depthcheck == 0] = NA
par(mfrow=c(2,2))
plot(ras[[1]], colNA = 'black', main = paste("First month", names(ras[[1]])))
plot(depthcheck, colNA = 'black', main = "depth/base map") ## Looks good

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
ras.smoo = ras ## Initialize

stepsneeded = 6
smoothsize = 3

for (j in 1:stepsneeded){
  print(paste("Step", j, Sys.time()))
  ras.smoo = smooth.na(ras.smoo, smoothsize)
}

## Mask smoothed rasters with depth map ----------------------------------------
depthNA = depth08min
depthNA[depthNA==0] = NA

ras.msk = mask(ras.smoo, depthNA)

## Plotcheck
par(mfrow=c(2,2))
plot(depthNA, colNA='black')
plot(ras.orig[[1]])
plot(ras.smoo[[1]])
plot(ras.msk[[1]], colNA = 'black')
