## -----------------------------------------------------------------------------
##
## Write out ASCII files for Ecospace 

rm(list=ls()); rm(.SavedPlots); graphics.off(); gc(); windows(record=T)
library(raster)
library(viridis)
library(tidyverse)

##------------------------------------------------------------------------------
##
## Set up directory paths
model     = "MOM6"
datelabel = "1960-01 to 2010-12"
dir_in   <- "./Ecospace-environmental-drivers/MOM6/"
dir_ras_out  <- "./Ecospace-environmental-drivers/Outputs/Bricks/"
fld_asc_out  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/"
dir_asc_avg  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/Averages/"
dir_pdf_out  <- "./Ecospace-environmental-drivers/Outputs/PDF-maps/"
source("./Ecospace-environmental-drivers/0-Make-PDF-maps-function.R") ## Call PDF-map function

## Read in depth/base map
depth <- raster("./global-data/shorelinecorrected-basemap-depth-131x53-08 min-14sqkm.asc")


## -----------------------------------------------------------------------------
##
## Loop along list
i = 1
overwrite <- 'y'
#stack_list <- list(t.surf.smoo, t.bot.smoo, t.avg.smoo, s.surf.smoo, s.bot.smoo, s.avg.smoo)
#orig_stack_list    <- list(t.surf.hycom, t.bot.hycom, t.avg.hycom, s.surf.hycom, s.bot.hycom, s.avg.hycom)
env_dr_list <- c("ChlA", "Temp-bot", "Temp-avg", "Sal-surf", "Sal-bot", "Sal-avg")
col_list <- c("brks", "turbo", "turbo", "virid", "virid", "virid")




env_driver = env_dr_list[i]


## Read in monthly stack
ras_orig = stack(paste0(dir_in, "MOM6_chl_1961-01_2010-12"))

## Make dataframe/tibble object from raster stack
df_ras <- rasterToPoints(ras_orig) %>% 
  as.data.frame() %>%   #Changing to data frame
  pivot_longer(cols = -c(x, y), names_to = "months_from_date") %>% #Reshaping data frame
  mutate(months_from_date = as.numeric(str_remove(months_from_date, "X")), #Removing the X before the number of months and turning into numeric data
         date = ymd("1901-01-01") %m+% months(months_from_date)) %>% #Calculate date
  mutate(YM = format(date, "%Y-%m")) %>% 
  dplyr::select(!months_from_date)  #Removing the months column
str(df_ras) #Checking results

## Rename layers to year-month
length(names(ras_orig)) == length(unique(df_ras$YM)) ## Ensure they're the same length
names(ras_orig) = unique(df_ras$YM)

## Get start month, end month, and number of months
start_date <- df_ras$YM[1]
end_date <- df_ras$YM[nrow(df_ras)]
n_months <- length(unique(df_ras$YM))

## Crop and resample to basemap grid 
ras = crop(ras_orig, depth)
ras = resample(ras, depth)
dim(depth); dim(ras) ## Check that dimensions should match but with different number of layers

## Plotchecks
par(mfrow=c(3,2))
plot(ras_orig[[1]], colNA = 'black')
plot(ras[[1]], colNA = 'black', main = paste("First month", names(ras[[1]])))
plot(ras_orig[[n_months]], colNA = 'black')
plot(ras[[n_months]], colNA = 'black', main = paste("Last month", names(ras[[n_months]])))
plot(depth)

## Check with basemap
depthcheck = depth
depthcheck[depthcheck == 0] = NA ## Convert 0s to NAs
par(mfrow=c(2,2))
plot(ras_orig[[n_months]], colNA = 'black')
plot(ras[[1]], colNA = 'black')
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
  #s = s.surf.rs #size = 11
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
ras_smoo = ras ## Initialize

stepsneeded = 4
smoothsize = 3

for (j in 1:stepsneeded){
  print(paste("Step", j, Sys.time()))
  ras_smoo = smooth.na(ras_smoo, smoothsize)
}

## Mask smoothed rasters with depth map ----------------------------------------
depthNA = depth
depthNA[depthNA==0] = NA
ras_msk = mask(ras_smoo, depthNA)
names(ras_msk) = names(ras_orig)

## Plotcheck
par(mfrow=c(2,2))
plot(depthNA, colNA='black')
plot(ras_orig[[1]], colNA='black')
plot(ras_smoo[[1]], colNA='black')
plot(ras_msk[[1]], colNA = 'black')

## Subset of months that match Ecospace simulation period
index_X1980_01 <- grep("X1980.01", names(ras_msk))
ras_subset <- subset(ras_msk, index_X1980_01:nlayers(ras_msk))
head(names(ras_subset)); tail(names(ras_subset))

## Write out files -----------------------------------------------------------

## Make PDF of plots
## Set plotting maximum to maximum of 99th percentile by month
pdf_map(ras_subset, colscheme = col_list[i], dir = dir_pdf_out, 
        env_name = env_driver[i], mintile = 0.0001, maxtile = 0.9999, modtype = model)

## Save raster
writeRaster(ras.comb, paste0(dir.ras.out, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)

## ASCII files by month
writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
            format = 'ascii', overwrite=T)
