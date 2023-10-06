## -----------------------------------------------------------------------------
##
## Write out ASCII files for Ecospace 

rm(list=ls())
library(raster)
library(viridis)
library(tidyverse)

## Set up directory paths
model     = "MOM6"
datelabel = "1960-01 to 2010-12"
dir_in   <- "./Ecospace-environmental-drivers/MOM6/data_downloads/"
dir_ras_out  <- "./Ecospace-environmental-drivers/Outputs/Bricks/"
fld_asc_out  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/"
dir_asc_avg  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/Averages/"
dir_pdf_out  <- "./Ecospace-environmental-drivers/Outputs/PDF-maps/MOM6-ISIMIP3a/"
source("./Ecospace-environmental-drivers/0-Make-PDF-maps-function.R") 
source("./Ecospace-environmental-drivers/0-Smoth-NA-function.R")

## Read in depth/base map
depth <- raster("./global-data/shorelinecorrected-basemap-depth-131x53-08 min-14sqkm.asc")

## Get file paths to the downloaded files
data_file <- list.files(path = './MOM6/data_downloads/', pattern = "nc$", full.names = T)
num_vars = length(data_file)
print(data_file)

## Open these as a list of raster brcks
raster_list <- lapply(data_file, brick) ## We just need level 1 so we can ignore the warning that "level" is set to 1

## Extract variable names
var_names <- character(num_vars)
for(i in 1:num_vars){
  file_name <- raster_list[[i]]@file@name
  var_name <- gsub(".*obsclim_([^_]+)_15arcmin.*", "\\1", file_name)
  var_names[i] <- var_name
}
print(var_names)

## -----------------------------------------------------------------------------
##
## Loop along list

col_list <- c("brks", "turbo", "turbo", "virid", "virid", "virid")
overwrite <- 'y'
i = 8

##
for (i in 1:num_vars){
  ras_orig = raster_list[[i]]
  var = var_names[i]
  var_desc <- attributes(ras_orig)$title
  print(var); print(var_desc)
  
  ## Make dataframe/tibble object from raster stack
  df_ras <- rasterToPoints(ras_orig) %>% 
    as.data.frame() %>%   #Changing to data frame
    pivot_longer(cols = -c(x, y), names_to = "months_from_date") %>% #Reshaping data frame
    mutate(months_from_date = as.numeric(str_remove(months_from_date, "X")), #Removing the X before the number of months and turning into numeric data
           date = ymd("1901-01-01") %m+% months(months_from_date)) %>% #Calculate date
    mutate(YM = format(date, "%Y-%m")) %>% 
    dplyr::select(!months_from_date)  #Removing the months column
  df_ras$var = var
  
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
  
  ## Iteratively run smooth.na function ------------------------------------------
  ras_smoo = ras ## Initialize
  stepsneeded = 4
  smoothsize = 3
  
  for (j in 1:stepsneeded){
    print(paste(var, "smoothing - step", j, "-", format(Sys.time(), "%H:%M:%S")))
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
  
  ## ----------------------------------------------------------------------------
  ## Write out files
  ## Make PDF of plots
  ## Set plotting maximum to maximum of 99th percentile by month
  pdf_map(ras_subset, colscheme = "turbo", dir = dir_pdf_out, 
          env_name = var, mintile = 0.0001, maxtile = 0.9999, modtype = model, ylab_name = var_desc)
  
  ## Save raster
  #writeRaster(ras.comb, paste0(dir.ras.out, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)
  
  ## ASCII files by month
  #writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
  #            format = 'ascii', overwrite=T)
  
}
