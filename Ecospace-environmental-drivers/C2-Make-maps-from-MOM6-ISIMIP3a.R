## -----------------------------------------------------------------------------
##
## Write out ASCII files for Ecospace 

rm(list=ls())
library(raster)
library(viridis)
library(tidyverse)
source("./Ecospace-environmental-drivers/0-Functions.R") 

## Set directory paths
model_name <-  "MOM6-ISIMIP3a"
dir_in     <- "./Ecospace-environmental-drivers/MOM6/data_downloads/"
dir_out    <- "./Ecospace-environmental-drivers/Outputs/"

## Set overwrite preferences
overwrite_pdf   <- TRUE
overwrite_avg   <- TRUE
overwrite_ascii <- TRUE

## -----------------------------------------------------------------------------
## Set-up

## Read in depth/base map
depth <- raster("./global-data/shorelinecorrected-basemap-depth-131x53-08 min-14sqkm.asc")

## Get file paths to the downloaded files
data_file <- list.files(path = './MOM6/data_downloads/', pattern = "nc$", full.names = T)
num_vars = length(data_file)
print(data_file)

## Open these as a list of raster bricks
print(paste("*** Ignore the warning that level is set to 1. We just need one level ***"))
raster_list <- lapply(data_file, brick) ## ignore the warning that "level" is set to 1. We just need one level. 

## Extract variable names
var_names <- character(num_vars)
for(i in 1:num_vars){
  file_name <- raster_list[[i]]@file@name
  var_name <- gsub(".*obsclim_([^_]+)_15arcmin.*", "\\1", file_name)
  var_names[i] <- var_name
}
print(var_names)

var_info <- data.frame(var_names = var_names, var_desc = NA)

## -----------------------------------------------------------------------------
##
## Loop along list

for (i in 1:num_vars){
#for (i in 8:8){
  options(scipen=10) ## Seems to fix: Error in if (getOption("scipen") <= min(digits)) { : missing value where TRUE/FALSE needed
  ras_orig = raster_list[[i]]
  var = var_names[i]
  var_desc <- attributes(ras_orig)$title
  var_info$var_desc[i] <- var_desc
  print(var_info[i,])

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
  
  ## Subset of months that match Ecospace simulation period --------------------
  index_X1980_01 <- grep("X1980.01", names(ras_orig))
  ras_subset_after1980 <- subset(ras_orig, index_X1980_01:nlayers(ras_orig))
  
  ## Crop and resample to basemap grid 
  ras = crop(ras_subset_after1980, depth)
  ras = resample(ras, depth)
  dim(depth); dim(ras) ## Check that dimensions should match but with different number of layers
  
  ## Iteratively run smooth.na function ----------------------------------------
  ras_smoo = ras ## Initialize
  stepsneeded = 4
  smoothsize = 3

  for (j in 1:stepsneeded){
    print(paste(var, "smoothing - step", j, "-", format(Sys.time(), "%H:%M:%S")))
    ras_smoo = smooth.na(ras_smoo, smoothsize)
  }
  
  ## Mask smoothed rasters with depth map --------------------------------------
  depthNA = depth
  depthNA[depthNA==0] = NA
  ras_msk = mask(ras_smoo, depthNA)
  names(ras_msk) = names(ras)
  
  ## Plotcheck
  par(mfrow=c(2,2))
  plot(depthNA, colNA='black', main = "Depth")
  plot(ras_orig[[1]], colNA='black', main = "Original")
  plot(ras_smoo[[1]], colNA='black', main = "Smoothed")
  plot(ras_msk[[1]], colNA = 'black', main = "Smoothed w/ Land Mask")
  par(mfrow=c(1,1))
  
  ## ---------------------------------------------------------------------------
  ## Get monthly averages for missing data (2010-2016 for MOM6)
  
  ## Make dataframe of dates from raster layers --------------------------------
  dates_ras = data.frame(year=substr(names(ras),2,5), month=substr(names(ras),7,8))
  dates_ras$yrmo = paste0(dates_ras$year, "-", dates_ras$month)
  head(dates_ras); tail(dates_ras)
  mo = unique(dates_ras$month)
  
  ## Make dataframe of year months after MOM6 ends
  startdummy = max(as.numeric(dates_ras$year))+1
  yr = startdummy:2016
  dummy_dates = data.frame(year = character(), month = character())
  for(y in yr){
    for(m in mo) dummy_dates = rbind(dummy_dates, c(y,m))
  }
  colnames(dummy_dates) = c("year", "month")
  dummy_dates$yrmo = paste(dummy_dates$year, dummy_dates$month, sep="-")
  head(dummy_dates); tail(dummy_dates)
  
  ## Get monthly averages -------------------------------------------------------
  month_stack = stack()
  for (month in mo){
    #month = "01"
    subset_month = raster::subset(ras_msk, grep(paste0('.', month), names(ras_msk), value = T, fixed = T))
    month_avg = calc(subset_month, mean)
    names(month_avg) = paste0(var, "_mo", month, "_avg", nlayers(subset_month),"y")
    month_stack = addLayer(month_stack, month_avg)
  }  
  
  ## Plot check
  par(mfrow=c(3,4))
  plot(month_stack, colNA = 'black', zlim=c(min(values(month_stack), na.rm=T), max(values(month_stack), na.rm=T)))
  par(mfrow=c(1,1))
  
  ## Combine data (1980-2010) and dummy raster set (2010-2016) 
  rep_stack = stack()
  for (year in yr){
    #year = 1980
    xx = month_stack
    names(xx) = paste0(year, "_", stringr::str_sub(labels(month_stack), start=-11))
    rep_stack = addLayer(rep_stack, xx)
  }
  
  ## Append averaged months on end 
  ras_subset <- raster::stack(ras_msk, rep_stack) 
  head(names(ras_subset)); tail(names(ras_subset))
  
  ## Calculate average to intialize Ecospace -----------------------------------
  avg_ras = calc(ras_msk, mean)
  par(mfrow=c(1,1))
  plot(avg_ras, colNA='black', main=paste("Global average", var, sep = " - "))
  
  ## ----------------------------------------------------------------------------
  ## Write out files
  ## Make PDF of plots
  
  ## Set output directories
  dir_pdf_out <- paste0(dir_out, "PDF-maps/", model_name,"/")
  dir_ras_out <- paste0(dir_out, "Bricks/", model_name,"/")
  fld_asc_out <- paste0(dir_out, "ASCII-for-ecospace/", model_name,"/")
  dir_asc_avg <- paste0(dir_out, "ASCII-for-ecospace/Averages/", model_name,"/")
  
  ## Function to get make date labels (YYYY-YYYY) from a raster stack
  date_label <- get_year_range(ras_subset)
  
  ## Write out global average ASCII
  check_directory(dir_asc_avg)
  filename <- paste("Avg", var, model_name, date_label, sep = "_")
  raster::writeRaster(avg_ras, filename = paste0(dir_asc_avg, filename), format = 'ascii', 
                      overwrite = overwrite_avg, NAflag = 0)
  
  ## Save raster brick
  check_directory(dir_ras_out)
  filename <- paste("EwE_Maps", var, model_name, date_label, sep = "_")
  raster::writeRaster(avg_ras, filename = paste0(dir_ras_out, filename), overwrite = TRUE)
  
  ## Write out monthly ASCII files for Ecospace
  check_directory(fld_asc_out)
  asc_folder <- paste0(fld_asc_out, var)
  check_directory(asc_folder, overwrite_ascii)
  dates_ras <- sub("^X", "", gsub("\\.", "-", names(ras_subset))) ## Get dates as YYYY-MM format
  asc_names <- paste(dates_ras, var, model_name, sep = "_") ## Starting with YYYY-MM allows Ecospace to set "Time" from file name (set to "year-month" in dropdown)
  raster::writeRaster(ras_subset, filename = paste0(asc_folder, "/", asc_names), bylayer=T, 
                      suffix = dates_ras, format = 'ascii', overwrite = overwrite_ascii, NAflag = -9999)
              
  ## Make PDF maps
  check_directory(dir_pdf_out)
  pdf_map(ras_subset, colscheme = "turbo", dir = dir_pdf_out, env_name = var, 
          mintile = 0.0001, maxtile = 0.9999, modtype = model_name, ylab_name = var_desc)
}; print(var_info)
