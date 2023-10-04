## -----------------------------------------------------------------------------
##
## Write out ASCII files for Ecospace 

rm(list=ls()); rm(.SavedPlots); graphics.off(); gc(); windows(record=T)
library(raster)
library(viridis)
options(scipen=10)

##------------------------------------------------------------------------------
##
## Set up directory paths
model     = "HYCOM"
datelabel = "1993-01 to 2020-12"
dir.in   <- "./Ecospace-environmental-drivers/HYCOM/"
dir.ras.out  <- "./Ecospace-environmental-drivers/Outputs/Bricks/"
fld.asc.out  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/"
dir.asc.avg  <- "./Ecospace-environmental-drivers/Outputs/ASCII-for-ecospace/Averages/"
dir.pdf.out  <- "./Ecospace-environmental-drivers/Outputs/PDF-maps/"
source("./Ecospace-environmental-drivers/0-Make-PDF-maps-function.R") ## Call PDF-map function

## HIRES monthly stacks
t.surf.hycom = stack( paste0(dir.in, 'HYCOM GOM temp surface ', datelabel))
t.bot.hycom  = stack( paste0(dir.in, 'HYCOM GOM temp bottom ', datelabel))
t.avg.hycom  = stack( paste0(dir.in, 'HYCOM GOM temp avg ', datelabel))
s.surf.hycom = stack( paste0(dir.in, 'HYCOM GOM salinity surface ', datelabel))
s.bot.hycom  = stack( paste0(dir.in, 'HYCOM GOM salinity bottom ', datelabel))
s.avg.hycom  = stack( paste0(dir.in, 'HYCOM GOM salinity avg ', datelabel))

## Resampled and smoothed stacks
t.surf.smoo  = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM temp surface 1993-01 to 2020-12.grd"))
t.bot.smoo   = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM temp bottom 1993-01 to 2020-12.grd"))
t.avg.smoo   = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM temp avg 1993-01 to 2020-12.grd"))
s.surf.smoo  = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM salinity surface 1993-01 to 2020-12.grd"))
s.bot.smoo   = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM salinity bottom 1993-01 to 2020-12.grd"))
s.avg.smoo   = stack( paste0(dir.in, "Resamp-smoothed HYCOM GOM salinity avg 1993-01 to 2020-12.grd"))

## Calculate average to intialize Ecospace -------------------------------------
avg.t.surf = calc(t.surf.smoo, mean)
avg.t.bot  = calc(t.bot.smoo,  mean)
avg.t.avg  = calc(t.avg.smoo,  mean)
avg.s.surf = calc(s.surf.smoo, mean)
avg.s.bot  = calc(s.bot.smoo,  mean)
avg.s.avg  = calc(s.avg.smoo,  mean)
#avg.t.avg = stackApply(t.avg.smoo, indices =  rep(1, nlayers(t.avg.smoo)), fun = "mean", na.rm = T)

## Plot check
par(mfrow=c(3,2))
plot(avg.t.surf, colNA='black',  main = "Temp surf")
plot(avg.t.bot,  colNA='black',  main = "Temp bot")
plot(avg.t.avg,  colNA='black',  main = "Temp avg")
plot(avg.s.surf, colNA='black',  main = "Sal surf")
plot(avg.s.bot,  colNA='black',  main = "Sal bot")
plot(avg.s.avg,  colNA='black',  main = "Sal avg")
par(mfrow=c(1,1)) 

## Save average GOM ascii layers for Ecospace ----------------------------------
writeRaster(avg.t.surf, paste0(dir.asc.avg,"Avg_temp_surf"), format='ascii', overwrite=T)
writeRaster(avg.t.bot,  paste0(dir.asc.avg,"Avg_temp_bot"),  format='ascii', overwrite=T)
writeRaster(avg.t.avg,  paste0(dir.asc.avg,"Avg_temp_avg"),  format='ascii', overwrite=T)
writeRaster(avg.s.surf, paste0(dir.asc.avg,"Avg_saln_surf"), format='ascii', overwrite=T)
writeRaster(avg.s.bot,  paste0(dir.asc.avg,"Avg_saln_bot"),  format='ascii', overwrite=T)
writeRaster(avg.s.avg,  paste0(dir.asc.avg,"Avg_saln_avg"),  format='ascii', overwrite=T)

## -----------------------------------------------------------------------------
##
## Make ASCII files (with replicates of monthly avg before data starts) 
## for the Ecospace
## HYCOM data start in 1993, so we need to make dummy copies per month from 
## Jan 1980 to Dec 1993

## Loop along list
overwrite <- 'y'
smoothed_stack_list <- list(t.surf.smoo, t.bot.smoo, t.avg.smoo, s.surf.smoo, s.bot.smoo, s.avg.smoo)
hires_stack_list    <- list(t.surf.hycom, t.bot.hycom, t.avg.hycom, s.surf.hycom, s.bot.hycom, s.avg.hycom)
env_dr_list <- c("Temp-surf", "Temp-bot", "Temp-avg", "Sal-surf", "Sal-bot", "Sal-avg")
col_list <- c("turbo", "turbo", "turbo", "virid", "virid", "virid")

for (i in 1:length(smoothed_stack_list)){
    # i = 1
  ## Input parameters-----------------------------------------------------------
  env_driver = env_dr_list[i]
  ras   = stack(smoothed_stack_list[i])
  hires = stack(hires_stack_list[i])
  dir.asc.out = paste0(fld.asc.out, env_driver, "/")
  if(overwrite == 'y') {unlink(dir.asc.out, recursive = TRUE); dir.create(dir.asc.out)} 
  print(paste("Env. driver = ", env_driver, "| Folder:", dir.asc.out))
  
  ## Make dataframe of dates from raster layers --------------------------------
  ras.dates = data.frame(year=substr(names(hires),2,5),month=substr(names(hires),7,8))
  ras.dates$yrmo = paste0(ras.dates$year, "-", ras.dates$month)
  head(ras.dates); tail(ras.dates)
  mo = unique(ras.dates$month)
  
  ## Make dataframe of year months before HYCOM data starts
  enddummy = min(as.numeric(ras.dates$year))-1
  yr = 1980:enddummy
  dummy.dates = data.frame(year = character(), month = character())
  for(y in yr){
    for(m in mo){
      dummy.dates = rbind(dummy.dates, c(y,m))
    }
  }
  colnames(dummy.dates) = c("year", "month")
  dummy.dates$yrmo = paste(dummy.dates$year, dummy.dates$month, sep="-")
  head(dummy.dates); tail(dummy.dates)
  
  ## Get monthly averages -------------------------------------------------------
  month.stack = stack()
  for (month in mo){
    #month = "01"
    subset.month = raster::subset(ras, grep(paste0('.', month), names(ras), value = T, fixed = T))
    month.avg = calc(subset.month, mean)
    names(month.avg) = paste0(env_driver, "_mo", month, "_avg", nlayers(subset.month),"y")
    month.stack = addLayer(month.stack, month.avg)
  }  
  
  ## Plot check
  par(mfrow=c(3,4))
  plot(month.stack, colNA = 'black', 
       zlim=c(min(values(month.stack), na.rm=T), max(values(month.stack), na.rm=T))
  )
  par(mfrow=c(1,1))
  names(ras.comb)
  
  ## Combine dummy raster set (1980-1992) and data (1993-2022)
  rep.stack = stack()
  for (year in yr){
    #year = 1980
    xx = month.stack
    names(xx) = paste0(year, "_", stringr::str_sub(labels(month.stack), start=-11))
    rep.stack = addLayer(rep.stack, xx)
  }
  ras.comb = raster::stack(rep.stack, ras)
  
  ## Write out raster
  start = as.numeric(str_sub(names(ras.comb)[1], 2, 5))
  stop  = as.numeric(str_sub(names(ras.comb)[nlayers(ras.comb)], 2, 5))
  
  ## Write out files -----------------------------------------------------------
  ## Make PDF of plots
  ## Set plotting maximum to maximum of 99th percentile by month
  pdf_map(ras.comb, colscheme = col_list[i], dir = dir.pdf.out, 
          env_name = env_driver, mintile = 0.0001, maxtile = 0.9999, modtype = model)
  
  ## Save raster
  writeRaster(ras.comb, paste0(dir.ras.out, 'EwE_Maps_', env_driver, '_', start, '-', stop), overwrite=T)
  
  ## ASCII files by month
  writeRaster(ras.comb, paste0(dir.asc.out, env_driver), bylayer=T, suffix = names(ras.comb), 
              format = 'ascii', overwrite=T)
  }



