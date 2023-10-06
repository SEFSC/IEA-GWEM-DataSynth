
## -----------------------------------------------------------------------------
##
## Function to return date label in the form of "YYYY-YYYY"

get_year_range <- function (raster_stack) {
  last <-  nlayers(raster_stack)
  date_range <- paste0(names(raster_stack[[1]]), "-", names(raster_stack[[last]]))
  date_label <- gsub("X([0-9]{4})\\.[0-9]{2}-X([0-9]{4})\\.[0-9]{2}", "\\1-\\2", date_range) 
  return(date_label)
}

## -----------------------------------------------------------------------------
##
## Function to overwrite file
check_directory <- function(folder_name, overwrite = FALSE) {
  ## First, check if folder exists
  if (dir.exists(folder_name)) {
    ## Second, if 'overwrite' is TRUE, remove and recreate the folder
    if (overwrite) {
      unlink(folder_name, recursive = TRUE)
      dir.create(folder_name)
      cat(sprintf("OVERWRITING: '%s' \n", folder_name))
    } else {
      cat(sprintf("FOLDER: '%s'\n", folder_name))
    }
  } else {
    # If folder doesn't exist, create it
    dir.create(folder_name)
    cat(sprintf("CREATED: '%s'\n", folder_name))
  }
}

## -----------------------------------------------------------------------------
##
## Build and run function to smooth NAs along the coast 

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

## -----------------------------------------------------------------------------
##
## PLOT SPATIAL-TEMPORAL ENVIRONMENTAL DRIVERS 
##           AND MAKE MULTI-PAGE PDFs

library(terra)
library(stringr)
library(viridis)

## Function to make PDF of maps with each page showing 12 months in that year
pdf_map = function(plt.stack, colscheme = 'brks', dir = './', env_name = '', modtype = '', 
                   mintile = 0.01, maxtile = 0.99, ylab_name = ''){

  maxval = as.numeric(quantile(values(plt.stack), maxtile, na.rm=T)) 
  minval = ifelse (mintile == 'zero', 0,
                   as.numeric(quantile(values(plt.stack), mintile, na.rm=T)))
  print(paste(env_name, "plotting range:", minval, "-", maxval))

  ## Determine color scheme
  brks = seq(0, maxval, maxval/50)
  if(colscheme == 'turbo'){
    color   = viridis(min(length(brks),100), option = "H")
    colid   = "col-turbo"
  } else if (colscheme == 'virid'){
    color   = viridis(min(length(brks),100), option = "D")
    colid   = "col-viridis"
  } else if (colscheme == 'rev-virid'){
    color   = rev(viridis(min(length(brks),100), option = "D"))
    colid   = "col-viridis"
  } else if (colscheme == 'brks') {
    colv    = c("purple4","purple", "blue", "darkblue", "cyan", "green","darkgreen", "yellow", "orange", "red", "darkred")
    funpal  = colorRampPalette(colv,bias=2)
    nbcols  = length(brks)-1
    color   = funpal(nbcols) 
    colid   = "col-brks"
  }
  
  ## Determine years
  start = as.numeric(str_sub(names(plt.stack)[1], 2, 5))
  stop  = as.numeric(str_sub(names(plt.stack)[nlayers(plt.stack)], 2, 5))
  yrs = seq(start, stop, 1)
  
  ## Make PDF 
  namepdf = paste0(dir, env_name, "_", modtype, "_", start, "-", stop, ".pdf")
  pdf(namepdf, onefile = T)
  for(y in yrs){
    #  y = 1980
    print(paste("Plotting", y))
    plt.yr = raster::subset(plt.stack, grep(paste0('X', y), names(plt.stack), value = T, fixed = T))
    
    ## Plot 12 months
    par(mfrow=c(4,3),mar=c(1,1,2,0), oma=c(2,2,0,5))
    for(i  in 1:nlayers(plt.yr)){
      plot(plt.yr[[i]], legend=F, col=color, colNA='darkgray', zlim=c(minval, maxval), breaks=brks,
           main = names(plt.yr)[i])
    }
    ## Add legend
    par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,1))
    fields::image.plot(plt.yr,legend.only=T, zlim=c(minval, maxval),col=color,add=T,legend.width=1,
                       legend.mar=4,legend.line=3,
                       legend.lab = paste(env_name, ylab_name))
  }
  dev.off()
}
