## -----------------------------------------------------------------------------
##
## FUNCTION: PLOT SPATIAL-TEMPORAL ENVIRONMENTAL DRIVERS 
##           AND MAKE MULIT-PAGE PDFs

## Function to make PDF of maps with each page showing 12 months in that year
pdf_map = function(plt.stack, colscheme = 'brks', dir = './', env_name = '', modtype = '', 
                   mintile = 0.01, maxtile = 0.99){
  #  plt.stack = ras.comb
  #  colscheme = 'virid'
  #  dir = dir.out
  #  env_name = env_driver
  #  modtype = "MODIS"
  #  mintile = 0; maxtile = .99
  
  library(terra)
  library(stringr)
  library(viridis)
  
  maxval = as.numeric(quantile(values(plt.stack), maxtile, na.rm=T)) 
  minval = ifelse (mintile == 'zero', 0,
                   as.numeric(quantile(values(plt.stack), mintile, na.rm=T)))
  print(paste(env_name, "plotting range:", minval, "-", maxval))
  #maxval = 40
  
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
    ##Add legend
    par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,1))
    fields::image.plot(plt.yr,legend.only=T, zlim=c(minval, maxval),col=color,add=T,legend.width=1,
                       legend.mar=4,legend.line=3,
                       legend.lab = env_name)
  }
  dev.off()
}
