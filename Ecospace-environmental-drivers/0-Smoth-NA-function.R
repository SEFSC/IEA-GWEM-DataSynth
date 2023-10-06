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