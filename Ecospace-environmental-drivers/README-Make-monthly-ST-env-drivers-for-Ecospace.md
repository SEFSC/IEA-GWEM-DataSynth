# US Gulf-Wide Ecospace Model (USGWEM) Environmental Drivers

This project generates spatial-temporal environmental drivers for the US Gulf-Wide Ecospace Model (USGWEM).
In Ecospace, spatial-temporal (ST) environmental drivers are used to provide dynamic environmental data for simulating and analyzing ecosystem dynamics. 

By incorporating monthly ST files for these environmental variables, the USGWEM simulates the interactions between species and a dynamic environment. These help capture the temporal variability and spatial heterogeneity of environmental conditions, allowing for a more realistic representation of ecosystem processes and responses to changing environmental conditions. For example, this allows the modeling of downstream impacts in the marine ecosystem from climatic changes in temperature patterns, precipitation, and circulation patterns through their effects Chl-A concentration, sea temperature, and salinity. Changes in sea temperature will also directly impact the thermal habitat of species, affecting their distribution, behavior, and ecological interactions. Collectively, these can provide insight into understanding and predicting the responses of the marine ecosystem to future environmental changes and supporting effective decision-making to promote climate resilient fisheries.

The US Gulf-Wide Ecospace Model (USGWEM) specifically incorporates monthly ST files for nutrients, temperature, and salinity:
1. **Chlorophyll-A (Chl-A) Concentration**: Chl-A is a proxy for primary productivity and serves as an indicator of phytoplankton biomass. Monthly Chl-A concentration data provides information about the availability of nutrients and drives primary production in the EwE model.
2. **Sea temperature**: Temperature influences the distribution and behavior of species, as well as the rates of biological processes. The USGWEM includes monthly surface, average, and bottom temperature data.
3. **Salinity**: Monthly salinity data provides can drive spatial distribution of species based on their physiological responses.

Data to to produce the ST files are collected by MODIS and HYCOM. 
- **MODIS** (Moderate Resolution Imaging Spectroradiometer) is a satellite-based sensor that provides high-resolution oceanographic data, including measurements of sea surface temperature, chlorophyll-a concentration, and other important environmental variables over large spatial scales.
- **HYCOM** (Hybrid Coordinate Ocean Model) is a numerical ocean model that integrates satellite observations, in situ measurements, and oceanographic data to simulate and forecast ocean currents, temperature, salinity, and other oceanographic variables with high spatial and temporal resolution.

Spatial-temporal environmental drivers are incorporated in Ecospace with external time series of maps that change spatially-explicit input values over time with data derived from external GIS data sources [(Steenbeek et al., 2013)](https://www.sciencedirect.com/science/article/pii/S0304380013002597?casa_token=vy7pmleLU54AAAAA:oYHkFWFcH9hnUZ2Qqpo7KzgZS4UdZJPoPLqpt-4B6t8Phhm-s5W4-EteILsoPT0sCRvYxc3r3w). These data files are gridded ASCII files. Since the data is geo-referenced, we convert the spatial information in such a way that the ASCII files contain one value per grid cell of our model area. 

The final outputs generage monthly ST files for 1980 to past 2016. We present a solution to needing ST data before data collection begins given that MODIS data collection does not begin until 2003 and HYCOM simulations begin in 1993. Monthly averages are taken (e.g., for Jan, Feb, etc.) are calculated from the entire available data set and used for the months simulated before data collection. 

The following README describes our work to query the relevant ST data, process it, and ultimately generate ST files for the Ecospoace USGWEM. 

## A1 Get MODIS data from ERDDAP
Downloads MODIS data from the [NOAA ERDAP server](https://coastwatch.pfeg.noaa.gov/erddap/index.html) and makes depth-integrated Chl-A data. Note that we also download ST maps for Cfl and POC, although they currently are not used in the USGWEM.

### Environmental drivers
- Depth-integrated Chlorophyll-A (ChlA)
- Normalized carbon flourescense (Cfl)
- Particulate organic carbon (POC)

### Dependencies
- curl
- raster
- rerddap

### Data source
The NOAA ERDDAP server (Environmental Research Division's Data Access Program) provides access to a wide range of oceanographic and environmental datasets. The server offers a convenient interface to search, access, and retrieve data using various query parameters. 

### Code setup and usage
1. Specifies the bounding box of the Gulf of Mexico (GOM) region and the directory where the MODIS data will be stored. The environmental variables of interest (Cfl, chla, POC) are defined in the `vars` vector.

### Process MODIS Data from NOAA ERDDAP Server
1. Loop over each environmental variable to retrieve monthly and daily composite imagery from the NOAA ERDDAP server. This is done using the `rerddap` package in R, which provides an interface to access and retrieve data from the NOAA ERDDAP data portal.
2. Process the MODIS data to create monthly raster files. Retrieves the monthly composite imagery for each variable and creates a raster stack using the `raster` package. The raster stack contains each monthly composite as a separate layer. 
3. Integrate surface chlorophyll-a over the euphotic depth (see below).
4. Write the resulting raster files to the specified output directory. The code saves the monthly raster stack and the integrated surface chlorophyll-a over euphotic depth as separate raster files in the specified directory.

#### Integrating Surface Chlorophyll-a over Euphotic Depth
**Euphotic depth** refers to the depth in the water column where photosynthetically active radiation (PAR) is sufficient to support photosynthesis.
- Calculate the euphotic depth (Zd.m) using the Lee (2007) z1% algorithm. The algorithm uses the relationship between surface chlorophyll-a concentration and euphotic depth to estimate the depth at which 1% of the surface PAR remains. Total chlorophyll-a concentration (Ctot) in the euphotic zone is calculated based on equations 3b and 3c in Morel and Berthon (1989). The equations consider different scenarios for low and high chlorophyll-a concentrations.
```{r eval=FALSE}
    Zd.m = calc(s.m3, function(x) 34*(x^-0.39)) #Lee (2007) z1%
    Ctot = calc(s.m3,function(x) ifelse(x<=1,38*x^0.423,40.3*x^0.505))
```
- Euphotic depth (Zehat) is calculated using equation 6 in Morel and Maritorena (2001), which provides an updated equation based on the work of Morel and Berthon (1989). The equation takes into account the total chlorophyll-a concentration and calculates the euphotic depth as a function of chlorophyll-a.
```{r eval=FALSE}
    Zehat1 = 912.5*Ctot^-0.839  #for Ze<102m
    Zehat2 = 426.3*Ctot^-0.547  #for Ze>102m
```
- Mean chlorophyll-a concentration in the euphotic depth (Cze) is calculated by dividing the total chlorophyll-a concentration (Ctot) by the euphotic depth (Zehat). Lastly, the integrated chlorophyll-a concentration (Cint) over the true euphotic depth from the satellite data is obtained by multiplying the mean chlorophyll-a concentration (Cze) with the euphotic depth (Zd.m).
The resulting integrated chlorophyll-a over euphotic depth is then saved as a raster file for further analysis.

### References
- Lee, Z. P., et al. (2007). Euphotic zone depth: its derivation and implication to ocean-color remote sensing. Journal of Geophysical Research, 112(C3).
- Morel, A., and Berthon, J. F. (1989). Surface pigments, algal biomass profiles, and potential production of the euphotic layer: relationships reinvestigated in review of remote-sensing applications. Limnology and Oceanography, 34(8), 1545-1562.
- Morel, A., and Maritorena, S. (2001). Bio-optical properties of oceanic waters: A reappraisal. Journal of Geophysical Research: Oceans, 106(C4), 7163-7180.
 
## A2 Make Maps from MODIS
This code generates monthly ST maps from spatial-imagery MODIS data for three environmental drivers: depth-integrated chlorophyll-A (ChlA), normalized carbon fluorescence (Cfl), and particulate organic carbon (POC). The code reads in a raster stack for each environmental driver. It crops and resamples them match the base/depth map, creates monthly averages for years needed before data-collection, and creates a global average. Note that the code assumes that the necessary MODIS data files are available in the specified input directory (`dir.ras.in`) and follows a specific file naming convention.

### Outputs
- Monthly spatial-temporal ASCII files for Ecospace,
- PDF maps for visualization,
- and a single global-average map for each environmental driver. 

### Make PDF and ASCII monthly maps
1. The code is divided into sections for each environmental driver: ChlA, Cfl, and POC. Each section includes a similar code structure for processing the data, generating maps, and writing outputs.
2. To use the code, modify the directory paths (`dir.ras.in`, `dir.ras.out`, `fld.asc.out`, `dir.asc.avg`, `dir.pdf.out`, `depth08min`) 
3. Customize the environmental driver specific settings and parameters within each section (e.g., `env_driver`, `overwrite`, `colscheme`, `mintile`, `maxtile`).
4. The code automatically crops and resamples each environmental driver map to match the base/depth map (`depth08min` variable) before further processing. This ensures spatial consistency between the environmental driver maps and the base/depth map.
5. The code automatically handles the scenario where MODIS data is unavailable before 2003. It creates monthly means for the years 1980 to December 1992 using the available data. Starting from 1993, the monthly data is used directly for mapping.
6. A single global-average map is generated for each environmental driver using the `mean` function. These global-average maps represent the average value across all available data points for each driver and are saved as ASCII files. These files can be used to initialize Ecospace simulations.
8. The code generates PDF maps using the `pdf_map` function, which visualizes the monthly maps for each year. The PDF maps are saved in the specified output directory (`dir.pdf.out`).

## B1 Get HYCOM data
This R script downloads HYCOM (Hybrid Coordinate Ocean Model) data from the hycom.org website. HYCOM provides oceanographic data such as temperature and salinity at different depths in the Gulf of Mexico region. This data can be used for various applications, including oceanographic research, ecosystem modeling, and climate studies. Specifically, we query HYCOM for netCDF files for temperature and salinity data. NetCDF (Network Common Data Form) is a file format commonly used for storing large scientific datasets. It allows for efficient storage and access of multidimensional data. Each netCDF file contains information on temperature or salinity at different depths for a specific time.

### Set-up
- The directory is set up to downloead netCDF files to an external hard drive due to their cumulative size. Trying to download them to a laptop resulted in the computer simply running out of available storage space. 
- The bounding box `bbox.gom`  specifies the geographical extent of the data to download. Our bounding box is for the Gulf of Mexico, and this could be adjusted for other uses.

### Usage 
1. Get list of HYCOM files on the FTP server. The code retrieves a list of HYCOM files available on the FTP server for different experiments and time periods.
2. Download HYCOM files. This section downloads the HYCOM files from the FTP server. It uses parallel processing to download multiple files simultaneously. The code sets up parallel processing using the makeSOCKcluster() function and the registerDoSNOW() function from the doSNOW package. It divides the files into chunks and assigns each chunk to a different core for downloading. It also displays a progress bar to track the download progress.
3. Build temperature and salinity raster bricks from HYCOM netCDF files. The code converts the downloaded HYCOM netCDF files into raster bricks for temperature and salinity. The code loops through the downloaded files, reads them as raster bricks using the brick() function from the terra package, and extracts the surface, bottom, and average values for temperature and salinity. It then adds these values to daily raster stacks.
4. Aggregate to month. Temperature and salinity values are extracted from the netCDF files as raster stacks with daily layers. These are aggregated by month. 
5. Write out raster stacks. Finally, the code writes the temperature and salinity raster stacks to ASCII files for the next steps described below. 

## B2 Resample and smooth HYCOM data maps
The objectives are to (1) crop and resample the HYCOM data maps to match the scale and resolution of the depth/base map and (2) smooth missing data caused by downscaling. 

1. **Crop and resample**: The HYCOM data maps are cropped to match the scale and extent of the depth/base map, and then resampled to have the same cell size and resolution as the depth/base map. This ensures consistency in spatial alignment. This creates an issue, however, in that the coastlines for depth and HYCOM maps are no longer aligned. The issue is particularly apparent for Louisiana marshlands.  
2. **Function to smooth missing**: To address this, a custom function called `smooth.na` is used.  Inputs include stack and pixel size of smoother, i.e., `size=3` means '3x3'. Size must be an odd number: e.g., 3x3, 5x5, 7x7.
```{r eval=FALSE}
smooth.na <- function(s, size = 3){
  middlecell = ceiling(size^2 / 2)
  ## Internal function: if cell is NA, fill with mean of surrounding grid
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
                          pad = TRUE, na.rm = FALSE)}
  return(newstack)
}
```
3. **Iteratively smooth data**. Run `smooth.na` iteratively to fill in the missing values by averaging information from the neighboring cells.
5. **Add land to maps**. Use `terra::mask()` to overlay land to the smoothed rasters. 
6. **Renaming and output**: Lastly, the rasters are checked and the code renames the layers in the processed stacks to match the original HYCOM data. The processed stacks are then written out as raster files for final processing in the next code section. 

## B3 Make HYCOM maps: Temperature and Salinity

This code conducts the third and final processing to generate monthly ST HYCOM data for six environmental drivers: surface temperature, bottom temperature, average temperature, surface salinity, bottom salinity, and average salinity. 

### Outputs (similiar to MODIS A2 code)
- Monthly spatial-temporal ASCII files for Ecospace,
- PDF maps for visualization,
- Single global-average map for each environmental driver.

### Make PDF and ASCII monthly maps
1. **Data pprep**: The code takes in smooth and resampled raster stacks for surface, bottom, and average temperature, as well as surface, bottom, and average salinity. These stacks contain monthly raster layers begining in 1993. Directory paths in the code include setting `dir.in`, `dir.ras.out`, `fld.asc.out`, `dir.asc.avg`, and `dir.pdf.out`.
2. **Global Mean Calculation**: The code calculates a global mean raster using the available HYCOM data. This global mean raster serves as an initial map for Ecospace analysis.
3. **Monthly Mapping**: The code creates a list of the raster stacks and loops through each stack. It automatically handles scenarios where HYCOM data is unavailable before 1993. For the years 1980 to December 1992, monthly means are calculated using the available data. From 1993 onwards, the monthly data is used directly for mapping. ASCII files for Ecospace analysis will be created in the `fld.asc.out` directory, containing monthly average data from 1980 to 2022.
4. **PDF Map Generation**: The code generates PDF maps using the `pdf_map` function to visualize the monthly maps for each year, and writes out the PDF to the specified output directory (`dir.pdf.out`). Map settings  can be customized by setting `colscheme`, `mintile`, and `maxtile`.
   
## Generate PDFs: the pdf_map function

This function, `pdf_map`, allows you to generate multi-page PDF maps (each page shows one year) to visualize. The plotting range are set by the minimum and maximum percentiles for all values in the data. The default for `mintile` and `maxtile` are 0.01 and 0.99, respectively, and may need to be adjusted to avoid anomalous values or excessive NAs.  

- Dependencies: `terra`, `stringr`, and `viridis`.
- Input parameters
   - `plt.stack`: The raster stack containing the monthly maps for the environmental driver.
   - `colscheme`: The color scheme to be used for the maps (e.g., 'brks', 'turbo', 'virid', 'rev-virid').
   - `dir`: The directory path where the PDF maps will be saved.
   - `env_name`: The name of the environmental driver.
   - `modtype`: The type of model or data source.
   - `mintile`: The minimum percentile value for color mapping (default is 0.01).
   - `maxtile`: The maximum percentile value for color mapping (default is 0.99).

#### Example
The PDF maps will be saved in the "./pdf_maps/" directory.
```R
## Define input parameters
plt.stack <- surf_temp
colscheme <- 'virid'
dir <- './pdf_maps/'
env_name <- 'Temperature'
modtype <- 'HYCOM'
mintile <- 0.001
maxtile <- 0.999

## Generate PDF maps
source("./Ecospace-environmental-drivers/0-Make-PDF-maps-function.R") 
pdf_map(plt.stack, colscheme, dir, env_name, modtype, mintile, maxtile)
```
