# US Gulf-Wide Ecospace Model (USGWEM) Environmental Drivers

This project aims to generate spatial-temporal environmental drivers for the US Gulf-Wide Ecospace Model (USGWEM). 

## A1 Get MODIS data from ERDDAP
Downloads MODIS data from the NOAA ERDAP server (https://coastwatch.pfeg.noaa.gov/erddap/index.html) and makes depth-integrated Chl-A data.

### Environmental drivers
- Depth-integrated Chlorophyll-A (ChlA)
- Normalized carbon flourescense (cfl)
- Particulate organic carbon (POC)

### Dependencies
- curl
- raster
- rerddap

### Data source
The NOAA ERDDAP server (Environmental Research Division's Data Access Program) provides access to a wide range of oceanographic and environmental datasets. The server offers a convenient interface to search, access, and retrieve data using various query parameters. [NOAA ERDDAP](https://coastwatch.pfeg.noaa.gov/erddap/index.html)

### Code setup and usage
1. Specifies the bounding box of the Gulf of Mexico (GOM) region and the directory where the MODIS data will be stored. The environmental variables of interest (cfl, chla, POC) are defined in the `vars` vector.

### Process MODIS Data from NOAA ERDDAP Server
1. Loop over each environmental variable to retrieve monthly and daily composite imagery from the NOAA ERDDAP server. This is done using the `rerddap` package in R, which provides an interface to access and retrieve data from the NOAA ERDDAP data portal.
2. Process the MODIS data to create monthly raster files. Retrieves the monthly composite imagery for each variable and creates a raster stack using the `raster` package. The raster stack contains each monthly composite as a separate layer. 
3. Integrate surface chlorophyll-a over the euphotic depth (see below).
4. Write the resulting raster files to the specified output directory. The code saves the monthly raster stack and the integrated surface chlorophyll-a over euphotic depth as separate raster files in the specified directory.

#### Integrating Surface Chlorophyll-a over Euphotic Depth
Euphotic depth refers to the depth in the water column where photosynthetically active radiation (PAR) is sufficient to support photosynthesis.
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
This code generates monthly spatial-temporal maps from spatial-imagery MODIS data for three environmental drivers: depth-integrated chlorophyll-A (ChlA), normalized carbon fluorescence (Cfl), and particulate organic carbon (POC). The code reads in a raster stack for each environmental driver. It crops and resamples them match the base/depth map, creates monthly averages for years needed before data-collection, and creates a global average. Note that the code assumes that the necessary MODIS data files are available in the specified input directory (`dir.ras.in`) and follows a specific file naming convention.

### Outputs
- Monthly spatial-temporal ASCII files for Ecospace,
- PDF maps for visualization,
- and a single global-average map for each environmental driver. 

### Make PDF and ASCII monthly maps
1. The code is divided into sections for each environmental driver: ChlA, Cfl, and POC. Each section includes a similar code structure for processing the data, generating maps, and writing outputs.
2. To use the code, modify the directory paths (`dir.ras.in`, `dir.ras.out`, `fld.asc.out`, `dir.asc.avg`, `dir.pdf.out`, `depth08min`) 
3. Customize the environmental driver specific settings and parameters within each section (e.g., `env_driver`, `overwrite`, `colscheme`, `mintile`, `maxtile`).
4. The code automatically crops and resamples each environmental driver map to match the base/depth map (`depth08min` variable) before further processing. This ensures spatial consistency between the environmental driver maps and the base/depth map.
5. The code automatically handles the scenario where MODIS data is unavailable before 1993. It creates monthly means for the years 1980 to December 1992 using the available data. Starting from 1993, the monthly data is used directly for mapping.
6. A single global-average map is generated for each environmental driver using the `mean` function. These global-average maps represent the average value across all available data points for each driver and are saved as ASCII files. These files can be used to initialize Ecospace simulations.
8. The code generates PDF maps using the `pdf_map` function, which visualizes the monthly maps for each year. The PDF maps are saved in the specified output directory (`dir.pdf.out`).
