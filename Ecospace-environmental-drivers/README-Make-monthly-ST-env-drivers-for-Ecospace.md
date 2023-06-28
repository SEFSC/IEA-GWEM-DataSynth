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
- Morel, A., and Maritorena, S. (
