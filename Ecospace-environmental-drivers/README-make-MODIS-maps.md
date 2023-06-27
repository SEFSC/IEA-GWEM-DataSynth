# Spatial-imagery MODIS Data Mapping for Environmental Drivers

This code generates monthly spatial-temporal maps from spatial-imagery MODIS data for three environmental drivers: depth-integrated chlorophyll-A (ChlA), normalized carbon fluorescence (Cfl), and particulate organic carbon (POC). The code reads in a raster stack for each environmental driver. It crops and resamples them match the base/depth map, creates monthly averages for years needed before data-collection, and creates a global average. Outputs: monthly spatial-temporal ASCII files for Ecospace, PDF maps for visualization, and a single global-average map for each environmental driver. 

## Dependencies
- terra
- stringr
- viridis

## Usage
1. The code is divided into sections for each environmental driver: ChlA, Cfl, and POC. Each section includes similar code structure for processing the data, generating maps, and writing outputs.
2. To use the code, modify the directory paths (`dir.ras.in`, `dir.ras.out`, `fld.asc.out`, `dir.asc.avg`, `dir.pdf.out`, `depth08min`) according to your file system.
3. Customize the environmental driver specific settings and parameters within each section (e.g., `env_driver`, `overwrite`, `colscheme`, `mintile`, `maxtile`) as per your requirements.
4. Run the code section by section or as a whole to process the MODIS data, generate maps, and create output files.
5. The code automatically crops and resamples each environmental driver map to match the base/depth map (`depth08min` variable) before further processing. This ensures spatial consistency between the environmental driver maps and the base/depth map.
6. The code automatically handles the scenario where MODIS data is unavailable before 1993. It creates monthly means for the years 1980 to December 1992 using the available data. Starting from 1993, the monthly data is used directly for mapping.
7. A single global-average map is generated for each environmental driver using the `mean` function. These global-average maps represent the average value across all available data points for each driver and are saved as ASCII files. These files can be used to initialize Ecospace simulations.
8. The code generates PDF maps using the `pdf_map` function, which visualizes the monthly maps for each year. The PDF maps are saved in the specified output directory (`dir.pdf.out`).

## Additional Notes
- The code assumes that the necessary MODIS data files are available in the specified input directory (`dir.ras.in`) and follows a specific file naming convention.

