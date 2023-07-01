# Make Ecospace Environmental Preference Functions

## Query data from Aquamaps: 
This code quueries the Aquamaps database. With further processing, we generate environmental preference functions for the Gulf-wide Ecospace Model. 

[AquaMaps](https://www.aquamaps.org/main/AboutAquaMaps.php) is a tool for generating model-based, large-scale predictions of natural occurrences of species. To date, they have standardized distribution maps for over 17,300 species of fishes, marine mammals, and invertebrates. The AquaMaps approach combines occurrence records, expert knowledge, and environmental data to compensate for biases and gaps in existing occurrence data sets. 
Estimates of species preferences, called environmental envelopes, are derived from large sets of occurrence data available from online collection databases such as GBIF (www.gbif.org) and OBIS (www.iobis.org), and from the literature about the distribution of a given species and its habitat usage that are available in FishBase (and in SeaLifeBase and AlgaeBase for non-fish). The modeling process involves constructing environmental envelopes that describe a species' habitat usage in relation to various environmental parameters. Various tools are available, ranging from simple environmental envelope models that rely on reported presences to complex models that consider both presences and absences (Guisan and Zimmermann, 2000). More sophisticated models tend to outperform simpler ones when tested with data from well-designed sampling schemes at regional scales (Elith, Graham et al., 2006); however, their accuracy largely relies on the quality of input data, and simpler models can perform equally well or even better than complex models when the input data quality is poor (Moisen and Frescino, 2002). These envelopes are used to calculate the relative likelihood of a species' presence in each grid cell based on its environmental conditions. AquaMaps also provides an expert review process to improve the accuracy of the predictions by incorporating additional knowledge and adjusting envelope settings. 

The environmental envelopes are matched against local environmental conditions to determine the suitability of a given area in the ocean for a particular species. Environmental envelopes for each species, along with the associated species input parameters, are stored in a corresponding Species Environmental Envelope File (HSPEN).

For the USGWEM, we use parameters queried from Aquamaps HSPEN files to develop environmental preference functions for **depth**, **temperature**, and **salinity**.

### Usage for 1-Query-env-prefs.R
1. Setup the [Aquamaps Package](https://raquamaps.github.io/aquamapsdata/articles/intro.html):
   - Install the `aquamapsdata` package from GitHub using `remotes::install_github()`.
   - Download the Aquamaps database locally using `download_db(force = TRUE)`.
   - Set the default database to SQLite using `default_db("sqlite")`.
   - Additional library dependencies: `dplyr` and `stringr`

2. QA/QC Species List and Get Aquamaps Keys:
   - Read in the species list from a CSV file. 
   - Define genus and species names. Filter out rows without species names. Periods in the scientific names (`fg$Sciname`) seem to break `am_search_fuzzy`. Also, all rows with 'sp' or 'spp' or 'spp.' are repeated
   - Query the Aquamaps database to obtain species keys for each species. 
   - Perform QA/QC checks to remove unwanted characters and duplicated rows. Also remove rows without keys or with duplicate keys
   - Write out the QA/QC species list with Aquamaps keys to a CSV file in "./global-data/".

3. Query HSPEN Environmental Preferences:
   - Create a long dataframe `long_fg` with a unique key for each species.
   - Query the HSPEN environmental preferences for each species from the Aquamaps database.
     ```R
        for (i in 1:nrow(long_fg)) {
          key <- long_fg$Key[i]
          prf <- am_hspen() %>% filter(SpeciesID == key) ## Query preferences from aquamaps
          prf <- prf %>% 
            select(LifeStage,   Pelagic,
                   DepthMin,    DepthPrefMin,    DepthPrefMax,    DepthMax,
                   TempMin,     TempPrefMin,     TempPrefMax,     TempMax,
                   SalinityMin, SalinityPrefMin, SalinityPrefMax, SalinityMax,
                   PrimProdMin, PrimProdPrefMin, PrimProdPrefMax, PrimProdPrefMax,
                   OxyMin,      OxyPrefMin,      OxyPrefMax,      OxyMax,
                   LandDistMin, LandDistPrefMin, LandDistPrefMax, LandDistMax,
                   NMostLat,    SMostLat,        WMostLong,       EMostLong)
          long_pref <- rbind(long_pref, cbind(long_fg[i ,], prf))
        }
        ```
   - Some species have multiple entries. Without information to differentiate them, I've decided to average them. Aggregate the preferences by species, calculating the mean of numeric preference values. Merge the preferences with the species list by species ID.
   - Finally, we wggregate the preferences by Ecospace functional groups, calculating the mean and count of preferences for each group.
   - Write out the preference parameters for each functional group in a CSV file.

### References for Aquamaps
- Elith, J., C. H. Graham, et al. (2006). "Novel methods improve prediction of species' distributions from occurrence data." Ecography 29: 129-151.
- Guisan, A. and N. Zimmermann (2000). "Predictive habitat distribution models in ecology." Ecological Modelling 135: 147-186.
- Kaschner, K., L. B. Christensen, et al. (2006). Mapping top consumers in marine ecosystems past and present: comparative consumption rates of great whales and fisheries (SC/58/E3). International Whaling Commission - Scientific Committee Meeting, (unpublished).
- Kaschner, K., R. Watson, et al. (2006). "Mapping worldwide distributions of marine mammals using a Relative Environmental Suitability (RES) model." Marine Ecology Progress Series 316: 285-310.
- Moisen, G. G. and T. S. Frescino (2002). "Comparing five modelling techniques for predicting forest characteristics." Ecological Modelling 157(2-3): 209-225.
