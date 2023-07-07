# Make Ecospace Environmental Preference Functions
This code queries the Aquamaps database and, with further processing, we generate environmental preference functions for the Gulf-wide Ecospace Model. 

Data to make these preference functions comes from the [AquaMaps](https://www.aquamaps.org/main/AboutAquaMaps.php), a tool for generating model-based, large-scale predictions of natural occurrences of species. To date, AquaMaps has standardized distribution maps for over 17,300 species of fishes, marine mammals, and invertebrates. This is done by combining occurrence records along with expert knowledge and environmental data to compensate for biases and gaps in existing occurrence data sets.

Estimates of species preferences, called environmental envelopes, are derived from large sets of occurrence data available from online collection databases such as GBIF (www.gbif.org) and OBIS (www.iobis.org), FishBase (and in SeaLifeBase and AlgaeBase for non-fish), and  from the literature for a given species and its habitat usage. The AquaMaps modeling process involves constructing environmental envelopes that describe a species' habitat usage in relation to various environmental parameters. AquaMaps provides an expert review process to improve the accuracy of the predictions by incorporating additional knowledge and adjusting envelope settings. The environmental envelopes are matched against local environmental conditions to determine the suitability of a given area in the ocean for a particular species. Environmental envelopes for each species, along with the associated species input parameters, are stored in a corresponding Species Environmental Envelope File (HSPEN). Species are assigned identification codes according to the [2018 Annual Checklist Catalogue of Life](https://www.catalogueoflife.org/annual-checklist/2018/).

For the USGWEM, we develop environmental preference functions for **depth**, **temperature**, and **salinity**. Preferences for each environmental driver are parameterized with values for absolute and preferred minima and maxima: 
1. Min<sub>abs</sub>  = absolute minimum in extracted data or 25th percentile - 1.5 × interquartile (whichever is lesser)
2. Min<sub>pref</sub> = 10th percentile of observed variation in an environmental predictor
3. Max<sub>pref</sub> = 90th percentile of observed variation in an environmental predictor
4. Max<sub>abs</sub>  = absolute maximum in extracted data or 75th percentile + 1.5 × interquartile (whichever is greater)

### References for Aquamaps
- Elith, J., C. H. Graham, et al. (2006). "Novel methods improve prediction of species' distributions from occurrence data." Ecography 29: 129-151.
- Guisan, A. and N. Zimmermann (2000). "Predictive habitat distribution models in ecology." Ecological Modelling 135: 147-186.
- Kaschner, K., L. B. Christensen, et al. (2006). Mapping top consumers in marine ecosystems past and present: comparative consumption rates of great whales and fisheries (SC/58/E3). International Whaling Commission - Scientific Committee Meeting, (unpublished).
- Kaschner, K., R. Watson, et al. (2006). "Mapping worldwide distributions of marine mammals using a Relative Environmental Suitability (RES) model." Marine Ecology Progress Series 316: 285-310.
- Moisen, G. G. and T. S. Frescino (2002). "Comparing five modelling techniques for predicting forest characteristics." Ecological Modelling 157(2-3): 209-225.

# Query data from Aquamaps: 1-Query-env-prefs.R
1. **Setup.** If this is an initial run, you'll need to setup the [Aquamaps Package](https://raquamaps.github.io/aquamapsdata/articles/intro.html):
   - Install the `aquamapsdata` package from GitHub using `remotes::install_github()`.
   - Download the Aquamaps database locally using `download_db(force = TRUE)`.
   - Set the default database to SQLite using `default_db("sqlite")`.
   - Additional library dependencies: `dplyr` and `stringr`

2. **QA/QC Species List and Get Aquamaps Keys.**
   - First, read in the species list from a CSV file. 
   - Define genus and species names. Filter out rows without species names: `fg <- fg %>% filter(!is.na(Species)); nrow(fg)`
   - Periods in the scientific names (`fg$Sciname`) seem to break `am_search_fuzzy`. Also, filter out all rows where 'sp', 'sp.','spp','spp.' etc. are included.
     ```R
     rm_ls <- paste(c('spp.','sp.','spp', 'sp.', '-', '#', "/", "<", ">", "0", ","), collapse = '|')
     fg <- fg %>% filter(!grepl(rm_ls, Sciname)); nrow(fg)
     ```
   - Query the Aquamaps database with `am_search_fuzzy` to get species keys for each species.
     ```R
     for (i in 1:nrow(fg)){
      fg$Key[i] <- paste(am_search_fuzzy(fg$Sciname[i]) %>% pull(key) %>% as.array(), collapse = ' | ')
     }
     ```
   - Perform QA/QC checks to remove unwanted characters and duplicated rows. Also remove rows without keys or with duplicate keys
   - Write out the QA/QC species list with Aquamaps keys to a CSV file in "./global-data/".

3. **Query HSPEN Environmental Preferences.** 
   - First, make a long dataframe `long_fg` with a unique key for each species.
     ```R 
     for (i in 1:nrow(fg)) {
        #i = 4
        row = fg[i, ]
        key_ls  <- as.list(scan(text=fg$Key[i], what=""))
        key_ls  <- key_ls[key_ls != "|"]
        row$Key <- NULL
        row     <- row %>% slice(rep(1:n(), each = length(key_ls)))
        key_df  <- do.call(rbind.data.frame, key_ls)
        names(key_df) <- "Key"
        row     <- cbind(row, key_df)
        long_fg <- rbind(long_fg, row)
      }
     ```    
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
   - Some species have multiple entries. Without information to differentiate them, I've decided to average them. Merge the preferences with the species list by species ID.
   - Finally, we aggregate the preferences by Ecospace functional groups, calculating the mean and count of preferences for each group.
   - Write out the preference parameters for each functional group in a CSV file.

# 2-Make-env-pref-matrices for Ecospace
  
## Functions: Making the double logistic function and plotting
```R
doublelogistic <- function(max = 400, steps = 400, range = 'wide', min_abs, min_prf, max_prf, max_abs){ 
```
The **`doublelogistic`** function generates a double logistic curve representing the preference of a species for a specific environmental parameter. Arguments:
- `max`: Maximum value of the environmental parameter.
- `steps`: Number of steps to divide the curve.
- `range`: Range type ('wide' or 'nar').
- `min_abs`: Min<sub>abs</sub> value of the environmental parameter.
- `min_prf`: Min<sub>pref</sub> value of the environmental parameter.
- `max_prf`: Max<sub>pref</sub> value of the environmental parameter.
- `max_abs`: Max<sub>abs</sub> value of the environmental parameter.
- `range`: Range type ('wide' or 'nar'). Determines how the curve is shaped inversely proportional to range size. If range is set to `range == wide`, then the `B` parameter is set to `1/sqrt(range)`. If `range == nar`, then `B = 1 / log10(range)`.

The two logistic equations in the double logistic function are as follows:

$$
\begin{equation}
f_1(x) =\frac{1}{\left(1 + e^{B_1 C_1}\cdot e^{-B_1 x}\right)^{S}} \quad \text{(Increasing logistic eq. where x < midpoint)}
\end{equation}
$$

$$
\begin{equation}
f_2(x) = 1 - \frac{1}{\left(1 + e^{B_2 C_2}\cdot e^{-B_2 x}\right)^{S}} \quad \text{(Decreasing logistic eq. where x >= midpoint)}
\end{equation}
$$

- Where 1 is the value of the horizontal asymptote when x→−∞ and 0 is the value of the horizontal asymptote when x→+∞
- `B1` and `B2` describe how rapidly the curve makes its transition between the two asymptotes
- `S` describes the asymmetry of the curve. (The curve is symmetric when S = 1.) `C` is a location parameter, which does not have a nice interpretation unless S=1 whereby the curve has an inflection point at x = C. We set S=1 for simplicity. In this case, C = the value of x for which f(x) is the midpoint between the two asymptotes.

```R
plot_pref_func(p1, p2, p3, p4, fg_num, fg_name, max = 400, xmin = 0, scale_xaxis = 'y', range = 'wide', driver = '')
```
The **plot_pref_func** function plots the preference function curve generated by the doublelogistic function. It takes the following arguments:
- `driver` represents the environmental driver (for the USGWEM: depth, temperature, or salinity).
- `p1`, `p2`, `p3`, and `p4` correspond to and are informed by `min_abs`, `min_pref`, `max_pref`, and `min_abs`, respectively. 
- `fg_num` is the EwE number of the species and `fg_name` represents the name of the functional group. 
- `xmin` and `max` represents the minimum and maximum values for the x-axis.
- `scale_xaxis`: Scale type for the x-axis ('y' or 'n').

