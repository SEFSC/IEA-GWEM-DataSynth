# An Empirically-Derived Approach for Estimating Dispersal Rates for Ecospace

The base dispersal rate in Ecospace controls how fast organisms in the model can move to find preferred habitats, seek prey, avoid predators, or relocate out of poor environmental conditions. It’s a key parameter for ecosystem models that examine spatial systems or forecast the effects of spatial planning (Walters et al., 1999; Pauly et al., 2000). Organisms with different dispersal rates react differently to their environments, influencing ecosystem dynamics. For example, organisms with low dispersal rates (e.g., sedentary invertebrates or benthic fishes with high site fidelity) may see more positive effects from localized protected areas compared to groups with high dispersal rates (e.g., large pelagic fishes), unless the protected area is sufficiently large (Walters et al., 1999). acute population effects can be driven by dispersal rates when environmental drivers or disturbance events are included in the model. For example, mobile organisms can capitalize on a local, perennial area of high productivity (e.g., driven by a seasonal nutrient plume) or physically swim away from an impacted area (e.g., driven by hypoxia, pollution, or oil spill). Sessile organisms may not be able to relocate before the environmental driver causes impaired fitness or mortality (Chagaris et al., 2020). 

Ecospace models often use a “300-30-3 rule” for assigning dispersal rates to relatively fast-moving, slow-moving, and sedentary species, respectively. However, research has shown that dispersal rates can affect the model fit and that determining these values should be investigated in more detail (Romagnoni et al., 2015). Here, we present a generalizable two-step method for determining dispersal rates. First, relative swimming speed is calculated based on the physiological characteristics of a species. Second, relative swimming speeds are scaled to known home ranges for species within the model. 

## Methodology

In Ecospace, the biomass density (B in mt per km2 or g per m2) for a functional group i is defined by its underlying Ecopath model. The model distributes the functional groups in the Ecopath with Ecosim evenly onto the Ecospace base map (Christensen et al., 2005). Then, biomass pools begin moving among the grid cells in the spatial-temporal simulations. 

We calculated the fraction of a functional group’s biomass (B’) that is always moving between grid cells (B_i^'=m_i B_i) by (1) estimating relative swim speed and (2) scaling to dispersal rates.  

Relative swimming speed (S in km per h) for each species was estimated using the empirically-derived log-linear model by Sambilay (1990), given as: 

```
log10  S= -0.828+0.61961 log10  (L)+ 0.3478 log10  (A)+0.7621 (M),
```

where L represents a species’ common length in mm, A is the aspect ratio of its caudal fin, and M is a binary swimming mode. The aspect ratio is defined as the relation of the squared height (h) of the caudal fin to its surface area (s), A = h^2 / s. Pelagic fishes with tall, thin caudal fins (e.g., tunas, sharks, and jacks) will have higher aspect ratios than bottom-dwelling fishes (e.g., gobies, groupers, scorpionfishes). 

Parameter values for L and A were collected for all GoM species by querying the FishBase data portal (Froese and Pauly, 2021). Finally, the predominant swimming mode for species was determined with author opinion of the species as being predominantly pelagic (M = 1) or benthic (M = 0).

Swimming speeds per species were scaled relative to experimentally derived annual dispersal rates for functional groups in the model. Dispersal rates for functional groups representing multiple species were calculated with the mean of all species within the group. 

```R
## Aggregate
dispersal_table = scaled %>% 
  group_by(EwE_num, EwE_name) %>% 
  summarise(
    Scaled_dispersal = mean(Scaled_avg),
    n_spp = n(),
    SD = sd(Scaled_avg),
  )
```

## Output and Discussion
The output of this script is a CSV file with scaled dispersal rates for different Ecospace functional groups, ready to be used directly in an Ecospace model. The columns in this output file include the Ecospace group number (`EwE_num`), the group name (`EwE_name`), the mean scaled dispersal rate (`Scaled_dispersal`), the number of species in the group (n_spp), and the standard deviation of the scaled dispersal rates within the group (`SD`).

In total, dispersal rates were estimated for 325 GoM species and 65 functional groups. Estimated dispersal rates ranged two orders of magnitude from less than 10 km y-1 (three species of Gobiidae) to over 1,000 km y-1 (Basking shark Cetorhinus maximus, Blue marlin Makaira nigricans, and Swordfish Xiphias gladius). 

The dispersal rate estimates from our approach appear reasonable are largely consistent with the “300-30-3” heuristic. The slowest-moving fish groups had dispersal rates above 15 km per y and generally represented aggregated groups of about 10 to 50 species for small reef demersal fishes. The USGWEM has higher species resolution for federally assessed fishery species, which are generally larger, and dispersal rates for the reef-associated and pelagic species were in ranges of 20–300 km per y. Highly migratory fishes and cetacean groups were estimated to have dispersal rates above 300 km per. 

![Plot](plot-dispersal-3panel.png)
**Figure 1** – Annual dispersal rates for estimated for functional groups of fishes in the Gulf of Mexico Ecosystem Model. Panels A, B, and C are separated by dispersal rate ranges with different corresponding y-axes. Mean values are shown and the number of species per functional group with error bars to indicate confidence intervals of 1.96×SE. The number of species in a given functional group is indicated by the number at the base of the plot. 

**Table 1** – Relative swim speed and scaled dispersal rates per species based on common length, tail aspect ratio, and swim mode. 
```markdown
|EwE_num        |EwE_name                              | Scaled_dispersal| n_spp|         SD|
|:--------------|:-------------------------------------|----------------:|-----:|----------:|
|06             |Blacktip shark                        |          0.00000|     1|         NA|
|07             |Dusky shark                           |        693.00000|     1|         NA|
|08             |Sandbar shark                         |        592.00000|     1|         NA|
|09             |Large coastal sharks                  |        622.00000|    11| 126.076961|
|10             |Large oceanic sharks                  |        737.80000|     5|  59.415486|
|11             |Atlantic sharpnose shark              |        241.00000|     1|         NA|
|12             |Small coastal sharks                  |        266.75000|     4|  38.012059|
|13             |Yellowfin tuna                        |        619.00000|     1|         NA|
|14             |Bluefin tuna                          |        753.00000|     1|         NA|
|15             |Other tunas                           |        409.50000|     2|  30.405592|
|16             |Billfish                              |        877.75000|     4| 148.562837|
|17             |Swordfish                             |       1005.00000|     1|         NA|
|18             |Pelagic coastal piscivores            |        199.22581|    31| 154.091036|
|19             |Amberjack                             |        320.00000|     2|  70.710678|
|20             |Cobia                                 |        272.00000|     1|         NA|
|21,22          |King mackerel (1+yr)                  |        381.00000|     1|         NA|
|25             |Skates-rays                           |        621.50000|     2| 210.010714|
|26,27          |Gag grouper (0-3yr)                   |         36.00000|     1|         NA|
|28,29          |Red grouper (0-3yr)                   |         33.00000|     1|         NA|
|30,31          |Yellowedge grouper (0-3yr)            |         34.00000|     1|         NA|
|32             |Goliath grouper                       |         66.00000|     1|         NA|
|33             |Deep-water grouper                    |         38.00000|     1|         NA|
|34             |Shallow-water grouper                 |         30.75000|     8|   6.649382|
|35,36,37       |Red snapper (0yr)                     |         40.00000|     1|         NA|
|38             |Vermilion snapper                     |         36.00000|     1|         NA|
|39             |Mutton snapper                        |         38.00000|     1|         NA|
|40             |Other snapper                         |         37.30000|    10|  10.011660|
|42             |Sea trout                             |         28.60000|     5|   5.029910|
|43             |Oceanic piscivores                    |         54.50000|     8|  32.120310|
|44             |Benthic piscivores                    |         26.20000|    10|   5.370702|
|45             |Reef piscivores                       |         40.66667|     6|  22.402381|
|46             |Reef invertebrate feeders             |         27.12766|    47|   8.157730|
|47             |Demersal coastal invertebrate feeders |         44.00000|    39|  64.386988|
|48             |Red drum                              |         56.00000|     1|         NA|
|49             |Benthic coastal invertebrate feeders  |         17.42857|    21|   7.338743|
|50             |Tilefish                              |         38.00000|     5|   7.968689|
|51             |Gray triggerfish                      |         34.00000|     1|         NA|
|52             |Coastal omnivores                     |         23.47368|    19|   9.923980|
|53             |Reef omnivores                        |         25.90909|    22|   8.722762|
|54             |Surface pelagics                      |        135.33333|    12|  41.961743|
|55             |Large oceanic planktivores            |       1285.00000|     1|         NA|
|57             |Sardine-herring-scad                  |        135.57143|    14|  43.994255|
|58,59,60,61,62 |Menhaden (0yr)                        |        110.00000|     1|         NA|
|63             |Anchovy-silverside-killifish          |         61.63636|    11|  17.153849|
|64             |Mullet                                |         30.00000|     3|  11.532563|
|65             |Butterfish                            |         24.50000|     2|   2.121320|
```
