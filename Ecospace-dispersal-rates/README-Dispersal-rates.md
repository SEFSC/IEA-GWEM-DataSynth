# Dispersal Rate in Ecospace Models

The base dispersal rate in Ecospace controls how fast organisms in the model can move to find preferred habitats, seek prey, avoid predators, or relocate out of poor environmental conditions. It’s a key parameter for ecosystem models that examine spatial systems or forecast the effects of spatial planning (Walters et al., 1999; Pauly et al., 2000). This document presents a generalizable two-step method for determining dispersal rates. 

## Introduction

Organisms with different dispersal rates react differently to their environments, influencing ecosystem dynamics. For example, organisms with low dispersal rates (e.g., sedentary invertebrates or benthic fishes with high site fidelity) may see more positive effects from localized protected areas compared to groups with high dispersal rates (e.g., large pelagic fishes), unless the protected area is sufficiently large (Walters et al., 1999). Dispersal rates can drive acute population effects when environmental drivers or disturbance events are included in the model. 

Ecospace models often use a “300-30-3 rule” for assigning dispersal rates to relatively fast-moving, slow-moving, and sedentary species, respectively. However, research has shown that dispersal rates can affect the model fit and that determining these values should be investigated in more detail (Romagnoni et al., 2015). 

## Methodology

In Ecospace, the biomass density (B in mt per km2 or g per m2) for a functional group i is defined by its underlying Ecopath model. The model distributes the functional groups in the Ecopath with Ecosim evenly onto the Ecospace basemap (Christensen et al., 2005).

We calculated the fraction of a functional group’s biomass (B’) that is always moving between grid cells (B_i^'=m_i B_i) by (1) estimating relative swim speed and (2) scaling to dispersal rates.  

Relative swimming speed (S in km per h) for each species was estimated using the empirically-derived log-linear model by Sambilay (1990), given as: 

```
log10  S= -0.828+0.61961 log10  (L)+ 0.3478 log10  (A)+0.7621 (M),
```

where L represents a species’ common length in mm, A is the aspect ratio of its caudal fin, and M is a binary swimming mode.

Parameter values for L and A were collected for all GoM species by querying the FishBase data portal (Froese and Pauly, 2021). Finally, the predominant swimming mode for species was determined with author opinion of the species as being predominantly pelagic (M = 1) or benthic (M = 0).

Swimming speeds per species were scaled relative to experimentally derived annual dispersal rates for functional groups in the SREM. Dispersal rates for functional groups representing multiple species were calculated with the mean of all species within the group. 

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
The output of this script is a CSV file with scaled dispersal rates for different Ecospace functional groups, ready to be used directly in an Ecospace model. The columns in this output file include the Ecospace group number (`EwE_num`), the group name (`EwE_name`), the mean scaled dispersal rate (`Scaled_dispersal`), the number of species in the group (n_spp), and the standard deviation of the scaled dispersal rates within the group (`SD`).

## Results and Discussion

Dispersal rates were estimated for 325 GoM species and 65 functional groups. Estimated dispersal rates ranged two orders of magnitude from less than 10 km y-1 (three species of Gobiidae) to over 1,000 km y-1 (Basking shark Cetorhinus maximus, Blue marlin Makaira nigricans, and Swordfish Xiphias gladius). 

The dispersal rate estimates from our approach give reasonable estimates for and are largely consistent with the “300-30-3” heuristic. Individual movement for such species are notoriously variable. For example, seven of nine shortfin mako sharks (Isurus oxyrinchus) tagged in the northern Gulf of Mexico moved less than approximately 600 km from their tagging location, while two individuals conducted seasonal migrations and traveled over 2500 km per year (Gibson et al. 2021).

![Plot](Ecospace-dispersal-rates/plot-dispersal-3panel.png)
**Figure 1** – Annual dispersal rates for estimated for functional groups of fishes in the Gulf of Mexico Ecosystem Model. Panels A, B, and C are separated by dispersal rate ranges with different corresponding y-axes. Mean values are shown and the number of species per functional group with error bars to indicate confidence intervals of 1.96×SE. The number of species in a given functional group is indicated by the number at the base of the plot. 
