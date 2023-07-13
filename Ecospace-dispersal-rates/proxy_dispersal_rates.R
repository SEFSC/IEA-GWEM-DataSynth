
rm(list=ls()); gc(); windows()
library(dplyr)
library(tidyverse)

## Set up and merge databases
fishbase = read.csv("./global-data/fishbaseSppEcolMatrix.csv")
fishbase = fishbase[, c('Sciname', 'AspectRatio', 'CommonLength')]
fishbase$Sciname = tolower(fishbase$Sciname)

fg = read.csv("./global-data/speciesListGoM_QAQC_AMkeys.csv") ## 'fg' represents Ecospace functional groups
fg$Sciname = tolower(fg$Sciname)

disp = merge(fg, fishbase, by = 'Sciname', all.x = TRUE)

## Clean up data frame
disp = disp %>% drop_na(Sciname, AspectRatio, CommonLength) 
disp = disp[order(disp$EwE_num), ]
disp = disp[!duplicated(disp$Sciname),]

write.csv(disp, "./Ecospace-dispersal-rates/intermediate-output/base-swim-speed.csv", row.names = FALSE)

## Scaled dispersal rates calculated in Excel workbook:
## First, base swim speeds estimated with equation
##   = 10^(-0.828+0.6196*LOG10(CommonLength)+0.3478*LOG10(AspectRatio)+0.7261*SwimMode)
##   Equation fom VC Sambilay Jr. 1990. Interrelationships between swimming speed, 
##   caudal fin aspect ratio and body length of fishes. International Center for Living Aquatic Resources Management.
## Second, swim speeds are scaled to dispersal rates for known species

scaled = read.csv("./Ecospace-dispersal-rates/intermediate-output/scaled-dispersal-rates-all-spp.csv")

## Aggregate
dispersal_table = scaled %>% 
  group_by(EwE_num, EwE_name) %>% 
  summarise(
    Scaled_dispersal = mean(Scaled_avg),
    n_spp = n(),
    SD = sd(Scaled_avg),
  )

## Write out table for Ecospace
write.csv(dispersal_table, "./Ecospace-dispersal-rates/output-for-Ecospace/scaled-dipersal-rates.csv", row.names = FALSE)
