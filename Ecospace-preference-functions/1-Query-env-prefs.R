##------------------------------------------------------------------------------
##
## Org:     NOAA / SEFSC / GoM IEA
## Project: Gulf-wide Ecospace Model
## Contact: Holden Earl Harris | holden.harris@noaa.gov
## Code:    Query Aquamaps database
##          Make environmental preference functions for Ecospace


rm(list=ls()); gc(); windows()
library(dplyr)
library(stringr)

##------------------------------------------------------------------------------
## Set up Aquamaps Package
## https://raquamaps.github.io/aquamapsdata/articles/intro.html

## First time running will need to install and set up aquamaps
## initial run-once step required to install remote db locally

rm(list=ls()); gc(); windows()

install.packages("devtools") 
library("devtools")
install_gitub("raquamaps/aquamapsdata", dependencies = TRUE)

## initial run-once step required to install remote db locally
library(aquamapsdata)
download_db(force = TRUE)
default_db("sqlite")


##------------------------------------------------------------------------------
## QA/QC species list and get query aquamaps key

## Read in species list for functional groups
fg         <- read.csv("./global-data/speciesListGoM_raw.csv") ## 'fg' represents Ecospace functional groups
fg$Genus   <- word(fg$Sciname, 1)
fg$Species <- word(fg$Sciname, 2)
nrow(fg) ## Check number of functional groups

## Use only rows with genus and species
fg <- fg %>% filter(!is.na(Species)); nrow(fg)

## QA/QC: Periods in Sciname seem to break am_search_fuzzy. Also, all rows with 'sp' or 'spp' or 'spp.' are repeated
rm_ls <- paste(c('spp.','sp.','spp', 'sp.', '-', '#', "/", "<", ">", "0", ","), collapse = '|')
fg <- fg %>% filter(!grepl(rm_ls, Sciname)); nrow(fg)

## Query aquamaps to get species keys for each species
for (i in 1:nrow(fg)){
  fg$Key[i] <- paste(am_search_fuzzy(fg$Sciname[i]) %>% pull(key) %>% as.array(), collapse = ' | ')
}

## QA/QC Remove rows with no keys or with duplicate keys
fg <- fg %>% 
  filter(Key != '') %>% 
  distinct(Key, .keep_all = TRUE); nrow(fg)
fg$ID_sp <- sprintf("Sp_%03d", 1:nrow(fg))

## Write out species list
write.csv(fg, "./global-data/speciesListGoM_QAQC_AMkeys.csv", row.names = F)

##------------------------------------------------------------------------------
## Query HSPEN environmental preferences
## Some species have multiple entries. Without information to differentiate them
## I've decided to average them.

## Make long dataframe with unique key
long_fg  <- data.frame()
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

## Query aquamap preferences
long_pref <- data.frame()
for (i in 1:nrow(long_fg)) {
  #i = 4
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

## Aggregate by species
summ_pref <- long_pref %>% 
  group_by(ID_sp) %>% 
  summarise(N_hspn = n(),
            across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE),2))
  )
              
pref_bysp <- merge(fg, summ_pref, by = "ID_sp", all.x = T)
pref_bysp <- pref_bysp[order(pref_bysp$ID_sp), ]

## Aggregate by EwE FG
pref_byfg <- pref_bysp %>% 
  group_by(EwE_num, EwE_name) %>% 
  summarise(
    N_spp  = n(),
    N_hspn = sum(N_hspn),
    across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE),2)),
  )

fg_pref         <- data.frame(EwE_num = unique(fg$EwE_num))
fg_pref$rownum  <- 1:nrow(fg_pref)
fg_pref         <- merge(fg_pref, pref_byfg, by = "EwE_num", all.x = T)
fg_pref         <- fg_pref[order(fg_pref$rownum), ]
fg_pref$rownum  <- NULL

write.csv(fg_pref, "./Ecospace-preference-functions/intermediate-ouput/fg-env-preference-parameters.csv", row.names = FALSE)

## End script