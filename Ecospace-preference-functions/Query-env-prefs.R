
##------------------------------------------------------------------------------
## Set up Aquamaps Package
## https://raquamaps.github.io/aquamapsdata/articles/intro.html

## install aquamapsdata from GitHub using devtools
## initial run-once step required to install remote db locally

#remotes::install_github("raquamaps/aquamapsdata", dependencies = TRUE)
#library(aquamapsdata)
#download_db(force = TRUE)
#default_db("sqlite")

##------------------------------------------------------------------------------
library(aquamapsdata); default_db("sqlite")
library(dplyr)
library(stringr)

##------------------------------------------------------------------------------
## QA/QC species list and get query aquamaps key

## Read in species list for functional groups
fg         <- read.csv("./global-data/speciesListGoM_raw.csv") ## 'fg' represents Ecospace functional groups
fg$Genus   <- word(fg$Sciname, 1)
fg$Species <- word(fg$Sciname, 2)
nrow(fg)

## Use only rows with genus and species
fg <- fg %>% filter(!is.na(Species)); nrow(fg)

## QA/Qc: Periods in Sciname seem to break am_search_fuzzy. Also, all rows with 'sp' or 'spp' or 'spp.' are repeated
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


  
  

