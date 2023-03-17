
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
fg$ID_sp <- sprintf("Sp_%03d", 1:nrow(fg))

## Write out species list
write.csv(fg, "./global-data/speciesListGoM_QAQC_AMkeys.csv", row.names = F)

##------------------------------------------------------------------------------
## Query HSPEN environmental preferences
## Some species have multiple entries. Without information to differentiate them
## I've decided to average them.

## Make long dataframe with unique key -----------------------------------------
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


## Make long dataframe with unique key -----------------------------------------
## and query aquamap preferences
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



##------------------------------------------------------------------------------
##
## Logistic function

fg_pref[22, 6:9]

max <- 100


#n = 22

## 1 = value of the horizontal asymptote when x→−∞
## 0 = value of the horizontal asymptote when x→+∞
## B describes how rapidly the curve makes its transition between the two asymptotes
## S describes the asymmetry of the curve. The curve is symmetric when S=1. 
## C is a location parameter, which does not have a nice interpretation, 
##   unless S=1 when the curve has an inflection point at x=C.
##   In the case when S=1, C is the value of x for which f(x) is the midpoint between the two asymptotes

doublelogistic <- function(max, min_abs, min_prf, max_prf, max_abs){ 
  #max = 400
  min_abs = fg_pref$DepthMin[j]
  min_prf = fg_pref$DepthPrefMin[j]
  max_prf = fg_pref$DepthPrefMax[j]
  max_abs = fg_pref$DepthMax[j] + 1
  
  mid <- min_prf + (max_prf - min_prf) / 2 ## Midpoint. Change from increasing to decreasing logistic function
  x1  <- seq(0, mid)
  x2  <- seq(mid + 1, max)
  r1  <- min_prf - min_abs ## Range size from 10% to 90%
  r2  <- max_abs - max_prf 
  C1  <- min_abs + r1 / 2
  C2  <- max_prf + r2 / 2
  B1  <- 1/sqrt(r1) ## Curve shape inversely proportional to range size
  B2  <- 1/sqrt(r2)
  S   <- 1
  A1 = 0; D1 = 1; A2 = 1; D2 = 0
  
  f1 <- function(x) 1 / (1 + exp(B1*(C1-x)))^S
  f2 <- function(x) 1 - 1 / (1 + exp(B2*(C2-x)))^S
  y1 <- f1(x1)
  y2 <- f2(x2)
  
  out <- data.frame(x = c(x1, x2), y = c(y1, y2))
  return(out)
}



## Plot depth with varying X-axis (depth) --------------------------------------
factor = 1.8
w = 11 * factor
h = 8.5 * factor
png("./Ecospace-preference-functions/figures/Dbl-logistic-depth-pref_vary-x-axis.png", width = w, height = h, units = 'in', res = 600)

par(mfrow=c(7,9))
for(j in 1:nrow(fg_pref)){
  p1 <- fg_pref$DepthMin[j]
  p2 <- fg_pref$DepthPrefMin[j] + 1
  p3 <- fg_pref$DepthPrefMax[j]
  p4 <- fg_pref$DepthMax[j] + 1
  
  pref_func <- doublelogistic(max = 400, p1, p2, p3, p4)
  
  xlim <- p4+p4*0.15
  xmax <- ifelse(xlim > max, max, xlim)
  plot(pref_func$x, pref_func$y, 
       main = paste(fg_pref$EwE_num[j], fg_pref$EwE_name[j]),
       type = "l", ylab = "", xlab = "", cex.main = 1, bty = 'n',
       xlim = c(0,max), lwd=2)
  abline(v = p1,  col = "red", lty = "dashed")
  abline(v = p2,  col = "blue", lty = "dashed")
  abline(v = p3,  col = "blue", lty = "dashed")
  abline(v = p4,  col = "red", lty = "dashed")
}
dev.off()

#abline(v = p1 + (p2 - p1) / 2,       col = "grey", lty = "dashed")
#abline(v = p3 + (p4 - p3) / 2,       col = "grey", lty = "dashed")

