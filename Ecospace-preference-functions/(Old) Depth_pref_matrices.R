rm(list=ls()); gc(); windows()
library(dplyr)
library(rfishbase)

## Pull data from fishbase ecology spread sheet --------------------------------
## Set up and merge databases
fishbase         = read.csv("./global-data/fishbaseSppEcolMatrix.csv")
fishbase_depths  = fishbase[, c('Sciname', 'AspectRatio', 'CommonLength')]
fishbase$Sciname = tolower(fishbase$Sciname)

fg               = read.csv("./global-data/speciesListGoM.csv") ## 'fg' represents Ecospace functional groups
fg$Sciname       = tolower(fg$Sciname)

depths           = merge(fg, fishbase, by = 'Sciname', all.x = TRUE)

## Clean up data frame
depths = depths %>% drop_na(Sciname, DepthRangeShallow, DepthRangeDeep) 
depths = depths[order(depths$EwE_num), ]

## Make 25th and 75th quantiles ------------------------------------------------
q = 0.25
depths$Range = depths$DepthRangeDeep - depths$DepthRangeShallow
depths$Q1 = round(depths$DepthRangeShallow + depths$Range * q, 0)
depths$Q2 = round(depths$DepthRangeDeep    - depths$Range * q, 0)
depths$Range = NULL

depths$mu = depths$DepthRangeShallow + 0.5 * (depths$DepthRangeDeep - depths$DepthRangeShallow)
depths$sd = (depths$DepthRangeDeep - depths$DepthRangeShallow) / 4

fg_depths = depths %>% 
  group_by(EwE_num, EwE_name) %>% 
  summarise(
    min  = round(mean(DepthRangeShallow, na.rm=T), 0),
    q1   = round(mean(Q1, na.rm=T), 0),
    q2   = round(mean(Q2, na.rm=T), 0), 
    max  = round(mean(DepthRangeDeep, na.rm=T), 0),
    mean = round(mean(mu, na.rm=T), 0),
    sd   = round(mean(sd, na.rm=T), 0),
    n_spp = n()
  )

write.csv(fg_depths, "./Ecospace-preference-functions/intermediate-ouput/fg-depths.csv", row.names = F)

################################################################################
##
## Make depth preferences

adj_depths = read.csv("./Ecospace-preference-functions/intermediate-ouput/fg-depths-adjusted.csv", 
                      header = TRUE, stringsAsFactors=FALSE)
adj_depths = adj_depths[!is.na(adj_depths$min), ]
adj_depths$EwE_name; nrow(adj_depths)

## Make depth preference matrix
steps = 1200 ## Ecospace "freehand" preference functions are 1200 values
depth_mat = matrix(nrow = 1203, ncol = nrow(adj_depths)) 
row.names(depth_mat) = c("Name", "Left_limit", "Right_limit", 1:1200)
colnames(depth_mat) = adj_depths$EwE_name


## Make grid
for (i in 1:ncol(depth_mat)){
  scaler = 1 ## Optional scaling parameter
  min = as.numeric(adj_depths$min[i]) * scaler
  q1  = as.numeric(adj_depths$q1[i])  * scaler
  q2  = as.numeric(adj_depths$q2[i])  * scaler
  max = as.numeric(adj_depths$max[i])  * scaler
  
  y = integer(steps)
  for(j in 1:steps){
    if(j < min) 
      y[j] = 0
    else {
      if(j < q1 & adj_depths$no_lower_limit[i] == 'y') 
        y[j] = 1
      if(j < q1 & adj_depths$no_lower_limit[i] == 'n') {
        dx = q1 - min
        y[j] = 1 / dx * (j - min)
      }
      if (j >= q1 & j <= q2)
        y[j] = 1
      if (j > q2 ) {
        dx = max - q2
        y[j] = - 1/dx * (j - max)
      }
      if(j > max)
        y[j] = 0
    }
  }

  name = paste0("Depth_", gsub("[[:space:]]", "-", adj_depths$EwE_name[i]), 
                "_grp", adj_depths$EwE_num[i])
  print(name)
  outvec = c(name, 0, steps, y)
  depth_mat[ ,i] = outvec
}

## Write out depth matrix for Ecospace
write.csv(depth_mat, "./Ecospace-preference-functions/output-for-Ecospace/depth-pref-mat-1200L-for-Ecospace.csv", row.names = T)


## Plot depth with varying X-axis (depth) --------------------------------------
factor = 1.5
w = 11 * factor
h = 8.5 * factor
png("./Ecospace-preference-functions/figures/plot-depth-pref_vary-x-axis.png", width = w, height = h, units = 'in', res = 600)
par(mfrow = c(6, 10), adj=0)
for (i in 1:nrow(adj_depths)){
  min = as.numeric(adj_depths$min[i])
  q1  = as.numeric(adj_depths$q1[i]) 
  q2  = as.numeric(adj_depths$q2[i])  
  max = as.numeric(adj_depths$max[i]) 
  
  y = integer(max * 1.15)
  for(j in 1:length(y)){
    if(j < min) 
      y[j] = 0
    else {
      if(j < q1 & adj_depths$no_lower_limit[i] == 'y') 
        y[j] = 1
      if(j < q1 & adj_depths$no_lower_limit[i] == 'n') {
        dx = q1 - min
        y[j] = 1 / dx * (j - min)
      }
      if (j >= q1 & j <= q2)
        y[j] = 1
      if (j > q2 ) {
        dx = max - q2
        y[j] = - 1/dx * (j - max)
      }
      if(j > max)
        y[j] = 0
    }
  }
  
  plot(y, main = paste(adj_depths$EwE_num[i], adj_depths$EwE_name[i]), 
       type = 'l', lwd = 1, col = 'red', cex.main = 1,
       ylab = '', xlab = '', bty = 'n')
  
}
dev.off()


## Plot depth with depth standardized (500 m) X-axis  --------------------------
factor = 1.5
w = 11 * factor
h = 8.5 * factor
png("./Ecospace-preference-functions/figures/plot-depth-pref_standardized-x-axis.png", width = w, height = h, units = 'in', res = 600)
par(mfrow = c(6, 10), adj=0)
for (i in 1:nrow(adj_depths)){
  min = as.numeric(adj_depths$min[i])
  q1  = as.numeric(adj_depths$q1[i]) 
  q2  = as.numeric(adj_depths$q2[i])  
  max = as.numeric(adj_depths$max[i]) 
  
  y = integer(500)
  for(j in 1:length(y)){
    if(j < min) 
      y[j] = 0
    else {
      if(j < q1 & adj_depths$no_lower_limit[i] == 'y') 
        y[j] = 1
      if(j < q1 & adj_depths$no_lower_limit[i] == 'n') {
        dx = q1 - min
        y[j] = 1 / dx * (j - min)
      }
      if (j >= q1 & j <= q2)
        y[j] = 1
      if (j > q2 ) {
        dx = max - q2
        y[j] = - 1/dx * (j - max)
      }
      if(j > max)
        y[j] = 0
    }
  }
  
  plot(y, main = paste(adj_depths$EwE_num[i], adj_depths$EwE_name[i]), 
       type = 'l', lwd = 1, col = 'blue', cex.main = 1,
       ylab = '', xlab = '', bty = 'n')
  
}
dev.off()
