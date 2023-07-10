## -----------------------------------------------------------------------------
##
## Make matrices for Ecospace

## Setup -----------------------------------------------------------------------
rm(list=ls()); gc()
source("./Ecospace-preference-functions/2-Doublelogistic-env-pref-functions.R")
fg_pref <- read.csv("./Ecospace-preference-functions/intermediate-ouput/fg-env-preference-parameters-adjusted.csv")
dir_out <- "./Ecospace-preference-functions/output-for-Ecospace/"

## Depth -----------------------------------------------------------------------
driver = "Depth"
range = "wide"
max = 551
n_steps = 1200

## Make depth preferences
fg_pref$EwE_name; nrow(fg_pref)

## Make depth preference matrix
depth_mat = matrix(nrow = 1203, ncol = nrow(fg_pref))
row.names(depth_mat) = c("Name", "Left_limit", "Right_limit", 1:1200)
colnames(depth_mat) = fg_pref$EwE_name

## Loop through functional groupos
for (i in 1:ncol(depth_mat)){
  ## Set the four HSPEN parameters
  absmin = fg_pref$DepthMin[i]
  prfmin = fg_pref$DepthPrefMin[i]
  prfmax = fg_pref$DepthPrefMax[i]
  absmax = fg_pref$DepthMax[i]
  ## Run double logistic function
  pref_func <- doublelogistic(max = max, steps = n_steps, range = range, 
                              absmin, prfmin, prfmax, absmax)
  ## Name column header
  name = paste0(driver, "_", fg_pref$EwE_num[i], "_", 
                gsub("[[:space:]]", "-", fg_pref$EwE_name[i]))
  print(name)
  outvec = c(name, 0, max, pref_func$y)
  depth_mat[ ,i] = outvec[1:1203]
}

## Write out depth matrix for Ecospace
write.csv(depth_mat, paste0(dir_out, driver, "-pref-func-", ncol(depth_mat), "fg-", "max", max, "-", n_steps, "L.csv"), row.names = T)

## -----------------------------------------------------------------------------
## Temperature 
driver = "Temp"
range = "nar"
max = 40
n_steps = 1200

## Make preference matrix
pref_mat = matrix(nrow = 1203, ncol = nrow(fg_pref))
row.names(pref_mat) = c("Name", "Left_limit", "Right_limit", 1:1200)
colnames(pref_mat) = fg_pref$EwE_name

## Loop through functional groups
for (i in 1:ncol(pref_mat)){
  #  i = 2
  absmin = fg_pref$TempMin[i]
  prfmin = fg_pref$TempPrefMin[i]
  prfmax = fg_pref$TempPrefMax[i]
  absmax = fg_pref$TempMax[i]
  
  pref_func <- doublelogistic(max = max, steps = n_steps, range = range, 
                              absmin, prfmin, prfmax, absmax)
  
  name = paste0(driver, "_", fg_pref$EwE_num[i], "_", 
                gsub("[[:space:]]", "-", fg_pref$EwE_name[i]))
  print(name)
  outvec = c(name, 0, max, pref_func$y)
  pref_mat[ ,i] = outvec[1:1203]
}

## Write out depth matrix for Ecospace
write.csv(pref_mat, paste0(dir_out, driver, "-pref-func-", ncol(depth_mat), "fg-", "max", max, "-", n_steps, "L.csv"), row.names = T)

## -----------------------------------------------------------------------------
## Salinity 
driver = "Sal"
range = "nar"
max = 40
n_steps = 1200

## Make preference matrix
pref_mat = matrix(nrow = 1203, ncol = nrow(fg_pref))
row.names(pref_mat) = c("Name", "Left_limit", "Right_limit", 1:1200)
colnames(pref_mat) = fg_pref$EwE_name

## Loop through functional groupos
for (i in 1:ncol(pref_mat)){
  #  i = 2
  absmin = fg_pref$SalinityMin[i]
  prfmin = fg_pref$SalinityPrefMin[i]
  prfmax = fg_pref$SalinityPrefMax[i]
  absmax = fg_pref$SalinityMax[i]
  
  pref_func <- doublelogistic(max = max, steps = n_steps, range = range, 
                              absmin, prfmin, prfmax, absmax)
  
  name = paste0(driver, "_", fg_pref$EwE_num[i], "_", 
                gsub("[[:space:]]", "-", fg_pref$EwE_name[i]))
  print(name)
  outvec = c(name, 0, max, pref_func$y)
  pref_mat[ ,i] = outvec[1:1203]
}

## Write out depth matrix for Ecospace
write.csv(pref_mat, paste0(dir_out, driver, "-pref-func-", ncol(depth_mat), "fg-", "max", max, "-", n_steps, "L.csv"), row.names = T)

## -----------------------------------------------------------------------------
## Make plots

dir_pdf = "./Ecospace-preference-functions/figures/"
n_rows = 6 ## Number of plots per row on a page
n_cols = 6 ## Number of plots per column on a page
pg_plts = nwide * nhigh
n_plots = nrow(fg_pref) * 3
max_depth = 400
max_temp = 40
max_sal = 40
w = 8.5
h = 11 

## Scale X-axis
pdf(paste0(dir_pdf, "Pref-functions_depth-temp-sal_byFG_xaxis-scaled.pdf"), 
           width = w, height = h, onefile = T)
for(i in seq(from = 1, to = n_plots, by = pg_plts)) {
#  i = 1
  par(mfrow=c(n_rows, n_cols))
  for(j in seq(from = i, to = i+(n_rows * n_cols - 1))) {
    ## Plot depth
    plot_pref_func(fg_pref$DepthMin[j], fg_pref$DepthPrefMin[j], 
                   fg_pref$DepthPrefMax[j], fg_pref$DepthMax[j], 
                   fg_num = fg_pref$EwE_num[j], 
                   fg_name = fg_pref$EwE_name[j],
                   max = max_depth, scale_xaxis = 'y', 
                   range = 'wide',
                   driver = "Depth")
    ## Plot temperature
    plot_pref_func(fg_pref$TempMin[j], fg_pref$TempPrefMin[j], 
                   fg_pref$TempPrefMax[j], fg_pref$TempMax[j], 
                   fg_num = fg_pref$EwE_num[j], 
                   fg_name = fg_pref$EwE_name[j],
                   max = max_temp, scale_xaxis = 'y', 
                   range = 'nar',
                   driver = "Temp")
    ## Plot salinity
    plot_pref_func(fg_pref$SalinityMin[j], fg_pref$SalinityPrefMin[j], 
                   fg_pref$SalinityPrefMax[j], fg_pref$SalinityMax[j], 
                   fg_num = fg_pref$EwE_num[j], 
                   fg_name = fg_pref$EwE_name[j],
                   max = max_sal, scale_xaxis = 'y', 
                   range = 'nar',
                   driver = "Sal")
    
  }
}
dev.off()

## Set X-axis
pdf(paste0(dir_pdf, "Pref-functions_depth-temp-sal_byFG_xaxis-set.pdf"), 
    width = w, height = h, onefile = T)
for(i in seq(from = 1, to = n_plots, by = pg_plts)) {
  par(mfrow=c(n_rows, n_cols))
  for(j in seq(from = i, to = i+(n_rows * n_cols - 1))) {
    ## Plot depth
    plot_pref_func(fg_pref$DepthMin[j], fg_pref$DepthPrefMin[j], 
                   fg_pref$DepthPrefMax[j], fg_pref$DepthMax[j], 
                   fg_num = fg_pref$EwE_num[j], 
                   fg_name = fg_pref$EwE_name[j],
                   max = max_depth, scale_xaxis = 'n', 
                   range = 'wide',
                   driver = "Depth")
    ## Plot temperature
    plot_pref_func(fg_pref$TempMin[j], fg_pref$TempPrefMin[j], 
                   fg_pref$TempPrefMax[j], fg_pref$TempMax[j], 
                   fg_num = fg_pref$EwE_num[j], 
                   fg_name = fg_pref$EwE_name[j],
                   max = max_temp, scale_xaxis = 'n', 
                   range = 'nar',
                   driver = "Temp")
    ## Plot salinity
    plot_pref_func(fg_pref$SalinityMin[j], fg_pref$SalinityPrefMin[j], 
                   fg_pref$SalinityPrefMax[j], fg_pref$SalinityMax[j], 
                   fg_num = fg_pref$EwE_num[j], 
                   fg_name = fg_pref$EwE_name[j],
                   max = max_sal, scale_xaxis = 'n', 
                   range = 'nar',
                   driver = "Sal")
    
  }
}
dev.off()

## Plot depth with varying X-axis (depth) --------------------------------------
n_plots = nrow(fg_pref) 
pdf(paste0(dir_pdf, "Pref_depth_xaxis-scaled.pdf"), width = w, height = h, onefile = T)
for(i in seq(from = 1, to = n_plots, by = pg_plts)) {
  for(j in seq(from = i, to = i+(n_rows * n_cols - 1))) {
    ## Plot depth
    plot_pref_func(fg_pref$DepthMin[j], fg_pref$DepthPrefMin[j], 
                   fg_pref$DepthPrefMax[j], fg_pref$DepthMax[j], 
                   fg_num = fg_pref$EwE_num[j], 
                   fg_name = fg_pref$EwE_name[j],
                   max = max_depth, scale_xaxis = 'n', 
                   range = 'wide',
                   driver = "Depth")
  }
}
dev.off()





for(j in 1:36){
  p1 <- fg_pref$DepthMin[j] 
  p2 <- fg_pref$DepthPrefMin[j] 
  p3 <- fg_pref$DepthPrefMax[j] 
  p4 <- fg_pref$DepthMax[j] 
  plot_pref_function(p1, p2, p3, p4, 
                           fg_num = fg_pref$EwE_num[j], 
                           fg_name = fg_pref$EwE_name[j],
                           max = max, scale_xaxis = 'n', range = 'wide')
}

for(j in 37:72){
  p1 <- fg_pref$DepthMin[j] 
  p2 <- fg_pref$DepthPrefMin[j] 
  p3 <- fg_pref$DepthPrefMax[j] 
  p4 <- fg_pref$DepthMax[j] 
  plot_pref_function(p1, p2, p3, p4, 
                           fg_num = fg_pref$EwE_num[j], 
                           fg_name = fg_pref$EwE_name[j],
                           max = max, 
                           scale_xaxis = 'n')
}
dev.off()



## Temperature -----------------------------------------------------------------
pdf("./Ecospace-preference-functions/figures/Temp-pref-dbl-logistic.pdf", width = w, height = h, 
    onefile = T)

max = 34
par(mfrow=c(6,6))
for(j in 1:36){
  p1 <- fg_pref$TempMin[j] 
  p2 <- fg_pref$TempPrefMin[j] 
  p3 <- fg_pref$TempPrefMax[j] 
  p4 <- fg_pref$TempMax[j] 
  plot_pref_function(p1, p2, p3, p4, 
                           fg_num = fg_pref$EwE_num[j], 
                           fg_name = fg_pref$EwE_name[j],
                           max = max, 
                           scale_xaxis = 'n')
}

for(j in 37:72){
  p1 <- fg_pref$TempMin[j] 
  p2 <- fg_pref$TempPrefMin[j] 
  p3 <- fg_pref$TempPrefMax[j] 
  p4 <- fg_pref$TempMax[j] 
  plot_pref_function(p1, p2, p3, p4, 
                           fg_num = fg_pref$EwE_num[j], 
                           fg_name = fg_pref$EwE_name[j],
                           max = max, 
                           scale_xaxis = 'n')
}
dev.off()


## Salinity -----------------------------------------------------------------
pdf("./Ecospace-preference-functions/figures/Salinity-pref-dbl-logistic.pdf", width = w, height = h, 
    onefile = T)

max(fg_pref$SalinityMax[j])
max = 38
par(mfrow=c(6,6))
for(j in 1:36){
  p1 <- fg_pref$SalinityMin[j] 
  p2 <- fg_pref$SalinityPrefMin[j] 
  p3 <- fg_pref$SalinityPrefMax[j] 
  p4 <- fg_pref$SalinityMax[j] 
  plot_pref_function(p1, p2, p3, p4, 
                           fg_num = fg_pref$EwE_num[j], 
                           fg_name = fg_pref$EwE_name[j],
                           max = max, xmin = 15,
                           scale_xaxis = 'y')
}

for(j in 37:72){
  p1 <- fg_pref$SalinityMin[j] 
  p2 <- fg_pref$SalinityPrefMin[j] 
  p3 <- fg_pref$SalinityPrefMax[j] 
  p4 <- fg_pref$SalinityMax[j] 
  plot_pref_function(p1, p2, p3, p4, 
                           fg_num = fg_pref$EwE_num[j], 
                           fg_name = fg_pref$EwE_name[j],
                           max = max, xmin = 15,
                           scale_xaxis = 'y')
}
dev.off()


