
rm(list=ls()); gc(); windows()
fg_pref <- read.csv("./Ecospace-preference-functions/intermediate-ouput/fg-env-preference-parameters-adjusted.csv")
dir_out <- "./Ecospace-preference-functions/output-for-Ecospace/"

## -----------------------------------------------------------------------------
##
## Logistic function

doublelogistic <- function(max = 400, steps = 400, min_abs, min_prf, max_prf, max_abs){ 

  #j = 2; max = 30; steps = 1200; min_abs = fg_pref$TempMin[j]; min_prf = fg_pref$TempPrefMin[j]; max_prf = fg_pref$TempPrefMax[j]; max_abs = fg_pref$TempMax[j]
  mid_prf <- min_prf + (max_prf - min_prf) / 2 ## Midpoint. Change from increasing to decreasing logistic function
  mid <- ifelse(mid_prf > max, max, mid_prf)
  step_size <-  max / steps
  x1  <- seq(0, mid-step_size, by = step_size)
  x2  <- seq(mid, max, by = step_size)
  r1  <- min_prf - min_abs ## Range size from 10% to 90%
  r2  <- max_abs - max_prf 
  C1  <- min_abs + r1 / 2
  C2  <- max_prf + r2 / 2
  B1  <- 1/sqrt(r1) ## Curve shape inversely proportional to range size
  B2  <- 1/sqrt(r2)
  S   <- 1
  A1 = 0; D1 = 1; A2 = 1; D2 = 0
  
  ## Logistic equations
  f1 <- function(x)     1 / (1 + exp(B1*(C1-x)))^S ## Increasing logistic eq.
  f2 <- function(x) 1 - 1 / (1 + exp(B2*(C2-x)))^S ## Decreasing logistic eq.
  ## 1 = value of the horizontal asymptote when x→−∞
  ## 0 = value of the horizontal asymptote when x→+∞
  ## B describes how rapidly the curve makes its transition between the two asymptotes
  ## S describes the asymmetry of the curve. The curve is symmetric when S=1. 
  ## C is a location parameter, which does not have a nice interpretation, 
  ##   unless S=1 when the curve has an inflection point at x=C.
  ##   In the case when S=1, C is the value of x for which f(x) is the midpoint between the two asymptotes
  
  y1 <- f1(x1)
  y2 <- f2(x2)
  out <- data.frame(x = c(x1, x2), y = c(y1, y2))
  out <- out[1:steps, ] 
  return(out)
}

## -----------------------------------------------------------------------------
##
## Make matrices for Ecospace

## Depth -----------------------------------------------------------------------
driver = "Depth"
max = 551
n_steps = 1200

## Make depth preferences
fg_pref$EwE_name; nrow(fg_pref)

## Make depth preference matrix
depth_mat = matrix(nrow = 1203, ncol = nrow(fg_pref))
row.names(depth_mat) = c("Name", "Left_limit", "Right_limit", 1:1200)
colnames(depth_mat) = fg_pref$EwE_name
#maxmapdepth = 1083

## Loop through functional groupos
for (i in 1:ncol(depth_mat)){
  #  i = 2
  absmin = fg_pref$DepthMin[i]
  prfmin = fg_pref$DepthPrefMin[i]
  prfmax = fg_pref$DepthPrefMax[i]
  absmax = fg_pref$DepthMax[i]
  
  pref_func <- doublelogistic(max = max, steps = n_steps, absmin, prfmin, prfmax, absmax)
  
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
max = 30
n_steps = 1200

## Make preference matrix
pref_mat = matrix(nrow = 1203, ncol = nrow(fg_pref))
row.names(pref_mat) = c("Name", "Left_limit", "Right_limit", 1:1200)
colnames(pref_mat) = fg_pref$EwE_name

## Loop through functional groupos
for (i in 1:ncol(pref_mat)){
  #  i = 2
  absmin = fg_pref$TempMin[i]
  prfmin = fg_pref$TempPrefMin[i]
  prfmax = fg_pref$TempPrefMax[i]
  absmax = fg_pref$TempMax[i]
  
  pref_func <- doublelogistic(max = max, steps = n_steps, absmin, prfmin, prfmax, absmax)
  
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
max = 37
n_steps = 1200

## Make preference matrix
pref_mat = matrix(nrow = 1203, ncol = nrow(fg_pref))
row.names(pref_mat) = c("Name", "Left_limit", "Right_limit", 1:1200)
colnames(pref_mat) = fg_pref$EwE_name

## Loop through functional groupos
for (i in 1:ncol(pref_mat)){
  #  i = 2
  absmin = fg_pref$SalinityMinMin[i]
  prfmin = fg_pref$SalinityPrefMin[i]
  prfmax = fg_pref$SalinityPrefMax[i]
  absmax = fg_pref$SalinityMax[i]
  
  pref_func <- doublelogistic(max = max, steps = n_steps, absmin, prfmin, prfmax, absmax)
  
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

## Plot depth with varying X-axis (depth) --------------------------------------
max = 400

plot_preference_function <- function(p1, p2, p3, p4, fg_num, fg_name,
                                     max = 400, xmin = 0, scale_xaxis = 'y') {
  pref_func <- doublelogistic(max = max, steps = max, p1, p2, p3, p4)
  xlim <- ifelse(p4 < max, p4+p4*0.15, max)
  xmax <- ifelse(scale_xaxis == 'y', xlim, max)
  plot(pref_func$x, pref_func$y, 
       main = paste(fg_num, fg_name),
       type = "l", ylab = "", xlab = "", cex.main = 1, bty = 'n',
       xlim = c(xmin, xmax), yaxt='n', lwd=2)
  axis(side = 2, at=c(0,0.5,1))
  abline(v = p1,  col = "red", lty = "dashed")
  abline(v = p2,  col = "blue", lty = "dashed")
  abline(v = p3,  col = "blue", lty = "dashed")
  abline(v = p4,  col = "red", lty = "dashed")
}


w = 8.5
h = 11 
pdf("./Ecospace-preference-functions/figures/Depth-pref-dbl-logistic_xaxis-set.pdf", width = w, height = h, 
    onefile = T)
par(mfrow=c(6,6))

max <- 400
for(j in 1:36){
  p1 <- fg_pref$DepthMin[j] 
  p2 <- fg_pref$DepthPrefMin[j] 
  p3 <- fg_pref$DepthPrefMax[j] 
  p4 <- fg_pref$DepthMax[j] 
  plot_preference_function(p1, p2, p3, p4, 
                           fg_num = fg_pref$EwE_num[j], 
                           fg_name = fg_pref$EwE_name[j],
                           max = max, 
                           scale_xaxis = 'n')
}

for(j in 37:72){
  p1 <- fg_pref$DepthMin[j] 
  p2 <- fg_pref$DepthPrefMin[j] 
  p3 <- fg_pref$DepthPrefMax[j] 
  p4 <- fg_pref$DepthMax[j] 
  plot_preference_function(p1, p2, p3, p4, 
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
  plot_preference_function(p1, p2, p3, p4, 
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
  plot_preference_function(p1, p2, p3, p4, 
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
  plot_preference_function(p1, p2, p3, p4, 
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
  plot_preference_function(p1, p2, p3, p4, 
                           fg_num = fg_pref$EwE_num[j], 
                           fg_name = fg_pref$EwE_name[j],
                           max = max, xmin = 15,
                           scale_xaxis = 'y')
}
dev.off()


