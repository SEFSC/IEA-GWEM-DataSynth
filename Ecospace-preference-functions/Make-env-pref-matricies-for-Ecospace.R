
rm(list=ls()); gc(); windows()


fg_pref <- read.csv("./Ecospace-preference-functions/intermediate-ouput/fg-env-preference-parameters-adjusted.csv")

## -----------------------------------------------------------------------------
##
## Logistic function

doublelogistic <- function(max, min_abs, min_prf, max_prf, max_abs){ 
  #max = 400
  #min_abs = fg_pref$DepthMin[j]
  #min_prf = fg_pref$DepthPrefMin[j]
  #max_prf = fg_pref$DepthPrefMax[j]
  #max_abs = fg_pref$DepthMax[j]
  
  mid <- min_prf + (max_prf - min_prf) / 2 ## Midpoint. Change from increasing to decreasing logistic function
  x1  <- seq(0, mid, by = 1)
  x2  <- seq(mid + 1, max, by = 1)
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
  return(out)
}

## -----------------------------------------------------------------------------
##
## Make depth preferences

driver = "Depth"

## Make depth preferences
fg_pref$EwE_name; nrow(fg_pref)

## Make depth preference matrix
depth_mat = matrix(nrow = 1203, ncol = nrow(fg_pref))
row.names(depth_mat) = c("Name", "Left_limit", "Right_limit", 1:1200)
colnames(depth_mat) = fg_pref$EwE_name
#maxmapdepth = 1083

## Make grid
steps = 1200
for (i in 1:ncol(depth_mat)){
  #  i = 2
  absmin = fg_pref$DepthMin[i]
  prfmin = fg_pref$DepthPrefMin[i]
  prfmax = fg_pref$DepthPrefMax[i]
  absmax = fg_pref$DepthMax[i]
  
  pref_func <- doublelogistic(max = steps, absmin, prfmin, prfmax, absmax)
  
  name = paste0(driver, "_", fg_pref$EwE_num[i], "_", 
                gsub("[[:space:]]", "-", fg_pref$EwE_name[i]))
  print(name)
  outvec = c(name, 0, steps, pref_func$y)
  depth_mat[ ,i] = outvec
}

## Write out depth matrix for Ecospace
write.csv(depth_mat, "./out/Depth_pref_matrix_1200-cells-for-Ecospace.csv", row.names = T)



## -----------------------------------------------------------------------------
## Make plots

## Plot depth with varying X-axis (depth) --------------------------------------
plot_preference_function <- function(p1, p2, p3, p4, fg_num, fg_name,
                                     max = 400, xmin = 0, scale_xaxis = 'y') {
  pref_func <- doublelogistic(max = 400, p1, p2, p3, p4)
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


