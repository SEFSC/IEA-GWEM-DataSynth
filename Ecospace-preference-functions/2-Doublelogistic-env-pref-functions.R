
rm(list=ls()); gc()

## -----------------------------------------------------------------------------
##
## Logistic function

doublelogistic <- function(max = 400, steps = 400, range = 'wide', min_abs, min_prf, max_prf, max_abs){ 

  #j = 42; max = 30; steps = 1200; range = 'nar'; min_abs = fg_pref$SalinityMin[j]; min_prf = fg_pref$SalinityPrefMin[j]; max_prf = fg_pref$SalinityPrefMax[j]; max_abs = fg_pref$SalinityMax[j]
  #j = 39; max = 30; steps = 1200; range = 'wide'; min_abs = fg_pref$DepthMin[j]; min_prf = fg_pref$DepthPrefMin[j]; max_prf = fg_pref$DepthPrefMax[j]; max_abs = fg_pref$DepthMax[j]
  mid_prf <- min_prf + (max_prf - min_prf) / 2 ## Midpoint. Change from increasing to decreasing logistic function
  mid <- ifelse(mid_prf > max, max, mid_prf)
  step_size <-  max / steps
  x1  <- seq(0, mid-step_size, by = step_size)
  x2  <- seq(mid, max, by = step_size)
  r1  <- min_prf - min_abs 
  r2  <- max_abs - max_prf 
  C1  <- min_abs + r1 / 2
  C2  <- max_prf + r2 / 2
  B1  <- ifelse(range == 'wide', 1/sqrt(r1), 1/log10(r1)) ## If range is 'wide', curve shape inversely proportional to range size
  B2  <- ifelse(range == 'wide', 1/sqrt(r2), 1/log10(r2))
  if(B1 < 0) B1 = Inf
  if(B2 < 0) B2 = Inf
  S  = 1; A1 = 0; D1 = 1; A2 = 1
  
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

plot_pref_func <- function(p1, p2, p3, p4, fg_num, fg_name,
                           max = 400, xmin = 0, scale_xaxis = 'y', range = 'wide', driver = '') {
  pref_func <- doublelogistic(max = max, steps = max, range = range, p1, p2, p3, p4)
  xlim <- ifelse(p4 < max, p4+p4*0.15, max)
  xmax <- ifelse(scale_xaxis == 'y', xlim, max)
  xmin <- ifelse(scale_xaxis == 'y', p1-p1*0.15, 0)
  plot(pref_func$x, pref_func$y, 
       main = paste(driver, fg_num, fg_name),
       type = "l", ylab = "", xlab = "", cex.main = 1, bty = 'n',
       xlim = c(xmin, xmax), ylim = c(0,1), yaxt='n', lwd=2)
  axis(side = 2, at=c(0,0.5,1))
  abline(v = p1,  col = "red", lty = "dashed")
  abline(v = p2,  col = "blue", lty = "dashed")
  abline(v = p3,  col = "blue", lty = "dashed")
  abline(v = p4,  col = "red", lty = "dashed")
}

## Test preference functions -------------------------
fg_pref <- read.csv("./Ecospace-preference-functions/intermediate-ouput/fg-env-preference-parameters-adjusted.csv")
j = 65 ## Can be set to any functional group. Shrimp are a good one to see responses.
par(mfrow=c(1,3))
plot_pref_func(fg_pref$DepthMin[j], fg_pref$DepthPrefMin[j], 
               fg_pref$DepthPrefMax[j], fg_pref$DepthMax[j], 
               fg_num = fg_pref$EwE_num[j], 
               fg_name = fg_pref$EwE_name[j],
               max = 400, scale_xaxis = 'y', range = 'wide',
               driver = "Depth")

plot_pref_func(fg_pref$TempMin[j], fg_pref$TempPrefMin[j], 
               fg_pref$TempPrefMax[j], fg_pref$TempMax[j], 
               fg_num = fg_pref$EwE_num[j], 
               fg_name = fg_pref$EwE_name[j],
               max = 40, scale_xaxis = 'y', range = 'nar',
               driver = "Temp")

plot_pref_func(fg_pref$SalinityMin[j], fg_pref$SalinityPrefMin[j], 
               fg_pref$SalinityPrefMax[j], fg_pref$SalinityMax[j], 
               fg_num = fg_pref$EwE_num[j], 
               fg_name = fg_pref$EwE_name[j],
               max = 40, scale_xaxis = 'y', range = 'nar',
               driver = "Sal")
