
rm(list=ls()); gc(); windows()


fg_prefs <- read.csv("./Ecospace-preference-functions/intermediate-ouput/")

#------------------------------------------------------------------------------
##
## Logistic function

max <- 100


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

