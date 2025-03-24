
####################################

rm(list=ls())
library(lavaan)
library(MASS)

SigmaTypical = lav_matrix_lower2full(c(
  + 1,
  + .38, 1,
  + .26, .35, 1,
  + .34, .43, .28, 1,
  + .25, .14, .15, .11, 1,
  + .33, .62, .33, .41, .13, 1,
  + .29, .35, .42, .35, .19, .38, 1,
  + .42, .41, .29, .43, .20, .40, .35, 1,
  + .27, .51, .24, .35, .15, .59, .30, .30, 1, 
  + .30, .27, .20, .24, .46, .24, .24, .26, .22, 1))
colnames(SigmaTypical) = c("BD","SI","DS","PCn","CD","VC","LN","MR","CO","SS")

####################################

N = 400

df = round(mvrnorm(n=N, mu=rep(10,nrow(SigmaTypical)), 
             Sigma=SigmaTypical*(3^2)))
df[df<1] = 1; df[df>19] = 19
df[sample(1:(N*10),(N*10*0.005))] = NA
df = data.frame(df)
colnames(df) = colnames(SigmaTypical)

write.csv(df,"wisc.csv",row.names=F)

####################################


