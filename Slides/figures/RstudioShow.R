
# Simulation example
niter = 10000
results = data.frame(iter=1:niter, 
                     pvalue=NA)
N  = 50
b0 = 0
b1 = 0.5
sigma = 1
for(i in 1:niter){
  x = rnorm(N,0,1)
  y = b0 + x*b1 + rnorm(N,0,sigma)
  df = data.frame(x,y)
  fit = lm(y~x, data=df)
  results$pvalue[i] = summary(fit)$coefficients["x","Pr(>|t|)"]
}
(power = mean(results$pvalue<0.05))
