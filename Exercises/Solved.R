
##########################################
##########################################

## NOT ALL SOLUTIONS ARE INCLUDED
## BUT ONLY A FEW PRESENTED IN CLASS

##########################################
##########################################

rm(list=ls())
library(readxl)

##########################################

# First steps in R

prf = read.csv("datasets/Performance.csv")

##########################################

# Vectors 

##########################################

# Dataframes

df = read.csv("datasets/exerData1.csv")

##########################################

# Data nightmare exercise

inv1 = read.csv("datasets/ExerData_Invalsi_1.csv", sep=";")
inv2 = read.csv("datasets/ExerData_Invalsi_2.csv")
wcs = data.frame(read_excel("datasets/ExerData_Wechsler.xlsx"))
exp = read.csv("datasets/ExerData_LabTrials.csv")
qst = read.csv("datasets/ExerData_Questionnaires.csv")
  
##########################################

# Programming and more

load("datasets/ExerProgr.RData")

for(i in 1:ncol(df1)){
  xxx = as.numeric(df1[,i])
  totalNA = sum(is.na(xxx))
  if(totalNA/length(xxx) < .20) df1[,i] = as.numeric(df1[,i])
}

library(MASS)
Sigma = matrix(c(1, .5,
                 .5, 1), nrow=2,ncol=2)
x = mvrnorm(n=30, mu=c(0,0), Sigma=Sigma, empirical=T)
cor(x)

dfLong1 = reshape(
  data = dfWide1, 
  varying=c("T0","T1","T2"),
  direction="long",
  timevar="Time",
  times=c("t0","t1","t2"),
  v.names="Performance"
)

##########################################

# Creating a word cloud 

qst = read.csv("wordcloud/scopustoff.csv")

##########################################

# Monte Carlo simulations for Power analysis

pvalues = c()
for(i in 1:1e4 ){
  controls = rnorm(n=40, mean=0, sd=1)
  cases = rnorm(n=40, mean=0, sd=.2)
  tt = t.test(controls, cases, var.equal=TRUE)
  pvalues[i] = tt$p.value
}
mean(pvalues<0.05)


##########################################
##########################################

