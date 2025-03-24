
#########################################################

rm(list=ls())
library(lavaan)
library(MASS)

#########################################################

# data cleaning with programming

set.seed(0)

N = 900
k = 25

Sigma = diag(k)
Sigma[Sigma==0] = runif(k^2-k,0.1,0.4)
x = data.frame(round(mvrnorm(N,mu=rep(0,k),Sigma=Sigma),5))

subjName = replicate(N,paste(sample(letters,7),collapse=""))
df1 = data.frame(cbind(subjName,x))
head(df1)

for(i in 2:ncol(df1)){
  if(rbinom(1,1,.4)){
    nletters = round(runif(1,2,5))
    df1[,i] = replicate(N,paste(sample(c(letters,0:9,0:9,0:9),nletters),collapse=""))
  }
  sa = sample(1:nrow(df1),round(nrow(df1)*runif(1,.01,.04)))
  df1[sa,i] = " "
  sa = sample(1:nrow(df1),round(nrow(df1)*runif(1,.008,.03)))
  df1[sa,i] = "."
  sa = sample(1:nrow(df1),round(nrow(df1)*runif(1,.008,.03)))
  df1[sa,i] = sample(letters,length(sa),replace=T)
}
head(df1)

for(i in 1:ncol(df1)) print(round(sum(!is.na(as.numeric(df1[,i])))/nrow(df1),2))

#########################################################

# reshape

library(tidyr)

set.seed(0)
N = 150
k = 50
rInt = rnorm(N,0,1)*.6
id = paste0("Subj_",1:N)
age = round(runif(N,7,10),1)
group = rep(0:1,each=N/2)
T0 = rbinom(N,k,prob=pnorm(-0.5 + rInt + age*.1)); hist(T0); cor(group,T0)
T1 = rbinom(N,k,prob=pnorm(-0.5 + rInt + age*.1 + group*.3)); hist(T1); cor(group,T1)
T2 = rbinom(N,k,prob=pnorm(-0.5 + rInt + age*.1 + group*.2)); hist(T2); cor(group,T2)

dfWide1 = data.frame(id,age,group,T0,T1,T2)

head(data.frame(pivot_longer(dfWide1,cols=c("T0","T1","T2"),names_to="Time")))


set.seed(1)
N = 150
k = 50
rInt = rnorm(N,0,1)*.6
id = paste0("Subj_",1:N)
age = round(runif(N,7,10),1)
group = rep(0:1,each=N/2)
Acc_T0 = rbinom(N,k,prob=pnorm(-0.5 + rInt + age*.1))
Acc_T1 = rbinom(N,k,prob=pnorm(-0.5 + rInt + age*.1 + group*.3))
Acc_T2 = rbinom(N,k,prob=pnorm(-0.5 + rInt + age*.1 + group*.2))
RT_T0 = 1000+rgamma(N,exp(0.8 + rInt*.1 + age*.1))*1000; hist(RT_T0); cor(group, RT_T0)
RT_T1 = 1000+rgamma(N,exp(0.8 + rInt*.1 - age*.005 - group*.3))*1000; hist(RT_T1); cor(group, RT_T1)
RT_T2 = 1000+rgamma(N,exp(0.8 + rInt*.1 - age*.005 - group*.1))*1000; hist(RT_T2); cor(group, RT_T2)

dfWide2 = data.frame(id,age,group,Acc_T0,Acc_T1,Acc_T2,RT_T0,RT_T1,RT_T2)

pivot_longer(
  data = dfWide2,
  cols = starts_with(c("Acc_", "RT_")), 
  names_to = c(".value", "Time"), 
  names_sep = "_"
)

reshape(
  dfWide2,
  varying = matrix(c("Acc_T0","Acc_T1","Acc_T2", "RT_T0","RT_T1","RT_T2"),2,3,byrow=T),
  v.names = c("Acc", "RT"),
  timevar = "Time",
  times = c("T0", "T1", "T2"),
  direction = "long"
)

reshape(
  dfWide2,
  varying = list(c("Acc_T0","Acc_T1","Acc_T2"),c("RT_T0","RT_T1","RT_T2")),
  v.names = c("Acc", "RT"),
  timevar = "Time",
  times = c("T0", "T1", "T2"),
  direction = "long"
)

reshape(
  dfWide2,
  varying = list(
    Acc = grep("^Acc_", names(dfWide2), value = TRUE), 
    RT = grep("^RT_", names(dfWide2), value = TRUE)
  ),
  v.names = c("Acc", "RT"),
  timevar = "Time",
  times = c("T0", "T1", "T2"),
  direction = "long"
)

save(df1, dfWide1, dfWide2, file="ExerProgr.RData")

#########################################################


