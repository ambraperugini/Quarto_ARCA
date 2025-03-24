
#########################################################

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

#########################################################

set.seed(1)

N = 180
k = 6

schoolID = sample(1:6,N,replace=T,prob=runif(6,0.5,1))
school_rInt1 = 0.3*rnorm(k)[schoolID]
school_rInt2 = 0.1*rnorm(k)[schoolID]
school_rInt3 = 0.2*rnorm(k)[schoolID]
school_rInt4 = 0.2*rnorm(k)[schoolID]

name = replicate(N,paste0(sample(letters,5),collapse=""))
school = paste0("school",schoolID)

iq_z = rnorm(N,0,1)

wmScore = rbinom(N,20,pnorm(qnorm(.5)+school_rInt3+iq_z*.25))

visualScore = rbinom(N,20,pnorm(qnorm(.5)+school_rInt4+iq_z*.3))

mathAcc = rbinom(N,20,pnorm(qnorm(.6)+school_rInt1+iq_z*.3))
aggregate(mathAcc,by=list(schoolID),mean)
hist(mathAcc)

mathAvgTime = round(5000+rgamma(N,exp(0.5+school_rInt2-iq_z*.323),1)*5000)
aggregate(mathAvgTime,by=list(schoolID),median)
hist(mathAvgTime)

df = data.frame(school,name,mathAcc,mathAvgTime,wmScore,visualScore)
df = df[order(schoolID),]
cor(df[,3:ncol(df)])

write.csv(df,"exerData1.csv",row.names=F)

#########################################################

# LIKE A DATA SCIENTIST

set.seed(101)

N = 670
id = 1:N
idName = replicate(N,paste0(sample(letters,8),collapse=""))

# wisc part
w = round(mvrnorm(n=N, mu=rep(10,nrow(SigmaTypical)), 
                   Sigma=SigmaTypical*(3^2)))
iq_z = as.numeric(scale(rowMeans(w)))
w[w<1] = 1; w[w>19] = 19
w[sample(1:(N*10),(N*10*0.003))] = ""
w[sample(1:(N*10),(N*10*0.001))] = " "
w[sample(1:(N*10),(N*10*0.001))] = "-"
w[sample(1:(N*10),(N*10*0.002))] = "/"
sa = sample(1:(N*10),(N*10*0.002))
w[sa] = paste0(w[sa],",")
sa = sample(1:(N*10),(N*10*0.002))
w[sa] = paste0(",",w[sa])
sa = sample(1:(N*10),(N*10*0.002))
w[sa] = paste0(w[sa],"_")
sa = sample(1:(N*10),(N*10*0.002))
w[sa] = paste0("/",w[sa])
sa = sample(1:(N*10),(N*10*0.1))
w[sa] = paste0(w[sa]," ")
sa = sample(1:(N*10),(N*10*0.1))
w[sa] = paste0(" ",w[sa])
w = data.frame(w)
saR = sample(1:nrow(w),1); saC = sample(1:ncol(w),1); w[saR,saC]; w[saR,saC] = 2000; w[saR,saC]
colnames(w) = colnames(SigmaTypical)
w = data.frame(idName,w)
w = w[order(sample(1:nrow(w),nrow(w))),]
write.csv(w,"ExerData_Wechsler.csv",row.names=F)

# questionnaires 
op = as.numeric(scale(rnorm(N)+iq_z*.17))
agr = as.numeric(scale(rnorm(N)+iq_z*0))
qu = data.frame(
  Openness1 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1,1),Inf))),
  Openness2 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1,1),Inf))),
  Openness3 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1,1),Inf))),
  Openness4 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1,1),Inf))),
  Openness5 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1,1),Inf))),
  Openness6 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1,1),Inf))),
  Openness7 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1,1),Inf))),
  Openness8 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1,1),Inf))),
  Agreab1 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1.5,1.5),Inf))),
  Agreab2 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1.5,1.5),Inf))),
  Agreab3 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1.5,1.5),Inf))),
  Agreab4 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1.5,1.5),Inf))),
  Agreab5 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1.5,1.5),Inf))),
  Agreab6 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1.5,1.5),Inf))),
  Agreab7 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1.5,1.5),Inf))),
  Agreab8 = as.numeric(cut(scale(op + rnorm(N)),c(-Inf,runif(4,-1.5,1.5),Inf)))
)
for(i in 1:ncol(qu)){
  qu[sample(1:nrow(qu),round(nrow(qu)*.003)),i] = ""
  qu[sample(1:nrow(qu),(nrow(qu)*0.001)),i] = " "
  qu[sample(1:nrow(qu),(nrow(qu)*0.002)),i] = "-"
  sa = sample(1:nrow(qu),(nrow(qu)*0.05))
  qu[sa,i] = paste0(qu[sa,i]," ")
  sa = sample(1:nrow(qu),(nrow(qu)*0.05))
  qu[sa,i] = paste0(" ",qu[sa,i])
}
qu[sample(1:nrow(qu),round(nrow(qu)*.006)),] = ""
qu = data.frame(idName,qu)
qu = qu[order(sample(1:nrow(qu),nrow(qu))),]
write.csv(qu,"ExerData_Questionnaires.csv",row.names=F)

# invalsi 
ach = as.numeric(scale(rnorm(N)+iq_z*.3))
gender = sample(c("M","F"," "),N,replace=T,prob=c(.45,.55,.02))
ach[gender=="F"] = ach[gender=="F"]+0.15
sa = sample(1:N,round(N*.02))
gender[sa] = tolower(gender[sa])
inv = data.frame(
  InvalsiRead1 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead2 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead3 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead4 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead5 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead6 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead7 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead8 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead9 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead10 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead11 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1,
  InvalsiRead12 = as.numeric(cut(scale(ach + rnorm(N)),c(-Inf,runif(1,-1,1),Inf)))-1
)
inv[sample(1:nrow(inv),round(nrow(inv)*.006)),] = ""
saR = sample(1:nrow(inv),1); saC = sample(1:ncol(inv),1); inv[saR,saC]; inv[saR,saC] = 10000; inv[saR,saC]
inv = data.frame(idName,gender,inv)
inv = inv[order(sample(1:nrow(inv),nrow(inv))),]
inv1 = inv[1:150,]
write.table(inv1,"ExerData_Invalsi_1.csv",sep=";",row.names=F)
inv2 = inv[151:nrow(inv),]
write.csv(inv2,"ExerData_Invalsi_2.csv",row.names=F)

# lab trials
rInt = scale(rnorm(N)+iq_z*2)
cor(rInt,iq_z)
k = 40
rInt = rep(rInt,each=k)
idNameLab = rep(idName,each=k)
resp = rbinom(N*k,1,prob=pnorm(0.6+rInt*.3))
hist(aggregate(resp,by=list(idNameLab),FUN=mean)$x)
lt = data.frame(
  idName = idNameLab,
  respAcc = resp
)
write.csv(lt,"ExerData_LabTrials.csv",row.names=F)


#########################################################



