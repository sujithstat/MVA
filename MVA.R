######### Pract1
#1
rm(list=ls())
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
x1=c(1.103,0.842,0.925,0.857,0.795,0.787,0.933,0.799,0.945,0.921,0.792,0.815,0.755,0.880,0.900,
     0.764,0.733,0.932,0.856,0.890,0.688,0.940,0.493,0.835,0.915)
x2=c(1.052,0.859,0.873,0.744,0.809,0.779,0.880,0.851,0.876,0.906,0.825,0.751,0.724,0.866,0.838,
     0.757,0.748,0.898,0.786,0.950,0.532,0.850,0.616,0.752,0.936)
x3=c(2.139,1.873,1.887,1.739,1.734,1.509,1.695,1.740,1.811,1.954,1.624,2.204,1.508,1.786,1.902,
     1.743,1.863,2.028,1.390,2.187,1.650,2.334,1.037,1.509,1.971)
x4=c(2.238,1.741,1.809,1.547,1.715,1.474,1.656,1.777,1.759,2.009,1.657,1.846,1.458,1.811,1.606,
     1.794,1.869,2.032,1.324,2.087,1.378,2.225,1.268,1.422,1.869)
x5=c(0.873,0.590,0.767,0.706,0.549,0.782,0.737,0.618,0.853,0.823,0.686,0.678,0.662,0.810,0.723,
     0.586,0.627,0.836,0.578,0.758,0.533,0.757,0.546,0.618,0.869)
x6=c(0.872,0.744,0.713,0.674,0.654,0.571,0.803,0.682,0.777,0.765,0.668,0.546,0.595,0.819,0.677,
     0.541,0.752,0.805,0.610,0.718,0.482,0.731,0.615,0.664,0.868)
d=data.frame(x1,x2,x3,x4,x5,x6)
R=cor(d);R
pairs.panels(d,main="Scatter Plot")
ggplot(data = as.data.frame(as.table(R)),aes(Var1,Var2,fill=Freq))+geom_tile()+scale_fill_gradient(low="blue",high = "red")+labs(title = "Heat Map",x="Variables",y="Variables",fill="correlation")

########## Pract5
#1
rm(list = ls())
x=c(0.314,0.333,0.191,0.761,0.901,0.847,0.403,0.159,0.741,0.133)
qqnorm(x)
qqline(x)

#2
rm(list = lS())
x=c(-1,-0.10,0.16,0.41,0.62,0.80,1.26,1.54,1.71,2.30)
qqnorm(x)
qqline(x)

#3
rm(list = ls())
x=c(0.15,0.09,0.18,0.10,0.05,0.12,0.08,0.05,0.08,
     0.10,0.07,0.02,0.01,0.01,0.01,0.01,0.02,0.10,
     0.10,0.40,0.10,0.05,0.03,0.05,0.15)
qqnorm(x)
qqline(x)

#4
rm(list = ls())
library(mvoutlier)
par(mfrow=c(1,2))
x1=c(108.28,152.36,95.04,65.45,62.97,263.99,265.19,285.06,92.01,165.68)
x2=c(17.05,16.59,10.91,14.14,9.52,25.33,18.54,15.73,8.10,11.13)
qqnorm(x1)
qqline(x1)
qqnorm(x2)
qqline(x2)
dat=data.frame(x1,x2);dat
n=nrow(dat);n
p=ncol(dat);p
xbar=apply(dat,2,mean);xbar
S=((n+1)/n)*var(dat);S
d=c()

for (j in 1:n) {
  xj=t(dat[j,])
  d[j]=t(xj - xbar) %*% solve(S) %*% (xj - xbar)
}
d
d=sort(d);d
j=1:n
qp=qchisq(lower.tail = T,p = (j-0.5)/n,df = p);qp
plot(y = qp,x = d)
chisq.plot(dat,ask = FALSE,quan = 1)




########## Pract6
rm(list=ls())
x1=c(3.7,5.7,3.8,3.2,3.1,4.6,2.4,7.2,6.7,5.4,3.9,4.5,3.5,4.5,1.5,8.5,4.5,6.5,4.1,5.5)
x2=c(48.5,67.1,47.2,53.2,55.5,36.1,24.8,33.1,47.4,54.1,36.9,58.8,27.8,40.2,13.5,56.4,71.6,52.8,44.1,40.9)
x3=c(9.3,8,10.9,12,9.7,7.9,14,7.6,8.5,11.3,12.7,12.3,9.8,8.4,10.1,7.1,8.2,10.9,11.2,9.4)
qqnorm(x1)
qqline(x1)
qqnorm(x2)
qqline(x2)
qqnorm(x3)
qqline(x3)
dat=data.frame(x1,x2,x3)
chisq.plot(dat,quan = 1,ask = F)





########## Pract7
#1
rm(list = ls())
mu=c(0,0)
sig=matrix(c(1,0.9,0.9,1),nrow = 2)
rMVN.eigen=function(n,mu,sig){
  p=length(mu)
  ev=eigen(sig,symmetric = TRUE)
  lambda=ev$values
  V=ev$vectors
  Q = V %*% diag(sqrt(lambda)) %*% t(V)
  Z = matrix(rnorm(n*p),nrow = n,ncol = p)
  X = Z %*% Q + matrix(mu,n,p,byrow = TRUE)
  return(X)
  }
X=rMVN.eigen(1000,mu,sig);X
qqnorm(X[,2])
qqline(X[,1])

#2
rm(list = ls())
mu=c(0,1,2)
sig=matrix(c(1,-0.5,0.5,-0.5,1,-0.5,0.5,-0.5,1),nrow = 3)
rMVN.eigen=function(n,mu,sig){
  p=length(mu)
  ev=eigen(sig,symmetric = TRUE)
  lambda=ev$values
  V=ev$vectors
  Q = V %*% diag(sqrt(lambda)) %*% t(V)
  Z = matrix(rnorm(n*p),nrow = n,ncol = p)
  X = Z %*% Q + matrix(mu,n,p,byrow = TRUE)
  return(X)
}
X=rMVN.eigen(1000,mu,sig);X
qqnorm(X)
qqline(X)

library(psych)
pairs.panels(X,main="Scattler Plot")

#3
rm(list = ls())
data("iris")
d=iris[,-5];d=as.matrix(d);d
mu=apply(d, 2, mean);mu
sig=cov(d);sig
rMVN.eigen=function(n,mu,sig){
  p=length(mu)
  ev=eigen(sig,symmetric = TRUE)
  lambda=ev$values
  V=ev$vectors
  Q = V %*% diag(sqrt(lambda)) %*% t(V)
  Z = matrix(rnorm(n*p),nrow = n,ncol = p)
  X = Z %*% Q + matrix(mu,n,p,byrow = TRUE)
  return(X)
}

rMVN.SVD=function(n,mu,sig){
  p=length(mu)
  ev=eigen(sig,symmetric = TRUE)
  lambda=ev$values
  S=svd(sig)
  Q = S$u %*% diag(sqrt(lambda)) %*% t(S$v)
  Z = matrix(rnorm(n*p),nrow = n,ncol = p)
  X = Z %*% Q + matrix(mu,n,p,byrow = TRUE)
  return(X)
}
rMVN.chol=function(n,mu,sig){
  p=length(mu)
  Q = chol(sig)
  Z = matrix(rnorm(n*p),nrow = n,ncol = p)
  X = Z %*% Q + matrix(mu,n,p,byrow = TRUE)
  return(X)
}

rMVN.eigen(1000,mu,sig)
rMVN.SVD(1000,mu,sig)
rMVN.chol(1000,mu,sig)
