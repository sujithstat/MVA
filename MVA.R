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















