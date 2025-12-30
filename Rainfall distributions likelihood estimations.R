Likelihood <- function(w1,B11,B21,X,n){
  r = rep(0, n)
  for(i in 1:n){
    r[i] = log(w1*B11*exp(-X[i]*B11)+(1-w1)*B21*exp(-X[i]*B21))
  }
  return(sum(r))
}

 

ParameterEstimator <- function(X){
  p = 1000
  B1 <- runif(p, min = 0, max = 1000)
  B2 <- runif(p, min = 0, max = 1000)
  w <- runif(p, min = 0, max = 1)
  results = rep(0, p)
  for(i in 1:p){
    results[i] = Likelihood(w[i],B1[i],B2[i],X,length(X))
  }
  index <- which.max(results)
  return(c(w[index],B1[index],B2[index],results[index]))
}

Parameters <- ParameterEstimator(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,]$Precipitation_Sum)
w <- Parameters[1]
B1 <- Parameters[2]
B2 <- Parameters[3]
x <- seq(from = 1, to = 100, by = 0.1)



GammaParameters <- function(X){
  D = log(mean(X))-(1/length(X))*sum(log(X))
  a = (1+sqrt(1+4*D/3))/(4*D)
  b = mean(X)/a
  return(c(a,b))
}

GammaParameters1 <- function(X){
  if(length(X[X$Precipitation_Sum > 0,]$Precipitation_Sum)<3){
    G <- c(1.1,7)
  }else{
    G <- egamma(X[X$Precipitation_Sum > 0,]$Precipitation_Sum)$parameters
    #G <- GammaParameters(X[X$Precipitation_Sum > 0,]$Precipitation_Sum)
  }
  return(G)
}


MixedExpParameters <- function(X){
  w <- runif(1000,min=0.5, max=0.5)
  b1 <- runif(1000,min=6, max=10)
  b2 <- runif(1000,min=6, max=10)
  loglikelihood= vector(mode='list', length=1000)
  for(i in 1:1000){
    loglikelihood[i] <- fitdist(X[X$Precipitation_Sum > 0,]$Precipitation_Sum,"mixedexp", start=list(w=w[i], b1=b1[i], b2=b2[i]))$loglik
  }
  index <- which.max(loglikelihood)
  parameters <- fitdist(X[X$Precipitation_Sum > 0,]$Precipitation_Sum,"mixedexp", start=list(w=w[index], b1=b1[index], b2=b2[index]))
  return(parameters)
}



library(fitdistrplus)
library(stats4)
library(MASS)
# for other necessary test or graphical tools
library(survival)
library(actuar)
library(distrMod)

install.packages("mixtools")
library(mixtools)
library(VGAM)
library(EnvStats)
library(evd)

descdist(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)

descdist(UsefulHRainfall_CP4_C[UsefulHRainfall_CP4_C$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulHRainfall_CP4_F[UsefulHRainfall_CP4_F$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulHRainfall_R25_C[UsefulHRainfall_R25_C$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulHRainfall_R25_F[UsefulHRainfall_R25_F$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulHRainfall_Aero[UsefulHRainfall_Aero$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulHRainfall_Bani[UsefulHRainfall_Bani$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)
descdist(UsefulHRainfall_Orst[UsefulHRainfall_Orst$Precipitation_Sum > 0,]$Precipitation_Sum, boot = 1000)

egamma(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,]$Precipitation_Sum)
fgev(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,]$Precipitation_Sum)
fitdist(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,]$Precipitation_Sum,"gamma")

WeibullParameters <- function(W){
  return(fitdist(W[W$Precipitation_Sum > 0,]$Precipitation_Sum,"weibull")$estimate)
}
fitdist(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,]$Precipitation_Sum,"weibull")

fitdist(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,]$Precipitation_Sum,"mixedexp", start=list(w=0.5, b1=6, b2 = 6))

dmixedexp <- function(x,w,b1,b2) (w/b1)*exp(-(x/b1))+((1-w)/b2)*exp(-(x/b2))
  
WeibullParameters(UsefulRainfall_CP4_C)

Gamma_GOF_Test <- function(d1){
  dy <- d1[d1$Precipitation_Sum > 0,]$Precipitation_Sum
  dy1 <- egamma(dy)$parameters
  X <- ad.test(dy,scale = dy1[2], shape = dy1[1],"pgamma",estimated=TRUE)
  return(X)
}

Gamma_GOF_Test2 <- function(d1,d2){
  dy <- d1[d1$Precipitation_Sum > 0,]$Precipitation_Sum
  dx <- d2[d2$Precipitation_Sum > 0,]$Precipitation_Sum
  dy1 <- egamma(dy)$parameters
  X <- ad.test(dx,scale = dy1[2], shape = dy1[1],"pgamma")
  return(X)
}

Gamma_GOF_Test(UsefulRainfall_CP4_C)
Gamma_GOF_Test(UsefulRainfall_CP4_F)
Gamma_GOF_Test(UsefulRainfall_R25_C)
Gamma_GOF_Test(UsefulRainfall_R25_F)
Gamma_GOF_Test(UsefulRainfall_Aero)
Gamma_GOF_Test(UsefulRainfall_Bani)
Gamma_GOF_Test(UsefulRainfall_Orst)

Gamma_GOF_Test2(UsefulRainfall_Aero, UsefulRainfall_CP4_C)
Gamma_GOF_Test2(UsefulRainfall_Bani, UsefulRainfall_CP4_C)
Gamma_GOF_Test2(UsefulRainfall_Orst, UsefulRainfall_CP4_C)

Gamma_GOF_Test2(UsefulRainfall_Aero, UsefulRainfall_R25_C)
Gamma_GOF_Test2(UsefulRainfall_Bani, UsefulRainfall_R25_C)
Gamma_GOF_Test2(UsefulRainfall_Orst, UsefulRainfall_R25_C)

Gamma_GOF_Test2(UsefulRainfall_CP4_C, UsefulRainfall_CP4_F)
Gamma_GOF_Test2(UsefulRainfall_R25_C, UsefulRainfall_R25_F)
