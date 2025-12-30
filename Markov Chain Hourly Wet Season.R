MarkovHourlyCounts <- function(d2){
  d1 = d2[d2$Month == c(6,7,8,9)]
  dx <- ifelse(d1$Precipitation_Sum < 1,0,1)
  dx1 <- dx[-1]
  n00 <- sum(ifelse(dx<1,ifelse(dx1<1,1,0),0))
  n01 <- sum(ifelse(dx<1,ifelse(dx1>0,1,0),0))
  n10 <- sum(ifelse(dx>0,ifelse(dx1>0,1,0),0))
  n11 <- sum(ifelse(dx>0,ifelse(dx1<1,1,0),0))
    p00=n00/(n00+n01)
    p01=n01/(n00+n01)
    p10=n10/(n10+n11)
    p11=n11/(n10+n11)
    P <- matrix(c(p00,p01,p10,p11),
                nrow = 2, ncol = 2, byrow = TRUE)
  return(P)}

MarkovHourlyProperties <- function(d2){
  X <- setNames(data.frame(matrix(ncol = 12, nrow = 1)), c("n00", "n01", "n10", "n11", "p00", "p01", "p10", "p11", "pi1", "pi0", "r1","chisq"))
  d1 = d2[d2$Month == c(6,7,8,9)]
  dx <- ifelse(d1$Precipitation_Sum < 1,0,1)
  dx1 <- dx[-1]
  n00 <- sum(ifelse(dx<1,ifelse(dx1<1,1,0),0))
  n01 <- sum(ifelse(dx<1,ifelse(dx1>0,1,0),0))
  n10 <- sum(ifelse(dx>0,ifelse(dx1>0,1,0),0))
  n11 <- sum(ifelse(dx>0,ifelse(dx1<1,1,0),0))
  p00=n00/(n00+n01)
  p01=n01/(n00+n01)
  p10=n10/(n10+n11)
  p11=n11/(n10+n11)
  pi1 = p01/(1+p01-p11)
  pi0 = 1-pi1
  r1 = p11-p01
  n=data.frame(c(n00,n10),
               c(n01,n11))
  chisq = 0
  for(i in 1:2){for(j in 1:2){
    eij <- sum(n[i,])*sum(n[,j]/sum(n))
    chisq = chisq + ((n[i,j]-eij)^2)/eij
  }}
  X$n00 <- n00
  X$n01 <- n01
  X$n10 <- n10
  X$n11 <- n11
  X$p00 <- p00
  X$p01 <- p01
  X$p10 <- p10
  X$p11 <- p11
  X$pi1 <- pi1
  X$pi0 <- pi0
  X$r1 <- r1
  X$chisq <- chisq
  X$significant_0.1 <- ifelse(chisq > 2.71, "Reject H0", "Accept H0")
  X$significant_0.05 <- ifelse(chisq > 3.84, "Reject H0", "Accept H0")
  X$significant_0.01 <- ifelse(chisq > 6.63, "Reject H0", "Accept H0")
  return(X)}

MarkovHourlyProperties_CP4_C <- MarkovHourlyProperties(UsefulHRainfall_CP4_C)
MarkovHourlyProperties_CP4_F <- MarkovHourlyProperties(UsefulHRainfall_CP4_F)
MarkovHourlyProperties_R25_C <- MarkovHourlyProperties(UsefulHRainfall_R25_C)
MarkovHourlyProperties_R25_F <- MarkovHourlyProperties(UsefulHRainfall_R25_F)

MarkovHourlyProperties_Aero <- MarkovHourlyProperties(UsefulHRainfall_Aero)
MarkovHourlyProperties_Bani <- MarkovHourlyProperties(UsefulHRainfall_Bani)
MarkovHourlyProperties_Orst <- MarkovHourlyProperties(UsefulHRainfall_Orst)

MarkovHourlyPropertiesAll <- rbind(MarkovHourlyProperties_CP4_C, MarkovHourlyProperties_CP4_F, MarkovHourlyProperties_R25_C, MarkovHourlyProperties_R25_F, MarkovHourlyProperties_Aero, MarkovHourlyProperties_Bani, MarkovHourlyProperties_Orst)

FourMonthHMarkov <- function(X){
  MP <- MarkovHourlyProperties(X)
  d1 <- X[X$Month == c(6,7,8,9)]
  MC <- MarkovChain(120*24,rbinom(1,1,((MP$n11+MP$n10)/sum(MP$n00,MP$n01,MP$n10,MP$n11))),MarkovHourlyCounts(X))
  G <- GammaParameters1(d1)
  Dist <- rgamma(120*24,shape=G[1], scale = G[2])
  Chain <- MC*Dist
  return(Chain)
}

NYearHMarkovSimulation <- function(n,X){
  d1 <- setNames(data.frame(matrix(ncol = 5, nrow = 120*24*n)), c("Hour", "Day", "Month", "Year", "Precipitation"))
  index <- seq(1,120*24*n)
  d1$Hour <- (index-1)%%24 + 1
  d1$Day <- ((index-1)%/%24)%%30 + 1
  d1$Month <- ((index-1)%/%(30*24))%%4 + 6
  d1$Year <- (index-1)%/%(120*24) + 1
  TotalChain = rep(0, 120*24*n)
  for(i in 1:n){
    TotalChain[((i-1)*120*24+1):(i*120*24)] <- FourMonthHMarkov(X)
  }
  d1$Precipitation <- TotalChain
  return(d1)
}

FourMonthHMarkov(UsefulHRainfall_R25_F)

x <- NYearHMarkovSimulation(2,UsefulHRainfall_R25_F)





