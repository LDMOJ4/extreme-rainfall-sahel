MarkovCounts <- function(d2){
  X <- vector(mode = "list", length = 0)
  for (row in 1:nrow(d2)) {
    precip_row <- d2[row,"Precipitation_Sum"]
    precip_row_plus1 <- d2[row+1,"Precipitation_Sum"]
    if(!(is.na(precip_row_plus1))){
      if(precip_row > 0){
        if(precip_row_plus1>0){
          d2$n11[row]=1
          d2$n10[row]=0
          d2$n01[row]=0
          d2$n00[row]=0
        }else{
          d2$n11[row]=0
          d2$n10[row]=1
          d2$n01[row]=0
          d2$n00[row]=0
        }
      } else{
        if(precip_row_plus1>0){
          d2$n11[row]=0
          d2$n10[row]=0
          d2$n01[row]=1
          d2$n00[row]=0
        }else{
          d2$n11[row]=0
          d2$n10[row]=0
          d2$n01[row]=0
          d2$n00[row]=1
        }
      }
    }
  }
  for (mon in 1:12){
    d1 = d2[d2$Month == mon]
    n00 = sum(d1$n00)
    n01 = sum(d1$n01)
    n10 = sum(d1$n10)
    n11 = sum(d1$n11)
    p00=n00/(n00+n01)
    p01=n01/(n00+n01)
    p10=n10/(n10+n11)
    p11=n11/(n10+n11)
    P <- matrix(c(p00,p01,p10,p11),
                nrow = 2, ncol = 2, byrow = TRUE)
    if(any(is.na(P))){
      P <- matrix(c(1,0,1,0),nrow = 2, ncol = 2, byrow = TRUE)
    }
    X[[mon]] <- P
  }
  return(X)}


# Simulates a Markov chain with transition matrix P for N steps from X0
MarkovChain <- function(N, X0, P) {
  X <- rep(0, N)    # empty vector of length N
  space <- nrow(P)  # number of points in sample space
  
  now <- X0+1
  for (n in 1:N) {
    now <- sample(space, 1, prob = P[now, ])
    X[n] <- (now - 1)
  }
  
  return(X)
}


TwelveMonthMarkov <- function(X){
  TotalChain = rep(0, 360)
  for (mon in 1:12){
    if(mon == 1){
    mc <- MarkovChain(30,0,X[[mon]])
    TotalChain[1:30] <- mc
    }else{
      mc <- MarkovChain(30,TotalChain[30*(mon-1)],X[[mon]])
      TotalChain[((30*(mon-1))+1):(30*mon)] <- mc
    }
  }
  return(TotalChain)
}

TwelveMonthDist <- function(X){
  M <- TwelveMonthMarkov(MarkovCounts(X))
  TotalChain = rep(0, 360)
  for (mon in 1:12){
    d1 = X[X$Month == mon]
    G <- GammaParameters1(d1)
    if(any(is.na(G))){
      Dist = rep(0, 30)
    }else{
      Dist = rgamma(30,shape=G[1], scale = G[2])
    }
      for(i in 1:30){
        TotalChain[((mon-1)*30 + i)] <- Dist[i]*M[((mon-1)*30 + i)]
      }
  }
  return(TotalChain)  
}

# TwelveMonthDistExp <- function(X){
#   M <- TwelveMonthMarkov(MarkovCounts(X))
#   TotalChain = rep(0, 360)
#   for (mon in 1:12){
#     d1 = X[X$Month == mon]
#     G <- MixedExpParameters(d1)
#     if(any(is.na(G))){
#       Dist = rep(0, 30)
#     }else{
#       Dist = rgamma(30,shape=G[1], scale = G[2])
#     }
#     for(i in 1:30){
#       TotalChain[((mon-1)*30 + i)] <- Dist[i]*M[((mon-1)*30 + i)]
#     }
#   }
#   return(TotalChain)  
# }

# TwelveMonthDist(UsefulRainfall_CP4_C)
# plot((0:360), c(0,TwelveMonthDist(UsefulRainfall_Orst)), type = "l", main = "360 step Markov Chain", xlab = "Day, n", ylab = "Markov Chain (Xn) state")

library(ggplot2)
ggplot()+
  geom_line(aes(x=(1:360),y = TwelveMonthDist(UsefulRainfall_CP4_C), color = "CP4_C"))+
  geom_line(aes(x=(1:360),y = TwelveMonthDist(UsefulRainfall_Orst), color = "Niamey ORSTOM"))+
  ggtitle("360 step Markov Chain") +
  xlab("Day, n") +
  ylab("Precipitation (mm)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model")
  

MarkovProperties <- function(d2){
  X <- setNames(data.frame(matrix(ncol = 12, nrow = 12)), c("n00", "n01", "n10", "n11", "p00", "p01", "p10", "p11", "pi1", "pi0", "r1","chisq"))
  for (row in 1:nrow(d2)) {
    precip_row <- d2[row,"Precipitation_Sum"]
    precip_row_plus1 <- d2[row+1,"Precipitation_Sum"]
    if(!(is.na(precip_row_plus1))){
      if(precip_row > 0){
        if(precip_row_plus1>0){
          d2$n11[row]=1
          d2$n10[row]=0
          d2$n01[row]=0
          d2$n00[row]=0
        }else{
          d2$n11[row]=0
          d2$n10[row]=1
          d2$n01[row]=0
          d2$n00[row]=0
        }
      } else{
        if(precip_row_plus1>0){
          d2$n11[row]=0
          d2$n10[row]=0
          d2$n01[row]=1
          d2$n00[row]=0
        }else{
          d2$n11[row]=0
          d2$n10[row]=0
          d2$n01[row]=0
          d2$n00[row]=1
        }
      }
    }
  }
  for (mon in 1:12){
    d1 = d2[d2$Month == mon]
    n00 = sum(d1$n00)
    n01 = sum(d1$n01)
    n10 = sum(d1$n10)
    n11 = sum(d1$n11)
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
    X$n00[mon] <- n00
    X$n01[mon] <- n01
    X$n10[mon] <- n10
    X$n11[mon] <- n11
    X$p00[mon] <- p00
    X$p01[mon] <- p01
    X$p10[mon] <- p10
    X$p11[mon] <- p11
    X$pi1[mon] <- pi1
    X$pi0[mon] <- pi0
    X$r1[mon] <- r1
    X$chisq[mon] <- chisq
    X$significant_0.1[mon] <- ifelse(chisq > 2.71, "Reject H0", "Accept H0")
    X$significant_0.05[mon] <- ifelse(chisq > 3.84, "Reject H0", "Accept H0")
    X$significant_0.01[mon] <- ifelse(chisq > 6.63, "Reject H0", "Accept H0")
  }
  return(X)}

MarkovProperties_CP4_C <- MarkovProperties(UsefulRainfall_CP4_C)
MarkovProperties_CP4_F <- MarkovProperties(UsefulRainfall_CP4_F)
MarkovProperties_R25_C <- MarkovProperties(UsefulRainfall_R25_C)
MarkovProperties_R25_F <- MarkovProperties(UsefulRainfall_R25_F)

MarkovProperties_Aero <- MarkovProperties(UsefulRainfall_Aero)
MarkovProperties_Bani <- MarkovProperties(UsefulRainfall_Bani)
MarkovProperties_Orst <- MarkovProperties(UsefulRainfall_Orst)

NYearMarkovSimulation <- function(n,X){
  d1 <- setNames(data.frame(matrix(ncol = 4, nrow = 360*n)), c("Day", "Month", "Year", "Precipitation"))
  index <- seq(1,360*n)
  d1$Day <- (index-1)%%30 + 1
  d1$Month <- ((index-1)%/%30)%%12 + 1
  d1$Year <- (index-1)%/%360 + 1
  TotalChain = rep(0, 360*n)
  for(i in 1:n){
    TotalChain[((i-1)*360+1):(i*360)] <- TwelveMonthDist(X)
  }
  d1$Precipitation <- TotalChain
  return(d1)
}

MarkovMonthMean_CP4_C <- Monthly_sum(NYearMarkovSimulation(10,UsefulRainfall_CP4_C))
MarkovMonthMean_CP4_F <- Monthly_sum(NYearMarkovSimulation(10,UsefulRainfall_CP4_F))
MarkovMonthMean_R25_C <- Monthly_sum(NYearMarkovSimulation(10,UsefulRainfall_R25_C))
MarkovMonthMean_R25_F <- Monthly_sum(NYearMarkovSimulation(10,UsefulRainfall_R25_F))

MarkovMonthMean_Aero <- Monthly_sum(NYearMarkovSimulation(10,UsefulRainfall_Aero))
MarkovMonthMean_Bani <- Monthly_sum(NYearMarkovSimulation(10,UsefulRainfall_Bani))
MarkovMonthMean_Orst <- Monthly_sum(NYearMarkovSimulation(10,UsefulRainfall_Orst))

MarkovMonthMean_CP4_C <- data.table(MarkovMonthMean_CP4_C)
MarkovMonthMean_CP4_F <- data.table(MarkovMonthMean_CP4_F)
MarkovMonthMean_R25_C <- data.table(MarkovMonthMean_R25_C)
MarkovMonthMean_R25_F <- data.table(MarkovMonthMean_R25_F) 

MarkovMonthMean_Aero <- data.table(MarkovMonthMean_Aero)
MarkovMonthMean_Bani <- data.table(MarkovMonthMean_Bani)
MarkovMonthMean_Orst <- data.table(MarkovMonthMean_Orst)

MarkovMonthMean_CP4_C$Precipitation.sum = MarkovMonthMean_CP4_C$Precipitation.sum/10
MarkovMonthMean_CP4_F$Precipitation.sum = MarkovMonthMean_CP4_F$Precipitation.sum/10
MarkovMonthMean_R25_C$Precipitation.sum = MarkovMonthMean_R25_C$Precipitation.sum/10
MarkovMonthMean_R25_F$Precipitation.sum = MarkovMonthMean_R25_F$Precipitation.sum/10

MarkovMonthMean_Aero$Precipitation.sum = MarkovMonthMean_Aero$Precipitation.sum/10
MarkovMonthMean_Bani$Precipitation.sum = MarkovMonthMean_Bani$Precipitation.sum/10
MarkovMonthMean_Orst$Precipitation.sum = MarkovMonthMean_Orst$Precipitation.sum/10

MarkovMonthMean_CP4_C$Precipitation.se = MarkovMonthMean_CP4_C$Precipitation.se*30/sqrt(10*30)
MarkovMonthMean_CP4_F$Precipitation.se = MarkovMonthMean_CP4_F$Precipitation.se*30/sqrt(10*30)
MarkovMonthMean_R25_C$Precipitation.se = MarkovMonthMean_R25_C$Precipitation.se*30/sqrt(10*30)
MarkovMonthMean_R25_F$Precipitation.se = MarkovMonthMean_R25_F$Precipitation.se*30/sqrt(10*30)

MarkovMonthMean_Aero$Precipitation.se = MarkovMonthMean_Aero$Precipitation.se*30/sqrt(10*30)
MarkovMonthMean_Bani$Precipitation.se = MarkovMonthMean_Bani$Precipitation.se*30/sqrt(10*30)
MarkovMonthMean_Orst$Precipitation.se = MarkovMonthMean_Orst$Precipitation.se*30/sqrt(10*30)

Headers = c("Month", "Precipitation_Mean", "Precipitation_SE")
names(MarkovMonthMean_CP4_C) = Headers
names(MarkovMonthMean_CP4_F) = Headers
names(MarkovMonthMean_R25_C) = Headers
names(MarkovMonthMean_R25_F) = Headers

names(MarkovMonthMean_Aero) = Headers
names(MarkovMonthMean_Bani) = Headers
names(MarkovMonthMean_Orst) = Headers

ggplot() + 
  geom_line(data = MonthMean_R25_F, aes(x=Month, y=Precipitation_Mean, color = "Model",group = 1), size = 1.5) +
  geom_errorbar(data = MonthMean_R25_F, aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE, color = "Model"), width=0.3, alpha=0.9, size=1)+
  geom_line(data = MarkovMonthMean_R25_F, aes(x=Month, y=Precipitation_Mean, color = "Markov"), size = 1.5) +
  geom_errorbar(data = MarkovMonthMean_R25_F, aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE, color = "Markov"), width=0.3, alpha=0.9, size=1)+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for each CP4 model run") +
  xlab("Month") +
  ylab("Precipitation (mm)")+
  theme(text = element_text(size = 15)) 

