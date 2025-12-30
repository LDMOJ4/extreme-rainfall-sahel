TempComposite <- function(d1,d2){
  i=1
  d3 <- data.frame()
  for (row in 1:nrow(d2)) {
    if(d2[row,7]+0.5==d1[i,1]){
      if(d2[row,10]==d1[i,2]){
        if(d2[row,8]==d1[i,3]){
          if(d2[row,9]==d1[i,4]){
            d3[1,i] <-d1[i,1]
            d3[2,i] <-d1[i,2]
            d3[3,i] <-d1[i,3]
            d3[4,i] <-d1[i,4]
            d3[5:41,i] <- d2[(row-12):(row+24),2]
            if(i<nrow(d1))
               {i=i+1}
          }
        }
      }
    }
  }
  d3 <- t(d3)
  rownames(d3) <- 1:nrow(d3)
  colnames(d3) <- c("Hour", "Day", "Month", "Year", "-12","-11","-10","-9","-8","-7","-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")
  d3 <-data.table(d3)
  return(d3)
}

CompTemp_CP4_C <- TempComposite(CompRainfall_CP4_C,CP4_C_Temp)
CompTemp_CP4_F <- TempComposite(CompRainfall_CP4_F,CP4_F_Temp)
CompTemp_R25_C <- TempComposite(CompRainfall_R25_C,R25_C_Temp)
CompTemp_R25_F <- TempComposite(CompRainfall_R25_F,R25_F_Temp)

CompTMeans_CP4_C <- CompositeMeans(CompTemp_CP4_C)
colnames(CompTMeans_CP4_C) <- c("CP4_C_Mean", "CP4_C_SE")
CompTMeans_CP4_F <- CompositeMeans(CompTemp_CP4_F)
colnames(CompTMeans_CP4_F) <- c("CP4_F_Mean", "CP4_F_SE")
CompTMeans_R25_C <- CompositeMeans(CompTemp_R25_C)
colnames(CompTMeans_R25_C) <- c("R25_C_Mean", "R25_C_SE")
CompTMeans_R25_F <- CompositeMeans(CompTemp_R25_F)
colnames(CompTMeans_R25_F) <- c("R25_F_Mean", "R25_F_SE")


Hour <- c(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
Hour <- Hour - 0.5
CompTMeansAll <- cbind(Hour,CompTMeans_CP4_C,CompTMeans_CP4_F,CompTMeans_R25_C,CompTMeans_R25_F)


ggplot(data = CompTMeansAll)+
  geom_line(aes(x=Hour, y=CP4_C_Mean, color = "CP4_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=CP4_F_Mean, color = "CP4_F"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_C_Mean, color = "R25_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_F_Mean, color = "R25_F"),linewidth = 1, group=1) +
  geom_errorbar(aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "CP4_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "CP4_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "R25_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "R25_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("Mean temperature around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("Temperature (°K)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model") +
  scale_color_manual(values = AM_colors)

CompTempAnom <- function(d1,d2){
  d1 <- data.frame(d1)
  for(row in 1:nrow(d1)){
    Hour <- d1$Hour[row]
    for(column in 5:41){
      i <- which((d2$Hour+0.5) == ((Hour+column-17)%%24))
      AvgTemp <- d2$Mean[i]
      d1[row,column] <- d1[row,column]-AvgTemp
    }
  }
  colnames(d1) <- c("Hour", "Day", "Month", "Year", "-12","-11","-10","-9","-8","-7","-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")
  d1 <-data.table(d1)
  return(d1)  
}

CompTAnomMeans_CP4_C <- CompositeMeans(CompTempAnom(CompTemp_CP4_C,Hourly_MeanT_CP4_C))
colnames(CompTAnomMeans_CP4_C) <- c("CP4_C_Mean", "CP4_C_SE")
CompTAnomMeans_CP4_F <- CompositeMeans(CompTempAnom(CompTemp_CP4_F,Hourly_MeanT_CP4_F))
colnames(CompTAnomMeans_CP4_F) <- c("CP4_F_Mean", "CP4_F_SE")
CompTAnomMeans_R25_C <- CompositeMeans(CompTempAnom(CompTemp_R25_C,Hourly_MeanT_R25_C))
colnames(CompTAnomMeans_R25_C) <- c("R25_C_Mean", "R25_C_SE")
CompTAnomMeans_R25_F <- CompositeMeans(CompTempAnom(CompTemp_R25_F,Hourly_MeanT_R25_F))
colnames(CompTAnomMeans_R25_F) <- c("R25_F_Mean", "R25_F_SE")

CompTAnomMeansAll <- cbind(Hour,CompTAnomMeans_CP4_C,CompTAnomMeans_CP4_F,CompTAnomMeans_R25_C,CompTAnomMeans_R25_F, CompTAnomMeans_Banizoumbou_2005)

ggplot(data = CompTAnomMeansAll)+
  geom_line(aes(x=Hour, y=CP4_C_Mean, color = "CP4_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=CP4_F_Mean, color = "CP4_F"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_C_Mean, color = "R25_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_F_Mean, color = "R25_F"),linewidth = 1, group=1) +
  geom_errorbar(aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "CP4_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "CP4_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "R25_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "R25_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("Mean temperature anomaly around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("Temperature (°K)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model") +
  scale_color_manual(values = AM_colors)
