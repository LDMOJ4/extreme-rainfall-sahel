Hourly_Rainfall_sum <- function(data_table){
  return(aggregate(Precipitation ~ Hour+Day+Month+Year, data_table, sum ))
}

Hourly_Rainfall_sum1 <- function(data_table){
  return(aggregate(Observation ~ Hour+Day+Month+Year, data_table, sum))
}

HourlySum_CP4_C <- Hourly_Rainfall_sum(CP4_C)
HourlySum_CP4_F <- Hourly_Rainfall_sum(CP4_F)
HourlySum_R25_C <- Hourly_Rainfall_sum(R25_C)
HourlySum_R25_F <- Hourly_Rainfall_sum(R25_F)

HourlySum_Bani <- Hourly_Rainfall_sum1(BANIZOUMBOU2)
HourlySum_Aero <- Hourly_Rainfall_sum1(NY_AEROPORT2)
HourlySum_Orst <- Hourly_Rainfall_sum1(NY_ORSTOM2)

UsefulHRainfall_CP4_C <- data.table(HourlySum_CP4_C)
UsefulHRainfall_CP4_F <- data.table(HourlySum_CP4_F)
UsefulHRainfall_R25_C <- data.table(HourlySum_R25_C)
UsefulHRainfall_R25_F <- data.table(HourlySum_R25_F)

UsefulHRainfall_Bani <- data.table(HourlySum_Bani)
UsefulHRainfall_Aero <- data.table(HourlySum_Aero)
UsefulHRainfall_Orst <- data.table(HourlySum_Orst)

Headers = c("Hour","Day", "Month", "Year",  "Precipitation_Sum")
names(UsefulHRainfall_CP4_C) = Headers
names(UsefulHRainfall_CP4_F) = Headers
names(UsefulHRainfall_R25_C) = Headers
names(UsefulHRainfall_R25_F) = Headers

names(UsefulHRainfall_Bani) = Headers
names(UsefulHRainfall_Aero) = Headers
names(UsefulHRainfall_Orst) = Headers

Ru = 1

UsefulHRainfall_CP4_C[UsefulHRainfall_CP4_C$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulHRainfall_CP4_F[UsefulHRainfall_CP4_F$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulHRainfall_R25_C[UsefulHRainfall_R25_C$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulHRainfall_R25_F[UsefulHRainfall_R25_F$Precipitation_Sum < Ru]$Precipitation_Sum = 0

UsefulHRainfall_Bani[UsefulHRainfall_Bani$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulHRainfall_Aero[UsefulHRainfall_Aero$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulHRainfall_Orst[UsefulHRainfall_Orst$Precipitation_Sum < Ru]$Precipitation_Sum = 0

UsefulHRainfall_Aero[] <- lapply(UsefulHRainfall_Aero,as.numeric)
UsefulHRainfall_Bani[] <- lapply(UsefulHRainfall_Bani,as.numeric)
UsefulHRainfall_Orst[] <- lapply(UsefulHRainfall_Orst,as.numeric)

WetEvent <- function(d2){
  d1 <- data.frame()
  i=0
  for (row in 1:nrow(d2)) {
    precip_current <- d2[row,5]
    hour_current <- d2[row,1]
    day_current <- d2[row,2]
    month_current <- d2[row,3]
    year_current <- d2[row,4]
    if(precip_current > 0){
      i = i + 1
      d1[1,i] <-hour_current
      d1[2,i] <-day_current
      d1[3,i] <-month_current
      d1[4,i] <-year_current
      d1[5:41,i] <- d2[(row-12):(row+24),5]
    }
  }
  d1 <- t(d1)
  rownames(d1) <- 1:nrow(d1)
  colnames(d1) <- c("Hour", "Day", "Month", "Year", "-12","-11","-10","-9","-8","-7","-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")
  d1 <-data.table(d1)
  d1 <- d1[d1$"-1" < 1,]
  d1 <- sapply(d1, as.numeric)
  return(d1)
}

CompRainfall_CP4_C <- WetEvent(UsefulHRainfall_CP4_C)
CompRainfall_CP4_F <- WetEvent(UsefulHRainfall_CP4_F)
CompRainfall_R25_C <- WetEvent(UsefulHRainfall_R25_C)
CompRainfall_R25_F <- WetEvent(UsefulHRainfall_R25_F)

CompRainfall_Aero <- WetEvent(UsefulHRainfall_Aero)
CompRainfall_Bani <- WetEvent(UsefulHRainfall_Bani)
CompRainfall_Orst <- WetEvent(UsefulHRainfall_Orst)


CompositeMeans <- function(d1){
dx <- colMeans(d1, na.rm = TRUE)
dy <- sapply(d1,sd, na.rm = TRUE)/sqrt(nrow(d1))
dx <- data.table(dx,dy)
dx <- dx[5:41,]
return(dx)
}

library(data.table)

CompRMeans_CP4_C <- CompositeMeans(data.table(CompRainfall_CP4_C))
colnames(CompRMeans_CP4_C) <- c("CP4_C_Mean", "CP4_C_SE")
CompRMeans_CP4_F <- CompositeMeans(data.table(CompRainfall_CP4_F))
colnames(CompRMeans_CP4_F) <- c("CP4_F_Mean", "CP4_F_SE")
CompRMeans_R25_C <- CompositeMeans(data.table(CompRainfall_R25_C))
colnames(CompRMeans_R25_C) <- c("R25_C_Mean", "R25_C_SE")
CompRMeans_R25_F <- CompositeMeans(data.table(CompRainfall_R25_F))
colnames(CompRMeans_R25_F) <- c("R25_F_Mean", "R25_F_SE")

CompRMeans_Aero <- CompositeMeans(data.table(CompRainfall_Aero))
colnames(CompRMeans_Aero) <- c("Aero_Mean", "Aero_SE")
CompRMeans_Bani <- CompositeMeans(data.table(CompRainfall_Bani))
colnames(CompRMeans_Bani) <- c("Bani_Mean", "Bani_SE")
CompRMeans_Orst <- CompositeMeans(data.table(CompRainfall_Orst))
colnames(CompRMeans_Orst) <- c("Orst_Mean", "Orst_SE")

Hour <- c(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
CompRMeansAll <- cbind(Hour,CompRMeans_CP4_C,CompRMeans_CP4_F,CompRMeans_R25_C,CompRMeans_R25_F,CompRMeans_Aero,CompRMeans_Bani,CompRMeans_Banizoumbou_2005, CompRMeans_Orst)

R <- ggplot(data = CompRMeansAll)+
  geom_line(aes(x=Hour, y=CP4_C_Mean, color = "CP4_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=CP4_F_Mean, color = "CP4_F"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_C_Mean, color = "R25_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_F_Mean, color = "R25_F"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=Aero_Mean, color = "Niamey Airport"),linewidth = 1, group=1) +
  #geom_line(aes(x=Hour, y=Bani_Mean, color = "Banizoumbou"),linewidth = 1, group=1, linetype="dashed") +
  #geom_line(aes(x=Hour, y=Orst_Mean, color = "Niamey ORSTOM"),linewidth = 1, group=1, linetype="dashed") +
  geom_errorbar(aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "CP4_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "CP4_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "R25_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "R25_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=Aero_Mean-Aero_SE, ymax = Aero_Mean+Aero_SE, color = "Niamey Airport"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(aes(x=Hour, ymin=Bani_Mean-Bani_SE, ymax = Bani_Mean+Bani_SE, color = "Banizoumbou"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(aes(x=Hour, ymin=Orst_Mean-Orst_SE, ymax = Orst_Mean+Orst_SE, color = "Niamey ORSTOM"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("Mean useful rainfall around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("Precipitation (mm)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model")+
  scale_color_manual(values = c(AM_colors[1],AM_colors[2], "Orange",AM_colors[3],AM_colors[4]))



