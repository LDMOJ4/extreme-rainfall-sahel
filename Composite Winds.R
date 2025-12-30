CP4_C_VWind <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_CP4_C_c03226.txt", header = FALSE)
CP4_F_VWind <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_CP4_F_c03226.txt", header = FALSE)
R25_C_VWind <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_R25_C_c03226.txt", header = FALSE)
R25_F_VWind <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_R25_F_c03226_NIM.txt", header = FALSE)

VWind_Headers = c("Time", "VWind")

names(CP4_C_VWind) = VWind_Headers
names(CP4_F_VWind) = VWind_Headers
names(R25_C_VWind) = VWind_Headers
names(R25_F_VWind) = VWind_Headers

CP4_C_VWind <- Add_months_years(CP4_C_VWind)
CP4_F_VWind <- Add_months_years(CP4_F_VWind)
R25_C_VWind <- Add_months_years(R25_C_VWind)
R25_F_VWind <- Add_months_years(R25_F_VWind)

VWindComposite <- function(d1,d2){
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

CompVWind_CP4_C <- VWindComposite(CompRainfall_CP4_C,CP4_C_VWind)
CompVWind_CP4_F <- VWindComposite(CompRainfall_CP4_F,CP4_F_VWind)
CompVWind_R25_C <- VWindComposite(CompRainfall_R25_C,R25_C_VWind)
CompVWind_R25_F <- VWindComposite(CompRainfall_R25_F,R25_F_VWind)

CompVWMeans_CP4_C <- CompositeMeans(CompVWind_CP4_C)
colnames(CompVWMeans_CP4_C) <- c("CP4_C_Mean", "CP4_C_SE")
CompVWMeans_CP4_F <- CompositeMeans(CompVWind_CP4_F)
colnames(CompVWMeans_CP4_F) <- c("CP4_F_Mean", "CP4_F_SE")
CompVWMeans_R25_C <- CompositeMeans(CompVWind_R25_C)
colnames(CompVWMeans_R25_C) <- c("R25_C_Mean", "R25_C_SE")
CompVWMeans_R25_F <- CompositeMeans(CompVWind_R25_F)
colnames(CompVWMeans_R25_F) <- c("R25_F_Mean", "R25_F_SE")


Hour <- c(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
CompVWMeansAll <- cbind(Hour,CompVWMeans_CP4_C,CompVWMeans_CP4_F,CompVWMeans_R25_C,CompVWMeans_R25_F)

CP4_C_UWind <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_CP4_C_c03225.txt", header = FALSE)
CP4_F_UWind <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_CP4_F_c03225.txt", header = FALSE)
R25_C_UWind <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_R25_C_c03225.txt", header = FALSE)
R25_F_UWind <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_R25_F_c03225_NIM.txt", header = FALSE)

UWind_Headers = c("Time", "UWind")

names(CP4_C_UWind) = UWind_Headers
names(CP4_F_UWind) = UWind_Headers
names(R25_C_UWind) = UWind_Headers
names(R25_F_UWind) = UWind_Headers

CP4_C_UWind <- Add_months_years(CP4_C_UWind)
CP4_F_UWind <- Add_months_years(CP4_F_UWind)
R25_C_UWind <- Add_months_years(R25_C_UWind)
R25_F_UWind <- Add_months_years(R25_F_UWind)

UWindComposite <- function(d1,d2){
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

CompUWind_CP4_C <- UWindComposite(CompRainfall_CP4_C,CP4_C_UWind)
CompUWind_CP4_F <- UWindComposite(CompRainfall_CP4_F,CP4_F_UWind)
CompUWind_R25_C <- UWindComposite(CompRainfall_R25_C,R25_C_UWind)
CompUWind_R25_F <- UWindComposite(CompRainfall_R25_F,R25_F_UWind)

Modulus <- function(U,V){
  M <- U
  for(i in length(t(U[,1]))){
    for(j in (length(U[1,])-5)){
      M[i,j+5] <- sqrt((U[i,j+5])^2 + (V[i,j+5])^2)
    }
  }
  return(M)
}

# CompUWind_CP4_C

CompWind_CP4_C <- Modulus(CompUWind_CP4_C,CompVWind_CP4_C)
CompWind_CP4_F <- Modulus(CompUWind_CP4_F,CompVWind_CP4_F)
CompWind_R25_C <- Modulus(CompUWind_R25_C,CompVWind_R25_C)
CompWind_R25_F <- Modulus(CompUWind_R25_F,CompVWind_R25_F) 

CompUWMeans_CP4_C <- CompositeMeans(CompUWind_CP4_C)
colnames(CompUWMeans_CP4_C) <- c("CP4_C_Mean", "CP4_C_SE")
CompUWMeans_CP4_F <- CompositeMeans(CompUWind_CP4_F)
colnames(CompUWMeans_CP4_F) <- c("CP4_F_Mean", "CP4_F_SE")
CompUWMeans_R25_C <- CompositeMeans(CompUWind_R25_C)
colnames(CompUWMeans_R25_C) <- c("R25_C_Mean", "R25_C_SE")
CompUWMeans_R25_F <- CompositeMeans(CompUWind_R25_F)
colnames(CompUWMeans_R25_F) <- c("R25_F_Mean", "R25_F_SE")

CompWindMeans_CP4_C <- CompositeMeans(CompWind_CP4_C)
colnames(CompWindMeans_CP4_C) <- c("CP4_C_Mean", "CP4_C_SE")
CompWindMeans_CP4_F <- CompositeMeans(CompWind_CP4_F)
colnames(CompWindMeans_CP4_F) <- c("CP4_F_Mean", "CP4_F_SE")
CompWindMeans_R25_C <- CompositeMeans(CompWind_R25_C)
colnames(CompWindMeans_R25_C) <- c("R25_C_Mean", "R25_C_SE")
CompWindMeans_R25_F <- CompositeMeans(CompWind_R25_F)
colnames(CompWindMeans_R25_F) <- c("R25_F_Mean", "R25_F_SE")

Hour <- c(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
CompWindMeansAll <- cbind(Hour,CompWindMeans_CP4_C,CompWindMeans_CP4_F,CompWindMeans_R25_C,CompWindMeans_R25_F, CompWindMeans_Banizoumbou_2005)


Hour <- c(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
CompUWMeansAll <- cbind(Hour,CompUWMeans_CP4_C,CompUWMeans_CP4_F,CompUWMeans_R25_C,CompUWMeans_R25_F)

ggplot(data = CompVWMeansAll)+
  geom_line(aes(x=Hour, y=CP4_C_Mean, color = "CP4_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=CP4_F_Mean, color = "CP4_F"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_C_Mean, color = "R25_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_F_Mean, color = "R25_F"),linewidth = 1, group=1) +
  geom_errorbar(aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "CP4_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "CP4_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "R25_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "R25_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("Mean VWind around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("VWind (m/s)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model") +
  scale_color_manual(values = AM_colors)#

ggplot(data = CompUWMeansAll)+
  geom_line(aes(x=Hour, y=CP4_C_Mean, color = "CP4_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=CP4_F_Mean, color = "CP4_F"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_C_Mean, color = "R25_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_F_Mean, color = "R25_F"),linewidth = 1, group=1) +
  geom_errorbar(aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "CP4_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "CP4_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "R25_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "R25_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("Mean UWind around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("UWind (m/s)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model") +
  scale_color_manual(values = AM_colors)

ggplot(data = CompWindMeansAll)+
  geom_line(aes(x=Hour, y=CP4_C_Mean, color = "CP4_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=CP4_F_Mean, color = "CP4_F"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_C_Mean, color = "R25_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Hour, y=R25_F_Mean, color = "R25_F"),linewidth = 1, group=1) +
  geom_errorbar(aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "CP4_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "CP4_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "R25_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "R25_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("Mean wind speed around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("Wind speed (m/s)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model") +
  scale_color_manual(values = AM_colors)

ggplot()+
  #geom_line(data = CompUWMeansAll,aes(x=Hour, y=CP4_C_Mean, color = "CP4_C U Winds"),linewidth = 1, group=1) +
  geom_line(data = CompUWMeansAll,aes(x=Hour, y=CP4_F_Mean, color = "CP4_F U Winds"),linewidth = 1, group=1) +
  #geom_line(data = CompVWMeansAll,aes(x=Hour, y=CP4_C_Mean, color = "CP4_C V Winds"),linewidth = 1, group=1) +
  geom_line(data = CompVWMeansAll,aes(x=Hour, y=CP4_F_Mean, color = "CP4_F V Winds"),linewidth = 1, group=1) +
  #geom_errorbar(data = CompUWMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "CP4_C U Winds"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompUWMeansAll,aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "CP4_F U Winds"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompVWMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "CP4_C V Winds"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompVWMeansAll,aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "CP4_F V Winds"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("Mean UWind around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("UWind (m/s)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model") +
  scale_color_manual(values = AM_colors)

ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=CP4_C_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  #geom_line(data = CompUWMeansAll,aes(x=Hour, y=CP4_C_Mean, color = "U Wind Speed"),linewidth = 1, group=1) +
  #geom_line(data = CompVWMeansAll,aes(x=Hour, y=CP4_C_Mean, color = "V Wind Speed"),linewidth = 1, group=1) +
  geom_line(data = CompTMeansAll, aes(x=Hour, y=(CP4_C_Mean-300)/1.5, color = "Temperature"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5+27, name="Temperature (°C)")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompUWMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "U Wind Speed"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompVWMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "V Wind Speed"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTMeansAll,aes(x=Hour, ymin=(CP4_C_Mean-CP4_C_SE-300)/1.5, ymax = (CP4_C_Mean+CP4_C_SE-300)/1.5, color = "Temperature"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("Mean temperature and rainfall around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("Precipitation (mm) & Wind Speed (m/s)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model")

CC <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=CP4_C_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  geom_line(data = CompWindMeansAll,aes(x=Hour, y=CP4_C_Mean, color = "Wind Speed"),linewidth = 1, group=1) +
  geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=CP4_C_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompWindMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "Wind Speed"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red", "Green"))

CF <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=CP4_F_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  geom_line(data = CompWindMeansAll,aes(x=Hour, y=CP4_F_Mean, color = "Wind Speed"),linewidth = 1, group=1) +
  geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=CP4_F_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompWindMeansAll,aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "Wind Speed"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red", "Green"))

RC <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=R25_C_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  geom_line(data = CompWindMeansAll,aes(x=Hour, y=R25_C_Mean, color = "Wind Speed"),linewidth = 1, group=1) +
  geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=R25_C_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompWindMeansAll,aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "Wind Speed"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red", "Green"))

RF <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=R25_F_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  geom_line(data = CompWindMeansAll,aes(x=Hour, y=R25_F_Mean, color = "Wind Speed"),linewidth = 1, group=1) +
  geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=R25_F_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompWindMeansAll,aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "Wind Speed"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red", "Green"))

B <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=Banizoumbou_2005_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  geom_line(data = CompWindMeansAll,aes(x=Hour, y=Banizoumbou_2005_Mean, color = "Wind Speed"),linewidth = 1, group=1) +
  geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=Banizoumbou_2005_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=Banizoumbou_2005_Mean-Banizoumbou_2005_SE, ymax = Banizoumbou_2005_Mean+Banizoumbou_2005_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompWindMeansAll,aes(x=Hour, ymin=Banizoumbou_2005_Mean-Banizoumbou_2005_SE, ymax = Banizoumbou_2005_Mean+Banizoumbou_2005_SE, color = "Wind Speed"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=Banizoumbou_2005_Mean-Banizoumbou_2005_SE, ymax = Banizoumbou_2005_Mean+Banizoumbou_2005_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red", "Green"))

grid.arrange(CC, RC,  CF, RF, ncol=2, nrow=2)

CCC <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=CP4_C_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  # geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=CP4_C_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  geom_line(data = CompTMeansAll, aes(x=Hour, y=(CP4_C_Mean-300)/1.5, color = "Temperature"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5+26, name="Temperature (°C)")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTMeansAll,aes(x=Hour, ymin=(CP4_C_Mean-CP4_C_SE-300)/1.5, ymax = (CP4_C_Mean+CP4_C_SE-300)/1.5, color = "Temperature"), width=0.3, alpha=0.9, linewidth=0.8)+
  #ggtitle("Mean temperature  and precitation amount around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("Precipitation (mm)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Parameter")+
  scale_color_manual(values = c("Blue", "Red"))

CCF <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=CP4_F_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  # geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=CP4_F_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  geom_line(data = CompTMeansAll, aes(x=Hour, y=(CP4_F_Mean-300)/1.5, color = "Temperature"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5+27, name="Temperature (°C)")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTMeansAll,aes(x=Hour, ymin=(CP4_F_Mean-CP4_F_SE-300)/1.5, ymax = (CP4_F_Mean+CP4_F_SE-300)/1.5, color = "Temperature"), width=0.3, alpha=0.9, linewidth=0.8)+
  #ggtitle("Mean temperature anomaly and precitation amount around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("Precipitation (mm)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red"))

CRC <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=R25_C_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  # geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=R25_C_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  geom_line(data = CompTMeansAll, aes(x=Hour, y=(R25_C_Mean-300)/1.5, color = "Temperature"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5+27, name="Temperature (°C)")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTMeansAll,aes(x=Hour, ymin=(R25_C_Mean-R25_C_SE-300)/1.5, ymax = (R25_C_Mean+R25_C_SE-300)/1.5, color = "Temperature"), width=0.3, alpha=0.9, linewidth=0.8)+
  #ggtitle("Mean temperature anomaly and precitation amount around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("Precipitation (mm)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red"))

CRF <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=R25_F_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  # geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=R25_F_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  geom_line(data = CompTMeansAll, aes(x=Hour, y=(R25_F_Mean-300)/1.5, color = "Temperature"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5+27, name="Temperature (°C)")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTMeansAll,aes(x=Hour, ymin=(R25_F_Mean-R25_F_SE-300)/1.5, ymax = (R25_F_Mean+R25_F_SE-300)/1.5, color = "Temperature"), width=0.3, alpha=0.9, linewidth=0.8)+
  #ggtitle("Mean temperature anomaly and precitation amount around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("Precipitation (mm)")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red"))

CACC <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=CP4_C_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=CP4_C_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  #geom_line(data = CompTMeansAll, aes(x=Hour, y=(CP4_C_Mean-300)/1.5, color = "Temperature"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=CP4_C_Mean-CP4_C_SE, ymax = CP4_C_Mean+CP4_C_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompTMeansAll,aes(x=Hour, ymin=(CP4_C_Mean-CP4_C_SE-300)/1.5, ymax = (CP4_C_Mean+CP4_C_SE-300)/1.5, color = "Temperature"), width=0.3, alpha=0.9, linewidth=0.8)+
  #ggtitle("Mean temperature  and precitation amount around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red"))

CACF <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=CP4_F_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=CP4_F_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  #geom_line(data = CompTMeansAll, aes(x=Hour, y=(CP4_F_Mean-300)/1.5, color = "Temperature"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=CP4_F_Mean-CP4_F_SE, ymax = CP4_F_Mean+CP4_F_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompTMeansAll,aes(x=Hour, ymin=(CP4_F_Mean-CP4_F_SE-300)/1.5, ymax = (CP4_F_Mean+CP4_F_SE-300)/1.5, color = "Temperature"), width=0.3, alpha=0.9, linewidth=0.8)+
  #ggtitle("Mean temperature anomaly and precitation amount around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red"))

CARC <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=R25_C_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=R25_C_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  #geom_line(data = CompTMeansAll, aes(x=Hour, y=(R25_C_Mean-300)/1.5, color = "Temperature"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=R25_C_Mean-R25_C_SE, ymax = R25_C_Mean+R25_C_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompTMeansAll,aes(x=Hour, ymin=(R25_C_Mean-R25_C_SE-300)/1.5, ymax = (R25_C_Mean+R25_C_SE-300)/1.5, color = "Temperature"), width=0.3, alpha=0.9, linewidth=0.8)+
  #ggtitle("Mean temperature anomaly and precitation amount around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red"))

CARF <- ggplot()+
  geom_line(data = CompRMeansAll, aes(x=Hour, y=R25_F_Mean, color = "Precipitation"),linewidth = 1, group=1) +
  geom_line(data = CompTAnomMeansAll, aes(x=Hour, y=R25_F_Mean, color = "Temperature Anomaly"),linewidth = 1, group=1) +
  #geom_line(data = CompTMeansAll, aes(x=Hour, y=(R25_F_Mean-305)/1.5, color = "Temperature"),linewidth = 1, group=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="")) +
  geom_errorbar(data = CompRMeansAll,aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "Precipitation"), width=0.3, alpha=0.9, linewidth=0.8)+
  geom_errorbar(data = CompTAnomMeansAll,aes(x=Hour, ymin=R25_F_Mean-R25_F_SE, ymax = R25_F_Mean+R25_F_SE, color = "Temperature Anomaly"), width=0.3, alpha=0.9, linewidth=0.8)+
  #geom_errorbar(data = CompTMeansAll,aes(x=Hour, ymin=(R25_F_Mean-R25_F_SE-300)/1.5, ymax = (R25_F_Mean+R25_F_SE-300)/1.5, color = "Temperature"), width=0.3, alpha=0.9, linewidth=0.8)+
  #ggtitle("Mean temperature anomaly and precitation amount around composite rainfall event") +
  xlab("Time since rainfall event (Hours)") +
  ylab("")+
  theme(text = element_text(size = 15)) +
  labs(color = "Variable")+
  scale_color_manual(values = c("Blue", "Red"))

grid.arrange(CACC, CARC,  CACF, CARF, ncol=2, nrow=2)
CARF
