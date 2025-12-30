CP4_C = read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_CP4_regridded_C_precip.txt", header = FALSE)
CP4_F = read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_CP4_regridded_F_precip.txt", header = FALSE)
R25_C = read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_R25_C_precip.txt", header = FALSE)
R25_F = read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/data_R25_F_precip.txt", header = FALSE)

Temp_Headers = c("Time", "Precipitation")

AM_colors <- RColorBrewer::brewer.pal(4, "Paired")[1:4]
CP_colors <- RColorBrewer::brewer.pal(4, "Paired")[1:2]
R_colors <- RColorBrewer::brewer.pal(4, "Paired")[3:4]
C_colors <- c(RColorBrewer::brewer.pal(4, "Paired")[1], RColorBrewer::brewer.pal(4, "Paired")[3])
F_colors <- c(RColorBrewer::brewer.pal(4, "Paired")[2], RColorBrewer::brewer.pal(4, "Paired")[4])

names(CP4_C) = Temp_Headers
names(CP4_F) = Temp_Headers
names(R25_C) = Temp_Headers
names(R25_F) = Temp_Headers


Add_months_years <- function(data_table){
data_table["Hours"] = data_table["Time"] - 233280
data_table["Days"] = data_table["Hours"]/24
data_table["Months"] = data_table["Days"]/30
data_table["Years"] = data_table["Months"]/12

data_table["Month"] = ifelse(data_table["Months"]%%12 < 1, "Jan", 
                        ifelse(data_table["Months"]%%12 < 2, "Feb", 
                               ifelse(data_table["Months"]%%12 < 3, "Mar", 
                                      ifelse(data_table["Months"]%%12 < 4, "Apr", 
                                             ifelse(data_table["Months"]%%12 < 5, "May", 
                                                    ifelse(data_table["Months"]%%12 < 6, "Jun", 
                                                           ifelse(data_table["Months"]%%12 < 7, "Jul", 
                                                                  ifelse(data_table["Months"]%%12 < 8, "Aug", 
                                                                         ifelse(data_table["Months"]%%12 < 9, "Sep",
                                                                                ifelse(data_table["Months"]%%12 < 10, "Oct",
                                                                                       ifelse(data_table["Months"]%%12 < 11, "Nov", "Dec")))))))))))
data_table["Year"] = ifelse(data_table["Years"]%%10 < 1, 1, 
                             ifelse(data_table["Years"]%%10 < 2, 2, 
                                    ifelse(data_table["Years"]%%10 < 3, 3, 
                                           ifelse(data_table["Years"]%%10 < 4, 4, 
                                                  ifelse(data_table["Years"]%%10 < 5, 5, 
                                                         ifelse(data_table["Years"]%%10 < 6, 6, 
                                                                ifelse(data_table["Years"]%%10 < 7, 7, 
                                                                       ifelse(data_table["Years"]%%10 < 8, 8, 
                                                                              ifelse(data_table["Years"]%%10 < 9, 9, 10)))))))))
data_table["Day"] = ifelse(data_table["Days"]%%30 < 1, 1, 
                            ifelse(data_table["Days"]%%30 < 2, 2, 
                                   ifelse(data_table["Days"]%%30 < 3, 3, 
                                          ifelse(data_table["Days"]%%30 < 4, 4, 
                                                 ifelse(data_table["Days"]%%30 < 5, 5, 
                                                        ifelse(data_table["Days"]%%30 < 6, 6, 
                                                               ifelse(data_table["Days"]%%30 < 7, 7, 
                                                                      ifelse(data_table["Days"]%%30 < 8, 8,
                                                                             ifelse(data_table["Days"]%%30 < 9, 9,
                                                                                    ifelse(data_table["Days"]%%30 < 10, 10,
                                                                                           ifelse(data_table["Days"]%%30 < 11, 11,
                                                                                                  ifelse(data_table["Days"]%%30 < 12, 12,
                                                                                                         ifelse(data_table["Days"]%%30 < 13, 13,
                                                                                                                ifelse(data_table["Days"]%%30 < 14, 14,
                                                                                                                       ifelse(data_table["Days"]%%30 < 15, 15,
                                                                                                                              ifelse(data_table["Days"]%%30 < 16, 16,
                                                                                                                                     ifelse(data_table["Days"]%%30 < 17, 17,
                                                                                                                                            ifelse(data_table["Days"]%%30 < 18, 18,
                                                                                                                                                   ifelse(data_table["Days"]%%30 < 19, 19,
                                                                                                                                                          ifelse(data_table["Days"]%%30 < 20, 20,
                                                                                                                                                                 ifelse(data_table["Days"]%%30 < 21, 21,
                                                                                                                                                                        ifelse(data_table["Days"]%%30 < 22, 22,
                                                                                                                                                                               ifelse(data_table["Days"]%%30 < 23, 23,
                                                                                                                                                                                      ifelse(data_table["Days"]%%30 < 24, 24,
                                                                                                                                                                                             ifelse(data_table["Days"]%%30 < 25, 25,
                                                                                                                                                                                                    ifelse(data_table["Days"]%%30 < 26, 26,
                                                                                                                                                                                                           ifelse(data_table["Days"]%%30 < 27, 27,
                                                                                                                                                                                                                  ifelse(data_table["Days"]%%30 < 28, 28,
                                                                                                                                                                                                                         ifelse(data_table["Days"]%%30 < 29, 29, 30)))))))))))))))))))))))))))))
#data_table["Hour"] = ifelse(data_table["Hours"]%%24 < 1, 1, 
#                           ifelse(data_table["Hours"]%%24 < 2, 2, 
#                                 ifelse(data_table["Hours"]%%24 < 3, 3, 
#                                         ifelse(data_table["Hours"]%%24 < 4, 4, 
#                                                ifelse(data_table["Hours"]%%24 < 5, 5, 
#                                                       ifelse(data_table["Hours"]%%24 < 6, 6, 
#                                                             ifelse(data_table["Hours"]%%24 < 7, 7, 
#                                                                     ifelse(data_table["Hours"]%%24 < 8, 8,
#                                                                            ifelse(data_table["Hours"]%%24 < 9, 9,
#                                                                                   ifelse(data_table["Hours"]%%24 < 10, 10,
#                                                                                          ifelse(data_table["Hours"]%%24 < 11, 11,
#                                                                                                 ifelse(data_table["Hours"]%%24 < 12, 12,
#                                                                                                        ifelse(data_table["Hours"]%%24 < 13, 13,
#                                                                                                               ifelse(data_table["Hours"]%%24 < 14, 14,
#                                                                                                                      ifelse(data_table["Hours"]%%24 < 15, 15,
#                                                                                                                             ifelse(data_table["Hours"]%%24 < 16, 16,
#                                                                                                                                    ifelse(data_table["Hours"]%%24 < 17, 17,
#                                                                                                                                           ifelse(data_table["Hours"]%%24 < 18, 18,
#                                                                                                                                                  ifelse(data_table["Hours"]%%24 < 19, 19,
#                                                                                                                                                         ifelse(data_table["Hours"]%%24 < 20, 20,
#                                                                                                                                                                ifelse(data_table["Hours"]%%24 < 21, 21,
#                                                                                                                                                                       ifelse(data_table["Hours"]%%24 < 22, 22,
#                                                                                                                                                                              ifelse(data_table["Hours"]%%24 < 23, 23,24)))))))))))))))))))))))
return(data_table)
}

CP4_C <- Add_months_years(CP4_C)
CP4_F <- Add_months_years(CP4_F)
R25_C <- Add_months_years(R25_C)
R25_F <- Add_months_years(R25_F)

Yearly_Rainfall <- aggregate(Precipitation ~ Year + Month, CP4_C, mean)

#install.packages("plotrix")
library(plotrix)

Monthly_sum <- function(data_table){
  return(aggregate(Precipitation ~ Month, data_table, FUN = function(x) c(sum = sum(x), se = sqrt(var(x)))))
}


#Reorder_dates <- function(data_table){
#data_table$Month <- ordered(data_table$Month, levels = c( 
#                              "Jan", "Feb", "Mar", 
#                              "Apr", "May", "Jun", 
#                              "Jul", "Aug", "Sep", 
#                              "Oct", "Nov", "Dec"))
#return(data_table %>% arrange(Month))
#}

library(data.table)

MonthMean_CP4_C <- Reorder_dates(Monthly_sum(CP4_C))
MonthMean_CP4_F <- Reorder_dates(Monthly_sum(CP4_F))
MonthMean_R25_C <- Reorder_dates(Monthly_sum(R25_C))
MonthMean_R25_F <- Reorder_dates(Monthly_sum(R25_F))

MonthMean_CP4_C <- data.table(MonthMean_CP4_C)
MonthMean_CP4_F <- data.table(MonthMean_CP4_F)
MonthMean_R25_C <- data.table(MonthMean_R25_C)
MonthMean_R25_F <- data.table(MonthMean_R25_F) 

MonthMean_CP4_C$Precipitation.sum = MonthMean_CP4_C$Precipitation.sum/10
MonthMean_CP4_F$Precipitation.sum = MonthMean_CP4_F$Precipitation.sum/10
MonthMean_R25_C$Precipitation.sum = MonthMean_R25_C$Precipitation.sum/10
MonthMean_R25_F$Precipitation.sum = MonthMean_R25_F$Precipitation.sum/10

MonthMean_CP4_C$Precipitation.se = MonthMean_CP4_C$Precipitation.se*24*30/sqrt(10*24*30)
MonthMean_CP4_F$Precipitation.se = MonthMean_CP4_F$Precipitation.se*24*30/sqrt(10*24*30)
MonthMean_R25_C$Precipitation.se = MonthMean_R25_C$Precipitation.se*24*30/sqrt(10*24*30)
MonthMean_R25_F$Precipitation.se = MonthMean_R25_F$Precipitation.se*24*30/sqrt(10*24*30)

Headers = c("Month", "Precipitation_Mean", "Precipitation_SE")
names(MonthMean_CP4_C) = Headers
names(MonthMean_CP4_F) = Headers
names(MonthMean_R25_C) = Headers
names(MonthMean_R25_F) = Headers

MonthMean_CP4_C$Model = "CP4_C"
MonthMean_CP4_F$Model = "CP4_F"
MonthMean_R25_C$Model = "R25_C"
MonthMean_R25_F$Model = "R25_F"

CP4_Monthly <- rbind(MonthMean_CP4_C, MonthMean_CP4_F)
R25_Monthly <- rbind(MonthMean_R25_C, MonthMean_R25_F)
Current_Monthly <- rbind(MonthMean_CP4_C, MonthMean_R25_C)
Future_Monthly <- rbind(MonthMean_CP4_F, MonthMean_R25_F)
  
Overall_Monthly <- rbind(MonthMean_CP4_C, MonthMean_CP4_F, MonthMean_R25_C, MonthMean_R25_F)

library("tidyverse")




ggplot(CP4_Monthly, aes(x=Month, y=Precipitation_Mean, fill=Model)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE), width=0.6, colour="orange", alpha=0.9, size=1.3, position=position_dodge(.9))+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for CP4 current and future") +
  xlab("Month") +
  ylab("Precipitation (mm)")+
  scale_fill_manual(values = CP_colors)+
  theme(text = element_text(size = 15)) 

ggplot(CP4_Monthly, aes(x=Month, y=Precipitation_Mean, group = Model, color = Model)) + 
  geom_line(size = 1.5) +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE, color = Model), width=0.3, alpha=0.9, size=1.3)+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for each CP4 model run") +
  xlab("Month") +
  ylab("Precipitation (mm)")+
  scale_color_manual(values = CP_colors)+
  theme(text = element_text(size = 15)) 

ggplot(R25_Monthly, aes(x=Month, y=Precipitation_Mean, fill=Model)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE), width=0.6, colour="orange", alpha=0.9, size=1.3, position=position_dodge(.9))+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for R25 current and future") +
  xlab("Month") +
  ylab("Precipitation (mm)")+
  scale_fill_manual(values = R_colors)+
  theme(text = element_text(size = 15)) 

ggplot(R25_Monthly, aes(x=Month, y=Precipitation_Mean, group = Model, color = Model)) + 
  geom_line(size = 1.5) +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE, color = Model), width=0.3, alpha=0.9, size=1.3)+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for each R25 model run") +
  xlab("Month") +
  ylab("Precipitation (mm)")+
  scale_color_manual(values = R_colors)+
  theme(text = element_text(size = 15)) 

ggplot(Current_Monthly, aes(x=Month, y=Precipitation_Mean, fill=Model)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE), width=0.6, colour="orange", alpha=0.9, size=1.3, position=position_dodge(.9))+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for both current models") +
  xlab("Month") +
  ylab("Precipitation (mm)")+
  scale_fill_manual(values = C_colors)+
  theme(text = element_text(size = 15)) 

ggplot(Current_Monthly, aes(x=Month, y=Precipitation_Mean, group = Model, color = Model)) + 
  geom_line(size = 1.5) +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE, color = Model), width=0.3, alpha=0.9, size=1.3)+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for each current model") +
  xlab("Month") +
  ylab("Precipitation (mm)")+
  scale_color_manual(values = C_colors)+
  theme(text = element_text(size = 15)) 

ggplot(Future_Monthly, aes(x=Month, y=Precipitation_Mean, fill=Model)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE), width=0.6, colour="orange", alpha=0.9, size=1.3, position=position_dodge(.9))+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for both future models") +
  xlab("Month") +
  ylab("Precipitation (mm)")+
  scale_fill_manual(values = F_colors)+
  theme(text = element_text(size = 15)) 

ggplot(Future_Monthly, aes(x=Month, y=Precipitation_Mean, group = Model, color = Model)) + 
  geom_line(size = 1.5) +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE, color = Model), width=0.3, alpha=0.9, size=1.3)+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for each future model") +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  scale_color_manual(values = F_colors)+
  theme(text = element_text(size = 15)) 

ggplot(Overall_Monthly, aes(x=Month, y=Precipitation_Mean, fill=Model)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE, colour = Model), width=0.6, colour="orange", alpha=0.9, size=1.3, position=position_dodge(.9))+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for each model") +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  scale_fill_manual(values = AM_colors)+
  theme(text = element_text(size = 15)) 

ggplot(Overall_Monthly, aes(x=Month, y=Precipitation_Mean, group = Model, color = Model)) + 
  geom_line(size = 1.5) +
  geom_errorbar( aes(x=Month, ymin=Precipitation_Mean-Precipitation_SE, ymax = Precipitation_Mean+Precipitation_SE, color = Model), width=0.3, alpha=0.9, size=1.3)+
  ggtitle("Mean monthly precipitation in Niamey over the ten year period for each model") +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  scale_color_manual(values = AM_colors)+
  theme(text = element_text(size = 15)) 
