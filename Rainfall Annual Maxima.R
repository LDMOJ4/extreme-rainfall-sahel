AnnualRainfallMaxima <- function(d1){
  ifelse(d1$Year>1996,d1$Year <- d1$Year-1996,1)
  X = data.frame(matrix(ncol = 0, nrow = max(d1$Year)))
  for(i in 1:max(d1$Year)){
    d2 <- d1[d1$Year == i,]
    PrecipMax <- max(d2$Precipitation_Sum)
    X$Year[i] <- i
    X$Max_Precipitation[i] <- PrecipMax
  }
  X <- data.table(X)
  return(X)
}

years <- 2:100
p <- (years-1)/years
dx <- AnnualRainfallMaxima(UsefulRainfall_CP4_C)
dx1 <- fgev(dx$Max_Precipitation)$estimate
dx2 <- fgev(dx$Max_Precipitation)$std.err
dgev(1:100,loc = dx1[1], scale = dx1[2], shape = dx1[3])
plot(1:100,dgev(1:100,loc = dx1[1], scale = dx1[2], shape = dx1[3]))
qgev(c(0.25,0.5,0.75,0.8,0.9,0.95),loc = dx1[1], scale = dx1[2], shape = dx1[3])
pgev(1:100,loc = dx1[1], scale = dx1[2], shape = dx1[3])
plot(1:100,pgev(1:100,loc = dx1[1], scale = dx1[2], shape = dx1[3]))
plot(years,qgev(p,loc = dx1[1], scale = dx1[2], shape = dx1[3]))

c(qgev(0.5,loc = dx1[1], scale = dx1[2], shape = dx1[3]), qgev(0.5,loc = dx1[1]-dx2[1], scale = dx1[2]-dx2[2], shape = dx1[3]-dx2[3]), qgev(0.5,loc = dx1[1]+dx2[1], scale = dx1[2]+dx2[2], shape = dx1[3]+dx2[3]))

ggplot()+
  geom_line(aes(x=years,y=qgev(p,loc = dx1[1], scale = dx1[2], shape = dx1[3])),color="Green")+
  geom_line(aes(x=years,y=qgev(p,loc = dx1[1]+dx2[1], scale = dx1[2]+dx2[2], shape = dx1[3]+dx2[3])),color="Blue")+
  geom_line(aes(x=years,y=qgev(p,loc = dx1[1]+dx2[1], scale = dx1[2]+dx2[2], shape = dx1[3]-dx2[3])))+
  geom_line(aes(x=years,y=qgev(p,loc = dx1[1]+dx2[1], scale = dx1[2]-dx2[2], shape = dx1[3]+dx2[3])))+
  geom_line(aes(x=years,y=qgev(p,loc = dx1[1]-dx2[1], scale = dx1[2]+dx2[2], shape = dx1[3]+dx2[3])))+
  geom_line(aes(x=years,y=qgev(p,loc = dx1[1]+dx2[1], scale = dx1[2]-dx2[2], shape = dx1[3]-dx2[3])))+
  geom_line(aes(x=years,y=qgev(p,loc = dx1[1]-dx2[1], scale = dx1[2]-dx2[2], shape = dx1[3]+dx2[3])))+
  geom_line(aes(x=years,y=qgev(p,loc = dx1[1]-dx2[1], scale = dx1[2]+dx2[2], shape = dx1[3]-dx2[3])))+
  geom_line(aes(x=years,y=qgev(p,loc = dx1[1]-dx2[1], scale = dx1[2]-dx2[2], shape = dx1[3]-dx2[3])),color="Red")

EVT_Quantiles <- function(d1){
  X = data.frame(matrix(ncol = 0, nrow = 99))
  dy <- AnnualRainfallMaxima(d1)
  dy1 <- fgev(dy$Max_Precipitation)$estimate
  dy2 <- fgev(dy$Max_Precipitation)$std.err
  X$Quantile = p
  X$Return_Period = 1/(1-X$Quantile)
  X$Precipitation_Amount = qgev(p,loc = dy1[1], scale = dy1[2], shape = dy1[3])
  X$Precip_lower = qgev(p,loc = (dy1[1]-dy2[1]), scale = (dy1[2]-dy2[2]), shape = (dy1[3]-dy2[3]))
  X$Precip_upper = qgev(p,loc = (dy1[1]+dy2[1]), scale = (dy1[2]+dy2[2]), shape = (dy1[3]+dy2[3]))
  X <- data.table(X)
  return(X)
}


YearlyRainfallMaxD_CP4_C <- AnnualRainfallMaxima(UsefulRainfall_CP4_C)
YearlyRainfallMaxD_CP4_F <- AnnualRainfallMaxima(UsefulRainfall_CP4_F)
YearlyRainfallMaxD_R25_C <- AnnualRainfallMaxima(UsefulRainfall_R25_C)
YearlyRainfallMaxD_R25_F <- AnnualRainfallMaxima(UsefulRainfall_R25_F)

YearlyRainfallMaxD_Aero <- AnnualRainfallMaxima(UsefulRainfall_Aero)
YearlyRainfallMaxD_Bani <- AnnualRainfallMaxima(UsefulRainfall_Bani)
YearlyRainfallMaxD_Orst <- AnnualRainfallMaxima(UsefulRainfall_Orst)

YearlyRainfallMaxD_All <- cbind(YearlyRainfallMaxD_CP4_C,YearlyRainfallMaxD_CP4_F$Max_Precipitation,YearlyRainfallMaxD_R25_C$Max_Precipitation, YearlyRainfallMaxD_R25_F$Max_Precipitation, YearlyRainfallMaxD_Aero$Max_Precipitation, YearlyRainfallMaxD_Bani$Max_Precipitation, YearlyRainfallMaxD_Orst$Max_Precipitation)
colnames(YearlyRainfallMaxD_All) <- c("Year", "CP4_C", "CP4_F", "R25_C", "R25_F", "Aero", "Bani", "ORSTOM")

EVT_Quantiles_CP4_C <- EVT_Quantiles(UsefulRainfall_CP4_C)
EVT_Quantiles_CP4_F <- EVT_Quantiles(UsefulRainfall_CP4_F)
EVT_Quantiles_R25_C <- EVT_Quantiles(UsefulRainfall_R25_C)
EVT_Quantiles_R25_F <- EVT_Quantiles(UsefulRainfall_R25_F)

EVT_Quantiles_Aero <- EVT_Quantiles(UsefulRainfall_Aero)
EVT_Quantiles_Bani <- EVT_Quantiles(UsefulRainfall_Bani)
EVT_Quantiles_Orst <- EVT_Quantiles(UsefulRainfall_Orst)

EVT_Quantiles_All <- cbind(EVT_Quantiles_CP4_C,EVT_Quantiles_CP4_F$Precipitation_Amount,EVT_Quantiles_R25_C$Precipitation_Amount, EVT_Quantiles_R25_F$Precipitation_Amount, EVT_Quantiles_Aero$Precipitation_Amount, EVT_Quantiles_Bani$Precipitation_Amount, EVT_Quantiles_Orst$Precipitation_Amount,
                           EVT_Quantiles_CP4_F$Precip_lower,EVT_Quantiles_R25_C$Precip_lower, EVT_Quantiles_R25_F$Precip_lower, EVT_Quantiles_Aero$Precip_lower, EVT_Quantiles_Bani$Precip_lower, EVT_Quantiles_Orst$Precip_lower,
                           EVT_Quantiles_CP4_F$Precip_upper,EVT_Quantiles_R25_C$Precip_upper, EVT_Quantiles_R25_F$Precip_upper, EVT_Quantiles_Aero$Precip_upper, EVT_Quantiles_Bani$Precip_upper, EVT_Quantiles_Orst$Precip_upper)
colnames(EVT_Quantiles_All) <- c("Quantile", "Return_Period", "CP4_C", "CP4_C_lower", "CP4_C_upper", "CP4_F", "R25_C", "R25_F", "Aero", "Bani", "Orst",
                                  "CP4_F_lower", "R25_C_lower", "R25_F_lower", "Aero_lower", "Bani_lower", "Orst_lower",
                                  "CP4_F_upper", "R25_C_upper", "R25_F_upper", "Aero_upper", "Bani_upper", "Orst_upper")

ggplot(data = EVT_Quantiles_All)+
  geom_line(aes(x=Return_Period, y=CP4_C, color = "CP4_C"),linewidth = 1, group=1) +
  geom_line(aes(x=Return_Period, y=CP4_F, color = "CP4_F"),linewidth = 1, group=1) +
  geom_line(aes(x=Return_Period, y=R25_C, color = "R25_C"),linewidth = 1, group=1) +
  #geom_line(aes(x=Return_Period, y=R25_F, color = "R25_F"),linewidth = 1, group=1) +
  geom_line(aes(x=Return_Period, y=Aero, color = "Aero"),linewidth = 1, group=1) +
  geom_line(aes(x=Return_Period, y=Bani, color = "Bani"),linewidth = 1, group=1) +
  #geom_line(aes(x=Return_Period, y=Orst, color = "Orst"),linewidth = 1, group=1) +
  # geom_errorbar(aes(x=Return_Period, ymin=CP4_C_lower, ymax = CP4_C_upper, color = "CP4_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  # geom_errorbar(aes(x=Return_Period, ymin=CP4_F_lower, ymax = CP4_F_upper, color = "CP4_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  # geom_errorbar(aes(x=Return_Period, ymin=R25_C_lower, ymax = R25_C_upper, color = "R25_C"), width=0.3, alpha=0.9, linewidth=0.8)+
  # geom_errorbar(aes(x=Return_Period, ymin=R25_F_lower, ymax = R25_F_upper, color = "R25_F"), width=0.3, alpha=0.9, linewidth=0.8)+
  # geom_errorbar(aes(x=Return_Period, ymin=Aero_lower, ymax = Aero_upper, color = "Aero"), width=0.3, alpha=0.9, linewidth=0.8)+
  # geom_errorbar(aes(x=Return_Period, ymin=Bani_lower, ymax = Bani_upper, color = "Bani"), width=0.3, alpha=0.9, linewidth=0.8)+
  # geom_errorbar(aes(x=Return_Period, ymin=Orst_lower, ymax = Orst_upper, color = "Orst"), width=0.3, alpha=0.9, linewidth=0.8)+
  ggtitle("Return levels for extreme rain events from the observations/model datasets") +
  xlab("Return period years") +
  ylab("Precipitation mmday-1")+
  theme(text = element_text(size = 15)) +
  labs(color = "Observation/Model") 

#Simulation data
Markov_CP4_C <- NYearMarkovSimulation(100,UsefulRainfall_CP4_C)
Markov_CP4_F <- NYearMarkovSimulation(100,UsefulRainfall_CP4_F)
Markov_R25_C <- NYearMarkovSimulation(100,UsefulRainfall_R25_C)
Markov_R25_F <- NYearMarkovSimulation(100,UsefulRainfall_R25_F)

Markov_Aero <- NYearMarkovSimulation(100,UsefulRainfall_Aero)
Markov_Bani <- NYearMarkovSimulation(100,UsefulRainfall_Bani)
Markov_Orst <- NYearMarkovSimulation(100,UsefulRainfall_Orst)

colnames(Markov_CP4_C) <- c("Day", "Month", "Year", "Precipitation_Sum")
colnames(Markov_CP4_F) <- c("Day", "Month", "Year", "Precipitation_Sum")
colnames(Markov_R25_C) <- c("Day", "Month", "Year", "Precipitation_Sum")
colnames(Markov_R25_F) <- c("Day", "Month", "Year", "Precipitation_Sum")

colnames(Markov_Aero) <- c("Day", "Month", "Year", "Precipitation_Sum")
colnames(Markov_Bani) <- c("Day", "Month", "Year", "Precipitation_Sum")
colnames(Markov_Orst) <- c("Day", "Month", "Year", "Precipitation_Sum")

EVT_Quantiles_Markov_CP4_C <- EVT_Quantiles(Markov_CP4_C)
EVT_Quantiles_Markov_CP4_F <- EVT_Quantiles(Markov_CP4_F)
EVT_Quantiles_Markov_R25_C <- EVT_Quantiles(Markov_R25_C)
EVT_Quantiles_Markov_R25_F <- EVT_Quantiles(Markov_R25_F)

EVT_Quantiles_Markov_Aero <- EVT_Quantiles(Markov_Aero)
EVT_Quantiles_Markov_Bani <- EVT_Quantiles(Markov_Bani)
EVT_Quantiles_Markov_Orst <- EVT_Quantiles(Markov_Orst)

EVT_Quantiles_Markov_CP4_C$model <- "CP4_C"
EVT_Quantiles_Markov_CP4_F$model <- "CP4_F"
EVT_Quantiles_Markov_R25_C$model <- "R25_C"
EVT_Quantiles_Markov_R25_F$model <- "R25_F"

EVT_Quantiles_Markov_Aero$model <- "Niamey Airport"
EVT_Quantiles_Markov_Bani$model <- "Banizoumbou"
EVT_Quantiles_Markov_Orst$model <- "Niamey ORSTOM"

EVT_Quantiles_Markov_All <- rbind(EVT_Quantiles_Markov_CP4_C,EVT_Quantiles_Markov_CP4_F, EVT_Quantiles_Markov_R25_C, EVT_Quantiles_Markov_R25_F, EVT_Quantiles_Markov_Aero)#, EVT_Quantiles_Markov_Bani, EVT_Quantiles_Markov_Orst)                               "CP4_F_upper", "R25_C_upper", "R25_F_upper", "Aero_upper", "Bani_upper", "Orst_upper")


EVT_Quantiles_PercentageChange_CP4 <- EVT_Quantiles_Markov_CP4_C
EVT_Quantiles_PercentageChange_R25 <- EVT_Quantiles_Markov_R25_C
EVT_Quantiles_PercentageChange_CP4$exact = 100*(EVT_Quantiles_Markov_CP4_F$Precipitation_Amount-EVT_Quantiles_Markov_CP4_C$Precipitation_Amount)/EVT_Quantiles_Markov_CP4_C$Precipitation_Amount
EVT_Quantiles_PercentageChange_CP4$lower = 100*(EVT_Quantiles_Markov_CP4_F$Precip_lower-EVT_Quantiles_Markov_CP4_C$Precip_upper)/EVT_Quantiles_Markov_CP4_C$Precip_upper
EVT_Quantiles_PercentageChange_CP4$upper = 100*(EVT_Quantiles_Markov_CP4_F$Precip_upper-EVT_Quantiles_Markov_CP4_C$Precip_lower)/EVT_Quantiles_Markov_CP4_C$Precip_lower
EVT_Quantiles_PercentageChange_CP4$model = "CP4"
EVT_Quantiles_PercentageChange_R25$exact = 100*(EVT_Quantiles_Markov_R25_F$Precipitation_Amount-EVT_Quantiles_Markov_R25_C$Precipitation_Amount)/EVT_Quantiles_Markov_R25_C$Precipitation_Amount
EVT_Quantiles_PercentageChange_R25$lower = 100*(EVT_Quantiles_Markov_R25_F$Precip_lower-EVT_Quantiles_Markov_R25_C$Precip_upper)/EVT_Quantiles_Markov_R25_C$Precip_upper
EVT_Quantiles_PercentageChange_R25$upper = 100*(EVT_Quantiles_Markov_R25_F$Precip_upper-EVT_Quantiles_Markov_R25_C$Precip_lower)/EVT_Quantiles_Markov_R25_C$Precip_lower
EVT_Quantiles_PercentageChange_R25$model = "R25"
EVT_Quantiles_PercentageChange <- rbind(EVT_Quantiles_PercentageChange_CP4,EVT_Quantiles_PercentageChange_R25)

D1 <- ggplot(data = EVT_Quantiles_Markov_All, aes(x=Return_Period, y=Precipitation_Amount, ymin=Precip_lower, ymax = Precip_upper , fill=model, linetype = model))+
  geom_line() +
  geom_ribbon(alpha=0.5) +
  ggtitle("Return levels for extreme daily rainfall events from the observations/model datasets") +
  xlab("Return period (years)") +
  ylab("Precipitation (mm day-1)")+
  theme(text = element_text(size = 15)) +
  labs(fill = "Observation/Model", linetype= "Observation/Model")+
  scale_fill_manual(values = c(AM_colors[1],AM_colors[2],"Red", AM_colors[3],AM_colors[4]))

ggplot(data = EVT_Quantiles_Markov_All, aes(x=Quantile, y=Precipitation_Amount, ymin=Precip_lower, ymax = Precip_upper , fill=model, linetype = model))+
  geom_line() +
  geom_ribbon(alpha=0.5) +
  ggtitle("Return levels for extreme rain events from the observations/model datasets") +
  xlab("Cumulative distribution") +
  ylab("Precipitation mmday-1")+
  theme(text = element_text(size = 15)) +
  labs(fill = "Observation/Model", linetype= "Observation/Model")


D2 <- ggplot(data = EVT_Quantiles_PercentageChange, aes(x=Return_Period, y=exact, ymin=lower, ymax = upper , fill=model,linetype = model))+
  geom_line() +
  geom_ribbon(alpha=0.5) +
  ggtitle("Change in precipitation return levels from current to future climate for extreme rain events for both models") +
  xlab("Return period (years)") +
  ylab("Increase in return level (%)")+
  theme(text = element_text(size = 15)) +
  labs(fill = "Model", linetype= "Model")

#Hourly simulation data

MarkovH_CP4_C <- NYearHMarkovSimulation(100,UsefulHRainfall_CP4_C)
MarkovH_CP4_F <- NYearHMarkovSimulation(100,UsefulHRainfall_CP4_F)
MarkovH_R25_C <- NYearHMarkovSimulation(100,UsefulHRainfall_R25_C)
MarkovH_R25_F <- NYearHMarkovSimulation(100,UsefulHRainfall_R25_F)

MarkovH_Aero <- NYearHMarkovSimulation(100,UsefulHRainfall_Aero)
MarkovH_Bani <- NYearHMarkovSimulation(100,UsefulHRainfall_Bani)
MarkovH_Orst <- NYearHMarkovSimulation(100,UsefulHRainfall_Orst)

MarkovH_CP4_C <- Daily_Rainfall_sum(MarkovH_CP4_C)
MarkovH_CP4_F <- Daily_Rainfall_sum(MarkovH_CP4_F)
MarkovH_R25_C <- Daily_Rainfall_sum(MarkovH_R25_C)
MarkovH_R25_F <- Daily_Rainfall_sum(MarkovH_R25_F)

MarkovH_Aero <- Daily_Rainfall_sum(MarkovH_Aero)
MarkovH_Bani <- Daily_Rainfall_sum(MarkovH_Bani)
MarkovH_Orst <- Daily_Rainfall_sum(MarkovH_Orst)

MarkovH_CP4_C <- data.table(MarkovH_CP4_C)
MarkovH_CP4_F <- data.table(MarkovH_CP4_F)
MarkovH_R25_C <- data.table(MarkovH_R25_C)
MarkovH_R25_F <- data.table(MarkovH_R25_F)

MarkovH_Aero <- data.table(MarkovH_Aero)
MarkovH_Bani <- data.table(MarkovH_Bani)
MarkovH_Orst <- data.table(MarkovH_Orst)

colnames(MarkovH_CP4_C) <- c("Day", "Month", "Year", "Precipitation_Sum", "Precipitation_SE")
colnames(MarkovH_CP4_F) <- c("Day", "Month", "Year", "Precipitation_Sum", "Precipitation_SE")
colnames(MarkovH_R25_C) <- c("Day", "Month", "Year", "Precipitation_Sum", "Precipitation_SE")
colnames(MarkovH_R25_F) <- c("Day", "Month", "Year", "Precipitation_Sum", "Precipitation_SE")

colnames(MarkovH_Aero) <- c("Day", "Month", "Year", "Precipitation_Sum", "Precipitation_SE")
colnames(MarkovH_Bani) <- c("Day", "Month", "Year", "Precipitation_Sum", "Precipitation_SE")
colnames(MarkovH_Orst) <- c("Day", "Month", "Year", "Precipitation_Sum", "Precipitation_SE")

EVT_Quantiles_MarkovH_CP4_C <- EVT_Quantiles(MarkovH_CP4_C)
EVT_Quantiles_MarkovH_CP4_F <- EVT_Quantiles(MarkovH_CP4_F)
EVT_Quantiles_MarkovH_R25_C <- EVT_Quantiles(MarkovH_R25_C)
EVT_Quantiles_MarkovH_R25_F <- EVT_Quantiles(MarkovH_R25_F)

EVT_Quantiles_MarkovH_Aero <- EVT_Quantiles(MarkovH_Aero)
EVT_Quantiles_MarkovH_Bani <- EVT_Quantiles(MarkovH_Bani)
EVT_Quantiles_MarkovH_Orst <- EVT_Quantiles(MarkovH_Orst)

EVT_Quantiles_MarkovH_CP4_C$model <- "CP4_C"
EVT_Quantiles_MarkovH_CP4_F$model <- "CP4_F"
EVT_Quantiles_MarkovH_R25_C$model <- "R25_C"
EVT_Quantiles_MarkovH_R25_F$model <- "R25_F"

EVT_Quantiles_MarkovH_Aero$model <- "Niamey Airport"
EVT_Quantiles_MarkovH_Bani$model <- "Banizoumbou"
EVT_Quantiles_MarkovH_Orst$model <- "Niamey ORSTOM"

EVT_Quantiles_MarkovH_All <- rbind(EVT_Quantiles_MarkovH_CP4_C,EVT_Quantiles_MarkovH_CP4_F, EVT_Quantiles_MarkovH_R25_C, EVT_Quantiles_MarkovH_R25_F, EVT_Quantiles_MarkovH_Aero)#, EVT_Quantiles_MarkovH_Bani, EVT_Quantiles_MarkovH_Orst)

EVT_Quantiles_PercentageChangeH_CP4 <- EVT_Quantiles_MarkovH_CP4_C
EVT_Quantiles_PercentageChangeH_R25 <- EVT_Quantiles_MarkovH_R25_C
EVT_Quantiles_PercentageChangeH_CP4$exact = 100*(EVT_Quantiles_MarkovH_CP4_F$Precipitation_Amount-EVT_Quantiles_MarkovH_CP4_C$Precipitation_Amount)/EVT_Quantiles_MarkovH_CP4_C$Precipitation_Amount
EVT_Quantiles_PercentageChangeH_CP4$lower = 100*(EVT_Quantiles_MarkovH_CP4_F$Precip_lower-EVT_Quantiles_MarkovH_CP4_C$Precip_upper)/EVT_Quantiles_MarkovH_CP4_C$Precip_upper
EVT_Quantiles_PercentageChangeH_CP4$upper = 100*(EVT_Quantiles_MarkovH_CP4_F$Precip_upper-EVT_Quantiles_MarkovH_CP4_C$Precip_lower)/EVT_Quantiles_MarkovH_CP4_C$Precip_lower
EVT_Quantiles_PercentageChangeH_CP4$model = "CP4"
EVT_Quantiles_PercentageChangeH_R25$exact = 100*(EVT_Quantiles_MarkovH_R25_F$Precipitation_Amount-EVT_Quantiles_MarkovH_R25_C$Precipitation_Amount)/EVT_Quantiles_MarkovH_R25_C$Precipitation_Amount
EVT_Quantiles_PercentageChangeH_R25$lower = 100*(EVT_Quantiles_MarkovH_R25_F$Precip_lower-EVT_Quantiles_MarkovH_R25_C$Precip_upper)/EVT_Quantiles_MarkovH_R25_C$Precip_upper
EVT_Quantiles_PercentageChangeH_R25$upper = 100*(EVT_Quantiles_MarkovH_R25_F$Precip_upper-EVT_Quantiles_MarkovH_R25_C$Precip_lower)/EVT_Quantiles_MarkovH_R25_C$Precip_lower
EVT_Quantiles_PercentageChangeH_R25$model = "R25"
EVT_Quantiles_PercentageChangeH <- rbind(EVT_Quantiles_PercentageChangeH_CP4,EVT_Quantiles_PercentageChangeH_R25)

H1 <- ggplot(data = EVT_Quantiles_MarkovH_All, aes(x=Return_Period, y=Precipitation_Amount, ymin=Precip_lower, ymax = Precip_upper , fill=model, linetype = model))+
  geom_line() +
  geom_ribbon(alpha=0.5) +
  ggtitle("Return levels for extreme daily rainfall events from the observations/model datasets") +
  xlab("Return period (years)") +
  ylab("Precipitation (mm day-1)")+
  theme(text = element_text(size = 15)) +
  labs(fill = "Observation/Model", linetype= "Observation/Model")+
  scale_fill_manual(values = c(AM_colors[1],AM_colors[2],"Red", AM_colors[3],AM_colors[4]))

ggplot(data = EVT_Quantiles_MarkovH_All, aes(x=Quantile, y=Precipitation_Amount, ymin=Precip_lower, ymax = Precip_upper , fill=model, linetype = model))+
  geom_line() +
  geom_ribbon(alpha=0.5) +
  ggtitle("Return levels for extreme rain events from the observations/model datasets") +
  xlab("Cumulative distribution") +
  ylab("Precipitation mmday-1")+
  theme(text = element_text(size = 15)) +
  labs(fill = "Observation/Model", linetype= "Observation/Model")


H2 <- ggplot(data = EVT_Quantiles_PercentageChangeH, aes(x=Return_Period, y=exact, ymin=lower, ymax = upper , fill=model,linetype = model))+
  geom_line() +
  geom_ribbon(alpha=0.5) +
  ggtitle("Change in precipitation return levels from current to future climate for extreme rain events for both models") +
  xlab("Return period (years)") +
  ylab("Increase in return level (%)")+
  theme(text = element_text(size = 15)) +
  labs(fill = "Model", linetype= "Model")

grid.arrange(D1, H1,  D2, H2, ncol=2, nrow=2)

#Anderson darling test

install.packages("goftest")
library(goftest)

EVT_GOF_Test <- function(d1){
  dy <- AnnualRainfallMaxima(d1)
  dy1 <- fgev(dy$Max_Precipitation)$estimate
  X <- ad.test(dy$Max_Precipitation,loc = dy1[1], scale = dy1[2], shape = dy1[3],"pgev",estimated=TRUE)
  return(X)
}

EVT_GOF_Test(UsefulRainfall_CP4_C)
EVT_GOF_Test(UsefulRainfall_CP4_F)
EVT_GOF_Test(UsefulRainfall_R25_C)
EVT_GOF_Test(UsefulRainfall_R25_F)
EVT_GOF_Test(UsefulRainfall_Aero)
EVT_GOF_Test(UsefulRainfall_Bani)
EVT_GOF_Test(UsefulRainfall_Orst)

EVT_GOF_Test(Markov_CP4_C)
EVT_GOF_Test(Markov_CP4_F)
EVT_GOF_Test(Markov_R25_C)
EVT_GOF_Test(Markov_R25_F)
EVT_GOF_Test(Markov_Aero)
EVT_GOF_Test(Markov_Bani)
EVT_GOF_Test(Markov_Orst)

EVT_GOF_Test(MarkovH_CP4_C)
EVT_GOF_Test(MarkovH_CP4_F)
EVT_GOF_Test(MarkovH_R25_C)
EVT_GOF_Test(MarkovH_R25_F)
EVT_GOF_Test(MarkovH_Aero)
EVT_GOF_Test(MarkovH_Bani)
EVT_GOF_Test(MarkovH_Orst)
