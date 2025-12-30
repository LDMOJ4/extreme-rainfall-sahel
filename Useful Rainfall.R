BANIZOUMBOU2 <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/Observations/BANIZOUMBOU.dat", header = FALSE)
NY_AEROPORT2 <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/Observations/NY_AEROPORT.dat", header = FALSE)
NY_ORSTOM2 <- read.csv("https://homepages.see.leeds.ac.uk/~lecdjp/MATH3001/MATH3001/Niamey/Observations/NY_ORSTOM.dat", header = FALSE)

library(dplyr)
library(tidyr)
BANIZOUMBOU2 <- BANIZOUMBOU2%>% separate(V1, c('Year', 'Month', 'Day', 'Hour', 'Observation'), sep = ' ')
NY_AEROPORT2 <- NY_AEROPORT2%>% separate(V1, c('Year', 'Month', 'Day', 'Hour', 'Observation'), sep = ' ')
NY_ORSTOM2 <- NY_ORSTOM2%>% separate(V1, c('Year', 'Month', 'Day', 'Hour', 'Observation'), sep = ' ')

BANIZOUMBOU2 <- BANIZOUMBOU2[-c(1:61367),] 
BANIZOUMBOU2 <- BANIZOUMBOU2[-c(87649:244223),]

NY_AEROPORT2 <- NY_AEROPORT2[-c(1:61367),] 
NY_AEROPORT2 <- NY_AEROPORT2[-c(87649:244223),]

NY_ORSTOM2 <- NY_ORSTOM2[-c(1:61367),] 
NY_ORSTOM2 <- NY_ORSTOM2[-c(87649:244223),]

BANIZOUMBOU2[BANIZOUMBOU2$Observation < 0, ]$Observation = 0
NY_AEROPORT2[NY_AEROPORT2$Observation < 0, ]$Observation = 0
NY_ORSTOM2[NY_ORSTOM2$Observation < 0, ]$Observation = 0

BANIZOUMBOU2$Observation <- as.numeric(BANIZOUMBOU2$Observation)
NY_AEROPORT2$Observation <- as.numeric(NY_AEROPORT2$Observation)
NY_ORSTOM2$Observation <- as.numeric(NY_ORSTOM2$Observation)

#Temperature and wind observation data

install.packages("worldmet")
library(worldmet)
getMeta()
x <- importNOAA(code = "610520-99999", year = 1997:2006)

Daily_Rainfall_sum <- function(data_table){
  return(aggregate(Precipitation ~ Day+Month+Year, data_table, FUN = function(x) c(sum = sum(x), se = sqrt(var(x)))))
}

Daily_Rainfall_sum1 <- function(data_table){
  return(aggregate(Observation ~ Day+Month+Year, data_table, FUN = function(x) c(sum = sum(x), se = sqrt(var(x)))))
}

DailySum_CP4_C <- Daily_Rainfall_sum(CP4_C)
DailySum_CP4_F <- Daily_Rainfall_sum(CP4_F)
DailySum_R25_C <- Daily_Rainfall_sum(R25_C)
DailySum_R25_F <- Daily_Rainfall_sum(R25_F)

DailySum_Bani <- Daily_Rainfall_sum1(BANIZOUMBOU2)
DailySum_Aero <- Daily_Rainfall_sum1(NY_AEROPORT2)
DailySum_Orst <- Daily_Rainfall_sum1(NY_ORSTOM2)

UsefulRainfall_CP4_C <- data.table(DailySum_CP4_C)
UsefulRainfall_CP4_F <- data.table(DailySum_CP4_F)
UsefulRainfall_R25_C <- data.table(DailySum_R25_C)
UsefulRainfall_R25_F <- data.table(DailySum_R25_F)

UsefulRainfall_Bani <- data.table(DailySum_Bani)
UsefulRainfall_Aero <- data.table(DailySum_Aero)
UsefulRainfall_Orst <- data.table(DailySum_Orst)

Headers = c("Day", "Month", "Year", "Precipitation_Sum", "Precipitation_SE")
names(UsefulRainfall_CP4_C) = Headers
names(UsefulRainfall_CP4_F) = Headers
names(UsefulRainfall_R25_C) = Headers
names(UsefulRainfall_R25_F) = Headers

names(UsefulRainfall_Bani) = Headers
names(UsefulRainfall_Aero) = Headers
names(UsefulRainfall_Orst) = Headers

UsefulRainfall_Aero[] <- lapply(UsefulRainfall_Aero,as.numeric)
UsefulRainfall_Bani[] <- lapply(UsefulRainfall_Bani,as.numeric)
UsefulRainfall_Orst[] <- lapply(UsefulRainfall_Orst,as.numeric)

Ru = 1

UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum < Ru]$Precipitation_Sum = 0

UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum < Ru]$Precipitation_Sum = 0
UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum < Ru]$Precipitation_Sum = 0

WetDryTable <- function(d2){

dryprecip_sum = 0
drylength = 0
wetprecip_sum = 0
wetlength = 0

d1 <- data.frame(Spell=character(), 
                 Length=numeric(),
                 Total_Precipitation=numeric(),
                 Intensity=numeric(),
                 stringsAsFactors=FALSE)
i=1
for (row in 1:nrow(d2)) {
  precip_current <- d2[row,"Precipitation_Sum"]
  if(precip_current > 0){
    d1[i,"Length"] = drylength
    d1[i,"Total_Precipitation"] = dryprecip_sum
    d1[i,"Spell"] = "Dry"
    wetlength = wetlength + 1
    wetprecip_sum = wetprecip_sum + precip_current
    i = i+drylength
    drylength = 0
  } else{
    d1[i,"Length"] = wetlength
    d1[i,"Total_Precipitation"] = wetprecip_sum
    d1[i,"Spell"] = "Wet"
    i=i+wetlength
    drylength = drylength + 1
    wetlength = 0
    wetprecip_sum= 0
    
  }
}

d1$Intensity = d1$Total_Precipitation / d1$Length
d1 <- na.omit(d1)

d1 <- merge(d1, d2,  by=0)

d1 <- d1[ -c(1,9:10) ]
d1 <- d1[, c(5,6,7,1,2,3,4)]
setorder(d1, 'Year')
rownames(d1) <- 1:nrow(d1)

return(d1)}

WDSpells_CP4_C <- WetDryTable(UsefulRainfall_CP4_C)
WDSpells_CP4_F <- WetDryTable(UsefulRainfall_CP4_F)
WDSpells_R25_C <- WetDryTable(UsefulRainfall_R25_C)
WDSpells_R25_F <- WetDryTable(UsefulRainfall_R25_F)

WDSpells_Aero <- WetDryTable(UsefulRainfall_Aero)
WDSpells_Bani <- WetDryTable(UsefulRainfall_Bani)
WDSpells_Orst <- WetDryTable(UsefulRainfall_Orst)

ggplot()+
  geom_point(data = WDSpells_CP4_F[WDSpells_CP4_F$Spell == "Wet",], aes(y = Intensity, x = Length), bins = 20)+
  ggtitle("Intensity vs Length CP4_C") +
  xlab("Length") +
  ylab("Intensity")+
  theme(text = element_text(size = 15))

# Wet Spell pdfs

ggplot()+
  geom_histogram(data = WDSpells_CP4_C[WDSpells_CP4_C$Spell == "Wet",], aes(x = Intensity), bins = 20)+
  ggtitle("PDF of Intensity of wet spells for CP4_C") +
  xlab("Intensity") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

ggplot()+
  geom_histogram(data = WDSpells_R25_C[WDSpells_R25_C$Spell == "Wet",], aes(x = Intensity), bins = 20)+
  ggtitle("PDF of Intensity of wet spells for R25_C") +
  xlab("Intensity") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

ggplot()+
  geom_histogram(data = WDSpells_CP4_F[WDSpells_CP4_F$Spell == "Wet",], aes(x = Intensity), bins = 20)+
  ggtitle("PDF of Intensity of wet spells for CP4_F") +
  xlab("Intensity") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

ggplot()+
  geom_histogram(data = WDSpells_R25_F[WDSpells_R25_F$Spell == "Wet",], aes(x = Intensity), bins = 20)+
  ggtitle("PDF of Intensity of wet spells for R25_F") +
  xlab("Intensity") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

LCC <- ggplot()+
  geom_histogram(data = WDSpells_CP4_C[WDSpells_CP4_C$Spell == "Wet",], aes(x = Length), bins = max(WDSpells_CP4_C[WDSpells_CP4_C$Spell == "Wet",]$Length))+
  ggtitle("PDF of length of wet spells for CP4_C") +
  xlab("Length") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

LRC <- ggplot()+
  geom_histogram(data = WDSpells_R25_C[WDSpells_R25_C$Spell == "Wet",], aes(x = Length), bins = max(WDSpells_R25_C[WDSpells_R25_C$Spell == "Wet",]$Length))+
  ggtitle("PDF of length of wet spells for R25_C") +
  xlab("Length") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

LCF <- ggplot()+
  geom_histogram(data = WDSpells_CP4_F[WDSpells_CP4_F$Spell == "Wet",], aes(x = Length), bins = max(WDSpells_CP4_F[WDSpells_CP4_F$Spell == "Wet",]$Length))+
  ggtitle("PDF of length of wet spells for CP4_F") +
  xlab("Length") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

LRF <- ggplot()+
  geom_histogram(data = WDSpells_R25_F[WDSpells_R25_F$Spell == "Wet",], aes(x = Length), bins = max(WDSpells_R25_F[WDSpells_R25_F$Spell == "Wet",]$Length))+
  ggtitle("PDF of length of wet spells for R25_F") +
  xlab("Length") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

ggplot()+
  geom_histogram(data = WDSpells_Orst[WDSpells_Orst$Spell == "Wet",], aes(x = Length), bins = max(WDSpells_Orst[WDSpells_Orst$Spell == "Wet",]$Length))+
  ggtitle("PDF of length of wet spells for Niamey ORSTOM") +
  xlab("Length") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

ggplot()+
  geom_density(data = WDSpells_CP4_C[WDSpells_CP4_C$Spell == "Wet",], aes(x = Length, color = "CP4_C"), linewidth = 1.5)+
  geom_density(data = WDSpells_CP4_F[WDSpells_CP4_F$Spell == "Wet",], aes(x = Length, color = "CP4_F"), linewidth = 1.5)+
  geom_density(data = WDSpells_R25_C[WDSpells_R25_C$Spell == "Wet",], aes(x = Length, color = "R25_C"), linewidth = 1.5)+
  geom_density(data = WDSpells_R25_F[WDSpells_R25_F$Spell == "Wet",], aes(x = Length, color = "R25_F"), linewidth = 1.5)+
  ggtitle("PDF of wet spell lengths for all 4 models") +
  xlab("Length") +
  ylab("Density")+
  labs(color = "Observation or Model")+
  theme(text = element_text(size = 15))

ggplot()+
  geom_density(data = WDSpells_CP4_C[WDSpells_CP4_C$Spell == "Wet",], aes(x = Intensity, color = "CP4_C"), linewidth = 1.5)+
  geom_density(data = WDSpells_CP4_F[WDSpells_CP4_F$Spell == "Wet",], aes(x = Intensity, color = "CP4_F"), linewidth = 1.5)+
  geom_density(data = WDSpells_R25_C[WDSpells_R25_C$Spell == "Wet",], aes(x = Intensity, color = "R25_C"), linewidth = 1.5)+
  geom_density(data = WDSpells_R25_F[WDSpells_R25_F$Spell == "Wet",], aes(x = Intensity, color = "R25_F"), linewidth = 1.5)+
  ggtitle("PDF of wet spell intensities for all 4 models") +
  xlab("Intensity") +
  ylab("Density")+
  labs(color = "Observation or Model")

# Dry Spell pdfs


ggplot()+
  geom_histogram(data = WDSpells_CP4_C[WDSpells_CP4_C$Spell == "Dry",], aes(x = Length), bins = 100)+
  ggtitle("PDF of length of dry spells for CP4_C") +
  xlab("Length") +
  ylab("Frequency")+
  theme(text = element_text(size = 15))

require(gridExtra)

grid.arrange(LCC, LRC, LCF, LRF, ncol=2, nrow=2)

#Useful Rainfall PDFs with distribution fit

bw <- 1

CCPDF <- ggplot()+
  geom_histogram(data = UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_CP4_C$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_CP4_C$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,])[2])) ), color = "orange", linewidth = 1)+
  #geom_line(aes(x = x, y = (dweibull(x, shape=WeibullParameters(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,])[1], scale = WeibullParameters(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,])[2])), color = 'Weibull'))+
  ggtitle("PDF of useful rainfall precipitation amounts for CP4_C") +
  xlab("") +
  ylab("Density")+
  theme(text = element_text(size = 15))
   

CFPDF <- ggplot()+
  geom_histogram(data = UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_CP4_F$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_CP4_F$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum > 0,])[2])) ), color = "orange", linewidth = 1)+
  #geom_line(aes(x = x, y = (dweibull(x, shape=WeibullParameters(UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum > 0,])[1], scale = WeibullParameters(UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum > 0,])[2])), color = 'Weibull'))+
  ggtitle("PDF of useful rainfall precipitation amounts for CP4_F") +
  xlab("") +
  ylab("Density")+
  theme(text = element_text(size = 15))
   

RCPDF <- ggplot()+
  geom_histogram(data = UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_R25_C$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_R25_C$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum > 0,])[2])) ), color = "orange", linewidth = 1)+
  #geom_line(aes(x = x, y = (dweibull(x, shape=WeibullParameters(UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum > 0,])[1], scale = WeibullParameters(UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum > 0,])[2])), color = 'Weibull'))+
  ggtitle("PDF of useful rainfall precipitation amounts for R25_C") +
  xlab("") +
  ylab("Density")+
  theme(text = element_text(size = 15))
   

RFPDF <- ggplot()+
  geom_histogram(data = UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_R25_F$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_R25_F$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum > 0,])[2])) ), color = "orange", linewidth = 1)+
  #geom_line(aes(x = x, y = (dweibull(x, shape=WeibullParameters(UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum > 0,])[1], scale = WeibullParameters(UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum > 0,])[2])), color = 'Weibull'))+
  ggtitle("PDF of useful rainfall precipitation amounts for R25_F") +
  xlab("") +
  ylab("Density")+
  theme(text = element_text(size = 15))
   

APDF <- ggplot()+
  geom_histogram(data = UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_Aero$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_Aero$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum > 0,])[2])) ), color = "orange", linewidth = 1)+
  #geom_line(aes(x = x, y = (dweibull(x, shape=WeibullParameters(UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum > 0,])[1], scale = WeibullParameters(UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum > 0,])[2])), color = 'Weibull'))+
  ggtitle("PDF of useful rainfall precipitation amounts for Niamey Airport") +
  xlab("") +
  ylab("Density")+
  theme(text = element_text(size = 15))
   

BPDF <- ggplot()+
  geom_histogram(data = UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_Bani$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_Bani$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum > 0,])[2]))),color = "orange", linewidth = 1)+
  #geom_line(aes(x = x, y = (dweibull(x, shape=WeibullParameters(UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum > 0,])[1], scale = WeibullParameters(UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum > 0,])[2])), color = 'Weibull'))+
  ggtitle("PDF of useful rainfall precipitation amounts for Banizoumbou") +
  xlab("") +
  ylab("Density")+
  theme(text = element_text(size = 15))+
  labs(color = "Distribution")
   

OPDF <- ggplot()+
  geom_histogram(data = UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_Orst$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_Orst$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum > 0,])[2]))), color = "orange", linewidth = 1)+
  #geom_line(aes(x = x, y = (dweibull(x, shape=WeibullParameters(UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum > 0,])[1], scale = WeibullParameters(UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum > 0,])[2])), color = 'Weibull'))+
  ggtitle("") +
  xlab("") +
  ylab("Density")+
  theme(text = element_text(size = 15))

# r*f against r

CCRPDF <-ggplot()+
  geom_histogram(data = UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)*x), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_CP4_C$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_CP4_C$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_CP4_C[UsefulRainfall_CP4_C$Precipitation_Sum > 0,])[2])*seq(0,max(UsefulRainfall_CP4_C$Precipitation_Sum), by=0.1))), color = "orange", linewidth = 1)+
  ggtitle("CC") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15))

CFRPDF <-ggplot()+
  geom_histogram(data = UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)*x), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_CP4_F$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_CP4_F$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_CP4_F[UsefulRainfall_CP4_F$Precipitation_Sum > 0,])[2])*seq(0,max(UsefulRainfall_CP4_F$Precipitation_Sum), by=0.1))), color = "orange", linewidth = 1)+
  ggtitle("CF") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15))

RCRPDF <-ggplot()+
  geom_histogram(data = UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)*x), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_R25_C$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_R25_C$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_R25_C[UsefulRainfall_R25_C$Precipitation_Sum > 0,])[2])*seq(0,max(UsefulRainfall_R25_C$Precipitation_Sum), by=0.1))), color = "orange", linewidth = 1)+
  ggtitle("RC") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15))

RFRPDF <-ggplot()+
  geom_histogram(data = UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)*x), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_R25_F$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_R25_F$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_R25_F[UsefulRainfall_R25_F$Precipitation_Sum > 0,])[2])*seq(0,max(UsefulRainfall_R25_F$Precipitation_Sum), by=0.1))), color = "orange", linewidth = 1)+
  ggtitle("RF") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15))

ARPDF <-ggplot()+
  geom_histogram(data = UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)*x), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_Aero$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_Aero$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_Aero[UsefulRainfall_Aero$Precipitation_Sum > 0,])[2])*seq(0,max(UsefulRainfall_Aero$Precipitation_Sum), by=0.1))), color = "orange", linewidth = 1)+
  ggtitle("") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15))

BRPDF <-ggplot()+
  geom_histogram(data = UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)*x), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_Bani$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_Bani$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_Bani[UsefulRainfall_Bani$Precipitation_Sum > 0,])[2])*seq(0,max(UsefulRainfall_Bani$Precipitation_Sum), by=0.1))), color = "orange", linewidth = 1)+
  ggtitle("B") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15))

ORPDF <-ggplot()+
  geom_histogram(data = UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum > 0,], aes(x = Precipitation_Sum, y = after_stat(density)*x), binwidth = bw) +
  geom_line(aes(x = seq(0,max(UsefulRainfall_Orst$Precipitation_Sum), by=0.1), y = (dgamma(seq(0,max(UsefulRainfall_Orst$Precipitation_Sum), by=0.1), shape=GammaParameters1(UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum > 0,])[1], scale = GammaParameters1(UsefulRainfall_Orst[UsefulRainfall_Orst$Precipitation_Sum > 0,])[2])*seq(0,max(UsefulRainfall_Orst$Precipitation_Sum), by=0.1))), color = "orange", linewidth = 1)+
  ggtitle("") +
  xlab("") +
  ylab("")+
  theme(text = element_text(size = 15))
   
grid.arrange(CCRPDF, RCRPDF, CFRPDF,RFRPDF, ncol=2, nrow=2)
grid.arrange(ARPDF, BRPDF, ORPDF, ncol=2, nrow=2)


grid.arrange(CCPDF, CFPDF, RCPDF, RFPDF,  APDF, BPDF, OPDF, ncol=2, nrow=4)
grid.arrange(CCPDF, CFPDF, RCPDF, RFPDF, ncol=2, nrow=2)
grid.arrange(APDF, BPDF, OPDF, ncol=2, nrow=2)

#Longer duration wet spells

CumulativeWD <- function(X){
  dx <- data.table()
  X <- X[X$Spell == "Wet",]
  X <- X[X$Length > 2,]
  dx$Count <- nrow(X)
  dx$Cumulation_Average <- mean(X$Total_Precipitation)
  dx$Length_Average <- mean(X$Length)
  dx$Cumulation_SE <- sd(X$Total_Precipitation)/sqrt(nrow(X))
  return(dx)
}

LWDSpells_CP4_C <- CumulativeWD(WDSpells_CP4_C)
LWDSpells_CP4_F <- CumulativeWD(WDSpells_CP4_F)
LWDSpells_R25_C <- CumulativeWD(WDSpells_R25_C)
LWDSpells_R25_F <- CumulativeWD(WDSpells_R25_F)
LWDSpells_Aero <- CumulativeWD(WDSpells_Aero)
LWDSpells_Bani <- CumulativeWD(WDSpells_Bani)
LWDSpells_Orst <- CumulativeWD(WDSpells_Orst)

LWDSpells <- rbind(LWDSpells_CP4_C,LWDSpells_CP4_F,LWDSpells_R25_C, LWDSpells_R25_F, LWDSpells_Aero, LWDSpells_Bani,LWDSpells_Orst)
LWDSpells <- cbind(LWDSpells,c("CP4_C", "CP4_F", "R25_C", "R25_F", "Niamey Airport", "Banizoumbou", "Niamey ORSTOM"))
?t.test()
t.test(LWDSpells_CP4_C$Total_Precipitation,LWDSpells_CP4_F$Total_Precipitation)
t.test(LWDSpells_R25_C$Total_Precipitation,LWDSpells_R25_F$Total_Precipitation)

t.test(LWDSpells_CP4_C$Total_Precipitation,LWDSpells_Aero$Total_Precipitation)
t.test(LWDSpells_R25_C$Total_Precipitation,LWDSpells_Aero$Total_Precipitation)
t.test(LWDSpells_CP4_C$Total_Precipitation,LWDSpells_Bani$Total_Precipitation)
t.test(LWDSpells_R25_C$Total_Precipitation,LWDSpells_Bani$Total_Precipitation)
t.test(LWDSpells_CP4_C$Total_Precipitation,LWDSpells_Orst$Total_Precipitation)
t.test(LWDSpells_R25_C$Total_Precipitation,LWDSpells_Orst$Total_Precipitation)