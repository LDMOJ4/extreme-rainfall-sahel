library(readxl)
Banizoumbou_2005 <- read_excel("C:\\Users\\lukep\\OneDrive\\Documents\\Leeds Uni Maths\\Maths\\Year 3\\MATH3001\\Banizoumbou 2005.xlsx")
Banizoumbou_2005 <- data.table(Banizoumbou_2005)
Banizoumbou_2005[Banizoumbou_2005$Precipitation < Ru]$Precipitation = 0
Banizoumbou_2005[is.na(Banizoumbou_2005$Precipitation)]$Precipitation = 0
Banizoumbou_2005[Banizoumbou_2005$Temperature < 0]$Temperature = NA
Banizoumbou_2005[Banizoumbou_2005$Wind < 0]$Wind = NA

CompRainfall_Banizoumbou_2005 <- WetEvent(Banizoumbou_2005)
CompRMeans_Banizoumbou_2005 <- CompositeMeans(data.table(CompRainfall_Banizoumbou_2005))
colnames(CompRMeans_Banizoumbou_2005) <- c("Banizoumbou_2005_Mean", "Banizoumbou_2005_SE")

TempComposite1 <- function(d1,d2){
  i=1
  d3 <- data.frame()
  for (row in 1:nrow(d2)) {
    if(d2[row,1]==d1[i,1]){
      if(d2[row,2]==d1[i,2]){
        if(d2[row,3]==d1[i,3]){
          if(d2[row,4]==d1[i,4]){
            d3[1,i] <-d1[i,1]
            d3[2,i] <-d1[i,2]
            d3[3,i] <-d1[i,3]
            d3[4,i] <-d1[i,4]
            d3[5:41,i] <- d2[(row-12):(row+24),6]
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

WindComposite1 <- function(d1,d2){
  i=1
  d3 <- data.frame()
  for (row in 1:nrow(d2)) {
    if(d2[row,1]==d1[i,1]){
      if(d2[row,2]==d1[i,2]){
        if(d2[row,3]==d1[i,3]){
          if(d2[row,4]==d1[i,4]){
            d3[1,i] <-d1[i,1]
            d3[2,i] <-d1[i,2]
            d3[3,i] <-d1[i,3]
            d3[4,i] <-d1[i,4]
            d3[5:41,i] <- d2[(row-12):(row+24),7]
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

CompTemp_Banizoumbou_2005 <- TempComposite1(CompRainfall_Banizoumbou_2005,Banizoumbou_2005)
CompWind_Banizoumbou_2005 <- WindComposite1(CompRainfall_Banizoumbou_2005,Banizoumbou_2005)

CompTMeans_Banizoumbou_2005 <- CompositeMeans(CompTemp_Banizoumbou_2005)
colnames(CompTMeans_Banizoumbou_2005) <- c("Banizoumbou_2005_Mean", "Banizoumbou_2005_SE")

CompWindMeans_Banizoumbou_2005 <- CompositeMeans(CompWind_Banizoumbou_2005)
colnames(CompWindMeans_Banizoumbou_2005) <- c("Banizoumbou_2005_Mean", "Banizoumbou_2005_SE")

JJA_Hourly_meanT2 <- function(data_table){
  data_table <- data_table[data_table$Month %in% c('5', '6','7','8', '9', '10'),]
  return(aggregate(Temperature ~ Hour, data_table, FUN = function(x) c(mean = mean(x), se = sqrt(var(x)/(10*30*12)))))
}


Hourly_MeanT_Banizoumbou_2005 <- Format_table(JJA_Hourly_meanT2(Banizoumbou_2005))

CompTempAnom1 <- function(d1,d2){
  d1 <- data.frame(d1)
  for(row in 1:nrow(d1)){
    Hour <- d1$Hour[row]
    for(column in 5:41){
      i <- which((d2$Hour) == ((Hour+column-17)%%24))
      AvgTemp <- d2$Mean[i]
      d1[row,column] <- d1[row,column]-AvgTemp
    }
  }
  colnames(d1) <- c("Hour", "Day", "Month", "Year", "-12","-11","-10","-9","-8","-7","-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")
  d1 <-data.table(d1)
  return(d1)  
}

CompTAnomMeans_Banizoumbou_2005 <- CompositeMeans(CompTempAnom1(CompTemp_Banizoumbou_2005,Hourly_MeanT_Banizoumbou_2005))
colnames(CompTAnomMeans_Banizoumbou_2005) <- c("Banizoumbou_2005_Mean", "Banizoumbou_2005_SE")
