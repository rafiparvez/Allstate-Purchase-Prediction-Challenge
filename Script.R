library(dplyr)
library(Amelia)
library(ggplot2)

##LOADING TRAINING AND TEST DATA

trainDataPath <-
  "E:/Zappos/ML/Repo/Allstate-Purchase-Prediction-Challenge/data/train.csv"
testDataPath <-
  "E:/Zappos/ML/Repo/Allstate-Purchase-Prediction-Challenge/data/test_v2.csv"

colClasses <- c(
  rep("integer", 2),
  "factor",
  "factor",
  "character",
  "factor",
  rep("integer", 2),
  "factor",
  "integer",
  "factor",
  rep("integer", 3),
  rep("factor", 2),
  "integer",
  rep("factor", 7),
  "integer"
)

trainData <-
  read.csv(file = trainDataPath,
           na.strings =
             c("NA", ""),
           colClasses = colClasses)
bkuptrainData<-trainData

testData <-
  read.csv(file = testDataPath ,
           na.strings =
             c("NA", ""),
           colClasses = colClasses)

bkuptestData<-testData
## ANALYZING MISSING AND FIXING MISSING DATA

# Function to analyze missing columns
visAndAnalyze <- function(df) {
  #Return columns with missing values in data
  missData <-
    apply(df, 2, function(x)
      sum(is.na(x)) * 100 / length(x))
  
  show(missData[missData > 0])
  
  missmap(df,
          main = "Allstate Data - Missings Map",
          col = c("red", "blue"),
          legend = FALSE)
}

#Analyzing missing values in training and test data

visAndAnalyze(trainData)
visAndAnalyze(testData)


#Function to fix missing values
fixMissData <- function(df) {
  #impute missing car_value
  s <-
    sapply(split(df$car_value, df$customer_ID), function(x)
      tail(x, 1))
  df$car_value[is.na(df$car_value)] <-
    s[as.character(df$customer_ID[is.na(df$car_value)])]
  
  #For car values which cannot be fetched from other rows for same customer
  
  with(testData[!is.na(testData$car_value),], plot(car_age, cost, col = car_value))
  
  multiModel <- multinom(car_value ~ car_age + cost, data = df[!is.na(df$car_value), ])
  prediction<-predict(multiModel, newdata=df[is.na(df$car_value), ])
  df$car_value[is.na(df$car_value)] <- prediction
  
  #TODO: Implement single function for c_previous & duration_previous
  
  nonMissElt <- function(x) {
    vect<- !is.na(x)
    if (sum(vect) == 0) {0} else {head(x[vect], 1)}
  }
  #impute missing c_previous
  levels(df$C_previous)<-c("0",levels(trainData$C_previous))
  c <-
    sapply(split(df$C_previous, df$customer_ID), nonMissElt)
  
  df$C_previous[is.na(df$C_previous)] <-
    c[as.character(df$customer_ID[is.na(df$C_previous)])]
  
  #impute missing duration_previous
  d <-
    sapply(split(df$duration_previous, df$customer_ID), nonMissElt)
  
  df$duration_previous[is.na(df$duration_previous)] <-
    d[as.character(df$customer_ID[is.na(df$duration_previous)])]
  
  #impute missing risk_value
  NARiskData <- df[is.na(df$risk_factor), ]
  RiskData <- df[!is.na(df$risk_factor), ]
  linearModel <- lm(risk_factor ~ age_youngest*group_size+married_couple+
                      homeowner, data=RiskData)
  prediction<- predict(linearModel, newdata=NARiskData)
  df$risk_factor[is.na(df$risk_factor)] <- round(prediction, 0)
  df$risk_factor<-as.factor(df$risk_factor)
  
  #impute missing location values
  df$location[is.na(df$location)] <- mean(round(df$location,0), na.rm = TRUE)
  
  return(df)
}
  

trainData<-fixMissData(trainData)
testData<-fixMissData(testData)

#Add a column quote which is concatenating plans A through G
trainData<-mutate(trainData, quote = paste(A, B, C, D, E, F, G, sep = ""))

#Get the PurchaseSet and NON-PurchaseSet
purchaseData<- as.data.frame(trainData %>% group_by(customer_ID) %>% slice(n()))
NonPurchaseData<-purchaseData<- as.data.frame(trainData %>% group_by(customer_ID) %>% slice(1:(n()-1)))


#Plots to understand relationship between different plans
plot(x=trainData$A,y=trainData$B, xlab ="A", ylab="B")
plot(x=trainData$A,y=trainData$C, xlab ="A", ylab="C")
plot(x=trainData$A,y=trainData$D, xlab ="A", ylab="D")
plot(x=trainData$A,y=trainData$E, xlab ="A", ylab="E")
plot(x=trainData$A,y=trainData$F, xlab ="A", ylab="F")
plot(x=trainData$A,y=trainData$G, xlab ="A", ylab="G")
plot(x=trainData$B,y=trainData$C ,xlab ="B", ylab="C")
plot(x=trainData$B,y=trainData$D ,xlab ="B", ylab="D")
plot(x=trainData$B,y=trainData$E ,xlab ="B", ylab="E")
plot(x=trainData$B,y=trainData$F ,xlab ="B", ylab="F")
plot(x=trainData$B,y=trainData$G ,xlab ="B", ylab="G")
plot(x=trainData$C,y=trainData$D ,xlab ="C", ylab="D")
plot(x=trainData$C,y=trainData$E ,xlab ="C", ylab="E")
plot(x=trainData$C,y=trainData$F ,xlab ="C", ylab="F")
plot(x=trainData$C,y=trainData$G ,xlab ="C", ylab="G")
plot(x=trainData$D,y=trainData$E ,xlab ="D", ylab="E")
plot(x=trainData$D,y=trainData$F ,xlab ="D", ylab="F")
plot(x=trainData$D,y=trainData$G ,xlab ="D", ylab="G")
plot(x=trainData$E,y=trainData$F ,xlab ="E", ylab="F")
plot(x=trainData$E,y=trainData$G ,xlab ="E", ylab="G")
plot(x=trainData$F,y=trainData$G ,xlab ="F", ylab="G")


