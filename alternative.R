
#reading and assigning data to a name
df <- read.csv("ai4i2020.csv", header=TRUE, sep=',')

#our libraries
library(ggplot2)
library(tidyverse)
library(data.table)
library(ROSE)
library(caret)
library(car)
library(class)
library(dplyr)
library(mlbench)
library(hrbrthemes)
library(randomForest)
library(rpart)

#create fucntion for easy examination
examines <- function(x){
  str(x)
  print("-------------------------------------------")
  print("-------------------------------------------")
  print("-------------------------------------------")
  head(x)
}

#initial examination of dataset
examines(df)
#change column names to more convenient names
setnames(df, old=c('�..UDI','Product.ID','Type',
                   'Air.temperature..K.','Process.temperature..K.','Rotational.speed..rpm.',
                   'Torque..Nm.','Tool.wear..min.','Machine.failure'),
         new = c('UDI','PID','Type','airtemp',
                 'Ptemp','Rspeed',
                 'Torque','Twear',
                 'Mfailure'))

#turn varables into appropriate types
df$UDI <-factor(df$UDI)
df$PID <- factor(df$PID)
df$Type <- factor(df$Type)
df$airtemp <- as.numeric(df$airtemp)
df$Ptemp <- as.numeric(df$Ptemp)
df$Rspeed <- as.numeric(df$Rspeed)
df$Torque <- as.numeric(df$Torque)
df$Twear <- as.numeric(df$Twear)
df$Mfailure <- factor(df$Mfailure)
df$TWF <- factor(df$TWF)
df$HDF <- factor(df$HDF)
df$PWF <- factor(df$PWF)
df$OSF <- factor(df$OSF)
df$RNF <- factor(df$RNF)

#examine results
examines(df)

# checking if there are any missing data location
table(is.na(df))

# create function to to check counts of types of failures
Ncases<- function(x){
  instances_failure <- matrix(c(nrow(x[x$TWF == 1,]),
                                nrow(x[x$HDF == 1,]),
                                nrow(x[x$PWF == 1,]),
                                nrow(x[x$OSF == 1,]),
                                nrow(x[x$RNF == 1,]),
                                nrow(x[x$Mfailure == 0,])),
                              ncol=6,byrow=TRUE)
  
  colnames(instances_failure) <- c("TWF","HDF","PWF","OSF","RNF","No failure")
  instances_failure <- as.table(instances_failure)
  return(instances_failure)
}

#use our function to see the current counts of failures vs normal 
Ncases(df)

#drop random failures
df<- subset(df, select= -c(RNF))

# randomly split data into train and testdata
set.seed(875)
dt <- sample(nrow(df), size =0.5*nrow(df))
train_failure <- df[dt,]
test_failure <- df[-dt,]


# use Manual sampling technique as its shown to work best for this data

Both <- ovun.sample(Mfailure~., data = train_failure,p=0.5, seed=1, 

                     method="both")$data 



# -----------create models for each out come value----------

#before i look at individual outcome variables, with anova to find out what items predictor variables outcome variables make sense
                
splitmodel <- lm(cbind(TWF, HDF, PWF, OSF)~ Type + airtemp + Ptemp + Rspeed + Torque + Twear,
                 data = Both )
summary(splitmodel)

#create split models based on our summary results
#TWF
RFTWF <- randomForest(TWF~ Rspeed + Torque + Twear,
                      data = Both,importance=TRUE, Proximity=TRUE)

round(importance(RFTWF),2)
predTWF <- predict(RFTWF, test_failure, type="class")
TWFstats <- caret::confusionMatrix(predTWF, test_failure$TWF)
TWFstats
Ncases(test_failure)

#HDF
RFHDF <- randomForest(HDF~ airtemp + Ptemp+ Rspeed  + Twear,
                      data = Both,importance=TRUE, Proximity=TRUE)

round(importance(RFHDF),2)
predHDF <- predict(RFHDF, test_failure, type="class")
HDFstats <- caret::confusionMatrix(predHDF, test_failure$HDF)
HDFstats
Ncases(test_failure)

#PWF
RFPWF <- randomForest(PWF~ airtemp+ Ptemp+ Rspeed + Torque + Twear,
                      data = Both,importance=TRUE, Proximity=TRUE)

round(importance(RFPWF),2)
predPWF <- predict(RFPWF, test_failure, type="class")
PWFstats <- caret::confusionMatrix(predPWF, test_failure$PWF)
PWFstats
Ncases(test_failure)

#OSF
RFOSF <- randomForest(OSF~ airtemp+ Rspeed + Torque + Twear,
                      data = Both,importance=TRUE, Proximity=TRUE)

round(importance(RFOSF),2)
predOSF <- predict(RFOSF, test_failure, type="class")
OSFstats <- caret::confusionMatrix(predOSF, test_failure$OSF)
OSFstats
Ncases(test_failure)


# ROC for each model
roc.curve(test_failure$TWF, predTWF,col=2, 
          lwd=2, lty=2)
roc.curve(test_failure$HDF, add=TRUE, predHDF,col=3, 
          lwd=1, lty=1)
roc.curve(test_failure$PWF,add=TRUE, predPWF,col=4, 
          lwd=2, lty=5, )
roc.curve(test_failure$OSF, add=TRUE,predOSF,col=9, 
          lwd=2, lty=2)


