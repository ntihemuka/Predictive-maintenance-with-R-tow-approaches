
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
str(df)

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

#input our failure type columns into rows 
#we create a new columns  Fail_Type and value

fuses <- df %>%
  pivot_longer(
    cols = TWF:OSF,
    names_to = "Fail_Type",
    values_to = "value"
  )

#examine our new dataset
examines(fuses)

#replace all the none failing rows by Operational
Mydataset <- transform(fuses, Fail_Type = ifelse(value == 0, c("Operational"), Fail_Type))

#factor the created column
Mydataset$Fail_Type <- as.factor(Mydataset$Fail_Type)
str(Mydataset)

#femove duplicate rows
F_data <-unique(Mydataset)
examines(F_data)
F_data$Fail_Type


          #PLOT OUR DATA for more understanding
#lets see what Types of machines fail the most often and by what failure 
F_data %>%
  count(Fail_Type, Type) %>%   #count the failures and types
  group_by(Fail_Type) %>%    #Group the failures by fail_type
  mutate(percent_Fail_Type = n/sum(n)) %>% #calculate their proportions the plot
  ggplot(aes(percent_Fail_Type, Type, fill = Fail_Type))+
  geom_col(position = "dodge")



#lets create a function to plot the data imbalance

hist_plot <- function(x){
x %>%
      ggplot( aes(Fail_Type, colour = Fail_Type)) +
      stat_count(width = 0.5)+
      stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
  }
  
#plot unsplit dat set 
hist_plot(F_data)  


#remove useless colums
#randomly split data for testing and training
set.seed(2)
dt <- sample(nrow(F_data), size =0.5*nrow(F_data))
train_failure <- F_data[dt,]
test_failure <- F_data[-dt,]

#lets re check our minority balances
hist_plot(train_failure)
hist_plot(test_failure)

examines(test_failure)



#find the best balancing algorithm


# balance dataset with different techniques

syntheticT <- ROSE(Mfailure ~., data = train_failure, N= nrow(train_failure))$data

underT <- ovun.sample(Mfailure~., data = train_failure,p=0.5, seed=1, 
                      method="under")$data 
overT <- ovun.sample(Mfailure~., data = train_failure,p=0.5, seed=1, 
                     method="over")$data 

bothT <- ovun.sample(Mfailure~., data = train_failure,p=0.5, seed=1, 
                     method="both")$data 


#drop Mfailure and value
train_failure<- subset(train_failure, select= -c(Mfailure,value))
test_failure<- subset(test_failure, select= -c(Mfailure,value))

#randomforest call function
library(randomForest)

#create models on the created datasets then validate on test dataset

#random forest trained on original dataset
RFOriginal <- randomForest(Fail_Type~ airtemp + Ptemp + Rspeed + Torque + Twear,
                           data = train_failure,importance=TRUE, Proximity=TRUE)

round(importance(RFOriginal),2)
predOriginal <- predict(RFOriginal, test_failure, type="class")
Original <- caret::confusionMatrix(predOriginal, test_failure$Fail_Type)

#random forest with synthetic  dataset
RFSynthetic <- randomForest(Fail_Type~ airtemp + Ptemp + Rspeed + Torque + Twear,
                            data = syntheticT,importance=TRUE, Proximity=TRUE)

round(importance(RFSynthetic),2)
predSynthetic <- predict(RFSynthetic, test_failure, type="class")
Synthetic <- caret::confusionMatrix(predSynthetic, test_failure$Fail_Type)

#random forest with Undersampling test dataset
RFUnderT <- randomForest(Fail_Type~ airtemp + Ptemp + Rspeed + Torque + Twear,
                         data = underT,importance=TRUE, Proximity=TRUE)

round(importance(RFUnderT),2)
predUnder <- predict(RFUnderT, test_failure, type="class")
Under <- caret::confusionMatrix(predUnder, test_failure$Fail_Type)

#random forest with oversampling test dataset
RFOverT <- randomForest(Fail_Type~ airtemp + Ptemp + Rspeed + Torque + Twear,
                        data = overT,importance=TRUE, Proximity=TRUE)

round(importance(RFOverT),2)
predOver <- predict(RFOverT, test_failure, type="class")
Over <- caret::confusionMatrix(predOver, test_failure$Fail_Type)

#random forest with under and over sampling test dataset
RFBoth <- randomForest(Fail_Type~ airtemp + Ptemp + Rspeed + Torque + Twear,
                       data = bothT,importance=TRUE, Proximity=TRUE)

round(importance(RFBoth),2)
predBoth <- predict(RFBoth, test_failure, type="class")
Both <- caret::confusionMatrix(predBoth, test_failure$Fail_Type)

#create table with results from all models
Models<- data.frame(cbind(Original$byClass,
                          Synthetic$byClass,
                          Under$byClass,
                          Over$byClass,
                          Both$byClass))
colnames(Models) <- c("Original","Synthetic","Under","Over","Both")
rbind(Models)





#lets examine  and compare-----


#plor roc curves to get best perfomin sampling technique

library(rpart)

roc.curve(test_failure$Fail_Type, predOriginal,col=5, 
          lwd=2, lty=2)
roc.curve(test_failure$Mfailure, add=TRUE, predSynthetic,col=3, 
          lwd=1, lty=1)
roc.curve(test_failure$Mfailure,add=TRUE, predUnder,col=4, 
          lwd=2, lty=3, )
roc.curve(test_failure$Mfailure, add=TRUE,predBoth,col=9, 
          lwd=2, lty=2)
roc.curve(test_failure$Mfailure, add=TRUE,predOver,col=5, 
          lwd=2, lty=7)
hist_plot(test_failure)






#lets balance the data manually

#manually balance dataset


operational_data<-  train_failure %>% filter( Fail_Type == "Operational")
other_data <- train_failure %>% filter(Fail_Type != "TWF", Fail_Type != "Operational")
TWFdata <- train_failure %>% filter( Fail_Type == "TWF")


hist_plot(other_data)
hist_plot(TWFdata)
#duplicate twf
TWFdata_dup <- TWFdata[rep(seq_len(nrow(TWFdata == "TWF")), each = 2.9), ]
hist_plot(TWFdata_dup)


#combine minority cases

minorities <- rbind(other_data, TWFdata_dup)
hist_plot(minorities)

nrow(operational_data)
minos <- minorities[rep(seq_len(nrow(minorities)), each = 90),]
hist_plot(minos)

#recombine and make our mega training data
Bigtrain <- rbind(minos,operational_data)
hist_plot(Bigtrain)

examines(Bigtrain)

#randomly shuffle big train around to insure good mix of rows
set.seed(123)
db <- sample(nrow(Big_knn), size =nrow(F_data))
Bigtrain <- train_failure <- Bigtrain[db,]

#creat our model usign new data
RF_Mine <- randomForest(Fail_Type~ airtemp + Ptemp + Rspeed + Torque + Twear,
                            data = Bigtrain,mtry=3, importance=TRUE, Proximity=TRUE)
round(importance(RF_Mine),2)
pred_Mine <- predict(RF_Mine, test_failure, type="class")
caret::confusionMatrix(pred_Mine, test_failure$Fail_Type)

head(Bigtrain)

#lets TUNE
 x <- Bigtrain[4:8]
 y <- Bigtrain[,10]
set.seed(123)
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#best my try is three