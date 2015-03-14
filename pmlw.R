library(caret)
library(doMC)
registerDoMC(cores = 3)

set.seed(2333)
data <- read.csv("pml-training_cleaned.csv")

#parralel processing is included
#Data that are used in this project are firstly processed , columns that are not dense
#are excluded , classe column values are changed with values 1 to 5 corresponding to letters
#A to E.

#There is high correlation betweene data for measurments on x axis so pca is efficient

#examples

  qplot(data$gyros_belt_x,data$accel_belt_x,color=data$classe)
  
  cor(data$gyros_belt_x,data$accel_belt_x)

trainingIndexes <- createDataPartition(data$classe,p = 0.60,list = F)
training <- data[trainingIndexes,]
testing <- data[-trainingIndexes,]

trainingControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10,verboseIter = T)

#number of variavles after pca is 18 , vith 90 % variance captured
pP <- preProcess(training[,-52],method=c("center","scale","pca"),thresh = 0.9)

trainObj <-predict(pP,training[,-52])
trainObj$classe <- training$classe

  modelFit <- train(classe ~.,data = trainObj,trControl = trainingControl,method = "glm" )

trainObj <-predict(pP,testing[,-52])

toCompare <- predict(modelFit,trainObj)

prediction = rep(0,length(trainObj$classe))

  limit<-1:length(toCompare)
  for(var in limit){
    #if(floor(testing[var,]$classe) == floor(toCompare[var])){
    if(testing[var,]$classe-0.4 <= toCompare[var] && toCompare[var] <= testing[var,]$classe+0.4){
      prediction[var] <- 1
    }else{
      prediction[var] <- 0
    }
  }
  sum(prediction)/length(testing$classe)

dataTest <- read.csv("pml-testing_cleaned.csv")

trainObj <-predict(pP,dataTest[,-52])

toCompare <- predict(modelFit,newdata = trainObj)

toCompare


