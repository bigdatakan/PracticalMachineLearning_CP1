## pml-cp-report.R

## load libraries caret and randon forest
library(caret)
library(randomForest)

## read data in pml-training.csv into trainTruth
trainTruth <- read.csv('pml-training.csv', sep=',', header=T, na.strings=c('', 'NA', '#DIV/0!'))

## read data in pml-testing.csv into testTruth
testTruth <- read.csv('pml-testing.csv', sep=',', header=T, na.strings=c('', 'NA', '#DIV/0!'))

## set random seed
set.seed(54321)

## use 75% of trainTruth data to be the training data of the prediction model
inTrain <- createDataPartition(trainTruth$classe, p=0.75, list=F)

train <- trainTruth[inTrain,] ## the training data set
valid <- trainTruth[-inTrain,] ## the validation data set

## remove descriptive columns
exclcol <- c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')
train <- train[, !(names(train) %in% exclcol)]

## remove near zero variance
train <- train[,-nearZeroVar(train)]

## remove columns containing a lot of NAs or empty value
nas <- apply(train, 2, function(x) { sum(is.na(x) || sum(x=='')) })
train <- train[, which(nas == 0)]

modFit <- randomForest(classe ~ ., data=train, importance=T, ntrees=10)

## predict with the train data set
pTrain <- predict(modFit, train)

## show performance of the prediction
print(confusionMatrix(pTrain, train$classe))

## cross validate the prediction model with the valid data set
pValid <- predict(modFit, valid)

## show performance of the prediction
print(confusionMatrix(pValid, valid$classe))

## predict the testTruth data set
pTest <- predict(modFit, testTruth)

## show prediction results
pTest

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

## create file for Prediction Assignment Submission
pml_write_files(pTest)
