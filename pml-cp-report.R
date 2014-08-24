library(caret)

trainTruth <- read.csv('pml-training.csv', sep=',', header=T, na.strings=c('', 'NA', '#DIV/0!'))
testTruth <- read.csv('pml-testing.csv', sep=',', header=T, na.strings=c('', 'NA', '#DIV/0!'))

set.seed(54321)
inTrain <- createDataPartition(trainTruth$classe, p=0.75, list=F)
train <- trainTruth[inTrain,]
valid <- trainTruth[-inTrain,]

exclcol <- c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')

train <- train[, !(names(train) %in% exclcol)]

train <- train[,-nearZeroVar(train)]

nas <- apply(train, 2, function(x) { sum(is.na(x) || sum(x=='')) })
train <- train[, which(nas == 0)]

modFit <- randomForest(classe ~ ., data=train, importance=T, ntrees=10)

pTrain <- predict(modFit, train)
print(confusionMatrix(pTrain, train$classe))

pValid <- predict(modFit, valid)
print(confusionMatrix(pValid, valid$classe))

pTest <- predict(modFit, testTruth)
pTest

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(pTest)