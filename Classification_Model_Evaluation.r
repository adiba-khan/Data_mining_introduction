data <- read.csv(file="Cancer_data_train.csv")

for (i in 1:ncol(data)){
  data[is.na(data[,i]),i] <- median(data[,i],na.rm=TRUE)
}

data$Cancer <- as.factor(data$Cancer)
data$Smoker <- as.factor(data$Smoker)

BMI <- function(height, weight){
  return(weight/(height*0.01)^2)
}

#we will do a 10-fold cross validation for SVM model
#create empty vectors :D

accuracy <- vector()
precision <- vector()
recall <- vector()

#10 folds om nom nom
set.seed(3)
datarandom <- data[sample(nrow(data)),]
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

#we're going to run a for loop so every bit of data is at some point 
#part of the test fold and part of the train fold

for(i in 1:10) {

testIndexes <- which(folds==i, arr.ind=TRUE)
trainIndexes <- which(folds!=i,arr.ind=TRUE)

data_all.test <- datarandom[testIndexes, ]
data_all.train <- datarandom[trainIndexes, ]

set.seed(3)
svm.model <- svm(data_all.train$Cancer~ ., data = data_all.train, kernel = "linear")
prediction <- predict(svm.model, data_all.test)

table <- table(prediction, data_all.test$Cancer)

accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))

}

table

accuracy
accuracyaverage = mean(accuracy)
accuracyaverage

accuracy <- vector()
precision <- vector()
recall <- vector()

#we will do the same thing with Random Forest model

for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  
  set.seed(3)
  rf.data <- randomForest(data_all.train$Cancer~ ., data = data_all.train, ntree=2)
  
  prediction <- predict(rf.data, data_all.test, type="class")
  table <- table(prediction, data_all.test$Cancer)
  
  #let's get to other parameters of evaluation like precision and recall
  
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
  
  precision <- c(precision,(table[2,2])/(table[2,2] + table[2,1]))
  
  recall <- c(recall, (table[2,2]/(table[2,2] + table[1,2])))
  
}

table

accuracy
accuracyaverage = mean(accuracy)
accuracyaverage

precision
precisionaverage = mean(precision)
precisionaverage

recall
recallaverage = mean(recall)
recallaverage

accuracy <- vector()
precision <- vector()
recall <- vector()

#evaluation metrics for SVM, polynomial kernel
for(i in 1:10) {
  
  testindexes <- which(folds==i, arr.ind=TRUE)
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  
  set.seed(3)
  svm.model <- svm(data_all.train$Cancer~ ., data = data_all.train, kernel = "polynomial")
  prediction <- predict(svm.model, data_all.test)
  
  table <- table(prediction, data_all.test$Cancer)
  
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
  
  precision <- c(precision,(table[2,2])/(table[2,2] + table[2,1]))
  
  recall <- c(recall, (table[2,2]/(table[2,2] + table[1,2])))
  
}

table

accuracy
accuracyaverage = mean(accuracy)
accuracyaverage

precision
precisionaverage = mean(precision)
precisionaverage

recall
recallaverage = mean(recall)
recallaverage

accuracy <- vector()
precision <- vector()
recall <- vector()

#evaluation metrics for SVM, radial kernel
for(i in 1:10) {
  
  testindexes <- which(folds==i, arr.ind=TRUE)
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  
  set.seed(3)
  svm.model <- svm(data_all.train$Cancer~ ., data = data_all.train, kernel = "radial")
  prediction <- predict(svm.model, data_all.test)
  
  table <- table(prediction, data_all.test$Cancer)
  
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
  
  precision <- c(precision,(table[2,2])/(table[2,2] + table[2,1]))
  
  recall <- c(recall, (table[2,2]/(table[2,2] + table[1,2])))
  
}

table

accuracy
accuracyaverage = mean(accuracy)
accuracyaverage

precision
precisionaverage = mean(precision)
precisionaverage

recall
recallaverage = mean(recall)
recallaverage

accuracy <- vector()
precision <- vector()
recall <- vector()

#evaluation metrics for RF, 5 trees
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  
  set.seed(3)
  rf.data <- randomForest(data_all.train$Cancer~ ., data = data_all.train, ntree=5)
  
  prediction <- predict(rf.data, data_all.test, type="class")
  table <- table(prediction, data_all.test$Cancer)
  
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
  
  precision <- c(precision,(table[2,2])/(table[2,2] + table[2,1]))
  
  recall <- c(recall, (table[2,2]/(table[2,2] + table[1,2])))
  
}

table

accuracy
accuracyaverage = mean(accuracy)
accuracyaverage

precision
precisionaverage = mean(precision)
precisionaverage

recall
recallaverage = mean(recall)
recallaverage

accuracy <- vector()
precision <- vector()
recall <- vector()


#evaluation metrics for RF, 10 trees
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  trainIndexes <- which(folds!=i,arr.ind=TRUE)
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  
  set.seed(3)
  rf.data <- randomForest(data_all.train$Cancer~ ., data = data_all.train, ntree=10)
  
  prediction <- predict(rf.data, data_all.test, type="class")
  table <- table(prediction, data_all.test$Cancer)
  
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(sum(table)))
  
  precision <- c(precision,(table[2,2])/(table[2,2] + table[2,1]))
  
  recall <- c(recall, (table[2,2]/(table[2,2] + table[1,2])))
  
}

table

accuracy
accuracyaverage = mean(accuracy)
accuracyaverage

precision
precisionaverage = mean(precision)
precisionaverage

recall
recallaverage = mean(recall)
recallaverage

accuracy <- vector()
precision <- vector()
recall <- vector()

#run best model against test data

set.seed(3)
model.data <- randomForest(data$Cancer~.,data, ntree=5)

test <- read.csv(file="Cancer_data_test.csv")
test$Cancer <- as.factor(test$Cancer)
test$Smoker <- as.factor(test$Cancer)

test$bmi <- BMI(test$Height,test$Weight)
test$Height <- NULL
test$Weight <- NULL

prediction <- predict(model.data, test, type="class")
table <- table(prediction, test$Cancer)
table