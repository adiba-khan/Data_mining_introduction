#import libraries
library.packages(c('tree', 'ISLR', 'randomForest', 'e1071'))
library(tree)
library(ISLR)
library(randomForest)
library(e1071)

data <- read.csv(file="Cancer_data_train.csv")

View(data)

for (i in 1:ncol(data)){
  data[is.na(data[,i]),i] <- median(data[,i],na.rm=TRUE)
}

data$Cancer <- as.factor(data$Cancer)
data$Smoker <- as.factor(data$Smoker)

BMI <- function(height, weight){
  return(weight/(height*0.01)^2)
}

data$bmi <- BMI(data$Height,data$Weight)
data$Height <- NULL
data$Weight <- NULL

#decision trees
set.seed(3)
train = sample(nrow(data), nrow(data)*2/3)
test=-train
data_test=data[test,]
data_train=data[train,]

set.seed(3)
tree.data_train=tree(data_train$Cancer~.,data_train)

summary(tree.data_train)
plot(tree.data_train)
text(tree.data_train ,pretty = 0)

#random forest
set.seed(3)
rf.data <- randomForest(data_train$Cancer~.,data_train,ntree=3)

prediction <- predict(rf.data, data_test, type = 'class')

res.rf <- table(prediction, data_test$Cancer)
res.rf

#SVM
set.seed(3)
svm.data <- svm(data_train$Cancer~.,data_train, kernel = "linear")

prediction <- predict(svm.data, data_test, type = 'class')

res.svm <- table(prediction, data_test$Cancer)
res.svm

#after generating these confusion matrices, we see that random
#forest is a better model because there are more correct 
#predictions and fewer errors

#lets apply these concepts further...
#use a cost complexity method to reduce overfitting in decision tree
cv.data_train = cv.tree(tree.data_train ,FUN=prune.misclass)

#plot cost complexity
plot(cv.data_train$size ,cv.data_train$dev ,type="b")
#y value is at a minimum when the number of leaf nodes = 4

prune.data_train=prune.misclass(tree.data_train,best=4)
plot(prune.data_train )
text(prune.data_train,pretty=0)

prediction <- predict(prune.data_train, data_test, type='class')

res.svm <- table(prediction, data_test$Cancer)
res.svm

#Regenerate random forest model with 3, 5, and 10 trees
#3
set.seed(3)
rf.data <- randomForest(data_train$Cancer~.,data_train,ntree=3)

prediction <- predict(rf.data, data_test, type = 'class')

res.rf <- table(prediction, data_test$Cancer)
res.rf

#5
set.seed(3)
rf.data <- randomForest(data_train$Cancer~.,data_train,ntree=5)

prediction <- predict(rf.data, data_test, type = 'class')

res.rf <- table(prediction, data_test$Cancer)
res.rf

#10
set.seed(3)
rf.data <- randomForest(data_train$Cancer~.,data_train,ntree=10)

prediction <- predict(rf.data, data_test, type = 'class')

res.rf <- table(prediction, data_test$Cancer)
res.rf

#now we generate SVM model with linear, polynomial, radial kernels

#linear
set.seed(3)
svm.data <- svm(data_train$Cancer~.,data_train, kernel = "linear")

prediction <- predict(svm.data, data_test, type = 'class')

res.svm <- table(prediction, data_test$Cancer)
res.svm

#polynomial
set.seed(3)
svm.data <- svm(data_train$Cancer~.,data_train, kernel = "polynomial")

prediction <- predict(svm.data, data_test, type = 'class')

res.svm <- table(prediction, data_test$Cancer)
res.svm

#radial
set.seed(3)
svm.data <- svm(data_train$Cancer~.,data_train, kernel = "radial")

prediction <- predict(svm.data, data_test, type = 'class')

res.svm <- table(prediction, data_test$Cancer)
res.svm