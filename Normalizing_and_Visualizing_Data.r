getwd()
dir()
setwd("/Users/Adiba/Documents/DSV R Labs/")
getwd()

#read.csv2 is for semi-colon separated data

data <- read.csv2(file="Cancer_data.csv")
View(data)

#read.csv is for comma separated data

data <- read.csv(file="Cancer_data.csv")
View(data)

#summary will give basic statistics of each column of data
summary(data)

#head will display the first few rows of data
head(data)

#tail will display the last few rows of data
#add integer to specify number of rows
tail(data)
tail(data, 3)

#names provides the column names
names(data)

#replace null values with mean of column
#run into an issue because mean is not the best parameter i.e. for binary classification
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

#we need to reload the original data because we've inserted values into null spots
data <- read.csv(file = "Cancer_data.csv")
View(data)

#replace null values with median of column
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- median(data[,i], na.rm = TRUE)
}

normalize <- function(x) {
  return ((x - min(x)) / max(x)-min(x))
}

datanorm <- as.data.frame(lapply(data, normalize))

data$Age
hist(data$Age)
boxplot(data$Age)
plot(data$Age) #scatterplot

#generate matrix of correlation and visualize it
cors <- cor(datanorm)
View(cors)

install.packages("corrplot")
library(corrplot)
corrplot(cors)

##

#define a function to calculate BMI given variables height and weight
BMI <- function(height, weight) {
  return(weight/(height*0.01)^2)
}

datanorm$bmi <- normalize(BMI(data$Height,data$Weight))

cors <- cor(datanorm)

library(corrplot)
corrplot(cors, method = "number")