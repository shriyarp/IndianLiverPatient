#import necessary packages
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(dplyr)
library(tidyverse)
library(caret)
library(data.table)

#load the dataset
#the dataset along with remaining files are available on github
#github link: https://github.com/shriyarp/IndianLiverPatient
indian_liver_patient <- read.csv("indian_liver_patient.csv", header = TRUE)

# tranform dataset

#add missing values
#identify missing idexes
indices <- which(is.na(indian_liver_patient$Albumin_and_Globulin_Ratio))
#calculate mean of the column
mean_agratio <- round(mean(indian_liver_patient$Albumin_and_Globulin_Ratio, na.rm = TRUE)*100)/100
#replace missing values with mean
for (i in indices) {
  indian_liver_patient$Albumin_and_Globulin_Ratio[i] <- mean_agratio
}

#change the output column to a factor
indian_liver_patient <- indian_liver_patient %>% mutate(Dataset = factor(Dataset))

#divide into training and test sets
set.seed(1)
#create test index
index <- sample(nrow(indian_liver_patient), round(0.25*nrow(indian_liver_patient)))
train <- indian_liver_patient[-index,]
test <- indian_liver_patient[index,]

#parameters for cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#fit the best model, AdaBoost Classification Trees

#Quadratic Discriminant Analysis
set.seed(4)
fit.qda <- train(Dataset~., data=train, method="qda", metric=metric, trControl=control)

#AdaBoost Classification Trees
set.seed(4)
fit.ab <- train(Dataset~., data=train, method="adaboost", metric=metric, trControl=control)

#Naive Bayes
set.seed(4)
fit.nb <- train(Dataset~., data=train, method="naive_bayes", metric=metric, trControl=control)

# Linear Discriminant Analysis
set.seed(4)
fit.lda <- train(Dataset~., data=train, method="lda", metric=metric, trControl=control)

# CART
set.seed(4)
fit.cart <- train(Dataset~., data=train, method="rpart", metric=metric, trControl=control)

# K-nearest neighbours
set.seed(4)
fit.knn <- train(Dataset~., data=train, method="knn", metric=metric, trControl=control)

# Support Vector Machines with Radial Basis Function Kernel
set.seed(4)
fit.svm <- train(Dataset~., data=train, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(4)
fit.rf <- train(Dataset~., data=train, method="rf", metric=metric, trControl=control)

#predict based on best model determined in report
y_hat <- predict(fit.ab,test)

#Accuracy measures of the best model
confusionMatrix(y_hat,test$Dataset)

