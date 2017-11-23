## Naive Bayes Comparison

install.packages("naivebayes")
library(naivebayes)

setwd("C:/Users/Simarpreet Singh/Desktop/Data Mining Project - Github Clone/fake_news_detection_twitter/src")

## Loading Test Data Set
test_set<-read.csv("Training_Dataset_1.csv",header=TRUE)

## Creating model for Naive Bayes
naivebayes<-naive_bayes(test_set[,-4],as.factor(test_set[,4]))

## Predicting the model
pred<-predict(naivebayes)

## creating Confusion Matrix
confusionMatrix(test_set[,4],pred)