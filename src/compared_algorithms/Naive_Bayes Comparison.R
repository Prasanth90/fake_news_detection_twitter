## Naive Bayes Comparison
library(naivebayes)
library(caret)

Accuracy<-function(){
  actual<-test_set[,4]
  print(actual)
  prediction<-pred
  print(length(prediction))
  naivebayes_cm<-table(actual,prediction)
  print(naivebayes_cm)
  confusionMatrix(actual,prediction)
  return(naivebayes_cm)
}

## Loading Test Data Set
test_set<-read.csv("Test_Dataset.csv",header=TRUE)

## Creating model for Naive Bayes
naivebayes<-naive_bayes(test_set[,-4],as.factor(test_set[,4]))

## Predicting the model
pred<-predict(naivebayes)

## creating Confusion Matrix
cm<-confusionMatrix(test_set[,4],pred)

naivebayes_cm<-Accuracy()

return(naivebayes_cm)
