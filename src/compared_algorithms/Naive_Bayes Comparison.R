## Naive Bayes Comparison



## Loading Test Data Set
test_set<-read.csv("Test_Dataset.csv",header=TRUE)

## Creating model for Naive Bayes
naivebayes<-naive_bayes(test_set[,-4],as.factor(test_set[,4]))

## Predicting the model
pred<-predict(naivebayes)

## creating Confusion Matrix
cm<-confusionMatrix(test_set[,4],pred)

return(cm$value)
