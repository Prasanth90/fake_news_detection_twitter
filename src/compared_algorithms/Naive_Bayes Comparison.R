## Naive Bayes Comparison
library(naivebayes)
library(caret)

bar_plot<-function(cm, title) {
  correct<-c(cm[1],cm[4])
  wrong<-c(cm[3],cm[2])
  m<-c(correct,wrong)
  mat<- matrix(m,nrow=2,ncol=2,byrow=TRUE)
  rownames(mat)<-c("Correct", "Wrong")
  colnames(mat)<-c("Spam", "NonSpam")
  accuracy<-(cm[1] + cm[4])*100/(cm[1]+cm[2]+cm[3]+cm[4])
  formatted_accuracy<-format(round(accuracy, 2), nsmall = 2)
  accuracy_label<-paste("Accuracy = ", formatted_accuracy, "%")
  barplot(mat,main=title, xlab=accuracy_label, ylab="Number of Samples", col=c("green","red"), legend = rownames(mat))
}

Accuracy<-function(){
  actual<-test_set[,4]
  #print(actual)
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
naivebayes<-naive_bayes(test_set[,-4],as.factor(test_set[,4]),usekernel = TRUE)

## Predicting the model
pred<-predict(naivebayes)

## creating Confusion Matrix
cm<-confusionMatrix(test_set[,4],pred)

naivebayes_cm<-Accuracy()

plot(naivebayes)
bar_plot(naivebayes_cm,"Naive Bayes classsifier for test data")
return(naivebayes_cm)
