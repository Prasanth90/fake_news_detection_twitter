library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
traindataset<-read.csv("Training_Dataset.csv")
#train <- data.frame(Retweets = c(90, 7, 2, 11, 84, 76, 65, 80, 43, 53),
#Favorites = c(89, 10, 9, 3, 2, 11, 60, 20, 12, 50),
#New_Feature = c(13, 25, 33, 42, 55, 43, 11, 60,76,43),
#Class = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE))
mytree <- rpart(Class ~ Retweets + Favorites + New_Feature, data = traindataset, method = "class",minsplit=2, minbucket = 1,cp=-1)
fancyRpartPlot(mytree, main = "Decision Tree")

testdataset<-read.csv("Test_Dataset.csv")
test.def <- testdataset$Class

Accuracy<-function(){
  actual<-test.def
  #print(actual)
  prediction<-unlist(predict(mytree,testdataset,type="class"))
  #print(length(actual))
  print(length(prediction))
  #print(prediction)
  dt_cm<-table(actual,prediction)
  confusionMatrix(actual,prediction)
  return(dt_cm)
}

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

dt_cm<-Accuracy()
print(dt_cm)
bar_plot(dt_cm,"Decision Tree - Accuracy of classifier using Test data")
return(dt_cm)