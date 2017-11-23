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
fancyRpartPlot(mytree)

testdataset<-read.csv("Test_Dataset.csv")
test.def <- testdataset$Class

Accuracy<-function(){
  actual<-test.def
  #print(actual)
  prediction<-unlist(predict(mytree,testdataset,type="class"))
  #print(length(actual))
  print(length(prediction))
  #print(prediction)
  table(actual,prediction)
  confusionMatrix(actual,prediction)
}
Accuracy()