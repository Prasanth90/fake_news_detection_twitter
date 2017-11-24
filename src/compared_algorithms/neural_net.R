library(neuralnet)
library(scatterplot3d)
library(caret)

set.seed(111)

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


#nn.reg(train.traindataset,test.testdataset,k=3)
Accuracy<-function(){
  actual<-test.def
  print(length(actual))
  print(length(pred_data$net.result))
  prediction<-as.numeric(unlist(pred_data$net.result))
  print(prediction)
  for(i in 1:length(prediction)){
    if(prediction[i]>1){
      prediction[i]=1
    }else{
      prediction[i]=-1
    }
  }
  nn_cm<-table(actual,prediction)
  print(nn_cm)
  confusionMatrix(actual,prediction)
  return(nn_cm)
}


traindataset <- read.csv("Training_Dataset.csv")
head (traindataset)

testdataset <- read.csv("Test_Dataset.csv")
head (testdataset)

traindataset.bkup <- traindataset
testdataset.bkup<-testdataset


## Convert the dependent var to factor. Normalize the numeric variables  
traindataset$Class <- factor(traindataset$Class)
num.vars <- sapply(traindataset, is.numeric)
traindataset[num.vars] <- lapply(traindataset[num.vars], scale)
par(mar = rep(2, 4))

## Convert the dependent var to factor. Normalize the numeric variables  
testdataset$Class <- factor(testdataset$Class)
num.vars1 <- sapply(testdataset, is.numeric)
testdataset[num.vars1] <- lapply(testdataset[num.vars1], scale)
par(mar = rep(2, 4))

## Selecting only 3 features 
myvars <- c("Retweets", "Favorites", "New_Feature")
traindataset.subset <- traindataset[myvars]
testdataset.subset<-testdataset[myvars]

summary(traindataset.subset)
summary(testdataset.subset)

#test <- 1:400
train.traindataset <- traindataset.subset
test.testdataset <- testdataset.subset

train.def <- traindataset$Class
test.def <- testdataset$Class

#print(test.def)
#Give the non-scaled input to neural network API
NN1<-neuralnet(Class ~ Retweets + Favorites + New_Feature, traindataset.bkup, hidden=4, startweights = c(8,5))
#plot(NN1)
#print(NN1$data[,4])

#s<-scatterplot3d(train.traindataset[,c("Retweets", "Favorites", "New_Feature")],color=c('black','red')[as.numeric(train.def)],pch=c(21,24)[as.numeric(train.def)],xlim=c(-1,5),ylim=c(-1,5),zlim=c(-1,5))
#s$points3d(test.testdataset[,c("Retweets", "Favorites", "New_Feature")],col=c('green','yellow')[as.numeric(test.def)],pch=c(21,24)[as.numeric(test.def)])

#legend("topleft",col=c('black','red','yellow','green'),pch=c(21,24),bg=c(1,1),
       #legend=c("TRAINED AS SPAM","TRAINED AS NON-SPAM","PREDICTED AS SPAM","PREDICTED AS NON-SPAM"),
       #title="Symbols",bty="n",cex=.8)

pred_data<-compute(NN1,test.testdataset)
nn_cm<-Accuracy()
bar_plot(nn_cm,"Neural Network - Accuracy of classifier using Test data")


