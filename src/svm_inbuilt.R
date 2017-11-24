library(e1071)
library(caret)

## Loading Annotated Dataset

dataset<-read.csv("Training_Dataset.csv",header=TRUE)

## Taking columns  as features
x<-matrix(c(dataset$Retweets,dataset$Favorites,dataset$New_Feature),ncol=3)

## Class label column to be predicted

y<-dataset$Class

## Creating Model using inbuilt svm function

svm_model<-svm(x,y, type = 'C', scale=FALSE, kernel = 'linear')

print(summary(svm_model))

## Predicting on training data using the model created above

## For assigning weights :

weights<-svm_model$coefs


testdataset<-read.csv("Test_Dataset.csv",header=TRUE)
pred<-predict(svm_model,testdataset[,-4],decision.values = TRUE)

confusion_matrix_svm_inbuilt<-table(pred,testdataset[,4])

print(confusion_matrix_svm_inbuilt)