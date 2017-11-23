compute_accuracy<-function(cm) {
  accuracy<-(cm[1] + cm[4])*100/(cm[1]+cm[2]+cm[3]+cm[4])
}

svm_confusion_matrix<-source('svm_3d.R')
svm_accuracy<- compute_accuracy(svm_confusion_matrix$value)

knn_confusion_matrix<-source('compared_algorithms/KNN.R')
knn_accuracy<- compute_accuracy(knn_confusion_matrix$value)

naivebayes_confusion_matrix<-source('compared_algorithms/Naive_Bayes Comparison.R')
naivebayes_accuracy<- compute_accuracy(naivebayes_confusion_matrix$value)

#Classifier names and their accuracy for the test data
classifier_names = c("SVM", "KNN","Naive Bayes")
classifier_accuracy = c(svm_accuracy, knn_accuracy,naivebayes_accuracy)

#Comparing the accuracy of the algorithms
barplot(classifier_accuracy, main="Comparison of accuracy of different classifiers", 
        ylim=c(0,100), xlab="Classifiers", ylab="Accuracy", names.arg= classifier_names ,col=c('green','yellow','orange'))
axis(2,at=seq(0,100,10))