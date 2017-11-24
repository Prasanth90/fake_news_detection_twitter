#Computes the accuracy of the given confusion matrix
#for the bar_plot input
compute_accuracy<-function(cm) {
  accuracy<-(cm[1] + cm[4])*100/(cm[1]+cm[2]+cm[3]+cm[4])
}
#Executing svm_3d code which returns the confusion matrix
svm_confusion_matrix<-source('svm_3d.R')
svm_accuracy<- compute_accuracy(svm_confusion_matrix$value)

#Executing KNN code which returns the confusion matrix
knn_confusion_matrix<-source('compared_algorithms/KNN.R')
knn_accuracy<- compute_accuracy(knn_confusion_matrix$value)

#Executing Decision tree code which returns the confusion matrix
dt_confusion_matrix<-source('compared_algorithms/DT.R')
dt_accuracy<-compute_accuracy(dt_confusion_matrix$value)

#Executing Naive bayes code which returns the confusion matrix
naivebayes_confusion_matrix<-source('compared_algorithms/Naive_Bayes Comparison.R')
naivebayes_accuracy<- compute_accuracy(naivebayes_confusion_matrix$value)

#Executing Neural network code which returns the confusion matrix
neuralnetwork_confusion_matrix<-source('compared_algorithms/neural_net.R')
neuralnetwork_accuracy<-compute_accuracy(neuralnetwork_confusion_matrix$value)

#Classifier names and their accuracy for the test data
classifier_names = c("SVM","KNN","NB","DT", "NN")
classifier_accuracy = c(svm_accuracy, knn_accuracy,naivebayes_accuracy,dt_accuracy, neuralnetwork_accuracy)

#Comparing the accuracy of the algorithms
#barplot for all the given classifiers
barplot(classifier_accuracy, main="Comparison of accuracy of different classifiers", 
        ylim=c(0,100), xlab="Classifiers", ylab="Accuracy", names.arg= classifier_names ,col=c('green','yellow','orange','blue', 'red'))
axis(2,at=seq(0,100,10))