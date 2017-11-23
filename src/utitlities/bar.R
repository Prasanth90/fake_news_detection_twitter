correct<-c(1000,1500)
wrong<-c(50,75)
m<-c(correct,wrong)
mat<- matrix(m,nrow=2,ncol=2,byrow=TRUE)
rownames(mat)<-c("Correct", "Wrong")
colnames(mat)<-c("Spam", "NonSpam")


par(mfrow=c(2,2))
barplot(mat,main="Accuracy of classifier using training data",
        xlab="Classes", col=c("green","red"), legend = rownames(mat))
barplot(mat,main="Accuracy of classifier using training data",
        xlab="Classes", col=c("green","red"), legend = rownames(mat))