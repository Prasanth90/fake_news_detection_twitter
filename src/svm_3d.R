## Data Preprocessing ................
library(rgl)
library(stringr)
library(stringi)
library(caret)
options(print.max=100000)

preprocessing<-function(rawdata){
  
  ## Changing Datatypes
  
  rawdata[,7]<-as.integer(rawdata[,7])
  rawdata[,8]<-as.integer(rawdata[,8])
  rawdata[,3]<-as.numeric(rawdata[,3])
  rawdata[,4]<-as.character(rawdata[,4])
  rawdata[,6]<-as.character(rawdata[,6])
  rawdata[,1]<-as.character(rawdata[,1])
  rawdata[,2]<-as.character(rawdata[,2])
  rawdata[,5]<-as.character(rawdata[,5])
  
  ## Identifying columns with Missing Values
  colSums(is.na(rawdata))
  
  ## Selecting only rows with complete values
  rawdata<-rawdata[complete.cases(rawdata),]
  
  ## Parsing Date Column in Raw Data
  
  for(i in 1:nrow(rawdata)){
    position1<-regexpr('\\[',rawdata[i,1])
    position2<-regexpr('\\]',rawdata[i,1])
    rawdata[i,1]<-substr(rawdata[i,1],position1[1]+1,position2[1]-1)
    
    
  }
  ##  rawdata[,1]<-as.Date(rawdata[,1])
  return(rawdata)
}


## Feature Extraction ...........................

feature_extraction<-function(processed_rawdata, frame1, frame2){
  
  
  names(processed_rawdata)<-c("Date","Tweet_Text","Tweet_id","User_id","User_Name","User_Screen_Name","Retweets","Favorites","Class")
  
  ## Removing white spaces in Tweet text
  
  
  processed_rawdata$Tweet_Text<-stri_trim(stri_enc_toutf8(processed_rawdata$Tweet_Text, validate = TRUE))
  
  ## Finding Length of Tweet Text
  processed_rawdata["Length of Tweet"]<-nchar(processed_rawdata$Tweet_Text)
  
  ## Finding Number of @ Mentions
  processed_rawdata["No_of_@ Mentions"]<-str_count(processed_rawdata$Tweet_Text,pattern="@")
  
  ## Finding Number of Hashtags
  processed_rawdata["No_of_Hashtags"]<-str_count(processed_rawdata$Tweet_Text,pattern="#")
  
  ## Finding Length of Screen Name
  processed_rawdata["Length of Screen Name"]<-nchar(processed_rawdata$User_Screen_Name)
  
  ## ********** For Finding No of Emoticons **********
  
  emoticons <- c(":\\)",":-\\(","\\):",":S","o_O","=D")
  
  processed_rawdata["No_of_Emoticons"]<-str_count(processed_rawdata$Tweet_Text,paste0(emoticons, collapse="|"))
  
  ## *********** For Finding No of URL ***********
  processed_rawdata["No_of_URL"]<-str_count(processed_rawdata$Tweet_Text,pattern="http://|https://")
  
  ## To find frequency of Spam Words
  
  tweettext<-as.vector(processed_rawdata$Tweet_Text)
  
  counter<-0
  for(j in 1:length(tweettext)){
    
    for(i in 1:nrow(frame1)){
      count_spam_words<-str_count(tweettext[j],coll(pattern=frame1[i,1],ignore_case=TRUE))
      counter<-counter+count_spam_words
      
    }
    processed_rawdata["No_of_Spam_Words"]<-counter
    counter<-0
  }
  
  ##  To find frequency of Swear Words
  
  counter2<-0
  for(j in 1:length(tweettext)){
    
    for(i in 1:nrow(frame2)){
      count_swear_words<-str_count(tweettext[j],coll(pattern=frame2[i,1],ignore_case=TRUE))
      counter2<-counter2+count_swear_words
      
    }
    processed_rawdata["No_of_Swear_Words"]<-counter2
    counter2<-0
  }
  return(processed_rawdata)
}

## Feature Selection....

feature_selection<-function(feature_extracted_data){
  
  for(i in 1:nrow(feature_extracted_data)){
    feature_extracted_data[i,"New_Feature"]<-sum(
      feature_extracted_data$`No_of_@ Mentions`[i],feature_extracted_data$No_of_Hashtags[i],
      feature_extracted_data$No_of_Emoticons[i],feature_extracted_data$No_of_URL[i],
      feature_extracted_data$No_of_Spam_Words[i],feature_extracted_data$No_of_Swear_Words[i])
    
    
  }
  
  visualizing_data<-data.frame("Retweets"=feature_extracted_data$Retweets,
                               "Favorites"=feature_extracted_data$Favorites,"New_Feature"=feature_extracted_data$New_Feature,"Class"=feature_extracted_data$Class)
  return(visualizing_data)
}


fit<-function(training_data){
  
  optimized_weight<-c()
  optimized_bias<-0
  
  # Assuming all the possible transforms for the 3d data (To find which quadrant the data lies)
  transforms<-list(c(1, 1,1),c(1,1,-1), c(-1, 1,-1), c(-1, -1,-1), c(1, -1,-1)
                   ,c(1,-1,1),c(-1,1,1),c(-1,-1,1))
  
  #compute min and max of feature vectors
  min_feature_value<-min(training_data[,-4])
  max_feature_value<-max(training_data[,-4])
  print(min_feature_value)
  print(max_feature_value)
  
  #Asssuming step sizes based on Max Feature value;
  step_sizes<-c(max_feature_value * 0.1, max_feature_value * 0.01)
  
  # extremely expensive
  b_range_multiple<-2
  # we dont need to take as small of steps
  # with b as we do w
  b_multiple<-5

  #Assuming the latest optimum based on the Max feature value
  latest_optimum<-max_feature_value*10
  
  hm<-c()
  for (step in step_sizes){
    w<-c(latest_optimum,latest_optimum,latest_optimum)
    # we can do this because convex
    optimized<-FALSE
    min_key <- 100000000
    b_value <-NULL
    w_t_value<-c()
    while(!optimized){
      #Iterating for all combinations of b
      for (b in seq((max_feature_value * b_range_multiple),
                    max_feature_value * b_range_multiple,
                    step * b_multiple)){
        
        
        for (i in 1:length(transforms)) {
          # Multiply with assumed value of w with each transform
          w_t<-w*transforms[[i]]
          
          found_option<-TRUE
          counter <-0
          
          for(i in 1:nrow(training_data)){
            class<-training_data[i,4]
            #Find the Dot product of all the feature with the computed weight matrix
            dot_product<-w_t%*%training_data[i,-4]
            
            
            if((class*(dot_product+b))< 1){
              # If any feature value's dot product is less than 1, then ignore that weight matrix
              counter <- counter + 1
              found_option<-FALSE
            }
          }
          # Consider a threshold for the error rate
          if(counter <=63){
            #Compute the normaized value for the Weight Matrix
            key<-norm(as.matrix(w_t), type="f")
            if((key <= min_key)) {
              #print("Optimizing")
              # If the newly computed weight matrix is less, then it is more optimized than the previous one
              min_key <- key
              b_value <- b
              w_t_value <- w_t
            }
          }
          
          if(found_option){
            
            key<-norm(as.matrix(w_t), type="f")
            if((key <= min_key)) {
              #print("Optimizing")
              min_key <- key
              b_value <- b
              w_t_value <- w_t
            }
          }
        }
      }
      
      if(w[1]<0){
        optimized<-TRUE
        print("Optimized Weight")
      }
      else{
        w<-w-step
      }
    }
    
    print(min_key)
    print(b_value)
    print(w_t_value)
    optimized_weight<-w_t_value
    optimized_bias<-b_value
    latest_optimum<-w_t_value[1] + (step*2)
  }
  return_list<-list("weight" <- optimized_weight, "bias" = optimized_bias,"min_feature_value" = min_feature_value,"max_feature_value" = max_feature_value)
  return(return_list)
}

##Predict Function 
svm_predict<-function(predict_us,svm_fit_data){
  pCnt<-1  
  return_list<-list()
  for (p in predict_us){
    # sign( x.w+b )
    # dot product of every point in p with w
    # and then the sign
    classification<-sign((t(svm_fit_data[[1]])%*%(p)+svm_fit_data[[2]])) ## t(w)%*%p +b
    return_list[[pCnt]]<-classification
    #  TODO set visualization to true
    if(classification==-1){
     ## points(p[1],p[2],pch=24,bg='red')
     ## scatter_object$points3d(p[1],p[2],p[3],pch=24,col=c("red"))
      #points3d(p[1],p[2],p[3],col='green', size = 6)
    }
    else if(classification==1){
     ## points(p[1],p[2],pch=24,bg='black')
      #points3d(p[1],p[2],p[3],col='yellow', size = 6)
    }
    pCnt<-pCnt+1
  }
  
  return(return_list)
}

visualize<-function(training_data, color_spam, color_non_spam){
  x<-training_data[,1]
  y<-training_data[,2]
  z<-training_data[,3]
  
  for(i in 1:nrow(training_data)){
    if(training_data[i,4]==-1){
      training_data[i,4]<- color_spam
    }
    if(training_data[i,4]==1){
      training_data[i,4]<- color_non_spam
    }
  }
  
  # Plotting the data samples for the feature vector
  points3d(x,y,z,col=training_data[,"Class"], size = 6)
}

Accuracy<-function(input, predicted){
  ## Expected labels for test data
  ## print(svm_fit_data)
  ## --- Change according to this ----
  actual<-input[,4]
  #confusionMatrix(actual,predicted)
  table("Real"=actual,"Predicted"=predicted)
}


# A generic function to plot the Bar plot for accuracy comparisons
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


process<-function(input_file_name, prefix){
  options(java.parameters = "-Xmx4096m")
  
  rawdata<-read.csv(input_file_name, header=FALSE)
  ##names(rawdata)<-c("Date","Tweet_Text","Tweet_id","User_id","User_Name","User_Screen_Name","Retweets","Favorites")
  
  ## Loading swear words file
  frame2<-read.csv("swear_words.csv",header=FALSE)
  frame2<-as.matrix(frame2)
  
  ## Loading spam words file 
  
  frame1<-read.csv("Spam_words.csv",header=FALSE)
  frame1<-as.matrix(frame1)
  
  processed_rawdata<-preprocessing(rawdata)
  feature_extracted_data<-feature_extraction(processed_rawdata, frame1, frame2)
  training_data<-feature_selection(feature_extracted_data)
  
  
  
  ## Store in some location ......
  ## Writing Cleaned Dataset File
  
  write.table(processed_rawdata,paste(prefix, "Cleaned.csv",sep="_"), sep=",",row.names=FALSE)
  
  ## Writing Feature Extracted Dataset File
  write.table(feature_extracted_data,paste(prefix,"Feature_Extraction_Dataset.csv",sep="_"), sep=",",row.names=FALSE)
  
  ## Writing Training Set file
  write.table(training_data, paste(prefix,"Dataset.csv", sep="_"), sep=",",row.names=FALSE)
}

print("Main Program")

process("RawTrainingDataSet.csv", "Training")
process("RawTestDataSet.csv", "Test")

# Training the SVM
sample<-read.csv("Training_Dataset.csv",header=TRUE)
sample<-as.matrix(sample)
svm_time_fit<-system.time(svm_fit_data<-fit(sample))
sample_data<- list()
for (i in 1:nrow(sample)) {
  sample_data[[i]]<- c(sample[i,1],sample[i,2],sample[i,3]) 
}


#Loading the test data
#test_data_svm<-list(c(1,10,1),c(1,3,4),c(3,4,7),c(3,5,2),c(5,5,1),c(5,6,8),c(6,-5,1),c(6,-3,1),c(5,8,7), c(4,8,5))
test_sample<-read.csv("Test_Dataset.csv",header=TRUE)
test_sample<-as.matrix(test_sample)
test_data<- list()
for (i in 1:nrow(test_sample)) {
  test_data[[i]]<- c(test_sample[i,1],test_sample[i,2],test_sample[i,3]) 
}

sample_combined <- rbind(sample, test_sample)

#Hyper Plane
detalization = 100;
grid <- expand.grid(seq(from=min(sample_combined[,1]),to=max(sample_combined[,1]),length.out=detalization),                                                                                                         
                    seq(from=min(sample_combined[,2]),to=max(sample_combined[,2]),length.out=detalization)) 

z <- (-svm_fit_data[[2]] - svm_fit_data[[1]][1]*grid[,1] - (svm_fit_data[[1]][2])*grid[,2]) / (svm_fit_data[[1]][3])

plot3d(grid[,1],grid[,2],z, bty = "g", top = TRUE, sub = "SVM Classifier (3 features) for Spam detection - Hyperplane", box = TRUE, axes = TRUE, cex = 2, ticktype = "detailed", col= 'purple', xlab = "Retweets" , ylab = "MultiFeatures", zlab = "Favourites" )
grid3d(c("x", "y+", "z"), lty=2, col = "steelblue", n = 20)

#Plotting Training Data
visualize(sample, 'red', 'black')

visualize(test_sample, 'yellow', 'green')

#Predicting the Class for Test Data
svm_time_predict<-system.time(predicted_list<-svm_predict(test_data, svm_fit_data))
predicted_list<-unlist(predicted_list)
cm<-Accuracy(test_sample,predicted_list)

#Predicting the Class for training Data
predicted_list_train_1<-svm_predict(sample_data, svm_fit_data)
predicted_list_train_1<-unlist(predicted_list_train_1)
cm_training<-Accuracy(sample, predicted_list_train_1)

par(mfrow=c(1,3))
bar_plot(cm_training, "SVM - Accuracy of classifier using Training data")
bar_plot(cm, "SVM - Accuracy of classifier using Test data")

time_for_inbuilt_svm<-system.time(source('svm_inbuilt.R'))
time_for_impl_svm <- svm_time_fit + svm_time_predict

barplot(c(time_for_inbuilt_svm[[3]], time_for_impl_svm[[3]]), main="Comparison of time between inbuilt and Custom  Built SVM", 
        ylim=c(0,10), xlab="Classifiers", ylab="Accuracy", names.arg= c("InBuilt", "Custom") ,col=c('green','yellow'))


return(cm)