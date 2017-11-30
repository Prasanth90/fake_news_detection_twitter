## Data Preprocessing ................

## Cleaning Raw Dataset 

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
  
  return(rawdata)
}


## Feature Extraction ...........................

feature_extraction<-function(processed_rawdata){
  
  ## Giving names to columns in Processed Data Set
  names(processed_rawdata)<-c("Date","Tweet_Text","Tweet_id","User_id","User_Name","User_Screen_Name","Retweets","Favorites","Class")
  
  ## Removing white spaces in Tweet text
  processed_rawdata$Tweet_Text<-str_trim(processed_rawdata$Tweet_Text)
  
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

  
  visualizing_data<-data.frame("Retweets"=feature_extracted_data$Retweets,
                               "Favorites"=feature_extracted_data$Favorites,
                               "Class"=feature_extracted_data$Class)
  return(visualizing_data)
}

#Fit Function
fit<-function(training_data){
  
  optimized_weight<-c()
  optimized_bias<-0
  
  ## Possible combinations for initial value of weight vector
  transforms<-list(c(1, 1), c(-1, 1), c(-1, -1), c(1, -1))
  
  
  
  #Print Data Set to Console
  print("Training Data Set")
  print(training_data)
  
  #compute min and max value of feature vectors
  
  min_feature_value<-min(training_data[,-3])
  max_feature_value<-max(training_data[,-3])
  
  ## Selecting steps for weight vector
  step_sizes<-c(max_feature_value * 0.1, max_feature_value * 0.01)
  
  # Initial value of bias
  b_range_multiple<-2
  
  ## Seelecting increment value for bias at each step
  b_multiple<-5
  latest_optimum<-max_feature_value*10
  
  hm<-c()
  for (step in step_sizes){
    # Initial value of weight vector
    w<-c(latest_optimum,latest_optimum)
    
    optimized<-FALSE
    min_key <- 100000000
    b_value <-NULL
    w_t_value<-c()
    while(!optimized){
      for (b in seq(-1 * (max_feature_value * b_range_multiple),
                    max_feature_value * b_range_multiple,
                    step * b_multiple)){
        
        
        for (i in 1:length(transforms)) {
          w_t<-w*transforms[[i]]
          found_option<-TRUE
          counter<-0
          
          for(i in 1:nrow(training_data)){
            class<-training_data[i,3]
            ## Evaluating dot product t(w)*x
            dot_product<-as.matrix(training_data[i,-3])%*%w_t
            
            ## Evaluating constraint for linear svm yi(t(w)*x+b)
            if((class*(dot_product+b))< 1){
             
              
              counter<- counter +1
              found_option<-FALSE
            }
          }
          
          #
          if(counter<=10){
            # Calculating norm of weight vector
            # Optimising weight and bias
            key<-norm(as.matrix(w_t), type="f")
            if((key <= min_key)) {
             min_key <- key
              b_value <- b
              w_t_value <- w_t
            }
          }
          
          if(found_option){
            key<-norm(as.matrix(w_t), type="f")
            if((key <= min_key)) {
              min_key <- key
              b_value <- b
              w_t_value <- w_t
            }
          }
        }
      }
      
      if(w[1]<0){
        optimized<-TRUE
        print("Optimized a step")
      }
      # Reducing weight vector by a step value
      else{
        w<-w-step
      }
    }
    
    print(min_key)
    print(b_value)
    print(w_t_value)
    # Optimised weight value
    optimized_weight = w_t_value
    # Optimised bias value
    optimized_bias = b_value
    latest_optimum<-w_t_value[1] + (step*2)
  }
  return_list<-list("weight" <- optimized_weight, "bias" = optimized_bias,"min_feature_value" = min_feature_value,"max_feature_value" = max_feature_value)
  return(return_list)
}

##Predict Function 
predict<-function(predict_us,svm_fit_data){
  
  pCnt<-1  
  return_list<-list()
  for (p in predict_us){
    print(p)
    
    # Checking sign of t(w)*x+b
    classification<-sign((t(svm_fit_data[[1]])%*%(p)+svm_fit_data[[2]])) ## t(w)%*%p +b 
    return_list[[pCnt]]<-classification
    print(classification)
    
    # Classifying spam and non spam points
    if(classification==-1){
      points(p[1],p[2],pch=24,bg='red')  
    }
    else if(classification==1){
      points(p[1],p[2],pch=24,bg='black')
    }
    pCnt<-pCnt+1
  }
  return(return_list)
}



## visualize function for plotting training data 

visualize<-function(training_data,svm_fit_data){
  
  
  x<-training_data[,1]
  y<-training_data[,2]
  
  for(i in 1:nrow(training_data)){
    if(training_data[i,3]==-1){
      training_data[i,3]<-2
    }
  }
  
  #assigning the list data to seperate variables
  w<-svm_fit_data[[1]]
  b<-svm_fit_data[[2]]
  min_feature_value<-svm_fit_data[[3]]
  max_feature_value<-svm_fit_data[[4]]
  
  
  plot(x,y,pch=19,col=as.integer(training_data[,"Class"]),xlim=c(0,45),ylim=c(0,40),xlab="Retweets",ylab="Favorites",main=
         "Linear SVM Plot for 2 features - Retweets & Favorites")
  
  legend("right",legend=c("Spam", "Non-Spam"),col=c("black", "red"), lty=1:2, cex=0.8)
  
  #setting data range
  #datarange = (self.min_feature_value * 0.9, self.max_feature_value * 1.1)
  data_range <- list(as.numeric(min_feature_value) * 0.9,as.numeric(max_feature_value) * 1.1)
  hyp_x_min<-data_range[1]
  hyp_x_max<-data_range[2]
  
  
  #positive support vector hyperplane
  psv1<-hyperplane(hyp_x_min,w,b,1)
  psv2<-hyperplane(hyp_x_max, w,b, 1)
  hyp_x_v<-c(hyp_x_min,hyp_x_max)
  psv_v<-c(psv1,psv2)
  lines(hyp_x_v,psv_v,col='blue')
  
  # negative support vector hyperplane
  nsv1<-hyperplane(hyp_x_min, w, b, -1)
  nsv2 <- hyperplane(hyp_x_max, w, b, -1)
  nsv_v<-c(nsv1,nsv2)
  lines(hyp_x_v,nsv_v,col='blue')
  
  # separating hyperplane
  db1<-hyperplane(hyp_x_min, w, b, 0)
  db2 <- hyperplane(hyp_x_max, w, b, 0)
  db_v<-c(db1,db2)
  lines(hyp_x_v,db_v,col='yellow',lty=2)
} 

#hyperplane function
hyperplane <- function(x,w,b,v){
  result<-(-w[1]*as.numeric(x)-b+v)/w[2]
}

## Accuracy function

Accuracy<-function(input, predicted){
  
  
## Expected labels for test data
 
 actual<-input[,3]
  
   
 print(table("Real"=actual,"Predicted"=predicted))
  
  
}

print("Main Program")


library(stringr)
library(caret)
options(java.parameters = "-Xmx4096m")
options(print.max=100000)

## Loading Raw Data
rawdata<-read.csv("RawTrainingDataSet_2D.csv",header=FALSE)


## Loading swear words file
frame2<-read.csv("swear_words.csv",header=FALSE)
frame2<-as.matrix(frame2)

## Loading spam words file 

frame1<-read.csv("Spam_words.csv",header=FALSE)
frame1<-as.matrix(frame1)

processed_rawdata<-preprocessing(rawdata)
feature_extracted_data<-feature_extraction(processed_rawdata)
training_data<-feature_selection(feature_extracted_data)





## Store in some location ......
## Writing Cleaned Dataset File

write.table(processed_rawdata,"Cleaned.csv", sep=",",row.names=FALSE)

## Writing Feature Extracted Dataset File
write.table(feature_extracted_data,"Feature_Extraction_Dataset.csv", sep=",",row.names=FALSE)

## Writing Training Set file
write.table(training_data,"Training_Dataset.csv", sep=",",row.names=FALSE)



#Fitting the training data
svm_fit_data<-fit(training_data)
print(svm_fit_data[[1]])
print(svm_fit_data[[2]])

#Predicting the test data for classification
test_data<-read.csv("Test_Dataset_2D.csv",header=TRUE)
test_data_list<-list()

for(i in 1: nrow(test_data)){
  
test_data_list[[i]]<-c(test_data[i,1],test_data[i,2])

}

#visualize the hyperplane and training data
visualize(training_data,svm_fit_data)
#predict the test_data
predicted<-predict(test_data_list, svm_fit_data)
predicted<-unlist(predicted)
Accuracy(test_data,predicted)

