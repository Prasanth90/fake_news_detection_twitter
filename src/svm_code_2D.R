## Data Preprocessing ................

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

feature_extraction<-function(processed_rawdata){
  
  
  names(processed_rawdata)<-c("Date","Tweet_Text","Tweet_id","User_id","User_Name","User_Screen_Name","Retweets","Favorites")
  
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
  
 ## for(i in 1:nrow(feature_extracted_data)){
 ##   feature_extracted_data[i,"New_Feature"]<-sum(
 ##     feature_extracted_data$`No_of_@ Mentions`[i],feature_extracted_data$No_of_Hashtags[i],
 ##     feature_extracted_data$No_of_Emoticons[i],feature_extracted_data$No_of_URL[i],
 ##     feature_extracted_data$No_of_Spam_Words[i],feature_extracted_data$No_of_Swear_Words[i])
    
    
##  }
  
  visualizing_data<-data.frame("Retweets"=feature_extracted_data$Retweets,
                               "Favorites"=feature_extracted_data$Favorites)
  return(visualizing_data)
}

#Fit Function
fit<-function(training_data){
  
  optimized_weight<-c()
  optimized_bias<-0
  
  transforms<-list(c(1, 1), c(-1, 1), c(-1, -1), c(1, -1))
  
  
  
  #Print Data Set to Console
  print("Training Data Set")
  print(training_data)
  
  #compute min and max of feature vectors
  
  min_feature_value<-min(training_data[,-3])
  max_feature_value<-max(training_data[,-3])
  
  
  step_sizes<-c(max_feature_value * 0.1, max_feature_value * 0.01)
  
  # extremely expensive
  b_range_multiple<-2
  # we dont need to take as small of steps
  # with b as we do w
  b_multiple<-5
  latest_optimum<-max_feature_value*10
  
  hm<-c()
  for (step in step_sizes){
    w<-c(latest_optimum,latest_optimum)
    # we can do this because convex
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
            dot_product<-w_t%*%training_data[i,-3]
            #print(dot_product + b)
            if((class*(dot_product+b))< 1){
              #print("FOUND_OPTION_FALSE")
              found_option<-FALSE
              counter<- counter +1
            }
          }
          
          if(counter<=3){
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
      else{
        w<-w-step
      }
    }
    
    print(min_key)
    print(b_value)
    print(w_t_value)
    optimized_weight = w_t_value
    optimized_bias = b_value
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
    print(p)
    # sign( x.w+b )
    # dot product of every point in p with w
    # and then the sign
    classification<-sign((t(svm_fit_data[[1]])%*%(p)+svm_fit_data[[2]])) ## t(w)%*%p +b 
    return_list[[pCnt]]<-classification
    print(classification)
    #  TODO set visualization to true
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
  
  
  plot(x,y,pch=19,col=as.integer(training_data[,"Class"]),xlim=c(0,25),ylim=c(0,25))
  
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
  
  # positive support vector hyperplane
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
 
 ## print(svm_fit_data)
  
  ## --- Change according to this ----
  actual<-input[,4]
  
  ## confusionMatrix(actual,prediction)
  table("Real"=actual,"Predicted"=predicted)
  
  
}

print("Main Program")

#setwd("C:/Users/Simarpreet Singh/Desktop/Data Mining Project")

library(stringr)
library(caret)
options(java.parameters = "-Xmx4096m")
options(print.max=100000)

## Loading Raw Data
rawdata<-read.csv("RawDataset.csv",header=FALSE)


## Loading swear words file
frame2<-read.csv("swear_words.csv",header=FALSE)
frame2<-as.matrix(frame2)

## Loading spam words file 

frame1<-read.csv("Spam_words.csv",header=FALSE)
frame1<-as.matrix(frame1)

##processed_rawdata<-preprocessing(rawdata)
##feature_extracted_data<-feature_extraction(processed_rawdata)
##training_data<-feature_selection(feature_extracted_data)



## Store in some location ......
## Writing Cleaned Dataset File

write.table(processed_rawdata,"C:/Users/Simarpreet Singh/Desktop/Data Mining Project/Cleaned.csv", sep=",",row.names=FALSE)

## Writing Feature Extracted Dataset File
write.table(feature_extracted_data,"C:/Users/Simarpreet Singh/Desktop/Data Mining Project/Feature_Extraction_Dataset.csv", sep=",",row.names=FALSE)

## Writing Training Set file
write.table(training_data,"C:/Users/Simarpreet Singh/Desktop/Data Mining Project/Training_Dataset.csv", sep=",",row.names=FALSE)



#Training Data
## ------------- Temporary code -------------------
training_data<-read.csv("Training_Dataset_Anil.csv",header=TRUE)
training_data<-as.matrix(training_data)

#Fitting the training data
svm_fit_data<-fit(training_data)
print(svm_fit_data[[1]])
print(svm_fit_data[[2]])

#Predicting the future data for classification
test_data<-list(c(1,10),c(1,3),c(-3,4),c(3,5),c(5,5),c(5,-8),c(6,-5),c(5,8))
#visulaize the hyperplane and training data
visualize(training_data,svm_fit_data)
#predict the test_data
predicted_list<-svm_predict(test_data, svm_fit_data)
predicted_list<-unlist(predicted_list)
Accuracy(test_data,predicted)
