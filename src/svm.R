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
          for(i in 1:nrow(training_data)){
            class<-training_data[i,3]
            dot_product<-w_t%*%training_data[i,-3]
            #print(dot_product + b)
            if((class*(dot_product+b))< 1){
              #print("FOUND_OPTION_FALSE")
              found_option<-FALSE
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
  
  
  for (p in predict_us){
    print(p)
    # sign( x.w+b )
    # dot product of every point in p with w
    # and then the sign
    classification<-sign((t(svm_fit_data[[1]])%*%(p)+svm_fit_data[[2]])) ## t(w)%*%p +b 
    print(classification)
    #  TODO set visualization to true
    if(classification==-1){
      points(p[1],p[2],pch=24,bg='red')  
    }
    else if(classification==1){
      points(p[1],p[2],pch=24,bg='black')
    }
  }
  
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
  
  
  plot(x,y,pch=19,col=as.integer(training_data[,"Class"]),xlim=c(0,8),ylim=c(-5,12.5))
  
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



print("Main Program")

#Training Data
training_data<-read.csv("svm_example.csv",header=TRUE)
training_data<-as.matrix(training_data)

#Fitting the training data
svm_fit_data<-fit(training_data)
print(svm_fit_data[[1]])
print(svm_fit_data[[2]])

#Predicting the future data for classification
test_data<-list(c(1,10),c(1,3),c(3,4),c(3,5),c(5,5),c(5,6),c(6,-5),c(5,8))
#visulaize the hyperplane and training data
visualize(training_data,svm_fit_data)
#predict the test_data
svm_predict(test_data, svm_fit_data)