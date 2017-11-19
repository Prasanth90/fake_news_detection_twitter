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
  
  return_list<-list("weight" = optimized_weight, "bias" = optimized_bias)
  return(return_list)
}

##Predict Function 
predict<-function(predict_us, svm_fit_data){

  for (p in predict_us){
    print(p)
    # sign( x.w+b )
    # dot product of every point in p with w
    # and then the sign
    classification<-sign((t(svm_fit_data[[1]])%*%(p)+svm_fit_data[[2]])) ## it should be t(w)%*%p +b 
                         print(classification)
                         #  TODO set visualization to true
                         if(classification!=0){
                           plot(p)  
                         }
  }

}


##Visualize function
visualize<-function(data){
    x_coord<-data[,1]
    y_coord<-data[,2]
    for(i in 1:nrow(training_data)){
    if(data[i,"Class"]==-1){
      print(data[i,"Class"])
    plot(x_coord,y_coord,type="p",pch=19,col="red")
    }
    else if(data[i,"Class"]==1){
      print(data[i,"Class"])
      points(x_coord,y_coord,type="p",pch=19,col="yellow")
    }
    }  
  }
  
  
print("Main Program")

#Training Data
training_data<-read.csv("svm_example.csv",header=TRUE)
training_data<-as.matrix(training_data)

#Fitting the training data
svm_fit_data<-fit(training_data)
print(svm_fit_data[[1]])
print(svm_fit_data[[2]])

#Predicting the future data
test_data<-list(c(1,10),c(1,3),c(3,4),c(3,5),c(5,5),c(5,6),c(6,-5),c(5,8))
predict(test_data, svm_fit_data)