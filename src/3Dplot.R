fit<-function(training_data){
  
  optimized_weight<-c()
  optimized_bias<-0
  
  transforms<-list(c(1, 1,1),c(1,1,-1), c(-1, 1,-1), c(-1, -1,-1), c(1, -1,-1)
                   ,c(1,-1,1),c(-1,1,1),c(-1,-1,1))
  
  #compute min and max of feature vectors
  
  min_feature_value<-min(training_data[,-4])
  max_feature_value<-max(training_data[,-4])
  print(min_feature_value)
  print(max_feature_value)
  
  step_sizes<-c(max_feature_value * 0.1, max_feature_value * 0.01)
  
  # extremely expensive
  b_range_multiple<-2
  # we dont need to take as small of steps
  # with b as we do w
  b_multiple<-5
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
      for (b in seq((max_feature_value * b_range_multiple),
                    max_feature_value * b_range_multiple,
                    step * b_multiple)){
        
        
        for (i in 1:length(transforms)) {
          w_t<-w*transforms[[i]]
          
          found_option<-TRUE
          counter <-0
          
          for(i in 1:nrow(training_data)){
            class<-training_data[i,4]
            dot_product<-w_t%*%training_data[i,-4]
            
            
            if((class*(dot_product+b))< 1){
              counter <- counter + 1
              found_option<-FALSE
            }
          }
          if(counter <=75){
            
            key<-norm(as.matrix(w_t), type="f")
            if((key <= min_key)) {
              print("Optimizing")
              min_key <- key
              b_value <- b
              w_t_value <- w_t
            }
          }
          
          if(found_option){
            
            key<-norm(as.matrix(w_t), type="f")
            if((key <= min_key)) {
              print("Optimizing")
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
    optimized_weight<-w_t_value
    optimized_bias<-b_value
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
    # sign( x.w+b )
    # dot product of every point in p with w
    # and then the sign
    classification<-sign((t(svm_fit_data[[1]])%*%(p)+svm_fit_data[[2]])) ## t(w)%*%p +b
    return_list[[pCnt]]<-classification
    #  TODO set visualization to true
    if(classification==-1){
     ## points(p[1],p[2],pch=24,bg='red')
     ## scatter_object$points3d(p[1],p[2],p[3],pch=24,col=c("red"))
      points3d(p[1],p[2],p[3],col='green')
    }
    else if(classification==1){
     ## points(p[1],p[2],pch=24,bg='black')
      points3d(p[1],p[2],p[3],col='yellow')
    }
    pCnt<-pCnt+1
  }
  
  return(return_list)
}


visualize<-function(training_data){
  x<-training_data[,1]
  y<-training_data[,2]
  z<-training_data[,3]
  
  for(i in 1:nrow(training_data)){
    if(training_data[i,4]==-1){
      training_data[i,4]<-2
    }
  }
  
  points3d(x,y,z,col=as.integer(training_data[,"Class"]))
}  

print("Main Program")

# Training the SVM
sample<-read.csv("data.csv",header=TRUE)
sample<-as.matrix(sample)
svm_fit_data<-fit(sample)


#Loading the test data
test_data_svm<-list(c(1,10,1),c(1,3,4),c(3,4,7),c(3,5,2),c(5,5,1),c(5,6,8),c(6,-5,1),c(6,-3,1),c(5,8,7), c(4,8,5))
test_sample<-read.csv("test_data.csv",header=TRUE)
test_sample<-as.matrix(test_sample)
test_data<- list()
for (i in 1:nrow(test_sample)) {
  test_data[[i]]<- c(test_sample[i,1],test_sample[i,2],test_sample[i,3]) 
}

#Hyper Plane
detalization = 100;
grid <- expand.grid(seq(from=min(sample[,1]),to=max(sample[,1]),length.out=detalization),                                                                                                         
                    seq(from=min(sample[,2]),to=max(sample[,2]),length.out=detalization)) 

z <- (-svm_fit_data[[2]] - svm_fit_data[[1]][1]*grid[,1] - (svm_fit_data[[1]][2])*grid[,2]) / (svm_fit_data[[1]][3])

plot3d(grid[,1],grid[,2],z,phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed", col= 'purple')

#Predicting the Class for Test Data
predict(test_data,svm_fit_data)

#Plotting Training Data
visualize(sample)