training_data<-read.csv("svm_example.csv",header=TRUE)
training_data<-as.matrix(training_data)

## Spam Dataset

# spam<-subset(data,Class==-1)

## Non-spam dataset

# nonspam<-subset(data,Class==1)*/
  
## Combining training sets from both the classes

## training_data<-list(s=spam,ns=nonspam)

## w vector transforms


function<-fit(training_data){
transforms<-list(c(1, 1), c(-1, 1), c(-1, -1), c(1, -1))



#Print Data Set to Console
print("Training Data Set")
print(training_data)

#compute min and max of feature vectors

min_feature_value<-min(training_data[,-3])
max_feature_value<-max(training_data[,-3])

print(max_feature_value)

step_sizes<-c(max_feature_value * 0.1, max_feature_value * 0.01, 
              max_feature_value * 0.001)

# extremely expensive
b_range_multiple<-2
# we dont need to take as small of steps
# with b as we do w
b_multiple<-5
latest_optimum<-max_feature_value*10


for (step in step_sizes){
  w<-c(latest_optimum,latest_optimum)
  # we can do this because convex
  optimized<-FALSE
  while(!optimized){
    for (b in seq(-1 * (max_feature_value * b_range_multiple),
                  max_feature_value * b_range_multiple,
                  step * b_multiple)){
      ## print(b)
      for (i in 1:length(transforms)) {
        w_t<-w*transforms[[i]]
        found_option<-TRUE
        for(i in 1:nrow(training_data)){
            class<-training_data[i,3]
           dot_product<-w_t%*%training_data[i,-3]
            for(j in 1:length(b)){
            if((class*(dot_product+b[j]))< 1){
              found_option<-FALSE
            }
              if(found_option){
                
               ## Hash Table code here  
              
              }
              if(w[1]<0){
                
                optimized<-TRUE
                print("Optimized a step")
              }
              else{
                w<-w-step
              }
              ## Sorting of keys in Hash Table code here
            }  
        }
      }
    }
  }  
      for(i in 1:nrow(training_data)){
        class<-training_data[i,3]
        dot_product<-w_t%*%training_data[i,-3]
        for(j in 1:length(b)){
          print((class*(dot_product+b[j])))
            
          }
      }
  }
  




predict_us<-list(c(1,10),c(1,3),c(3,4),c(3,5),c(5,5),c(5,6),c(6,-5),c(5,8))
## Predict Function 

function<-predict(predict_us){

for (p in predict_us){
  print(p)
  # sign( x.w+b )
  # dot product of every point in p with w
  # and then the sign
  
  classification<-sign((t(w)%*%(p)+b) ## it should be t(w)%*%p +b 
                       print(classification)
                       #  TODO set visualization to true
                       if(classification!=0){
                         plot(p)  
                       }
}





