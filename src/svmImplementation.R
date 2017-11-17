#R version 3.3.2 

#Spam Data Set
spam = list(c(1,7),
            c(2,8),
            c(3,8))

#Non Spam Data Set
nonspam = list(c(5,1),
               c(6,-1),
               c(7,3))

transforms = list(c(1, 1), c(-1, 1), c(-1, -1), c(1, -1))

#Combining training sets from both the classess
training_data = list(s = spam , ns = nonspam)

#Print Data Set to Console
#print("Training Data Set")
#print(training_data)


#Flattening data to compute min and max of feature vectors
flat_data = c()
for (data in training_data ){
  for (d in data) {
    flat_data <- c(flat_data, d)
  }
}


min_feature_value = min(flat_data)
max_feature_value = max(flat_data)

#print("Min and Max data")
#print(min_feature_value)
#print(max_feature_value)

opt_dict = list()
step_sizes = c(max_feature_value * 0.1, max_feature_value * 0.01, max_feature_value * 0.001)

#print("Step Sizes")
#print(step_sizes)


# extremely expensive
b_range_multiple = 2
# we dont need to take as small of steps
# with b as we do w
b_multiple = 5
latest_optimum = max_feature_value*10
for(step in step_sizes) {
  w = c(latest_optimum,latest_optimum)
  # we can do this because convex
  optimized = FALSE
  while (!optimized){
    for (b in seq(-1*max_feature_value*b_range_multiple, max_feature_value*b_range_multiple, step*b_multiple)) {
      #print(b)
      optimized = TRUE
      for (transformation in transforms) {
        w_t = w * transformation
        found_option = TRUE
        for (i in seq_along(training_data)) {
          if(i==1){
            i=1
          }else{
            i=-1
          }
          #print(i)
          for (xi in training_data[i]) {
            #print(as.matrix(sapply(xi, as.numeric)))
            yi<-i
            #TO check-DOUBT
            c1<-(t(w_t)%*%as.matrix(sapply(xi, as.numeric)))+b
            #print(c1)
            c2 = yi * c1
            print(c2)
            break
          }
          if(found_option){
            
          }
          break
        }
        break
      }
    }
  }
}