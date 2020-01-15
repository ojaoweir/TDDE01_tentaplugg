# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)
data(spam)
library(kernlab)

mean_square_error = function(v1, v2) {
  SE = (v1-v2)
  SE = SE^2
  SE = mean(SE)
  return(SE)
}

missclass_func=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

DataFrame <- spam
n=dim(DataFrame)[1]
id=sample(1:n, floor(n*0.5))
train=DataFrame[id,]
test=DataFrame[-id,]
c_vector =  seq(0.1,3,0.1)
missclass = rep(0, length(c_vector))
MSE = rep(0, length(c_vector))
for(c in 1:length(c_vector)) {
  svm = ksvm(type~., data = train, kernel = rbfdot, kpar = list(sigma=0.05), C = c)
  prediction = predict(svm, test)
  missclass[c] = missclass_func(test$type, prediction)
}
# Lowest missclass at C = 0.05
# Misslass for this is 0.787