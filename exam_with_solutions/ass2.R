# Remove all saved variables and plots 
dev.off() 
rm(list=ls()) 
# Setting up rng and seed 
RNGversion('3.5.1') 
set.seed(12345)
library(kernlab)
C = seq(0.5,1.5, 0.5)
data(spam)
data1 <- spam
n=dim(data1)[1] 
id=sample(1:n, floor(n*0.70)) 
train=data1[id,] 
test=data1[-id,]

missclass=function(X,X1){ 
  n=length(X) 
  return(1-sum(diag(table(X,X1)))/n) 
  }

ksvm_model <- function(c_in) { 
  model_svm = ksvm(type~., data = train, kernel = "rbfdot", kpar=list(sigma=0.05), C = c_in) 
  prediction =predict(model_svm, test) 
  print(paste0("misslcass for ", c_in, ": ", missclass(prediction,test$type)))
  }

for(val in C) {
  ksvm_model(val)
}

#Final svm, bets value for c = 1
model_svm = ksvm(type~., data = train, kernel = "rbfdot", kpar=list(sigma=0.05), C = 1) 