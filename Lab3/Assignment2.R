# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)


# ------------------ Assignment 2 ---------------------------------
# Use the function ksvm from the R package kernlab to learn a SVM for classifying the 
# spam dataset that is included with the package. Consider the radial basis function 
# kernel (also known as Gaussian) with a width of 0.05. For the C parameter, consider
# values 0.5, 1 and 5. This implies that you have to consider three models. 
#   - Perform model selection, i.e. select the most promising of the three models 
#(use any method of your choice except cross-validation or nested cross-validation).
#   - Estimate the generalization error of the SVM selected above (use any method of
# your choice except cross-validation or nested cross-validation).
#   - Produce the SVM that will be returned to the user, i.e. show the code.
#   - What is the purpose of the parameter C ?

data(spam)
data <- spam
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.70))
train=data[id,]
test=data[-id,]
ksvm.model <- function(x) {
  model <- ksvm(type~., data = train, kernel = "rbfdot", kpar=list(sigma=0.05), C = x)
  pred.model <- predict(model, test)
  confmat <- table(test$type, pred.model)
  misclass <- 1 - sum(diag(confmat)/sum(confmat))
  cat("\nModel misclassification rate for C = ", x, "is: ", misclass)
  confmat
}
ksvm.model(0.5)
ksvm.model(1)
ksvm.model(5)
model.new <- ksvm(type~., data = train, kernel = "rbfdot", kpar=list(sigma=0.05), C = 1)
model.new