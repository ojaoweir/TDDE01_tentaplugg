# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)
#Step1
getLLH = function(l, m) {
  fact = 0
  for(i in 1:m) {
    fact = fact + log(i)
  }
  return(-((-l) + m*log(l) - fact))
}
# P(y=Y|lambda) = exp(-lambda)*lambda^(Y) / Y!
# log = -lambda + Y*log(lambda) - log(factorial(Y))
minusLLH = function(mort, lambda) {
  mLLH_vals = rep(0,length(mort))
  for(i in 1:length(mort)) {
    mLLH_vals[i] = getLLH(lambda,mort[i])
  }
  return(sum(mLLH_vals))
}

Dataframe=read.csv2("Influenza.csv")
lambdas = seq(10,2910,100)
mLLH = rep(0,length(lambdas))
for(i in 1:length(lambdas)) {
  mLLH[i] = minusLLH(Dataframe$Mortality, lambdas[i])
}

plot(lambdas, mLLH, type="l", col="blue")

plot(lambdas, mLLH, type="l", col="blue", xlim = c(1500,2500))
# optimal lambda = 1800


#Step2
data1 = Dataframe
for(col in dim(data1)[2]) {
  data1[,col] = scale(data1[,col])
}
data1$Mortality = Dataframe$Mortality
n=dim(data1)[1]
id=sample(1:n, floor(n*0.5))
train=data1[id,]
test=data1[-id,]
test = test[-230,]

library('glmnet')
lambdas = seq(0,10,0.001)
fit_LASSO = glmnet(as.matrix(train[,-3]), train[,3],lambda = lambdas, alpha=1, family="poisson")
cv_lasso = cv.glmnet(as.matrix(train[,-3]), train[,3],lambda = lambdas, alpha=1, family="poisson")
plot(cv_lasso)
cv_coef = coef(cv_lasso, s = "lambda.1se")
plot(cv_lasso$lambda,cv_lasso$cvm)
#Best lambda = 5

mean_square_error = function(v1, v2) {
  SE = (v1-v2)
  SE = SE^2
  SE = mean(SE)
  return(SE)
}

prediction = predict(fit_LASSO, newx = as.matrix(test[,-3]), s = cv_lasso$lambda.1se)
MSE1 = mean_square_error(prediction, test$Mortality)

#step 3
library(tree)
n=dim(Dataframe)[1]
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,]
test = test[-230,]
fit_tree = tree(Mortality ~., data = train)
optree = cv.tree(fit_tree)
plot(optree$size, optree$dev, type="b")
fit_tree = prune.tree(fit_tree, best = 8)
prediction = predict(fit_tree, newdata = test)
MSE2 = mean_square_error(prediction, test$Mortality)

#step 4
data1 = Dataframe
n=dim(data1)[1]
id=sample(1:n, floor(n*0.5))
train=data1[id,]
train1 = train
train$Mortality = c()
test=data1[-id,]
test = test[-230,]
pca = prcomp(train)

lambda = pca$sdev^2
sprintf("%2.3f",lambda/sum(lambda)*100)
# 2st comps
lambdas = seq(0,50,0.1)
fit_LASSO = glmnet(as.matrix(pca$x), train1[,3],lambda = lambdas, alpha=1, family="poisson")
cv_lasso = cv.glmnet(as.matrix(pca$x), train1[,3], lambda = lambdas, alpha=1, family="poisson")
plot(log(cv_lasso$lambda),cv_lasso$cvm, type="b")
# It chooses 3 variables