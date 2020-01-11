# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)

# ------------------ Assignment 3 ---------------------------------
# Implement an R function that performs feature selection (best subset selection) 
# in linear regression by using k-fold cross-validation without using any specialized 
# function like lm() (use only basic R functions). Your function should depend on:
#   X: matrix containing X measurements.
#   Y: vector containing Y measurements
#   Nfolds: number of folds in the cross-validation.
# You may assume in your code that matrix X has 5 columns. The function should plot the 
# CV scores computed for various feature subsets against the number of features, and it 
# should also return the optimal subset of features and the corresponding cross-validation (CV) score. 
# Before splitting into folds, the data should be permuted, and the seed 12345 should be used for that purpose.

# Report the resulting plot and interpret it. Report the optimal subset of features 
# and comment whether it is reasonable that these specific features have largest 
# impact on the target.
mylin=function(X,Y, Xpred){
  Xpred1 = cbind(1,Xpred)
  X1 = cbind(1, X)
  beta = solve( t(X1) %*% X1) %*% t(X1) %*% Y # obtained using the "training" data matrix X
  Res = Xpred1%*%beta # y_hat for the "test" data
  return(Res)
}
myCV=function(X,Y,Nfolds){
  n=length(Y) # number of observations (rows)
  p=ncol(X) # number of covariates (variables or columns)
  set.seed(12345)
  ind=sample(n,n) # indexes are randomized
  X1=X[ind,] # randomize the order of the observations
  Y1=Y[ind]
  sF=floor(n/Nfolds) # number of observations inside each fold
  MSE=numeric(2^p-1) # vector of the length of 2^p-1 combinations
  9
  Nfeat=numeric(2^p-1)
  Features=list() # features that will be selected
  curr=0 # current
  folds_obs <- cut(1:n,breaks=Nfolds,labels=FALSE)
  #we assume 5 features.
  for (f1 in 0:1)
    for (f2 in 0:1)
      for(f3 in 0:1)
        for(f4 in 0:1)
          for(f5 in 0:1){
            model= c(f1,f2,f3,f4,f5)
            if (sum(model)==0) next()
            SSE=0
            for (k in 1:Nfolds){
              #MISSING:compute which indices should belong to current fold
              indices <- ind[which(folds_obs==k)] #indeces of the observations in fold k
              #MISSING:implement cross-validation for model with features in "model" and iteration i.
              X_mylin <- X[-indices,which(model==1)]
              XPred_mylin <- X[indices,which(model==1)]
              Y_mylin <- Y[-indices]
              #MISSING:Get the predicted values for fold k, Ypred, and the original values for fold 'k', Yp.
              Ypred <- mylin(X_mylin, Y_mylin, XPred_mylin)
              Yp <- Y[indices]
              SSE=SSE+sum((Ypred-Yp)^2)
            }
            curr=curr+1
            MSE[curr]=SSE/n
            Nfeat[curr]=sum(model)
            Features[[curr]]=model
          }
  plot(Nfeat, MSE) #MISSING: plot MSE against number of features
  abline(h=MSE[which.min(MSE)], col="red")
  i=which.min(MSE)
  return(list(CV=MSE[i], Features=Features[[i]]))
}

# 2.Test your function on data set swiss available in the standard R repository:
#   Fertility should be Y
#   All other variables should be X
#   Nfolds should be 5
data("swiss")
myCV(as.matrix(swiss[,2:6]), swiss[[1]], 5)