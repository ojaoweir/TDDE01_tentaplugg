# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)


# ------------------ Assignment 3 ---------------------------------
# The data file State.csv contains per capita state and local public 
# expenditures and associated state demographic and economic 
# characteristics, 1960, and there are variables
#   MET: Percentage of population living in standard metropolitan areas
#   EX: Per capita state and local public expenditures ($)
library(knitr)
RNGversion('3.5.1')
set.seed(12345)
# Reorder your data with respect to the increase of MET and plot EX 
# versus MET. Discuss what kind of model can be appropriate here. 
# Use the reordered data in steps 2-5.
data2 <-read.csv2("State.csv")
data2 <- data2[order(data2$MET),]
plot(data2$MET,data2$EX, xlab = "MET",ylab = "EX",
     main = "MET vs. EX")

# 2. Use package tree and fit a regression tree model with target 
# EX and feature MET in which the number of the leaves is selected 
# by cross-validation, use the entire data set and set minimum 
# number of observations in a leaf equal to 8 (setting minsize in tree.control). 
# Report the selected tree. Plot the original and the fitted data and 
# histogram of residuals. Comment on the distribution of the 
# residuals and the quality of the fit.
library(tree)
reg_tree <- tree(EX~MET,data = data2,
                 control =tree.control
                 (nrow(data2), minsize = 8))
# use cv.tree to select optimal tree depth
cvreg_tree <- cv.tree(reg_tree)
15
plot(cvreg_tree$size,cvreg_tree$dev,type = "b",
     xlab = "tree size",ylab = "dev of cvtree",
     main = "tree size vs. deviation")
# pruned tree with 3 leaves is the best one
pru_regtree <- prune.tree(reg_tree,best = 3)
plot(pru_regtree)
text(pru_regtree, pretty = 0)
# predict y by pruned tree
#y_hat <- predict(reg_tree,newdata = data2)
# predict by original tree
y_hat <- predict(pru_regtree,newdata = data2)
ex_y_residual <- data.frame(original_value=data2$EX,
                            fitted_value=y_hat,
                            residual=data2$EX-y_hat)
plot(c(1:nrow(data2)),data2$EX,col="red",
     xlab = "observations",ylab = "values of EX",
     main = "origianl value vs. fitted value")
points(c(1:nrow(data2)),y_hat,col="blue")
legend("top",legend = c("original","fitted"),
       col = c("red","blue"),lty = 1:2,cex = 0.4)
ex_residual <- ex_y_residual$residual
hist(ex_residual, prob=TRUE,xlab = "residual",
     main = "Histogram of residual ")
lines(density(ex_y_residual$residual),lwd=3,
      col="red")

# 3.Compute and plot the 95% confidence bands for the regression 
# tree model from step 2 (fit a regression tree with the same 
# settings and the same number of leaves as in step 2 to the 
# resampled data) by using a non-parametric bootstrap. Comment 
# whether the band is smooth or bumpy and try to explain why. 
# Consider the width of the confidence band and comment whether 
# results of the regression model in step 2 seem to be reliable.
library(boot)
f <- function(data,index){
  data3 <- data[index,]
  reg_tree <- tree(EX~MET,data = data3,
                   control =tree.control(nrow(data3),
                                         minsize = 8))
  pru_regtree <- prune.tree(reg_tree,best = 3)
  pre_boot <- predict(pru_regtree,newdata = data2)
  return(pre_boot)
}
res <- boot(data2,f,R=1000)
e <- envelope(res) #compute confidence bands,level=0.95(default)
plot(c(1:nrow(data2)),data2$EX,col="black",
     xlab = "observations",ylab = "values of EX",
     main = "95% confidence band with non-parametric bootstrap")
points(c(1:nrow(data2)),y_hat,col="blue")
# plot 95% confidence bands of regression tree model
# the upper confidence band is stored in e$point[1,]
# the lower confidence band is stored in e$point[2,]
points(c(1:nrow(data2)),e$point[2,],col="pink")
points(c(1:nrow(data2)),e$point[1,],col="green")
lines(c(1:nrow(data2)),e$point[2,],col="pink")
16
lines(c(1:nrow(data2)),e$point[1,],col="green")

# 4. Compute and plot the 95% confidence and prediction bands 
# the regression tree model from step 2 (fit a regression tree 
# with the same settings and the same number of leaves as in 
# step 2 to the resampled data) by using a parametric bootstrap, 
# assume Y~N(my_i, sigma^2) where my_i are labels in the tree leaves 
# and sigma^2 is the residual variance. Consider the width of the 
# confidence band and comment whether results of the regression model 
# in step 2 seem to be reliable. Does it look like only 5% of 
# data are outside the prediction band? Should it be?
mle <- prune.tree(tree(EX~MET,data = data2,
                       control = tree.control(nrow(data2),
                                              minsize = 8)),best = 3)
rng <- function(data,mle){
  data3 <- data.frame(EX=data$EX,MET=data$MET)
  n <- length(data3$EX)
  data3$EX <- rnorm(n, mean=predict(mle,newdata=data3),
                    sd(data3$EX-predict(mle,newdata=data3)))
  return(data3)
}
f1 <- function(data3){
  reg_tree <- tree(EX~MET,data = data3,
                   control =tree.control(nrow(data3),
                                         minsize = 8))
  pru_regtree <- prune.tree(reg_tree,best = 3)
  pre_boot <- predict(pru_regtree,newdata = data2)
  return(pre_boot)
}
res2 <- boot(data2,statistic =f1,R=1000,
             mle = mle, ran.gen = rng, sim = "parametric")
e2 <- envelope(res2)
plot(c(1:nrow(data2)),data2$EX,col="black",
     xlab = "observations",ylab = "values of EX ",
     main = "95% confidence band with parametric bootstrap")
points(c(1:nrow(data2)),y_hat,col="blue")
points(c(1:nrow(data2)),e2$point[2,],col="pink")
points(c(1:nrow(data2)),e2$point[1,],col="green")
lines(c(1:nrow(data2)),e2$point[2,],col="pink")
lines(c(1:nrow(data2)),e2$point[1,],col="green")
# prediction interval band
f2 <- function(data3){
  reg_tree <- tree(EX~MET,data = data3,
                   control =tree.control(nrow(data3),
                                         minsize = 8))
  pru_regtree <- prune.tree(reg_tree,best = 3)
  pre_boot <- predict(pru_regtree,newdata = data2)
  n <- length(data2$EX)
  predictEX <- rnorm(n,pre_boot,
                     sd(data2$EX-pre_boot))
  return(predictEX)
}
res3 <- boot(data2, statistic = f2, R=1000 ,mle = mle,
             ran.gen = rng, sim = "parametric")
e3 <- envelope(res3)
# unable to achieve requested overall error rate
plot(c(1:nrow(data2)),data2$EX,col="black",
     xlab = "observations",ylab = "values of EX",
     ylim = c(130,470), # the range(e3$point[1,]) and [2,]
     main = "95% prediction band with parametric bootstrap")
points(c(1:nrow(data2)),y_hat,col="blue")
points(c(1:nrow(data2)),e3$point[2,],col="pink")
points(c(1:nrow(data2)),e3$point[1,],col="green")
lines(c(1:nrow(data2)),e3$point[2,],col="pink")
lines(c(1:nrow(data2)),e3$point[1,],col="green")