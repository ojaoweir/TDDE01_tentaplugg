# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)


# ------------------ Assignment 2 ---------------------------------
# The data file creditscoring.xls contains data retrieved from a 
# database in a private enterprise. Each row contains information 
# about one customer. The variable good/bad indicates how the customers 
# have managed their loans. The other features are potential predictors. 
# Your task is to derive a prediction model that can be used to predict 
# whether or not a new customer is likely to pay back the loan.

# 1.Import the data to R and divide into training/validation/test 
# as 50/25/25: use data partitioning code specified in Lecture 1e.

Dataframe=read.csv2("creditscoring.csv")

n=dim(Dataframe)[1]
id=sample(1:n, floor(n*0.4)) 
train=Dataframe[id,] 
id1=setdiff(1:n, id)
id2=sample(id1, floor(n*0.3)) 
valid=Dataframe[id2,]
id3=setdiff(id1,id2)
test=Dataframe[id3,]

# 2. Fit a decision tree to the training data by using the following measures of impurity
#    a. Deviance
#    b. Gini index
# and report the misclassification rates for the training and test 
# data. Choose the measure providing the better results for the following steps.
library(tree)

# Calculate and print missclassification rate
missclass_rate = function(title ,v1, v2) {
  t_table = table(v1, v2)
  missclass = 1-sum(diag(t_table))/sum(t_table)
  print(paste0("missclassification rate for ", title, ": ", missclass))
}

class_and_value = function(title, t_tree, data_in, threshold) {
  classification = predict(t_tree, newdata = data_in)
  prediction = ifelse(classification[,2] > threshold, "good", "bad")
  missclass_rate(title, prediction, data_in$good_bad)
}

step2 = function(impurity) {
  t_tree = tree(good_bad ~ ., data = train, split = impurity)
  for(d in list(train, test)) {
    class_and_value(impurity, t_tree, d, 0.5)
  }
}
for(imp in c("deviance", "gini")) {
  step2(imp)
}

# Deviance is the better choice
t_tree = tree(good_bad ~ ., data = train, split = "deviance")

# 3. Use training and validation sets to choose the optimal 
# tree depth. Present the graphs of the dependence of deviances 
# for the training and the validation data on the number of 
# leaves. Report the optimal tree, report it's depth and 
# the variables used by the tree. Interpret the information 
# provided by the tree structure. Estimate the 
# misclassification rate for the test data.

calcScore = function(t_tree, data_in) {
  prediction = predict(t_tree, newdata = data_in, type="tree")
  return(deviance(prediction))
}

score_train = rep(0,20)
score_valid = rep(0,20)

for( i in 2:20) {
  pruned_tree = prune.tree(t_tree, best = i)
  score_train[i] = calcScore(pruned_tree, train)
  score_valid[i] = calcScore(pruned_tree, valid)
}

plot(score_train, type="l", col="blue", xlim = c(2,20))
plot(score_valid, type="l", col="red", xlim = c(2,20))

t_tree = prune.tree(t_tree, best = 4)
plot(t_tree)
text(t_tree, pretty=0)
# Depth is 3, variables used are: duration, history and savings
prediction = predict(t_tree, newdata = test)
prediction = ifelse(prediction[,2] > 0.5, "good", "bad")
missclass_rate("pruned tree", prediction, test$good_bad)

# 4.Use training data to perform classification using Naïve Bayes 
# and report the confusion matrices and misclassification rates 
# for the training and for the test data. Compare the results 
# with those from step 3.
library(e1071)
navbay = naiveBayes(good_bad ~ ., data = train)
for(d in list(train, test)) {
  prediction = predict(navbay, newdata = d)
  missclass_rate("naive bayes", prediction, d$good_bad)
  
}

# 5. Use the optimal tree and the Naïve Bayes model to 
# classify the test data by using the following principle:
# Y=1 if p(Y="good"|x)>pi otherwise Y=0, 
# where pi = 0.05, 0.1, ..., 0.9, 0.95. 
# Compute the TPR and FPR values for the two models and 
# plot the corresponding ROC curves. Conclusion?

conf_matrix = function(true, predicted) {
  t_table = table(true, predicted)
  return(t_table)
}

step5 = function(pi,model_in, d, pred_type) {
  TPR = rep(1:length(pi))
  FPR = rep(1:length(pi))
  for(i in 1:length(pi)) {
    prediction = predict(model_in, newdata = d, type=pred_type)
    prediction = ifelse(prediction[,2] > pi[i], "good", "bad")
    confusion = conf_matrix(d$good_bad, prediction)
    TPR[i] = confusion[1,1]/sum(confusion[1,])
    FPR[i] = confusion[2,1]/sum(confusion[2,])
  }
  return(c(TPR, FPR))
}
pi = seq(0.05,0.95,0.05)

vals_navbay = step5(pi, navbay, train, "raw")
vals_tree = step5(pi, t_tree, train, "vector")
plot(vals_navbay[1:19], vals_navbay[20:38], type = "l", col="blue")
plot(vals_tree[1:19], vals_tree[20:38], type = "l", col="blue")

# 6.Repeat Naïve Bayes classification as it was in step 4 but use the 
# following loss matrix:
# L = (0,1; 10,0) and report the confusion matrix for the training 
# and test data. Compare the results with the results from step 4 
# and discuss how the rates has changed and why.

for(d in list(train, test)) {
  prediction = predict(navbay, newdata = d, type = "raw")
  classified = rep(1:dim(prediction)[1])
  for(i in 1:dim(prediction)[1]) {
    classified[i] = ifelse(prediction[i,1]*10 < prediction[i,2]*1, "good", "bad")
  }
  print(conf_matrix(d$good_bad, classified))
}

