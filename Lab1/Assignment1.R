# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)

# ------------------ Assignment 1 ---------------------------------
#The data file spambase.xlsx contains information about the frequency of various 
#words, characters etc for a total of 2740 e-mails. Furthermore, these e-mails have 
#been manually classified as spams (spam = 1) or regular e-mails (spam = 0). 

#1.  Import the data into R and divide it into training and test sets (50%/50%) by using 
#the following code:
Dataframe=read.csv2("spambase.csv")
n=dim(Dataframe)[1]
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,]

# 2.  Use logistic regression (functions glm(), predict()) to classify the training and test 
#data by the classification principle:
# Y = 1 if (Y= 1|X)>0.5 otherwise Y=0

# Functions
# Predict and Classify the data
predict_with_classif = function(model_in, data_in, classif_val) {
  prediction = predict(model_in, newdata = data_in)
  classif = ifelse(prediction > classif_val, 1, 0)
}

# Create and print confunsion matrix
conf_matrix = function(title, true, predicted) {
  t_table = table(true, predicted)
  print(title)
  print(t_table)
}

# Calculate and print missclassification rate
missclass_rate = function(title ,v1, v2) {
  t_table = table(v1, v2)
  missclass = 1-sum(diag(t_table))/sum(t_table)
  print(paste0("missclassification rate for ", title, ": ", missclass))
}

task2n3 = function(model_in, title, data_in, classif_val) {
  prediction = predict_with_classif(model_in, data_in, classif_val)
  conf_matrix(title, data_in$Spam, prediction)
  missclass_rate(title, data_in$Spam, prediction)
}

model_glm = glm(Spam~., data = train)
task2n3(model_glm, "Training data", train, 0.5)
task2n3(model_glm, "Test data", test, 0.5)

# 3.Use logistic regression to classify the test data by the classification principle:
# # Y = 1 if (Y= 1|X)>0.8 otherwise Y=0
#and report the confusion matrices (use table()) and the misclassification rates for training and test data. Compare the results. What effect did the new rule have?

task2n3(model_glm, "Training data", train, 0.8)
task2n3(model_glm, "Test data", test, 0.8)
# Answer: Due to the new rule almost nothing gets classified as not spam. The missclassification rate is higher as result

# 4. Use standard classifier kknn() with K=30 from package kknn, 
# report the the misclassification rates for the training and test 
# data and compare the results with step 2.
library('kknn')
step4n5 = function(title, data_train, data_test, threshold) {
  prediction = kknn(Spam ~., train = data_train, test=data_test, k = threshold)
  prediction = ifelse(prediction$fitted.values > 0.5, 1, 0)
  conf_matrix(title, data_test$Spam, prediction)
  missclass_rate(title, data_test$Spam, prediction)
}

step4n5("Training data", train, train, 30)
step4n5("Test data", train, test, 30)

# 5. Repeat step 4 for K=1 and compare the results with step 4. What effect does the decrease of K lead to and why?
step4n5("Training data", train, train, 1)
step4n5("Test data", train, test, 1)
