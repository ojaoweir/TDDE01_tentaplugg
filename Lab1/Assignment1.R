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
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,]

#2.  Use logistic regression (functions glm(), predict()) to classify the training and test 
#data by the classification principle:
# Y = 1 if (Y= 1|X)>0.5 otherwise Y=0

model_glm = glm(Spam~., data = train)
