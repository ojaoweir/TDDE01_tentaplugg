# Remove all saved variables and plots
dev.off() 
rm(list=ls()) 
# Setting up rng and seed
RNGversion('3.5.1') 
set.seed(12345)

Dataframe=read.csv2("video.csv")
n=dim(Dataframe)[1]
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,]

# 1. Perform principal component analysis using the numeric variables in 
# the training data except of "utime" variable. Do this analysis with and 
# without scaling of the features. How many components are necessary to 
# explain more than 95% variation of the data in both cases? Explain why 
# so few components are needed when scaling is not done. (2p)
data1 = train[-2]
data1 = data1[-18]


pca = prcomp(data1, scale. = FALSE)
lambda = pca$sdev^2
sprintf("%2.3f",lambda/sum(lambda)*100)
pca = prcomp(data1, scale. = TRUE)
lambda = pca$sdev^2
sprintf("%2.3f",lambda/sum(lambda)*100)

# Answer:
# Withjout Scale - one variable required
# With scale - 9 variables required
# This is because that the original data is on a very different scale which means one component dominates

# 2.Write a code that fits a principle component regression 
# ("utime" as response and all scaled numerical variables as features) 
# with ???????? components to the training data and estimates the training
# and test errors, do this for all feasible ???????? values. Plot dependence
# of the training and test errors on ???????? and explain this plot in terms
# of bias-variance tradeoff. (Hint: prediction function for principal 
# component regression has some peculiarities, see predict.mvr) (2p)
library(pls)

mean_square_error = function(v1, v2) {
  SE = (v1-v2)
  SE = SE^2
  SE = mean(SE)
  return(SE)
}
predict_mse = function(model_in, M, data_in) {
  prediction = predict(model_in, newdata = data_in, ncomp=M)
  return(mean_square_error(prediction, data_in$utime))
}

data1 = train[-2]
#Number of components = number of variables
Ms = seq(1,17)
model_pcr = pcr(utime ~., data = data1, ncomp = 17)
MSE_trn = rep(0,17)
MSE_tst = rep(0,17)
for(i in Ms) {
  MSE_trn[i] = predict_mse(model_pcr, i, train[-2])
  MSE_tst[i] = predict_mse(model_pcr, i, test[-2])
}
plot(MSE_trn, type = "l", col="blue", xlim=c(0,17), ylim = c(100,300))
lines(MSE_tst, type = "l", col="red")

# Answer:
# With more components the less the bias
# We see that best value is M = 14, but M = 8 gives almost the same and do not hurt the bias as much
# As complexity goes up, bias goes down, while varaiance goes up

# 3. Use PCR model with ????????=8 and report a fitted probabilistic
# model that shows the connection between the target and 
# the principal components. (1p)
model_pcr = pcr(utime ~., data = data1, ncomp = 8, validation="none", scale="T")
Yloadings(model_pcr)

# Answer???

# 4.Use original data to create variable "class" that shows "mpeg" if variable 
# "codec" is equal to "mpeg4", and "other" for all other values of "codec". 
# Create a plot of "duration" versus "frames" where cases are colored by "class". 
# Do you think that the classes are easily separable by a linear decision boundary? (1p)
data1 = Dataframe
data1$class = ifelse(data1$codec == "mpeg4", "mpeg", "other")
plot(data1$frames, data1$duration, col = as.factor(data1$class))

# Answer:
# The two classes could be pretty good classified by a boundary

# 5. Fit a Linear Discriminant Analysis model with "class" as target and "frames" 
# and "duration" as features to the entire dataset (scale features first). 
# Produce the plot showing the classified data and report the training error. 
# Explain why LDA was unable to achieve perfect (or nearly perfect) 
# classification in this case. (2p)

missclass_rate = function(title ,v1, v2) {
  t_table = table(v1, v2)
  missclass = 1-sum(diag(t_table))/sum(t_table)
  print(paste0("missclassification rate for ", title, ": ", missclass))
}


data1 = data1[-(2:9)]
data1 = data1[-(3:11)]
data1$duration = scale(data1$duration)
data1$frames = scale(data1$frames)
library('MASS') 
model_lda = lda(class ~frames + duration, data=data1)
prediction = predict(model_lda, newdata = data1)
missclass_rate("lda", prediction$class, data1$class)
plot(data1$frames, data1$duration, col =prediction$class)

# Answer:
# The covariance matrixes are very different

# 6. Fit a decision tree model with "class" as target and 
# "frames" and "duration" as features to the entire dataset, 
# choose an appropriate tree size by cross-validation. 
# Report the training error. How many leaves are there in 
# the final tree? Explain why such a complicated tree is 
# needed to describe such a simple decision boundary.
library(tree)
data1 = Dataframe
data1$class = ifelse(data1$codec == "mpeg4", "mpeg", "other")
model_tree = tree(as.factor(class) ~ frames + duration, data =data1)
cv = cv.tree(model_tree)
plot(cv$size, cv$dev, type="b", col="red")
plot(model_tree)

# Answeprediction = predict(model_tree, newdata = data1, type="class")
missclass_rate("tree", prediction, data1$class)r
:
# it is complex because the boundary is linear but not pararell with any axis