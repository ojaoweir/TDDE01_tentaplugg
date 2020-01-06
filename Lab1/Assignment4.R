# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)

# ------------------ Assignment 4 ---------------------------------
# The Excel file tecator.xlsx contains the results of study aimed to 
# investigate whether a near infrared absorbance spectrum can be used 
# to predict the fat content of samples of meat. For each meat sample 
# the data consists of a 100 channel spectrum of absorbance records and 
# the levels of moisture (water), fat and protein. The absorbance is 
# -log10 of the transmittance measured by the spectrometer. 
# The moisture, fat and protein are determined by analytic chemistry.

# 1 Import data to R and create a plot of Moisture versus Protein. 
# Do you think that these data are described well by a linear model?

Dataframe=read.csv2("tecator.csv")
plot(Dataframe$Moisture, Dataframe$Protein)

# 2. Consider model M_i in which Moisture is normally distributed, and 
# the expected Moisture is a polynomial function of Protein including 
# the polynomial terms up to power i (i.e M_1 is a linear model, M_2 is a quadratic model and so on). 
# Report a probabilistic model that describes M_i. 
# Why is it appropriate to use MSE criterion when fitting this model to a training data?

# M_i = a*i * protein^i + a*(i-1) * protein^(i-1) + ... + a * protein

# 3. Divide the data into training and validation sets( 50%/50%) and fit models
# M_i, i=1.6. For each model, record the training and the validation MSE and present 
# a plot showing how training and validation MSE depend on i 
# (write some R code to make this plot). Which model is best according to the plot? 
#How do the MSE values change and why? Interpret this picture in terms of bias-variance tradeoff.

mean_square_error = function(v1, v2) {
  SE = (v1-v2)
  SE = SE^2
  SE = mean(SE)
  return(SE)
}

calc_MSE = function(the_model, data) {
  SE = predict(the_model, newdata = data)
  SE = data$Moisture - SE
  SE = SE^2
  MSE = mean(SE)
  return(MSE)
}

n=dim(Dataframe)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=Dataframe[id,]
test=Dataframe[-id,]
test = test[1:dim(train)[1],]
MSEs = vector("numeric", length=0)

step3 = function(data_in, data_train) {
  for(i in seq(1,6)) {
    model_i = lm(Moisture ~poly(Protein,i), data = data_train)
    prediction = predict(model_i, newdata = data_in)
    MSEs = c(MSEs, mean_square_error(prediction, data_in$Moisture))
  }
  return (MSEs)
}

MSE_train = step3(train, train)
MSE_test = step3(test, train)

plot(seq(1,6), MSE_train, type="l", col="blue", ylim=c(20,45))
lines(seq(1,6), MSE_test, type="l", col="red")

# 4. Perform variable selection of a linear model in which Fat 
# is response and Channel1-Channel100 are predictors by using stepAIC. 
# Comment on how many variables were selected.

t_data = Dataframe[,0:(dim(Dataframe)[2]-2)]
model_lm = lm(Fat ~., data=t_data)
AIC_lm = stepAIC(model_lm)
print(paste0(length(AIC_lm$coefficients), " were selected"))

# 5. Fit a Ridge regression model with the same predictor and response variables. 
# Present a plot showing how model coefficients depend on the log of the penalty 
# factor lambda and report how the coefficients change with lambda.
library('glmnet')

step5n6 = function(data_in, alpha_in) {
  t_model = glmnet(as.matrix(data_in[,2:101]), data_in[,102], alpha=alpha_in, family="gaussian")
  plot(t_model, xvar="lambda", label=TRUE)
}

step5n6(t_data, 0)

# 6. Repeat step 5 but fit LASSO instead of the Ridge regression and 
# compare the plots from steps 5 and 6. Conclusions?
step5n6(t_data, 1)

# 7. Use cross-validation to find the optimal LASSO model 
# (make sure that case lambda=0 is also considered by the procedure),
# report the optimal lambda and how many variables were chosen by the 
# model and make conclusions. Present also a plot showing the dependence 
# of the CV score and comment how the CV score changes with lambda.
lambdas = seq(0,50, 0.01)
cv_lasso = cv.glmnet(as.matrix(t_data[,2:101]), t_data[,102], alpha=1, family="gaussian", lambda=lambdas)
plot( cv_lasso, type="b", col="Green")