# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)


# ------------------ Assignment 1 ---------------------------------
# The data file australian-crabs.csv contains measurements of various crabs, 
# such as Frontal lobe, Rear width and others

Dataframe=read.csv2("australian-crabs.csv")

# 1. Use australian-crabs.csv and make a scatterplot of carapace length (CL) 
# versus rear width (RW) where observations are colored by Sex. Do you think 
# that this data is easy to classify by linear discriminant analysis? 
# Motivate your answer.

plot(Dataframe$CL, Dataframe$RW, col=Dataframe$sex)

# 2. Make LDA analysis with target Sex and features CL and RW and proportional
# prior by using lda() function in package MASS. Make a scatter plot of CL 
# versus RW colored by the predicted Sex and compare it with the plot in step 1. 
# Compute the misclassification error and comment on the quality of fit.

missclass_rate = function(title ,v1, v2) {
  t_table = table(v1, v2)
  missclass = 1-sum(diag(t_table))/sum(t_table)
  print(paste0("missclassification rate for ", title, ": ", missclass))
}

step2n3 = function(t_model) {
  predict_lda = predict(model_lda, newdata = Dataframe)
  plot(Dataframe$CL, Dataframe$RW, col=predict_lda$class)
  missclass_rate("lda", predict_lda$class, Dataframe$sex)
}

library('MASS')
model_lda = lda(sex ~ RW+CL, data = Dataframe)
step2n3(model_lda)

# 3. Repeat step 2 but use priors p(Male)=0.9,p(Female)=0.1 instead. 
# How did the classification result change and why?

model_lda = lda(sex ~ RW+CL, data = Dataframe, prior = c(0.1,0.9))
step2n3(model_lda)

# 4. Make a similar kind of classification by logistic regression 
# (use function glm()), plot the classified data and compute the 
# misclassification error. Compare these results with the LDA results.
# Finally, report the equation of the decision boundary and draw the 
# decision boundary in the plot of the classified data.

model_glm = glm(sex ~ RW+CL, data = Dataframe, family =  "binomial")
predict_glm = predict(model_glm, newdata = Dataframe)
plot(predict_glm, type = "l")
predict_glm = ifelse(predict_glm > 0.5, "Male", "Female")
missclass_rate("GLM", predict_glm, Dataframe$sex)

plot(Dataframe$CL, Dataframe$RW, col=as.factor(predict_glm))

slope <- coef(model_glm)[2]/(-coef(model_glm)[3])
intercept <- coef(model_glm)[1]/(-coef(model_glm)[3])
print(paste0("Equation: ", intercept, " + ", slope, "x"))
abline(intercept , slope)
