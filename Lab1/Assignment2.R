# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)

# ------------------ Assignment 2 ---------------------------------
#The data file machines.xlsx contains information about the lifetime of 
#certain machines, and the company is interested to know more about the 
#underlying process in order to determine the warranty time. The variable is following:
#  Length: shows lifetime of a machine

# 1. Import the data to R.
Dataframe=read.csv2("machines.csv")

# 2. Assume the probability model p(x|theta)=theta*e^(-theta*x)
# for x=Length in which observations are independent and identically distributed. 
# What is the distribution type of x? Write a function that computes the log-likelihood 
# logp(x|theta) for a given theta and a given data vector x. Plot the curve showing 
# the dependence of log-likelihood on theta where the entire data is used for fitting. 
# What is the maximum likelihood value of theta according to the plot?

loglike = function(x, theta) {
  n = length(x)
  return (n*log(theta)-theta*sum(x))
}

step2n3 = function(data_in, thetas) {
  LLH = vector("numeric", length =0)
  for(theta in thetas) {
    LLH = c(LLH, loglike(data_in, theta))
  }
  print(thetas[which.max(LLH)])
  return(LLH)
}

step2 = function(data_in, thetas) {
  LLH = step2n3(data_in$Length, thetas)
  plot(thetas, LLH, type="l",col="blue")
}
thetas = seq(0, 30, 0.01)
step2(Dataframe, thetas)

# 3. Repeat step 2 but use only 6 first observations from the data, and put the two 
# log-likelihood curves (from step 2 and 3) in the same plot. What can you say about 
# reliability of the maximum likelihood solution in each case?

step3 = function(data_in, thetas) {
  LLH = step2n3(data_in, thetas)
  points(thetas, LLH, type="l",col="red")
}
step3(Dataframe[0:6,], thetas)

# 4. Assume now a Bayesian model with p(x|theta)=theta*e^(-theta*x) and a prior p(theta)=lambda*e^(-lambda*theta)
# lambda=10. Write a function computing l(theta)=logp(x|theta)p(theta). 
# What kind of measure is actually computed by this function? Plot the curve showing the dependence
# of l(theta) on theta computed using the entire data and overlay it with a plot from step 2. 
# Find an optimal theta and compare your result with the previous findings.

bayes = function(x, theta) {
  lambda = 10
  log_prior = log(lambda)-lambda*theta
  return(log_prior+loglike(x,theta))
}

LLH = vector("numeric", length=0)
for(theta in thetas) {
  LLH = c(LLH, bayes(Dataframe$Length, theta))
}

points(thetas, LLH, type="l",col="green")

# 5. Use theta value found in step 2 and generate 50 new observations from p(x|theta)=theta*e^(-theta*x) 
# (use standard random number generators). Create the histograms of the original and the new data and make conclusions.
rand_obs = rexp(50, rate = 1.13)
hist(rand_obs,
     col=rgb(1,0,0,0.5),
     xlim=c(0,6), 
     ylim=c(0,30),
     main="Overlapping Histogram",
     xlab="Variable")

hist(Dataframe$Length, 
     col=rgb(0,0,1,0.5), 
     add=T)
box()
