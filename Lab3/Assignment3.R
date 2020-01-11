# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)


# ------------------ Assignment 3 ---------------------------------
# Train a neural network to learn the trigonometric sine function. To do so, sample 50 points
# uniformly at random in the interval [0 : 10]. Apply the sine function to each point. The resulting
# pairs are the data available to you. Use 25 of the 50 points for training and the rest for validation.
# The validation set is used for early stop of the gradient descent. That is, you should
# use the validation set to detect when to stop the gradient descent and so avoid overfitting.
# Stop the gradient descent when the partial derivatives of the error function are below a given
# threshold value. Check the argument threshold in the documentation. Consider threshold
# values i / 1000, i = 1, ..., 10. Initialize the weights of the neural network to random values in
# the interval [-1:1]. Use a neural network with a single hidden layer of 10 units. Use the default
# values for the arguments not mentioned here. Choose the most appropriate value for
# the threshold. Motivate your choice. Provide the final neural network learned with the chosen
# threshold. Feel free to use the following template.

mean_square_error = function(v1, v2) {
  SE = (v1-v2)
  SE = SE^2
  SE = mean(SE)
  return(SE)
}
# Function to predict and return the MSE
# For NN use compute
pred_MSE = function(model_in, data_in) {
  prediction = compute(model_in, covariate=data_in)$net.result
  return(mean_square_error(prediction, data_in$Sin))
}

library(neuralnet)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

#Hidden = 10 from assignment
hidden_val = 10
i_vals = seq(1,10)

# Random initialization of the weights in the interval [-1, 1]
# Number of starting weights is number of connections in the plot of the nn
winit = runif(31, -1, 1)
MSE_tr = rep(0, length(i_vals))
MSE_va = rep(0, length(i_vals))
  for(i in i_vals) {
    nn <- neuralnet(Sin ~ Var, data = tr, hidden = hidden_val, startweights = winit, threshold = i/1000)
    MSE_tr[i] = MSE_tr[i] + pred_MSE(nn, tr)
    MSE_va[i] = MSE_va[i] + pred_MSE(nn, va)
  }

plot(MSE_tr, type= "l")
plot(MSE_va, type= "l")
# Lowest MSE for i = 5
nn <- neuralnet(Sin ~ Var, data = tr, hidden = hidden_val, startweights = winit, threshold = 5/1000)
plot(nn)

# Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn)$rep1)
points(trva, col = "red")
  
  