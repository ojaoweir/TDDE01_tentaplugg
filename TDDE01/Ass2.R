# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
#RNGversion('3.5.1')

library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 3)
tr <- data.frame(Var, Sin=sin(Var))
Var <- runif(50, 3, 9)
te <- data.frame(Var, Sin=sin(Var))
hidden_val = 3
winit = runif(10,-1,1)
my_nn = neuralnet(Sin~Var, data = tr, hidden = hidden_val, startweights = winit)
#plot(my_nn)
prediction = compute(my_nn, covariate=tr)$net.result
plot(te$Var, prediction, col="red", xlim=c(0,10), ylim=c(-2,2))
points(tr, col="black")
points(te, col="blue")

