# Remove all saved variables and plots
dev.off()
rm(list=ls())
# Setting up rng and seed
RNGversion('3.5.1')
set.seed(12345)


# ------------------ Assignment 4 ---------------------------------
# The data file NIRspectra.csv contains near-infrared spectra and 
# viscosity levels for a collection of diesel fuels. Your task is 
# to investigate how the measured spectra can be used to 
# predict the viscosity.

Dataframe=read.csv2("NIRSpectra.csv")

# 1. Conduct a standard PCA by using the feature space and provide 
# a plot explaining how much variation is explained by each feature. 
# Does the plot show how many PC should be extracted? Select the 
# minimal number of components explaining at least 99% of the total 
# variance. Provide also a plot of the scores in the 
# coordinates (PC1, PC2). Are there unusual diesel fuels 
# according to this plot?

#PCA is unsupervised learning
Dataframe$Viscosity = c()
pca = prcomp(Dataframe)
lambda = pca$sdev^2
plot(pca)
sprintf("%2.3f",lambda/sum(lambda)*100)
# We see from the line above that the first 2 components cover 99%

plot(pca$x[,1], pca$x[,2], ylim=c(-0.15,0.15))

# 2.Make trace plots of the loadings of the components selected 
# in step 1. Is there any principle component that is explained 
# by mainly a few original features?

U = pca$rotation
plot(U[,1], main="PC1")
plot(U[,2], main="PC2")

# 3. Perform Independent Component Analysis with the number of 
# components selected in step 1 (set seed 12345). Check the 
# documentation for the fastICA method in R and do the following:
#   a. Compute W' = K*W and present the columns of W' in form of
# the trace plots. Compare with the trace plots in step 2 and 
# make conclusions. What kind of measure is represented by the matrix W'?
#   b. Make a plot of the scores of the first two latent features 
# and compare it with the score plot from step 1.
library(fastICA)
fica = fastICA(Dataframe, 2)
# Use %*% when you want to use matrix operation
Wtick = fica$K %*% fica$W

plot(Wtick[,1], main="PC1")
plot(Wtick[,2], main="PC2")
