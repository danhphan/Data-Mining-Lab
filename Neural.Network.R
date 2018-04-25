
##################################
## Neural network in R
## Using iris data set
##################################

library(MASS)
data("iris")

iris <- as_tibble(iris)

# Use caret package for NN
install.packages("caret")
library(lattice)
library(caret)

# Train NN model
# Using all predictors
nn_model <- train(Species~.,iris,method="nnet",trace=FALSE)
# Using 2 predictors
#nn_model <- train(Species~Petal.Length+Petal.Width,iris,method="nnet",trace=FALSE)

# Predict
prediction <- predict(nn_model,iris)
# Evaluate result
table(prediction,iris$Species)

# Plot the Neural network
#install.packages("devtools")
library(devtools)
install.packages("reshape")
library(reshape)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn_model,nid=T,var.labs=F)

#################################################
## How accurately can our network model the data?


