# Test SVM model

x=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
y=c(3,4,5,4,8,10,10,11,14,20,23,24,32,34,35,37,42,48,53,60)

#Create a data frame of the data
train=data.frame(x,y)

#Plot the dataset
plot(train,pch=16)

#Linear regression
model <- lm(y ~ x, train)

#Plot the model using abline
abline(model)

#SVM
library(e1071)

#Fit a model. The function syntax is very similar to lm function
model_svm <- svm(y ~ x , train)

#Use the predictions on the data
pred <- predict(model_svm, train)

#Plot the predictions and the plot to see our model fit
points(train$x, pred, col = "blue", pch=4)

#Linear model has a residuals part which we can extract and directly calculate rmse
error <- model$residuals 
lm_error <- sqrt(mean(error^2)) # 3.832974

#For svm, we have to manually calculate the difference between actual values (train$y) with our predictions (pred)
error_2 <- train$y - pred
svm_error <- sqrt(mean(error_2^2)) # 2.696281


# perform a grid search
svm_tune <- tune(svm, y ~ x, data = train,
                 ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
)
print(svm_tune)

#Parameter tuning of ‘svm’:

# - sampling method: 10-fold cross validation 

#- best parameters:
# epsilon cost
#0 8

#- best performance: 2.872047 

#The best model
best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, train) 

error_best_mod <- train$y - best_mod_pred 

# this value can be different on your computer
# because the tune method randomly shuffles the data
best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 
plot(svm_tune)

plot(train,pch=16)
points(train$x, best_mod_pred, col = "blue", pch=4)

