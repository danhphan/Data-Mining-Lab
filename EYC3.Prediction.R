#EY-eyc3-data-science-challenge

ls()
rm(list=ls())
gc()

library(readr)
library(dplyr)

# Load cleaned data sources
trainC <- read_csv("D:/Drive/Persona/MIT Study/R Projects/Data-Mining-Lab/train_cleaned.csv")
testC <- read_csv("D:/Drive/Persona/MIT Study/R Projects/Data-Mining-Lab/test_cleaned.csv")

dim(trainC)
trainX <- trainC[,3:42]
str(trainX)

dim(testC)
testX <- testC[,3:41]
str(testX)
dim(trainX)
str(trainX)

# Remove LOCATION
trainX$LOCATION <- NULL
testX$LOCATION <- NULL
dim(trainX)
dim(testX)
summary(trainX)
summary(testX)

# # Scale data => SHOULD NOT SCALE
# trainXX <- scale(trainX[,1:38])
# trainXX <- data.frame(trainXX,trainX$Quality_of_life_measure)
# testXX <- data.frame(scale(testX))
# colnames(trainXX) <- names(trainX)
# colnames(testXX) <- names(testX)
# 
# summary(trainXX)
# str(trainXX)
# summary(testXX)
# str(testXX)

# Using random forest
#install.packages("randomForest")
library(randomForest)
names(trainX) <- make.names(names(trainX))
names(testX) <- make.names(names(testX))
rdfr_model <- randomForest(Quality_of_life_measure ~ ., data = trainX,ntree=500,mtry=6, importance = TRUE)
rdfr_predict <- predict(rdfr_model,testX)
rdfr_predict

results <- data.frame(id=testC$id,target = rdfr_predict)
write.csv(results, file = "prediction_rdfr.csv",row.names = FALSE)


# Test using NN
library(lattice)
library(caret)
# Fit model
nn_model <- train(Quality_of_life_measure~.,trainX,method="nnet",trace=FALSE)

nn_predict <- predict(nn_model,testX)
nn_predict
results <- data.frame(id=testC$id,target = nn_predict)

write.csv(results, file = "prediction_nn4.csv",row.names = FALSE)


## SVM Model
library(e1071)
svm_model <- svm(Quality_of_life_measure~.,trainX)
svm_predict <- predict(svm_model,testX)
svm_predict
results <- data.frame(id=testC$id,target = svm_predict)
write.csv(results, file = "prediction_svm.csv",row.names = FALSE)

svm_tune <- tune(svm, Quality_of_life_measure~., data = trainX,
                 ranges = list(epsilon = seq(0,1,0.02), cost = 2^(2:8)))

print(svm_tune)

######### NEXT STEPS ##########
# TEST NORMALISATION
# TEST SVM
# Outlier Analysis
# Revise missing process


# ####################################
# dim(trainC)
# a <- unique(trainC$LOCATION)
# b <- unique(testC$LOCATION)
# c <- c("CZE","FIN","USA")
# a
# b
# which(b %in% a )
# which(c %in% a)
