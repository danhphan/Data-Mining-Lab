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

summary(trainC)

View(trainX)



# Remove LOCATION
trainX$LOCATION <- NULL
testX$LOCATION <- NULL
dim(trainX)
dim(testX)
summary(trainX)
summary(testX)

pairs(trainX)

set.seed(123)
rown <- nrow(trainX)
nid <- sample(1:rown,size=0.75*rown)
evaluateX <- trainX[-nid,]
trainX <- trainX[nid,]


library(corrplot)
library(caret)

corMatMy <- cor(trainX) #compute the correlation matrix
corrplot(corMatMy, order = "hclust")
corMatMy

highlyCor <- findCorrelation(corMatMy, 0.80)
highlyCor
#Apply correlation filter at 0.70,
#then we remove all the variable correlated with more 0.7.
trainX <- trainX[,-highlyCor]
corMatMy2 <- cor(trainX)
corrplot(corMatMy2, order = "hclust")
pairs(trainX)


corTest <- cor(testX)
corrplot(corTest)

highCor <- findCorrelation(corTest, 0.8)
testXXX <- testX[,-highCor]


# Do linear regression

#glm.fit=glm(Quality_of_life_measure~., data=trainX)
trainY <- trainX[,c(names(testXXX),"Quality_of_life_measure")]
glm.fit=glm(Quality_of_life_measure~., data=trainY)

newdataaa <- cbind(as.data.frame(scale(trainY[,1:19])),Quality_of_life_measure=trainY$Quality_of_life_measure)
glm.fit.scale =glm(Quality_of_life_measure~., data=newdataaa)

summary(glm.fit)

trainXX <- trainX
trainXX$Quality_of_life_measure <- NULL

evaluateXX <- evaluateX[,names(trainXX)]
testXX <- testX[,names(trainXX)]


glm.pred <- predict(glm.fit,newdata = evaluateX)

mse <- mean((evaluateX$Quality_of_life_measure - glm.pred)^2)
mse # 0.005803032 0.006088293

glm.pred2 <- predict(glm.fit,newdata = evaluateXX)
mse2 <- mean((evaluateX$Quality_of_life_measure - glm.pred2)^2)
mse2 # 0.005803032 => SAME RESULTS 0.008177823

glm.pred.future <- predict(glm.fit,newdata = testXX)
glm.pred.future

###############
glm.pred.future <- predict(glm.fit,newdata = testXXX)
glm.pred.future

results <- data.frame(id=testC$id,target = glm.pred.future)
write.csv(results, file = "prediction_linear2.csv",row.names = FALSE)




glm.pred.future2 <- predict(glm.fit,newdata = testXXX)
glm.pred.future


glm.slace <- predict(glm.fit.scale,newdata = as.data.frame(scale(testXXX)))

glm.slace
results <- data.frame(id=testC$id,target = glm.slace)
write.csv(results, file = "prediction_linear_scale.csv",row.names = FALSE)

# Test using NN
library(lattice)
library(caret)
# Fit model
set.seed(123)
dim(trainX)
nn_model <- train(Quality_of_life_measure~.,trainX,method="nnet",trace=FALSE)

nn_predict <- predict(nn_model,testXX)
nn_predict

mean((evaluateX$Quality_of_life_measure - nn_predict)^2)
# 0.008177823

results <- data.frame(id=testC$id,target = nn_predict)

write.csv(results, file = "prediction_nn5.csv",row.names = FALSE)






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
