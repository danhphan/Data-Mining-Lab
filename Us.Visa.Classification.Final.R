################################
# Assignment 2
# The Danh Phan - 44116543
################################

# IMPLEMENTING SVM MODELS

# Remove old variables and release memory
rm(list=ls())
gc()
memory.limit()

# Load libraries
library(readr)
library(e1071)

######### Function #########

# transform_numeric function: 
# Convert flag and ordinal variables into number
transform_numeric <- function(train)
{
  # Convert factor to numeric 
  # Factor with 2 levels
  train$job_info_experience <- ifelse((train$job_info_experience == "Y"),1,0)
  train$job_info_job_req_normal <- ifelse((train$job_info_job_req_normal == "Y"),1,0)
  train$job_info_training <- ifelse((train$job_info_training == "Y"),1,0)
  # Convert Ordinal factor to numeric
  train$pw_level_9089 <- as.numeric(train$pw_level_9089)
  
  return(train)
}
  
# transform_indicator function: 
# Convert categorical variables into indicator variables
transform_indicator <- function(train)
{
  
  train <- transform_numeric(train)
  
  # Nominal factor with more than 2 levels
  # job_info_education_indicators <- model.matrix( ~ job_info_education - 1, data=train )
  # train <- cbind(train,job_info_education_indicators)
  # train$job_info_education <- NULL
  # 
  foreign_worker_info_education_indicators <- model.matrix( ~ foreign_worker_info_education - 1, data=train )
  train <- cbind(train,foreign_worker_info_education_indicators)
  train$foreign_worker_info_education <- NULL
  

  job_info_work_state_indicators <- model.matrix( ~ job_info_work_state - 1, data=train )
  train <- cbind(train,job_info_work_state_indicators)
  train$job_info_work_state <- NULL
  
  # employer_state_indicators <- model.matrix( ~ employer_state - 1, data=train )
  # train <- cbind(train,employer_state_indicators)
  #train$employer_state <- NULL
  
  return(train)
}

# Get measures such as class accucary, lifts, Sensitivity, Specificity
get_measures <- function(label, prediction)
{
  
  result <- table(label, prediction) 
  # Get class accuracy
  AccuracyCertified <- 100 * result[1] / (result[1]+result[3])
  AccuracyDenied <- 100 * result[4] / (result[2]+result[4])
  
  # Get lift
  tb1 <- table(label)
  # Certified    Denied 
  # 31266      2503 
  LiftCertified <- (result[1] / (result[1]+result[2]))/(tb1[1]/(tb1[1]+tb1[2]))
  LiftDenied <- (result[4] / (result[3]+result[4]))/(tb1[2]/(tb1[1]+tb1[2]))
  
  # Get Sensitivity and Specificity
  Sensitivity <- 100 * result[1] / (result[1]+result[2])
  Specificity <- 100 * result[4] / (result[3]+result[4]) 
  
  return(c("AccuracyCertified" = AccuracyCertified,"AccuracyDenied" = AccuracyDenied,
           "LiftCertified" = LiftCertified,"LiftDenied" = LiftDenied, 
           "Sensitivity" = Sensitivity, "Specificity" = Specificity))
  
}
######### End Function #########

# Load data
train <- read.csv("final_usvisas_train_scale.csv")
evalu <- read.csv("final_usvisas_evaluation_scale.csv")
test <- read.csv("final_usvisas_test_scale.csv")

##################################################################
# SUPPORT VECTOR MACHINE - SVM

# OPTION 1: ONLY NUMERIC, FLAG, AND ORDINAL VARIABLES
# Select features
features.svm1 <- c("case_status","employer_num_employees","job_info_experience","job_info_job_req_normal","job_info_training",
                        "salary","case_recieved_year","decision_year","weeks_range","pw_level_9089")
# Get new train data
train.svm <- train[,features.svm1]

# Convert factors variable to numeric variable
train.svm <- transform_numeric(train.svm)
# Fit svm model
(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", 
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial")
(t2 <- format(Sys.time()))

summary(model.svm) # Number of Support Vectors:  3433

pred.svm.train<-(predict(model.svm, train.svm))

# Get class accuracy and lift
(get_measures(train.svm$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.225996               51.817819                1.039641               11.369954 

## Check for evaluation data set
evalu.svm <- evalu[,features.svm1]
evalu.svm <- transform_numeric(evalu.svm)

pred.svm.evl <-(predict(model.svm, evalu.svm))
# Get class accuracy and lift
(get_measures(evalu.svm$case_status, pred.svm.evl))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.345713               49.768519                1.039441               11.249884 

## Check for test data set
test.svm <- test[,features.svm1]
test.svm <- transform_numeric(test.svm)
pred.svm.test <-(predict(model.svm, test.svm))
# Get class accuracy and lift
(get_measures(test.svm$case_status, pred.svm.test))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.145627               49.166667                1.037733               11.025282


##################################################################
# OPTION 2: OPTION 1 + ADD CATEGORICAL VARIABLES: foreign_worker_info_education, and job_info_work_state

# Reload all the usvisas data
usvisas <- read.csv("final_usvisas_scale.csv")
usvisas <- na.omit(usvisas)

features.svm2 <- c("case_status","employer_num_employees","job_info_experience","job_info_job_req_normal","job_info_training",
                   "salary","case_recieved_year","decision_year","weeks_range","pw_level_9089",
                   "foreign_worker_info_education", "job_info_work_state")

# We need to transform data prior to divide into train, evaluation and test set
usvisas <- usvisas[,features.svm2]
usvisas <- transform_indicator(usvisas)

# DIVIDE into train, evaluation, and test data
nro <- nrow(usvisas)
set.seed(123) # Keep the same seed as in data preprocessing 
nid <- sample(1:nro,size=0.6*nro)
# Get train data
train <- usvisas[nid,] 

test <- usvisas[-nid,]
tid <- sample(1:nrow(test),size=0.5*nrow(test))
# Get evaluation data
evaluation <- test[tid,]
# Get test data
test <- test[-tid,]


# Get new train data
train.svm <- train
(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification",
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial")
# model.svm <- svm(case_status ~ ., data = train, type="C-classification", 
#                  cost=10, gamma=0.1, scale=TRUE, kernel="linear")
(t2 <- format(Sys.time()))

summary(model.svm) #5309

pred.svm.train<-(predict(model.svm, train.svm))

# Get class accuracy and lift
(get_measures(train.svm$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.71535               67.15941                1.05231               12.81303 

## Check for evaluation data set
evalu.svm <- evaluation
pred.svm.evl <-(predict(model.svm, evalu.svm))
# Get class accuracy and lift
(get_measures(evalu.svm$case_status, pred.svm.evl))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 98.970461               44.675926                1.035034               10.201154 


## Check for test data set
test.svm <- test
pred.svm.test <-(predict(model.svm, test.svm))
# Get class accuracy and lift
(get_measures(test.svm$case_status, pred.svm.test))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 98.828837               45.952381                1.034995               10.182794 

# Its overfitting => OPTION 1 IS BETTER THAN OPTION 2


##################################################################
# OPTION 3: TUNE OPTION 1 TO GET BEST PARAMETERS

# Get new train data
train.svm <- train[,features.svm1]

# Tune svm model
(t1 <- format(Sys.time()))
# Set a seed for replicable results
set.seed(1)
tune.obj <- tune.svm(case_status ~ ., data = train.svm[1:15000,],
                     gamma = 10^(-3:-1), cost = 10^(1:4))
(t2 <- format(Sys.time()))

summary(tune.obj)
# Took 8 minutes to tune with 5000 observations
# - best parameters:
# gamma cost
# 0.1  1000  
# Took 54 minutes to tune with 15000 observations
# - best parameters:
# gamma cost
# 0.1  1000  

# Run model with best parameters: cost=1000, gamma=0.1
(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", 
                 cost=1000, gamma=0.1, scale=TRUE, kernel="radial")
(t2 <- format(Sys.time()))
summary(model.svm)

pred.svm.train<-(predict(model.svm, train.svm))

# Get class accuracy and lift of train data
(get_measures(train.svm$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.373121               60.926888                1.047095               11.954910 

## Check for evaluation data set
evalu.svm <- evalu[,features.svm1]
evalu.svm <- transform_numeric(evalu.svm)

pred.svm.evl <-(predict(model.svm, evalu.svm))

# Get class accuracy and lift of evaluation data
(get_measures(evalu.svm$case_status, pred.svm.evl))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.153276               54.745370                1.043538               10.985181 


## Check for test data set
test.svm <- test[,features.svm1]
test.svm <- transform_numeric(test.svm)
pred.svm.test <-(predict(model.svm, test.svm))

# Get class accuracy and lift of test data
(get_measures(test.svm$case_status, pred.svm.test))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 96.150523               54.404762                1.040837                7.137930



##################################################################
########  OPTION 4 = OPTION 3 + ADD WEIGHTS

# Set misclassification costs
(table(train.svm$case_status))
weight <- c("Certified"=1,"Denied"=9)

(t1 <- format(Sys.time()))
# set.seed(1234567) # test different seeds, show similar results.
model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", 
                 cost=1000, gamma=0.1, scale=TRUE, kernel="radial", class.weights = weight)
(t2 <- format(Sys.time()))

summary(model.svm)

pred.svm.train<-(predict(model.svm, train.svm))

# Get class accuracy and lifts
(get_measures(train.svm$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 93.075545               87.974431                1.068998                6.802859 


## Check for evaluation data set
evalu.svm <- evalu[,features.svm1]
evalu.svm <- transform_numeric(evalu.svm)
pred.svm.evl <-(predict(model.svm, evalu.svm))

# Get class accuracy and lifts
(get_measures(evalu.svm$case_status, pred.svm.evl))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 92.398730               80.671296                1.064619                6.107040 

## Check for test data set
test.svm <- test[,features.svm1]
test.svm <- transform_numeric(test.svm)
pred.svm.test <-(predict(model.svm, test.svm))

# Get class accuracy and lifts
(get_measures(test.svm$case_status, pred.svm.test))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 95.027359               62.023810                1.046901                6.719942 

##################################################################
# SAVE DATA FOR SPSS ANALYSIS

# Save data to check in IBM SPSS modeler
write.csv(train.svm,file = "train_svm.csv")
write_csv(evalu.svm,path = "evaluation_svm.csv")
write_csv(test.svm,path = "test_svm.csv")

##################################################################
# MODEL EXPLORATION

# Plot the classification results in two dimensions
plot(model.svm,train.svm,decision_year~case_recieved_year)
plot(model.svm,train.svm,decision_year~salary)
plot(model.svm,train.svm,case_recieved_year~salary)
plot(model.svm,train.svm,weeks_range~salary)
plot(model.svm,train.svm,weeks_range~job_info_experience)
plot(model.svm,train.svm,weeks_range~pw_level_9089)
plot(model.svm,train.svm,decision_year~pw_level_9089)

##################################################################
# PLOT ROC CURVES OF BEST SVM MODELS

#install.packages("ROCR")
library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

# Rerun the best SVM model with decision.values=TRUE
(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", decision.values=T,
                 cost=1000, gamma=0.1, scale=TRUE, kernel="radial", class.weights = weight)
(t2 <- format(Sys.time()))

summary(model.svm)

# Prepare data
fitted.train <- attributes(predict(model.svm,train.svm,decision.values=TRUE))$decision.values
fitted.evalu <- attributes(predict(model.svm,evalu.svm,decision.values=TRUE))$decision.values
fitted.test <- attributes(predict(model.svm,test.svm,decision.values=TRUE))$decision.values

fitted.train <- 1 - fitted.train
fitted.evalu <- 1 - fitted.evalu
fitted.test <- 1 - fitted.test

# Plot ROC curves
rocplot(fitted.train,train.svm[,"case_status"],main="ROC Curves of the best SVM model",col="black",cex =.2)
rocplot(fitted.evalu,evalu.svm[,"case_status"],add=T,col="red")
rocplot(fitted.test,test.svm[,"case_status"],add=T,col="blue")
legend("bottomright",legend =c("Train" ,"Evaluation","Test"),
        col=c("black","red","blue"),lty =1, lwd =2, cex =.8)

####### END ####### 
