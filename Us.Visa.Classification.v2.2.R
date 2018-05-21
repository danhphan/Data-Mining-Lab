
# Remove old variables and release memory
rm(list=ls())
gc()
memory.limit()

# Load libraries
library(readr)
library(e1071)

######### Function #########

data_preparation <- function(train)
{
  # Convert factor to numeric feature
  # Factor with 2 levels
  train$job_info_experience <- ifelse((train$job_info_experience == "Y"),1,0)
  train$job_info_job_req_normal <- ifelse((train$job_info_job_req_normal == "Y"),1,0)
  train$job_info_training <- ifelse((train$job_info_training == "Y"),1,0)
  
  # Ordinal factor
  train$pw_level_9089 <- as.numeric(train$pw_level_9089)
  
  # Nominal factor with more than 2 levels
  
  job_info_education_indicators <- model.matrix( ~ job_info_education - 1, data=train )
  train <- cbind(train,job_info_education_indicators)
  train$job_info_education <- NULL
  
  foreign_worker_info_education_indicators <- model.matrix( ~ foreign_worker_info_education - 1, data=train )
  train <- cbind(train,foreign_worker_info_education_indicators)
  train$foreign_worker_info_education <- NULL
  
  # class_of_admission_indicators <- model.matrix( ~ class_of_admission - 1, data=train )
  # train <- cbind(train,class_of_admission_indicators)
  train$class_of_admission <- NULL
  
  job_info_work_state_indicators <- model.matrix( ~ job_info_work_state - 1, data=train )
  train <- cbind(train,job_info_work_state_indicators)
  train$job_info_work_state <- NULL
  
  # employer_state_indicators <- model.matrix( ~ employer_state - 1, data=train )
  # train <- cbind(train,employer_state_indicators)
  train$employer_state <- NULL
  
  return(train)
}

get_measures <- function(label, prediction)
{
  
  result <- table(label, prediction) 
  (AccuracyCertified = 100 * result[1] / (result[1]+result[3])) # 99.28677
  (AccuracyDenied = 100 * result[4] / (result[2]+result[4])) # 57.09149
  
  # Get lift
  tb1 <- table(label)
  # Certified    Denied 
  # 31266      2503 
  LiftCertified <- (result[1] / (result[1]+result[2]))/(tb1[1]/(tb1[1]+tb1[2]))
  LiftCertified # 1.043938
  LiftDenied <- (result[4] / (result[3]+result[4]))/(tb1[2]/(tb1[1]+tb1[2]))
  LiftDenied # 11.67023
  
  return(c("AccuracyCertified" = AccuracyCertified,"AccuracyDenied" = AccuracyDenied,
           "LiftCertified" = LiftCertified,"LiftDenied" = LiftDenied))
  
}
######### End Function #########




#########################################
# SUPPORT VECTOR MACHINE - SVM


# Select features for model input

# Option 1: Only select numeric varialbes and factor variables with has two levels

# Reload data
train <- read.csv("final_usvisas_train_scale.csv")
evalu <- read.csv("final_usvisas_evaluation_scale.csv")
test <- read.csv("final_usvisas_test_scale.csv")

features.svm1 <- c("case_status","employer_num_employees","job_info_experience","job_info_job_req_normal","job_info_training",
                        "salary","case_recieved_year","decision_year","weeks_range")

train.svm <- train[,features.svm1]

model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", 
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial")
summary(model.svm)

pred.svm.train<-(predict(model.svm, train.svm))

# Get class accuracy and lift
(get_measures(train.svm$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.066078               53.016380                1.040548               11.058123


## Check for evaluation data set
evalu.svm <- evalu[,features.svm1]
pred.svm.evl <-(predict(model.svm, evalu.svm))
# Get class accuracy
(get_measures(evalu.svm$case_status, pred.svm.evl))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.18214                53.35648                 1.04238                11.00062 


## Check for test data set
test.svm <- test[,features.svm1]
pred.svm.test <-(predict(model.svm, test.svm))
# Get class accuracy
(get_measures(test.svm$case_status, pred.svm.test))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 98.982433               51.309524                1.039408               10.755890 



########  add WEIGHTS
(table(train.svm$case_status))
(wts <- 30000 / table(train.svm$case_status))

(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", 
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial", class.weights = wts)
(t2 <- format(Sys.time()))

summary(model.svm)

pred.svm.train<-(predict(model.svm, train.svm))


# Get class accuracy and lift
(get_measures(train.svm$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 90.785518               87.335198                1.068126                5.820451 


## Check for evaluation data set
evalu.svm <- evalu[,features.svm1]
pred.svm.evl <-(predict(model.svm, evalu.svm))
# Get class accuracy
(get_measures(evalu.svm$case_status, pred.svm.evl))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 90.772635               85.648148                1.069081                5.674757 


## Check for test data set
test.svm <- test[,features.svm1]
pred.svm.test <-(predict(model.svm, test.svm))
# Get class accuracy
(get_measures(test.svm$case_status, pred.svm.test))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 90.861092               84.166667                1.065663                5.711056 

####### TUNE



(t1 <- format(Sys.time()))
tune.obj <- tune.svm(case_status ~ ., data = train.svm[1:5000,], class.weights = wts,
                 gamma = 10^(-3:-1), cost = 10^(1:4))
(t2 <- format(Sys.time()))

summary(tune.obj)

# - best parameters:
#   gamma cost class.weights
# 0.1 1000     0.9595087


########  RE-Run with new best parameters
(table(train.svm$case_status))
(wts <- 30000 / table(train.svm$case_status))

(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", 
                 cost=1000, gamma=0.1, scale=TRUE, kernel="radial", class.weights = wts)
(t2 <- format(Sys.time()))

summary(model.svm)

pred.svm.train<-(predict(model.svm, train.svm))


# Get class accuracy and lift
(get_measures(train.svm$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 91.294057               88.413903                1.069192                6.049948 

## Check for evaluation data set
evalu.svm <- evalu[,features.svm1]
pred.svm.evl <-(predict(model.svm, evalu.svm))
# Get class accuracy
(get_measures(evalu.svm$case_status, pred.svm.evl))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 91.070913               84.027778                1.067568                5.718868


## Check for test data set
test.svm <- test[,features.svm1]
pred.svm.test <-(predict(model.svm, test.svm))
# Get class accuracy
(get_measures(test.svm$case_status, pred.svm.test))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 91.177882               82.857143                1.064498                5.775374 







########  RE-Run with new best parameters
(table(train.svm$case_status))
(wts <- 30000 / table(train.svm$case_status))
wts[1] <- 1
wts[2] <- 10

(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", 
                 cost=1000, gamma=0.1, scale=TRUE, kernel="radial", class.weights = wts)
(t2 <- format(Sys.time()))

summary(model.svm)

pred.svm.train<-(predict(model.svm, train.svm))

# Get class accuracy and lift
(get_measures(train.svm$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 92.103243               87.654814                1.068589                6.347878 

## Check for evaluation data set
evalu.svm <- evalu[,features.svm1]
pred.svm.evl <-(predict(model.svm, evalu.svm))
# Get class accuracy
(get_measures(evalu.svm$case_status, pred.svm.evl))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 91.773309               83.564815                1.067244                5.965055 


## Check for test data set
test.svm <- test[,features.svm1]
pred.svm.test <-(predict(model.svm, test.svm))
# Get class accuracy
(get_measures(test.svm$case_status, pred.svm.test))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 91.725065               80.714286                1.062621                5.900005 


















#########################
# Option 2: Only select numeric varialbes and factor variables with less than 100 levels


usvisas <- read.csv("final_usvisas_scale.csv")

usvisas <- na.omit(usvisas)


features.svm2 <- c("case_status","employer_num_employees","job_info_experience","job_info_job_req_normal","job_info_training",
                   "salary","case_recieved_year","decision_year","weeks_range",
                   "job_info_education","foreign_worker_info_education",
                   "pw_level_9089", "class_of_admission","employer_state","job_info_work_state")

usvisas <- usvisas[,features.svm2]
usvisas <- na.omit(usvisas)
usvisas <- data_preparation(usvisas)



# DIVIDE into train, evaluation, and test data
nro <- nrow(usvisas)
set.seed(123)
nid <- sample(1:nro,size=0.6*nro)
# Get train data
train <- usvisas[nid,] 
dim(train) # 33769   26
table(train$case_status)
# Certified    Denied 
# 31266      2503

test <- usvisas[-nid,]
tid <- sample(1:nrow(test),size=0.5*nrow(test))
# Get evaluation data
evaluation <- test[tid,]
dim(evaluation) # 11257    26
table(evaluation$case_status)
# Certified    Denied 
# 10393       864

# Get test data
test <- test[-tid,]
dim(test) # 11257    26
table(test$case_status)
# Certified    Denied 
# 10417       840 

#############
train.svm2 <- train
#train.svm2 <- data_preparation(train.svm2)

evalu.svm2 <- evaluation
#evalu.svm2 <- data_preparation(evalu.svm2)

(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm2, type="C-classification", 
                 cost=10, gamma=0.1, scale=FALSE, kernel="radial")
(t2 <- format(Sys.time()))

summary(model.svm)

pred.svm.train<-(predict(model.svm, train.svm2))

# Get class accuracy and lift
(get_measures(train.svm2$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.974413               99.121055                1.079295               13.448047 

## Check for evaluation data set
pred.svm.evl2 <-(predict(model.svm, evalu.svm2))
# Get class accuracy and lift
(get_measures(evalu.svm2$case_status, pred.svm.evl2))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.807563               14.236111                1.010918               11.206706 



# SCALE DATA FIRST
# Option 3: Only select numeric varialbes and factor variables with less than 100 levels
# scale data with svm scale=TRUE

usvisas <- read.csv("final_usvisas_scale.csv")

usvisas <- na.omit(usvisas)


features.svm2 <- c("case_status","employer_num_employees_scale","job_info_experience","job_info_job_req_normal","job_info_training",
                   "salary","case_recieved_year_scale","decision_year_scale","weeks_range_scale",
                   "job_info_education","foreign_worker_info_education",
                   "pw_level_9089", "class_of_admission","employer_state","job_info_work_state")

usvisas <- usvisas[,features.svm2]
usvisas <- na.omit(usvisas)
usvisas <- data_preparation(usvisas)



# DIVIDE into train, evaluation, and test data
nro <- nrow(usvisas)
set.seed(123)
nid <- sample(1:nro,size=0.6*nro)
# Get train data
train <- usvisas[nid,] 
dim(train) # 33769   26
table(train$case_status)
# Certified    Denied 
# 31266      2503

test <- usvisas[-nid,]
tid <- sample(1:nrow(test),size=0.5*nrow(test))
# Get evaluation data
evaluation <- test[tid,]
dim(evaluation) # 11257    26
table(evaluation$case_status)
# Certified    Denied 
# 10393       864

# Get test data
test <- test[-tid,]
dim(test) # 11257    26
table(test$case_status)
# Certified    Denied 
# 10417       840 

#############
train.svm2 <- train
#train.svm2 <- data_preparation(train.svm2)

evalu.svm2 <- evaluation
#evalu.svm2 <- data_preparation(evalu.svm2)

(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm2, type="C-classification", 
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial")
(t2 <- format(Sys.time()))

summary(model.svm)

pred.svm.train<-(predict(model.svm, train.svm2))

# Get class accuracy and lift
(get_measures(train.svm2$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.747329               70.755094                1.055286               12.915291 

## Check for evaluation data set
pred.svm.evl2 <-(predict(model.svm, evalu.svm2))
# Get class accuracy and lift
(get_measures(evalu.svm2$case_status, pred.svm.evl2))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 98.951217               40.625000                1.031670                9.941644 



# Option 4: Only select numeric varialbes and factor variables with less than 100 levels
# Normal data with svm scale=TRUE

usvisas <- read.csv("final_usvisas_scale.csv")

usvisas <- na.omit(usvisas)


features.svm2 <- c("case_status","employer_num_employees","job_info_experience","job_info_job_req_normal","job_info_training",
                   "salary","case_recieved_year","decision_year","weeks_range",
                   "job_info_education","foreign_worker_info_education",
                   "pw_level_9089", "class_of_admission","employer_state","job_info_work_state")

usvisas <- usvisas[,features.svm2]
usvisas <- na.omit(usvisas)
usvisas <- data_preparation(usvisas)



# DIVIDE into train, evaluation, and test data
nro <- nrow(usvisas)
set.seed(123)
nid <- sample(1:nro,size=0.6*nro)
# Get train data
train <- usvisas[nid,] 
dim(train) # 33769   26
table(train$case_status)
# Certified    Denied 
# 31266      2503

test <- usvisas[-nid,]
tid <- sample(1:nrow(test),size=0.5*nrow(test))
# Get evaluation data
evaluation <- test[tid,]
dim(evaluation) # 11257    26
table(evaluation$case_status)
# Certified    Denied 
# 10393       864

# Get test data
test <- test[-tid,]
dim(test) # 11257    26
table(test$case_status)
# Certified    Denied 
# 10417       840 

#############
train.svm2 <- train
#train.svm2 <- data_preparation(train.svm2)

evalu.svm2 <- evaluation
#evalu.svm2 <- data_preparation(evalu.svm2)

(t1 <- format(Sys.time()))
model.svm <- svm(case_status ~ ., data = train.svm2, type="C-classification", 
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial")
(t2 <- format(Sys.time()))

summary(model.svm)

pred.svm.train<-(predict(model.svm, train.svm2))

# Get class accuracy and lift
(get_measures(train.svm2$case_status, pred.svm.train))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 99.747329               70.755094                1.055286               12.915291 

## Check for evaluation data set
pred.svm.evl2 <-(predict(model.svm, evalu.svm2))
# Get class accuracy and lift
(get_measures(evalu.svm2$case_status, pred.svm.evl2))
# AccuracyCertified          AccuracyDenied LiftCertified.Certified       LiftDenied.Denied 
# 98.951217               40.625000                1.031670                9.941644 





