
# Remove old variables and release memory
rm(list=ls())
gc()
memory.limit()

# Load libraries
library(readr)

# Reload data
#train <- read.csv("final_cleaned_usvisas.csv")
train <- read.csv("final_usvisas_train_scale.csv")
evalu <- read.csv("final_usvisas_evaluation_scale.csv")
test <- read.csv("final_usvisas_test_scale.csv")

dim(train)
dim(evalu)
str(train)


#########################################
# SUPPORT VECTOR MACHINE - SVM
# Load library
library(e1071)

# Select features for model input

# Option 1: Only select numeric varialbes and factor variables with has two levels

features.svm1 <- c("case_status","employer_num_employees","job_info_experience","job_info_job_req_normal","job_info_training",
                        "salary","case_recieved_year","decision_year","weeks_range")

train.svm <- train[,features.svm1]

model.svm <- svm(case_status ~ ., data = train.svm, type="C-classification", 
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial")
summary(model.svm)

pred.svmT<-(predict(model.svm, train.svm))

# Get class accuracy
table(train.svm$case_status, pred.svmT)
result <- table(train.svm$case_status, pred.svmT) 
(AccuracyCertified = 100 * result[1] / (result[1]+result[3])) # 99.28677
(AccuracyDenied = 100 * result[4] / (result[2]+result[4])) # 57.09149

# Get lift
tb1 <- table(train.svm$case_status)
# Certified    Denied 
# 31266      2503 
LiftCertified <- (result[1] / (result[1]+result[2]))/(tb1[1]/(tb1[1]+tb1[2]))
LiftCertified # 1.043938
LiftDenied <- (result[4] / (result[3]+result[4]))/(tb1[2]/(tb1[1]+tb1[2]))
LiftDenied # 11.67023


## Check for evaluation data set
evalu.svm <- evalu[,features.svm1]
pred.svm.evl <-(predict(model.svm, evalu.svm))
# Get class accuracy
result <- table(evalu.svm$case_status, pred.svm.evl) 
(AccuracyCertified = 100 * result[1] / (result[1]+result[3])) # 99.2976
(AccuracyDenied = 100 * result[4] / (result[2]+result[4])) # 54.62963

# Get lift
tb1 <- table(evalu.svm$case_status)
LiftCertified <- (result[1] / (result[1]+result[2]))/(tb1[1]/(tb1[1]+tb1[2]))
LiftCertified # 1.043496
LiftDenied <- (result[4] / (result[3]+result[4]))/(tb1[2]/(tb1[1]+tb1[2]))
LiftDenied # 11.28378

#########################
# Option 2: Only select numeric varialbes and factor variables with less than 100 levels

features.svm2 <- c("case_status","employer_num_employees","job_info_experience","job_info_job_req_normal","job_info_training",
                   "salary","case_recieved_year","decision_year","weeks_range",
                   "job_info_education","foreign_worker_info_education",
                   "pw_level_9089", "class_of_admission","employer_state","job_info_work_state")

#train.svm2 <- train[,features.svm2]
train <- train[,features.svm2]

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

class_of_admission_indicators <- model.matrix( ~ class_of_admission - 1, data=train )
train <- cbind(train,class_of_admission_indicators)
train$class_of_admission <- NULL

job_info_work_state_indicators <- model.matrix( ~ job_info_work_state - 1, data=train )
train <- cbind(train,job_info_work_state_indicators)
train$job_info_work_state <- NULL

# employer_state_indicators <- model.matrix( ~ employer_state - 1, data=train )
# train <- cbind(train,employer_state_indicators)
train$employer_state <- NULL



model.svm <- svm(case_status ~ ., data = train, type="C-classification", 
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial")
summary(model.svm)

pred.svmT<-(predict(model.svm, train.svm2))

# Get class accuracy
table(train.svm$case_status, pred.svmT)
result <- table(train.svm$case_status, pred.svmT) 
(AccuracyCertified = 100 * result[1] / (result[1]+result[3])) # 99.62899
(AccuracyDenied = 100 * result[4] / (result[2]+result[4])) # 74.35078

# Get lift
tb1 <- table(train.svm$case_status)
# Certified    Denied 
# 31266      2503 
LiftCertified <- (result[1] / (result[1]+result[2]))/(tb1[1]/(tb1[1]+tb1[2]))
LiftCertified # 1.058245
LiftDenied <- (result[4] / (result[3]+result[4]))/(tb1[2]/(tb1[1]+tb1[2]))
LiftDenied # 12.69981



## Check for evaluation data set
evalu.svm <- evalu[,features.svm2]
pred.svm.evl <-(predict(model.svm, evalu.svm))
# Get class accuracy
result <- table(evalu.svm$case_status, pred.svm.evl) 
(AccuracyCertified = 100 * result[1] / (result[1]+result[3])) # 99.2976
(AccuracyDenied = 100 * result[4] / (result[2]+result[4])) # 54.62963

# Get lift
tb1 <- table(evalu.svm$case_status)
LiftCertified <- (result[1] / (result[1]+result[2]))/(tb1[1]/(tb1[1]+tb1[2]))
LiftCertified # 1.043496
LiftDenied <- (result[4] / (result[3]+result[4]))/(tb1[2]/(tb1[1]+tb1[2]))
LiftDenied # 11.28378





# features.svm.scale <- c("case_status","employer_num_employees_scale","job_info_experience","job_info_education","job_info_job_req_normal","job_info_training",
#                   "foreign_worker_info_education","pw_level_9089","salary_scale","case_recieved_year_scale","decision_year_scale","weeks_range_scale")






# Using train data to build classification model
library(tree)
help(tree.control)
ctrl <- tree.control(nobs = 33769, mincut = 2000, minsize = 4000, mindev = 0)
# Build tree

features <- c("case_status","employer_num_employees","job_info_experience","job_info_education","job_info_job_req_normal","job_info_training",
              "foreign_worker_info_education","pw_level_9089","naics_us_code","salary","case_recieved_year","decision_year","weeks_range")


#######################################################################################
# Sample 10% of train for SPSS Modeler
ids <- sample(1:nrow(train),size = 0.1*nrow(train))


train_sample <- train[ids,features]
train_sample <- as.data.frame(train_sample)
head(train_sample)
write_csv(train_sample,path = "C:/Users/44116543/Downloads/train_sammple.csv")

train_sample2 <- train[,features]
write_csv(train_sample2,path = "C:/Users/44116543/Downloads/train_sammple2.csv")
########################################################################################


####################################
# DECISION TREE
#train.tree <- tree(case_status~.,data=train[,features])
train.tree <- tree(case_status~.,control=ctrl,data=train[,features])
# Tree
train.tree

#Plot tree
plot(train.tree,
     type = c("uniform"))

# show node labels on the tree
# pretty = 0 will display category names (pretty = 1, 2, 3, ... sets length of
# strings used in labels)
# cex reduces the font size by 50% of the default size
text(train.tree,
     pretty = 0,
     cex = 0.6)

# get predicted values
pred1 <- predict(train.tree, train, type = "class")

# predicted vs actual cross-tabulation
tbl1 <- table(train$case_status, pred1)
row.names(tbl1) <- paste("actual", row.names(tbl1))
colnames(tbl1) <- paste("predicted", colnames(tbl1))
tbl1

# Get class accuracy
result <- tbl1
(AccuracyCertified = 100 * result[1] / (result[1]+result[3])) # 99.09167
(AccuracyDenied = 100 * result[4] / (result[2]+result[4])) # 49.46065








# Using random forest
#install.packages("randomForest")
library(randomForest)
usvisas_train <- na.omit(usvisas_train)
str(usvisas_train)
names(usvisas_train) <- make.names(names(usvisas_train))
names(usvisas_valid) <- make.names(names(usvisas_valid))
rdfr_model <- randomForest(case_status ~ ., data = usvisas_train,ntree=100,mtry=4, importance = TRUE)

rdfr_predict <- predict(rdfr_model,usvisas_valid)
rdfr_predict
table(rdfr_predict,usvisas_valid$case_status)




