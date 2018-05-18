
# Remove old variables and release memory
rm(list=ls())
gc()
memory.limit()

# Load libraries
library(readr)
library(mice)
library(lattice)
library(VIM)

# Reload data
#train <- read.csv("final_cleaned_usvisas.csv")

train <- read.csv("C:/Users/44116543/Downloads/final_usvisas_train.csv")
evalu <- read.csv("C:/Users/44116543/Downloads/final_usvisas_evaluation.csv")

dim(train)
dim(evalu)
str(train)



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



#########################################
# SVM
library(e1071)

features.svm <- c("case_status","employer_num_employees","job_info_experience","job_info_education","job_info_job_req_normal","job_info_training",
              "foreign_worker_info_education","pw_level_9089","naics_us_code","salary","case_recieved_year","decision_year","weeks_range")

train.svm <- train[,features.svm]

model.svm <- svm(case_status ~ ., data = train.svm,
                 type="C-classification", cost=10, gamma=0.1, scale=TRUE,
                 kernel="radial")
summary(model.svm)

pred.svmT<-(predict(model.svm, train.svm))



# Get class accuracy
table(train.svm$case_status, pred.svmT)
result <- table(train.svm$case_status, pred.svmT) 
(AccuracyCertified = 100 * result[1] / (result[1]+result[3])) # 99.33794
(AccuracyDenied = 100 * result[4] / (result[2]+result[4])) # 60.32761

# Get lift
tb1 <- table(train.svm$case_status)
# Certified    Denied 
# 31266      2503 
LiftCertified <- (result[1] / (result[1]+result[2]))/(tb1[1]/(tb1[1]+tb1[2]))
LiftCertified # 1.046594
LiftDenied <- (result[4] / (result[3]+result[4]))/(tb1[2]/(tb1[1]+tb1[2]))
LiftDenied # 11.8649


## Check for evaluation data set
evalu.svm <- evalu[,features.svm]
pred.svmE <-(predict(model.svm, evalu.svm))
# Get class accuracy
result <- table(evalu.svm$case_status, pred.svmE) 
(AccuracyCertified = 100 * result[1] / (result[1]+result[3])) # 99.1629
(AccuracyDenied = 100 * result[4] / (result[2]+result[4])) # 56.8287

# Get lift
tb1 <- table(evalu.svm$case_status)
LiftCertified <- (result[1] / (result[1]+result[2]))/(tb1[1]/(tb1[1]+tb1[2]))
LiftCertified # 1.045301
LiftDenied <- (result[4] / (result[3]+result[4]))/(tb1[2]/(tb1[1]+tb1[2]))
LiftDenied # 11.06783






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




