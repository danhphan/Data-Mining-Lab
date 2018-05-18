
# Remove old variables and release memory
rm(list=ls())
gc()
memory.limit()

# Load libraries
library(readr)
library(mice)
library(lattice)
library(VIM)


#######################################################################
## Function
update.state <- function(test,ustates){
  n <- length(test)
  k <- nrow(ustates)
  k
  for(i in 1:n){
    for(j in 1:k){
      if(test[i] %in% ustates$Code[j])
      {
        test[i] <- ustates$UpState[j]
      }
    } 
    
  }
  
  return(test)
}

#######################################################################

# Reload data
usvisas <- read_csv("cleaned_usvisas_v2.csv")
ustates <- read_csv("us_states.csv")
dim(usvisas) # 56335    18
usvisas <- usvisas[,1:18]

dim(ustates)
str(ustates)

str(usvisas)
# Get year of received_date and decision_date
usvisas$case_recieved_year <- year(usvisas$case_received_date)
usvisas$decision_year <- year(usvisas$decision_date)


# Get ranges of weeks between the two dates
usvisas$weeks_range <- difftime(usvisas$decision_date,usvisas$case_received_date,units = "weeks")
usvisas$weeks_range <- as.numeric(usvisas$weeks_range)

hist(usvisas$weeks_range,breaks = 60)
usvisas[which(usvisas$weeks_range > 400),c("case_status","decision_date","weeks_range","employer_name")]


# job_info_work_state
table(usvisas$job_info_work_state)
unique(usvisas$job_info_work_state)
# update states (convert code to state name)
usvisas$job_info_work_state <- update.state(usvisas$job_info_work_state,ustates)


# employer_state
unique(usvisas$employer_state)
usvisas$employer_state <- update.state(usvisas$employer_state,ustates)
table(usvisas$employer_state)


# Check features 
unique(usvisas$class_of_admission)

unique(usvisas$job_info_training)
unique(usvisas$job_info_experience)

length(unique(usvisas$employer_name))
barchart(usvisas$employer_name,usvisas)

length(unique(usvisas$country_of_citizenship))
barchart(usvisas$country_of_citizenship,usvisas)

# OUTLIERS

# salary
summary(usvisas$salary)
boxplot(usvisas$salary)
hist(usvisas$salary)
# Who earn more than 4.000.000$ a year
usvisas[which(usvisas$salary > 4000000),c("case_status","decision_date","employer_name","salary")] # Only 4 cases => Remove
usvisas <- usvisas[which(usvisas$salary <= 4000000),]

# employer_num_employees => STRANGE => DO WE NEED IT???
summary(usvisas$employer_num_employees)
boxplot(usvisas$employer_num_employees)
# What companies have more than 2 million employees?
usvisas[which(usvisas$employer_num_employees > 2000000),] # Only 6 cases => Remove
usvisas <- usvisas[which(usvisas$employer_num_employees <= 2000000),] 

# Companies with no employee?
usvisas[which(usvisas$employer_num_employees == 0),] # 39 cases => Remove
usvisas <- usvisas[which(usvisas$employer_num_employees > 0),] 

boxplot(usvisas$employer_num_employees)
hist(usvisas$employer_num_employees,breaks=50)
# usvisas[which(usvisas$employer_num_employees > 1000000),c("case_status","employer_name","employer_num_employees")]
# unique(usvisas[which(usvisas$employer_num_employees > 1000000),c("case_status","employer_name","employer_num_employees")])
# usvisas[which(usvisas$employer_name %in% "WAL-MART ASSOCIATES, INC."),c("case_status","employer_name","employer_num_employees")]
# View(usvisas[which(usvisas$employer_name %in% "WAL-MART ASSOCIATES, INC."),c("case_status","employer_name","employer_num_employees")])
# unique(usvisas[which(usvisas$employer_name %in% "WAL-MART ASSOCIATES, INC."),c("case_status","employer_name","employer_num_employees")])
# unique(usvisas[which(usvisas$employer_name %in% "IBM CORPORATION"),c("case_status","employer_name","employer_num_employees")])
# dim(usvisas[which(usvisas$employer_num_employees == 1),c("case_status","employer_name","employer_num_employees")])
# unique(usvisas[which(usvisas$employer_name %in% "AMGEN INC."),c("case_status","employer_name","employer_num_employees")])

dim(usvisas) # 56290    21

# Save data for further analysis
usvisas <- na.omit(usvisas)
dim(usvisas) # 56287    21
write_csv(usvisas,path = "final_cleaned_usvisas.csv")



#######################################################################
rm(list=ls())
# Reload data
usvisas <- read_csv("final_cleaned_usvisas.csv")
dim(usvisas) # 56283    21
usvisas <- usvisas[,1:21]
str(usvisas)

table(usvisas$case_status)
# Certified    Denied 
# 52076      4207

# Add scale columns for employer_num_employees, salary, and weeks_range

help("scale")

# DIVIDE into train, evaluation, and test data
nro <- nrow(usvisas)
set.seed(123)
nid <- sample(1:nro,size=0.6*nro)
# Get train data
train <- usvisas[nid,] 
dim(train) # 33769    21
table(train$case_status)
# Certified    Denied 
# 31266      2503

test <- usvisas[-nid,]
tid <- sample(1:nrow(test),size=0.5*nrow(test))
# Get evaluation data
evaluation <- test[tid,]
dim(evaluation) # 11257    21
table(evaluation$case_status)
# Certified    Denied 
# 10393       864

# Get test data
test <- test[-tid,]
dim(test) # 11257    21
table(test$case_status)
# Certified    Denied 
# 10417       840 

# Save data for model building and evaluation
str(train)

write_csv(train,path = "final_usvisas_train.csv")
write_csv(evaluation,path = "final_usvisas_evaluation.csv")
write_csv(test,path = "final_usvisas_test.csv")


########################################################
## DATA EXPLORATION
library(ggplot2)


# Top 10 Employers with High Certified Visa Applications
usvisas[which(usvisas$case_status %in% "Certified"),] %>% group_by(employer_name) %>%
  summarise(CountOfEmployer = n()) %>%
  arrange(desc(CountOfEmployer)) %>%
  mutate(employer_name = reorder(employer_name, CountOfEmployer)) %>%
  head(10) %>%
  
  ggplot(aes(x = employer_name,y = CountOfEmployer)) +
  geom_bar(stat='identity',colour="white",fill="green") +
  geom_text(aes(x = employer_name, y = 1, label = paste0("(",CountOfEmployer,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Employer Name', y = 'Count Of Certified Applications', title = 'Employer Name and Certified Visas') +
  coord_flip() + 
  theme_bw()


# Top 10 coutries with High Certified Visa Applications
usvisas[which(usvisas$case_status %in% "Certified"),] %>% group_by(country_of_citizenship) %>%
  summarise(CountOfEmployer = n()) %>%
  arrange(desc(CountOfEmployer)) %>%
  mutate(country_of_citizenship = reorder(country_of_citizenship, CountOfEmployer)) %>%
  head(10) %>%
  
  ggplot(aes(x = country_of_citizenship,y = CountOfEmployer)) +
  geom_bar(stat='identity',colour="white",fill="green") +
  geom_text(aes(x = country_of_citizenship, y = 1, label = paste0("(",CountOfEmployer,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Country Name', y = 'Count Of Certified Applications', title = 'Country Name and Certified Visas') +
  coord_flip() + 
  theme_bw()

# Top 10 job titles with High Certified Visa Applications
usvisas[which(usvisas$case_status %in% "Certified"),] %>% group_by(naics_us_title) %>%
  summarise(CountOfEmployer = n()) %>%
  arrange(desc(CountOfEmployer)) %>%
  mutate(naics_us_title = reorder(naics_us_title, CountOfEmployer)) %>%
  head(10) %>%
  
  ggplot(aes(x = naics_us_title,y = CountOfEmployer)) +
  geom_bar(stat='identity',colour="white",fill="green") +
  geom_text(aes(x = naics_us_title, y = 1, label = paste0("(",CountOfEmployer,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Job title', y = 'Count Of Certified Applications', title = 'Job title and Certified Visas') +
  coord_flip() + 
  theme_bw()


# Top 10 states with High Certified Visa Applications
usvisas[which(usvisas$case_status %in% "Certified"),] %>% group_by(job_info_work_state) %>%
  summarise(CountOfEmployer = n()) %>%
  arrange(desc(CountOfEmployer)) %>%
  mutate(job_info_work_state = reorder(job_info_work_state, CountOfEmployer)) %>%
  head(10) %>%
  
  ggplot(aes(x = job_info_work_state,y = CountOfEmployer)) +
  geom_bar(stat='identity',colour="white",fill="green") +
  geom_text(aes(x = job_info_work_state, y = 1, label = paste0("(",CountOfEmployer,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'State name', y = 'Count Of Certified Applications', title = 'State name and Certified Visas') +
  coord_flip() + 
  theme_bw()

# Top 15 Companies with High Mean Salary
usvisas[which(usvisas$case_status %in% "Certified"),] %>% group_by(employer_name) %>%
  summarise(CountOfEmployer = mean(salary)) %>%
  arrange(desc(CountOfEmployer)) %>%
  mutate(employer_name = reorder(employer_name, CountOfEmployer)) %>%
  head(15) %>%
  
  ggplot(aes(x = employer_name,y = CountOfEmployer)) +
  geom_bar(stat='identity',colour="white",fill="green") +
  geom_text(aes(x = employer_name, y = 1, label = paste0("(",CountOfEmployer,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Company name', y = 'Mean Salary', title = 'Company name and average Salary') +
  coord_flip() + 
  theme_bw()



# salary
summary(usvisas$salary)
hist(usvisas$salary,col="blue",breaks = 40,
     xlab="Salary",ylab="Numbers of Applications",
     main="Histogram of salary")
boxplot(usvisas$salary,col="green",
        ylab="salary",main="Boxplot of salary")


# employer_num_employees => STRANGE => DO WE NEED IT???
summary(usvisas$employer_num_employees)

hist(usvisas$employer_num_employees,col="blue",breaks = 40,
     xlab="Number of employees",ylab="Numbers of Applications",
     main="Histogram of number of employees")
boxplot(usvisas$employer_num_employees,col="green",
        ylab="Number of employees",main="Boxplot of number of employees")
