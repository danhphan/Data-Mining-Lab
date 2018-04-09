## REFERENCES
# Missing Data Analysis with mice: http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
# Winnipeg workshop: Handling missing data in R with mice: https://stefvanbuuren.github.io/Winnipeg/
# Load libraries
library(lattice)
library(mice)
library(VIM)
library(readr)

# Load data
#usvisa <- read.csv("D:/us_perm_visas.csv")

us_perm_visas <- read_csv("D:/us_perm_visas.csv")
dim(usvisa)
summary(usvisa[,127:154])
str(usvisa,list.len=200)
x <- c("birth")
x %in% names(usvisa)

summary(usvisa[,"fw_info_birth_country"])

# Check missing data

### CHECK MISSING PARTTERNS FOR 30 FIRST VARIABLES
usvisa_aggr <- aggr(usvisa[,1:30], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,1:30]), cex.axis=.6, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
#usvisa_aggr <- aggr(usvisa[,1:20], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,1:20]), cex.axis=.6, gap=5, ylab=c("Proportion of missingness","Missingness Pattern"))

### VARIABLES 31-60
usvisa_aggr <- aggr(usvisa[,31:60], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,31:60]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
### VARIABLES 61-90
usvisa_aggr <- aggr(usvisa[,61:90], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,61:90]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
### VARIABLES 91-120
usvisa_aggr <- aggr(usvisa[,91:120], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,91:120]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
### VARIABLES 121-154
usvisa_aggr <- aggr(usvisa[,121:150], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,121:150]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#----------------------------------------------------#
### REMOVE ALL COLUMNS WITH COMPLETE MISSINGS
class(usvisa)
# Collumns with complete missing
drops <- c("naics_us_code","naics_us_title","fw_info_alt_edu_experience","fw_info_birth_country","fw_info_education_other","fw_info_postal_code","fw_info_rel_occup_exp","fw_info_req_experience","fw_info_training_comp","fw_info_yr_rel_edu_completed","fw_ownership_interest","ji_fw_live_on_premises","ji_offered_to_sec_j_fw","pw_job_title_908","recr_info_barg_rep_notified","recr_info_prof_org_advert_from","recr_info_prof_org_advert_to")
# Collumns with more than 60% missing
drops2 <- c("employer_yr_estab","employer_num_employees","job_info_training_num_months","job_info_experience_num_months","job_info_alt_occ_num_months","job_info_alt_cmb_ed_oth_yrs","foreign_worker_yr_rel_edu_completed")
usvisa_drop <- usvisa[,!(names(usvisa) %in% drops)]
usvisa_drop <- usvisa_drop[,!(names(usvisa_drop) %in% drops2)]
dim(usvisa_drop)

