
# Load data
#library(readr)
#us_perm_visas <- read_csv("D:/us_perm_visas.csv")
usvisa <- read.csv("D:/us_perm_visas.csv")
dim(usvisa)
summary(usvisa[,127:154])
str(usvisa[,127:154])

str(usvisa,list.len=200)
?str
# Check missing data
library(mice)
library(lattice)




### CHECK MISSING PARTTERNS FOR 30 FIRST VARIABLES
usvisa_aggr <- aggr(usvisa[,1:30], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,1:30]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
usvisa_aggr <- aggr(usvisa[,1:20], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,1:20]), cex.axis=.6, gap=5, ylab=c("Proportion of missingness","Missingness Pattern"))

### VARIABLES 31-60
usvisa_aggr <- aggr(usvisa[,31:60], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,31:60]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
### VARIABLES 61-90
usvisa_aggr <- aggr(usvisa[,61:90], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,61:90]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
### VARIABLES 91-120
usvisa_aggr <- aggr(usvisa[,91:120], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,91:120]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
### VARIABLES 121-154
usvisa_aggr <- aggr(usvisa[,121:150], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,121:150]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

str(usvisa)
str(usvisa[,127:136])
summary(usvisa[,127:136])
md.pattern(usvisa[,127:136])
# Missingness pattern can also be visualised in VIM package by
library(VIM)
usvisa_aggr <- aggr(usvisa[,127:136], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,127:136]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

usvisa_aggr <- aggr(usvisa[,127:136], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,127:136]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# Next ten variables
usvisa_aggr <- aggr(usvisa[,137:146], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,137:146]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# Last eight variables
usvisa_aggr <- aggr(usvisa[,147:154], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(usvisa[,147:154]), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
summary(usvisa[,147:154])
str(usvisa[,147:154])
