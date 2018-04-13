#######################################
## Userful commands to remember
#######################################



a <- 10
b <- 20
ls()
rm(a)
rm(list=ls())
# Free system memory
gc()

# Get current working directory
getwd()
setwd("D:/Drive/Persona/MIT Study/R Projects/Data-Mining-Lab")

##### VECTOR #####
vec1 <- c(10,20,15,40) # Numeric vector
vec2 <- c("a","b","c",NA) # Character vector
vec3 <- c(TRUE,FALSE,TRUE,TRUE)
nv <- numeric(100)
nv
class(nv)
typeof(nv)
class(vec3)
typeof(vec3)

logic1 <- vec1 < 15
logic1
vec1[logic1] # elements in TRUE positions will be included in subset
vec1[1:2]
vec1[c(1,4)]
vec1[-1]
vec1[-2]

sort(vec1) # ascending sort
sort(vec1,decreasing = T) # Descending sort
help(sort)

help(order)
order(vec1)
vec1[order(vec1)] # ascending sort
vec1[order(vec1,decreasing=T)] # descending sort

# Sequence vector
seq(1,10)
seq(1,10,by=3)
seq(1,10,length=20)

# Repeat
rep(2,5)
rep(1:3,5)
rep(1:3,each=5)
help(rep)
example(rep)

#Remove missing values
vec2 <- c("a", "b", "c", NA)
is.na(vec2)
vec2.a <- vec2[!is.na(vec2)] # Remove missing values
vec2.a
vec2[c(TRUE,TRUE,FALSE,FALSE)] # Get "a" and "b"

##### MATRIX ######

# Create a random matrix
