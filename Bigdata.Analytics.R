
### BIG DATA ANALYTIC

#install.packages("sparklyr")
library(sparklyr)
library(ggplot2)

spark_install()

sc <- spark_connect(master="local")

sc

small.xy <- data.frame(X=c(8,5,4,5,6,8,9,7,5,7),Y=c(2,3,4,2,5,6,4,4,6,5))
small.xy

cor(small.xy$X,small.xy$y)
var(small.xy$X,small.xy$y)
lm <- lm(Y~X,data=small.xy)
summary(lm)


data(diamonds)
diamonds
dm.lm <- lm(log(price)~log(carat),data=diamonds)
summary(dm.lm)

# Test linear discriminant analysis (LDA)


