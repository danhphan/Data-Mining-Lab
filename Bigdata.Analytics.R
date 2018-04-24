
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
#install.packages("lda")
library(lda)
library(MASS)
library(tibble)
data("iris")
summary(iris)
# Convert to a tibble
iris <- as.tibble(iris)
summary(iris)

ggplot(iris,aes(Sepal.Length,Sepal.Width,col=Species)) +
  geom_point()

# LDA model
iris.lda <- lda(Species ~ Sepal.Length + Sepal.Width, data=iris)
iris.lda

sepal.grid <- expand.grid(Sepal.Length = seq(4,8,length=100),
                          Sepal.Width = seq(2,4.5,length=100))

sepal.grid <- as.tibble(sepal.grid)
head(sepal.grid)
sepal.grid$Species <- predict(iris.lda,sepal.grid)$class
sepal.grid
