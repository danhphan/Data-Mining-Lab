
### BIG DATA ANALYTIC

### Instal Spark
#install.packages("sparklyr")
library(sparklyr)
library(ggplot2)

spark_install()
sc <- spark_connect(master="local")

### Install H20
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wolpert/8/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()





sc

small.xy <- data.frame(X=c(8,5,4,5,6,8,9,7,5,7),Y=c(2,3,4,2,5,6,4,4,6,5))
small.xy

cor(small.xy$X,small.xy$y)
var(small.xy$X,small.xy$y)
lm <- lm(Y~X,data=small.xy)
summary(lm)
broom::glance(lm)

### TEST AIC AND BIC for explaination model

data(diamonds)
summary(diamonds)
dm.lm <- lm(log(price)~log(carat),data=diamonds)
summary(dm.lm)
broom::glance(dm.lm)

lm.null <- lm(price ~ 1, data=diamonds)
summary(lm.null)
AIC(lm.null)
BIC(lm.null)


# Perform linear discriminant analysis (LDA) for IRIS data
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

ggplot(sepal.grid,aes(Sepal.Length,Sepal.Width,fill=Species))+
  geom_tile(alpha=0.2)+
  geom_point(data=iris,aes(col=Species,fill=NA))
