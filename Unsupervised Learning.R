
install.packages("ISLR")
library(ISLR)
library(MASS)
library(xtable)

summary(USArrests)
u <- USArrests
head(u)
xtable(head(u))

apply(u,2,mean)
apply(u,2,var)

# DO PCA
help("prcomp")
pca.out <- prcomp(u,scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out,scale = 0)


# EXCERCISE 9
set.seed(1234)
dim(u)

# 1. Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.
dis <- dist(u,method="euclidean")
hc.complete <- hclust(dis,"complete")
plot(hc.complete)
# 2. Cut the dendrogram at a height that results in three distinct clusters. Which states belong to which clusters?
cl3.complete <- cutree(hc.complete,k=3)
table(cl3.complete)

# 3. Scaling
dis.scale <- dist(scale(x=u),method="euclidean")
hc.scale <- hclust(dis.scale,"complete")
plot(hc.scale)
cl3.scale <- cutree(hc.scale,k=3)
table(cl3.scale)

# 4. Compare
par(mfrow=c(1,2))
plot(hc.complete)
plot(hc.scale)
par(mfrow=c(1,1))


# EXCERCISE 10

# 1. Generate simulated data
rn1 <- rnorm(1000,mean=1,sd=2)
matrix1 <- matrix(rn1,nrow=20)
rn2 <- rnorm(1000,mean=5,sd=2)
matrix2 <- matrix(rn2,nrow=20)
rn3 <- rnorm(1000,mean=12,sd=2)
matrix3 <- matrix(rn3,nrow=20)
dim(matrix3)
mym <- rbind(matrix1,matrix2,matrix3)
dim(mym)



# 2. Perform PCA
?prcomp
pc.out <- prcomp(mym,scale=TRUE)
plot(pc.out$x[,1:2],col=c(1,3,5),xlab="Z1",ylab="Z2",pch=12)
biplot(pc.out,scale=0)

# 3. Perform K-Means
kc <- kmeans(mym,centers=3,nstart = 20)
kc$cluster
table(kc$cluster)


###################### FROM LECTURE
# a
set.seed(2)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1
dim(x)
head(x[,1:5])
# b. PCA
pca.o <- prcomp(x)
summary(pca.o)
pca.o$x[,1:2]
plot(pca.o$x[,1:2], col=c(1,3,5), xlab="Z1", ylab="Z2", pch=19)
?plot

# c. K-Means with K = 3 
km.out = kmeans(x, 3, nstart=20)
km.out$cluster <- paste("km",km.out$cluster)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
# Perfect match

# d. K-Means with K = 2
km.out2 <- kmeans(x, 2, nstart = 20)
km.out2$cluster <- paste("km2",km.out2$cluster)
table(km.out2$cluster,c(rep(1,20),rep(2,20),rep(3,20)))

# e. K-Means with K = 4
km.out4 <- kmeans(x, 4, nstart = 20)
km.out4$cluster <- paste("km4",km.out4$cluster)
table(km.out4$cluster, c(rep(1,20),rep(2,20),rep(3,20)))

# f. K-means clustering with K =3 on the first two principal component score vectors
km.pca <- kmeans(pca.o$x[,1:2],centers = 3,nstart = 20)
km.pca$cluster <- paste("km.pca",km.pca$cluster)
table(km.pca$cluster, c(rep(1,20),rep(2,20),rep(3,20)))

# K-Means with scale with K = 3
km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster <- paste("km",km.out$cluster)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

# NEXT PART
# COMBINE PCA WITH LOGISTIC REGRESSION

# FINISH


