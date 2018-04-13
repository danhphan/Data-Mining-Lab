#############################
# WEEK 6 TUTORIALS
# 

#############################


# 1. LOADING DATA AND DATA CHECKS

# read csv file into R
#cancer <- read.csv("C:/Users/avuser/Documents/CancerData2.csv")
library(readr)
cancer <- read_csv("D:/Drive/Persona/MIT Study/Semester 3/STAT828 Data Mining/Week 6/CancerData2.csv")

dim(cancer)
# get first 10 variable names
names(cancer[1:10])

# get numerical summary for first 5 variables
summary(cancer[,c(1:5)])

# just take the first 100 genes
cancer <- cancer[,c(1:101)]

# 2. HIERARCHICAL CLUSTERING

# install the Hmisc package
# install.packages("Hmisc")

# add the Hmisc package
library(Hmisc)

# (i) Euclidean distance with complete linkage method
# get distance matrix
d = dist(cancer[,c(2:101)], method = "euclidean")
d = dist(cancer[,c(2:101)], method="euclidean")
# run heirarchical clustering algorithm - complete linkage method
hc1 <- hclust(d, method = "complete")
# dendogram
plot(hc1,
     labels = cancer$CancerType,
     main = "Complete Linkage Method",
     xlab = "", ylab = "", sub = "",
     cex = 0.4)
# show groupings on dendogram (for 3 clusters)
rect.hclust(hc1, k=5)
# get cluster labels for each row
hccut1 <- cutree(hc1, k=3)
# add results to dataset
cancer$hc1 <- as.factor(hccut1)
summary(hccut1)
hist(hccut1)
# get help on these functions
help(dist)
help(hclust)

# (ii) re-run using Ward.D2 method
hc2 <- hclust(d, method = "ward.D2")
plot(hc2,
     labels = cancer$CancerType,
     main = "ward.D2 Method",
     xlab = "", ylab = "", sub = "",
     cex = 0.4)
rect.hclust(hc2, k=3)
hccut2 <- cutree(hc2, k=3)
cancer$hc2 <- as.factor(hccut2)

# (iii) standardise variables
cancerS <- scale(cancer[,c(2:101)])

# add cancer types back to scaled data
cancerS <- cbind(cancer[,1], cancerS)
summary(cancerS)
help(cbind)
help(scale)
# re-run for scaled variables
d3 = dist(cancerS[,c(2:101)], method = "euclidean")
hc3 <- hclust(d3, method = "ward.D2")
plot(hc3,
     labels = cancer$CancerType,
     main = "ward.D2 Method",
     xlab = "", ylab = "", sub = "Normalised Variables",
     cex = 0.4)
rect.hclust(hc3,k=3)
cancer$hc2 <- cutree(hc3,k=3)

# Show two dendrograms on a plot
par(mfrow = c(1,2))
plot(hc2,
     labels = cancer$CancerType,
     main = "ward.D2 Method",
     xlab = "", ylab = "", sub = "",
     cex = 0.4)
plot(hc3,
     labels = cancer$CancerType,
     main = "ward.D2 Method",
     xlab = "", ylab = "", sub = "Normalised Variables",
     cex = 0.4)
par(mfrow=c(1,1))
# (iv) check to see if cancer types fall into meaninful clusters
help(table)
table(cancer$CancerType, cancer$hc1)
table(cancer$CancerType, cancer$hc2)

# (v) compare results of (i) and (ii) in a cross-tabulation
# note that rows are for hc1 and columns for hc2
table(cancer$hc1, cancer$hc2)

cancer$hc1 <- paste("hc1", cancer$hc1)
cancer$hc2 <- paste("hc2", cancer$hc2)
table(cancer$hc1, cancer$hc2)

# 3. K MEANS CLUSTERING

# set random seed so you can get the same results
set.seed(123)

# k means solutions for 2, 3 & 4 clusters
# nstart does random start points n times and the best solution is chosen
# default value for nstart is 1
help(kmeans)
kc2 <- kmeans(cancer[,c(2:101)], centers = 2, nstart = 25)
kc3 <- kmeans(cancer[,c(2:101)], centers = 3, nstart = 25)
kc4 <- kmeans(cancer[,c(2:101)], centers = 4, nstart = 25)
kc2
kc2$withinss
kc4$cluster
summary(kc2)
# get help on kmeans
help(kmeans)

# calculate within-cluster sum of squares for 6 clusters
wss <- rep(0,6)
for (i in 1:6) {
  wss[i] <- sum(kmeans(cancer[,c(2:101)], centers = i)$withinss)
}

# plot within-cluster sum of square to select number of clusters
# https://www.statmethods.net/graphs/line.html
plot(1:6, wss,
     type="b", pch = 19,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# add kmeans cluster solutions to the dataset 
cancer$kc2 <- as.factor(kc2$cluster)
cancer$kc3 <- as.factor(kc3$cluster)
cancer$kc4 <- as.factor(kc4$cluster)

# check to see if cancer types fall into meaningful clusters
table(cancer$CancerType,cancer$kc3)
table(cancer$CancerType, cancer$kc2)
table(cancer$CancerType, cancer$kc3)
table(cancer$CancerType, cancer$kc4)

# compare groupings of different methods in a cross-tabulation
#cancer$kc2 <- paste("kc2", cancer$kc2)
#cancer$kc3 <- paste("kc3", cancer$kc3)
#cancer$kc4 <- paste("kc4", cancer$kc4)
# for example ... 
table(cancer$kc3, cancer$hc2)
table(cancer$kc2, cancer$hc2)
cancer$kc3 <- paste("",cancer$kc3)
cancer$kc3 
help(paste)
table(cancer$kc3,cancer$hc2)


# centres gives means of each variable for each cluster
kc2$centers
kc3$centers
kc4$centers

# exploratory chart to compare cluster means
plot(kc2$centers[1,c(1:100)], type = "o", col = "red")
lines(kc2$centers[2,c(1:100)], type = "o", col = "blue")

plot(kc4$centers[1,c(1:100)], type="o",col="1")
lines(kc4$centers[2,c(1:100)], type="o",col="2")
lines(kc4$centers[3,c(1:100)], type="o",col="3")
lines(kc4$centers[4,c(1:100)], type="o",col="4")

# 2-dimensional charts by cluster
pairs(cancer[,c(2:6)], col = c(1:2)[kc2$cluster])
pairs(cancer[,c(41:45)], col = c(1:2)[kc2$cluster])
plot(cancer$Gene44, cancer$Gene45, col = c(1:2)[kc2$cluster])
library(ggplot2)
ggplot(cancer, aes(Gene44, Gene45, color = kc2, shape = kc2)) + geom_point()
ggplot(cancer, aes(Gene1,Gene2,color=kc4, shape=kc4)) + geom_point()

# UPDATE

# 2-dimensional pplot of clusters using first 2 principal components
install.packages("cluster")
library(cluster)
clusplot(cancer[,c(2:101)],
         cancer$hc2,
         color = TRUE,
         shade = FALSE,
         labels = 4,
         lines = 0,
         main = "Clusters of the Cancer Dataset")
# K =3
clusplot(cancer[,c(2:101)],cancer$kc3, color=T,shade = F,labels=3, main = "K-Means cluster for k=3")

# K = 4
clusplot(cancer[,c(2:101)],cancer$kc4, color=T,shade = F,labels=4, main = "K-Means cluster for k=4")

# Compare 3 Clusters between K-means and Hierarchy cluster
par(mfrow=c(1,2))
clusplot(cancer[,c(2:101)],cancer$kc3, color=T,shade = F,labels=3, main = "K-Means cluster for k=3")
clusplot(cancer[,c(2:101)],cancer$hc2, color=T,shade=F,labels=3, main = "Hierarchy with 3 clusters")

