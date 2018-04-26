
# Spark in R
library(ggplot2)
library(dplyr)
library(sparklyr)
spark_install()
sc <- spark_connect(master = "local")

#install.packages("nycflights13")
library(nycflights13)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
src_tbls(sc)
# Filter with year of 2013
flights_tbl %>% filter(year == 2013)

flights_tbl %>% select(year,origin)

flights_tbl %>%
  group_by(origin) %>%
  summarise(mean_delay = mean(dep_delay))
flights_tbl %>% 
  group_by(origin) %>% 
  summarise(mean_delay = mean(dep_delay))

delay <- flights_tbl %>% 
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>% 
  collect
# Display
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

flights_tbl

# Convert to binary variable
flights_tbl %>%
  ft_binarizer("distance","long_flight",threshold = 1500) %>%
  select(distance,long_flight)

# Move to R again
flights_tbl %>%
  ft_binarizer("distance","long_flight",threshold = 1500) %>%
  select(distance,long_flight) %>%
  collect() %>%
  mutate(long_flight2 = ifelse(long_flight == 0,"short","long"))

# More categorical values?

flights_tbl %>%
  ft_bucketizer("distance","distance_cate",splits = c(0,500,1500,Inf)) %>%
  select(distance,distance_cate)
      

###################################################
## Principal component analysis using sparklyr
###################################################

iris_tbl <- copy_to(sc,iris,"iris",overwrite = TRUE)

pca_model <- tbl(sc,"iris") %>%
  select(-Species) %>%
  ml_pca()

print(pca_model)
pca_model
names(pca_model)

# Matrix
D <- as.matrix(iris[1:4])
E <- as.matrix(pca_model$pc)
P <- D %*% E

PCs <- as.data.frame(P)
PCs$Species <- iris$Species

# Plot PCs

ggplot(PCs,aes(PC1,PC2)) +
  geom_point(aes(colour=Species))


#######################################################
## K-means clustering in sparklyr
#######################################################


kmeans_model <- iris_tbl %>%
  select(Petal_Width, Petal_Length) %>%
  ml_kmeans(formula= ~ Petal_Width + Petal_Length, centers = 3)

print(kmeans_model)

# predict the associated class
predicted <- sdf_predict(kmeans_model, iris_tbl) %>%
  collect

table(predicted$Species, predicted$prediction)

# Plot cluster membership
sdf_predict(kmeans_model) %>%
  collect() %>%
  ggplot(aes(Petal_Length,Petal_Width)) +
  geom_point(aes(Petal_Width,Petal_Length),col=factor(predicted$prediction + 1), size=2,alpha=0.5) +
  geom_point(data=kmeans_model$centers,aes(Petal_Width,Petal_Length), pch='x',size=12) +
  scale_color_discrete(name="Predicted Cluser")
