
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

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)
