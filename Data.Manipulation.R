
# Manipulating Data with dplyr

# add libarary
library(sparklyr)
library(dplyr)
library(nycflights13)
library(ggplot2)


head(flights)
head(airlines)
sc <- spark_connect(master="local")
flights <- copy_to(sc,flights,"flights")
airlines <- copy_to(sc,airlines,"airlines")

# Check tables
src_tbls(sc)

# Data manipulating
select(flights,year:day,arr_delay,dep_delay)

filter(flights,dep_delay > 1000)

arrange(flights,desc(dep_delay))

summarise(flights, mean_dep_deplay = mean(dep_delay))

m1 <- mutate(flights, speed=distance/air_time * 60)
m2 <- select(m1,carrier:air_time)
m2

# Laziness
c1 <- filter(flights,day==17,month==5,carrier %in% c('UA','WN','AA','DL'))
c2 <- select(c1, year,month,day,carrier,dep_delay,air_time,distance)
c3 <- arrange(c2,year,month,day,carrier)
c4 <- mutate(c3,air_time_hours = air_time/60)
c4

# Piping
# c5 combine all c1->c4
c5 <- flights %>%
  filter(day==17,month==5,carrier %in% c('UA','WN','AA','DL')) %>%
  select(carrier,dep_delay,air_time,distance) %>%
  arrange(carrier) %>%
  mutate(air_time_hours = air_time/60)
c5

# Grouping 
c4 %>% 
  group_by(carrier) %>%
  summarise(count = n(),mean_dep_delay = mean(dep_delay))

# Collecting to R
carrierhours <- collect(c4)

# Test the significance of pairwise differences and plot the results
with(carrierhours,pairwise.t.test(air_time,carrier))

ggplot(carrierhours,aes(carrier,air_time_hours)) +
  geom_boxplot() +
  geom_violin(col="red")


