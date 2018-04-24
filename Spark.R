
# Spark in R

library(dplyr)
library(sparklyr)
spark_install()
sc <- spark_connect(master = "local")

#install.packages("nycflights13")
library(nycflights13)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
src_tbls(sc)