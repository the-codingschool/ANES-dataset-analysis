#downloading libraries
library(ggplot2)
library(dplyr)

#loading in csv file + saving as r object
anes <- read.csv("WeisProject/anes_timeseries_2020_wei.csv")
saveRDS(anes, "WeisProject/anes_timeseries_2020_wei.csv")
View(anes)
