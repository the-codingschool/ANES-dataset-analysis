#loading in libraries
library(ggplot2)
library(dplyr)

#loading in csv file
anes <- read.csv("WeisProject/anes_wei_2020.csv")
saveRDS(anes, "WeisProject/anes_wei_2020.csv")
View(anes)