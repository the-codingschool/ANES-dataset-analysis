data <- read.csv("data/anes_timeseries_2020.csv")

library(dplyr)
library(ggplot2)
library(tibble)

votes <- data$V201033
votes <- votes[votes %in% c(1,2,3)]

votes_summary <- as.data.frame(table(votes))
df2 <- data.frame("1"=81282632, "2"=74223234, "3"=1864873)



ggplot(data=votes_summary, aes(x=votes,y=Freq)) + geom_bar(stat="identity")

