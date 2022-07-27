data <- read.csv("data/anes_timeseries_2020.csv")

library(dplyr)
library(ggplot2)
library(tibble)

library(gbm)



votes_pres <- data %>% filter(V201033 %in% c(1,2) & V201042 %in% c(1,2))
one.way <- aov(V201033 ~ V201042 + V202175 + V201043y, data=votes_pres)
summary(one.way)


cor(votes_pres$V201033,votes_pres$V201042)


iris_len <- nrow(votes_pres)
votes_pres$label <- c(rep("train", ceiling(.9*iris_len)), rep("test", ceiling(.1*iris_len)))

lmmodel <- gbm(V201033 ~ V201115 + V201121 + V201122 + V201124 + V201130 + V201133 + V201142 + V201136 + V201239 + V201240 + V201252 + V201255, data=votes_pres %>% filter(label=="test"),n.trees=500,distribution="gaussian")
summary(lmmodel)

votes_pred <- votes_pres %>% filter(label=="test") %>% predict(object=lmmodel)
dd <- votes_pres %>% filter(label=="test") %>% select(V201033, V201121)
dd$pred <- votes_pred
view(dd)


votes <- data$V201033
votes <- votes[votes %in% c(1,2,3)]

votes_summary <- as.data.frame(table(votes))

votes_sum <- data.frame(
  c("1","2","3","1","2","3"),
  c("dperc", "dperc","dperc", "aperc", "aperc", "aperc"),
  c(.5441517, .4365953, .01925304, .51314164,.46857528,.011773044))
colnames(votes_sum) <- c("vote","cat","perc")

ggplot(data=votes_sum,aes(x=vote,y=perc,fill=cat)) + geom_bar(stat="identity", position="dodge")


