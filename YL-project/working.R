data <- read.csv("data/anes_timeseries_2020.csv")

library(dplyr)
library(ggplot2)
library(tibble)

library(gbm)

jo <- data %>% filter(V201033 == 5)
jo %>% select(V201115, V201121, V201124, V201130, V201142, V201136, V201139, V202143, V202144, V201324, V201336) %>% View()




votes_pres <- data %>% filter(V201033 %in% c(1,2) & V201042 %in% c(1,2))
one.way <- aov(V201033 ~ V201042 + V202175 + V201043y, data=votes_pres)
summary(one.way)


cor(votes_pres$V201033,votes_pres$V201042)


iris_len <- nrow(votes_pres)
votes_pres$label <- c(rep("train", ceiling(.9*iris_len)), rep("test", ceiling(.1*iris_len)))

lmmodel <- gbm(V201033 ~ V201115 + V201121 + V201122 + V201124 + V201130 + V201133 + V201142 + V201136 + V201239 + V201240 + V201252 + V201255, data=votes_pres %>% filter(label=="train"),n.trees=1,distribution="gaussian")
summary(lmmodel)

lmmodel2 <- lm(V201033 ~ V201115 + V201121 + V201122 + V201124 + V201130 + V201133 + V201142 + V201136 + V201239 + V201240 + V201252 + V201255, data=votes_pres %>% filter(label=="train"))
summary(lmmodel2)


votes_pred <- votes_pres %>% filter(label=="test") %>% predict(object=lmmodel)
dd <- votes_pres %>% filter(label=="test") %>% select(V201033, V201121)
dd$pred <- round(votes_pred)
dd$pred2 <- votes_pres %>% filter(label=="test") %>% predict(object=lmmodel2)
dd$pred2 <- round(dd$pred2)
View(dd)

library(Metrics)

mae(dd$V201033, dd$pred)
mae(dd$V201033, dd$pred2)
accuracy(dd$V201033, dd$pred)
try_with_n_trees <- function(trees) {
  lmmodel <- gbm(V201033 ~ V201115 + V201121 + V201122 + V201124 + V201130 + V201133 + V201142 + V201136 + V201239 + V201240 + V201252 + V201255, data=votes_pres %>% filter(label=="train"),n.trees=trees,distribution="gaussian")
  votes_pred <- votes_pres %>% filter(label=="test") %>% predict(object=lmmodel)
  dd <- votes_pres %>% filter(label=="test") %>% select(V201033, V201121)
  dd$pred <- round(votes_pred)
  return(mae(dd$V201033, dd$pred))
}
try_with_n_trees(100)

perf <- data.frame(
  c(.04242424, .04242424, .04242424,.03636364,.03030303,.02626263),
  c(2,3,4,15,20,25)
)
colnames(perf) <- c("Performance", "Trees")


ggplot(data=perf, aes(x=Trees, y=Performance)) +
  geom_line()




votes <- data$V201033
votes <- votes[votes %in% c(1,2,3)]

votes_summary <- as.data.frame(table(votes))

votes_sum <- data.frame(
  c("1","2","3","1","2","3"),
  c("dperc", "dperc","dperc", "aperc", "aperc", "aperc"),
  c(.5441517, .4365953, .01925304, .51314164,.46857528,.011773044))
colnames(votes_sum) <- c("vote","cat","perc")

ggplot(data=votes_sum,aes(x=vote,y=perc,fill=cat)) + geom_bar(stat="identity", position="dodge")


