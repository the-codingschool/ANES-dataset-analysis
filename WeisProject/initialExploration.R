#loading in libraries
library(ggplot2)
library(dplyr)

#loading in csv file
anes <- read.csv("ANES_losingmymind.csv")
saveRDS(anes, "ANES_losingmymind.csv")
View(anes)

lightningTalk <- select(anes, V201336, V201600)
lightningTalk <- filter(lightningTalk, V201336 > 0, V201336 != 5, V201600 > 0)
ggplot(lightningTalk, aes(x = as.factor(V201336), fill = as.factor(V201600))) +
  geom_bar(position = position_dodge()) +
  labs(title = "Americans' opinions on abortion",
       x = "opinion on abortion",
       y = "count",
       fill = "sex") +
  scale_fill_manual(labels = c("male", "female"), values = c("light blue", "purple")) +
  scale_x_discrete(labels = c("1" = "never", "2" = "sometimes", "3" = "most of the time", "4" = "always")) +
  theme(axis.text.x = element_text(face="bold", color="black"))

dp <- select(anes, V201345x,V201336)
dp <- filter(dp, V201345x > 0, V201345x < 5, V201336 > 0, V201336 < 5)
ggplot(dp, aes(x = as.factor(V201345x), fill = as.factor(V201336), position = "dodge")) +
  geom_bar(position = position_dodge()) +  
  labs(title = "death penalty and abortion opinion overlap",
       x = "opinion on the death penalty",
       y = "count",
       fill = "opinion on abortion") +
  scale_fill_manual(labels = c("there should be no abortions", "only for cases where woman is in danger", "fine as long as purpose is clearly established","abortion should be allowed no matter what"), values = c("pink", "orange", "light blue", "magenta")) +
  scale_x_discrete(labels = c("1" = "favor strongly", "2" = "favor not strongly", "3" = "oppose not strongly", "4" = "oppose strongly"), position = "bottom") +
  theme(axis.text.x = element_text(face="bold", color="black", angle = 20))
  
lc <- select(anes, V201336, V201200)
lc <- filter(lc, V201336 > 0, V201336 != 5, V201200 > 0, V201200 != 99)
ggplot(lc, aes(x = as.factor(V201336), fill = as.factor(V201200))) +
  geom_bar(position = position_dodge(), color = "black") +
  labs(title = "Americans' opinions on abortion",
       x = "should abortions be allowed?",
       y = "count",
       fill = "liberal or conservative") +
  scale_fill_manual(labels = c("very lib", "liberal", "slightly lib", "middle ground", "slightly cons", "conservative", "very cons"), values = c('#ff0000', '#ff7b5a', '#ffbfaa', '#FFFFFF', '#cfb1ff', '#9265ff', '#0000ff')) +
  scale_x_discrete(labels = c("1" = "never", "2" = "sometimes", "3" = "most times", "4" = "always")) +
  theme(axis.text.x = element_text(face="bold", color="black"))
