#loading in libraries
library(ggplot2)
library(dplyr)

#loading in csv file
anes <- read.csv("WeisProject/anes_wei_2020.csv")
saveRDS(anes, "WeisProject/anes_wei_2020.csv")
View(anes)

lightningTalk <- select(anes, V201336, V201600)
lightningTalk <- filter(lightningTalk, V201336 > 0, V201336 != 5, V201600 > 0)
ggplot(lightningTalk, aes(x = as.factor(V201336), fill = as.factor(V201600))) +
  geom_bar() +
  labs(title = "Americans' opinions on abortion",
       x = "opinion on abortion",
       y = "count",
       fill = "sex") +
  scale_fill_manual(labels = c("male", "female"), values = c("light blue", "purple")) +
  scale_x_discrete(labels = c("1" = "should never 
  be permitted", "2" = "only in cases when the 
  woman’s life is in danger", "3" = "should be permitted for 
  all cases regardless 
  of if woman is in danger but only 
  with an established need", "4" = "should always 
  be permitted")) +
  theme(axis.text.x = element_text(face="bold", color="black", angle=45))
