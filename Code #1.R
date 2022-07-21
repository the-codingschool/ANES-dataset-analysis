install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
library(ggplot2)
df <- read_csv("anes_timeseries_2020_csv_20220210.csv")
table(df['V201021']) #
table(df['V201376']) #How concerned are you that the government might undermine the media?
table(df['V202064'])
table(df['V202429']) #which party represents your views the best?

ggplot(df, aes(x = V201021)) +
  geom_bar()+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = c(-9,-8,-1,1,2,3,4,5,6,7,8,9,10),
                    labels = c("Refused", "Don't Know", "Inapplicable", "Joe Biden",
                               "Michael Bloomberg", "Pete Buttigieg", "Amy Klobuchar", 
                               "Bernie Sanders","Elizabeth Warren", "Another Democrat", 
                               "Donald Trump", "Another Republican","Not Republican or Democrat")) + 
                    labs(x = "Voted for Who",
                         y = "Count",
                         title = "Who Citizens Voted for in the Presidential Primary or Caucus")

table(df['V202429'])
