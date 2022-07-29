install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
library(ggplot2)
df <- read_csv("anes_timeseries_2020_csv_20220210.csv")
table(df['V201021']) #
table(df['V201376']) #How concerned are you that the government might undermine the media?
table(df['V201377']) #How much do you trust the media?
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

ggplot(df, aes(x = as.factor(V201376))) +
  geom_bar(color = "black", fill = "light blue")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.75)) +
  scale_x_discrete(breaks = c(-9,-8,1,2,3,4,5),
                     labels = c("Refused", "Don't know", "None", "A little", 
                              "A moderate amount", "A lot", "A great deal")) +
  labs(x = "Amount of Trust in the Media",
       y = "Count",
       title = "How Much Trust Do US Citizens Have In The Media?") 
   
  

table(df['V202429'])
table(df['V200004'])


data$Q1 <- sub(c(0,1,2), c("yes","no","maybe"), data$Q1)

data$Q1 <- sub(0, "yes", data$Q1)

data$Q1 <- recode(data$Q1, "0" = "Yes", "1" = "No")

fdf <- filter(df, 'V202429' == 1)
fdf
none <- filter(fdf, 'V201376' == 1)

none
df

filter(V202429, 'V202429' == 1)
View(iris)


df(c(1:8281), c(181))


filtered <- filter(df, V202429 == 1 & V201377 == 5) #285

filter(df, V202429 == 1 & V201377 == 1) #110



df %>%
  filter(V201377 == 3) %>%
  ggplot(df, mapping=aes(x = as.factor(V202429), y = V201377)) +
  geom_bar(stat='identity', color = "light blue", fill = "light blue")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.75)) +
  scale_x_discrete(breaks = c(-9,-7,-6,-5,-1,1,5,7),
                   labels = c("Refused", "No post-election data", "No post-election interview",
                              "Interview Breakoff", "Inapplicable",
                              "Democratic", "Republican", "Other")) +
  labs(x = "Categories",
       y = "Count",
       title = "Count of Respondents Having a Great Deal of Trust in the Media") 
  


df %>%
  filter(df, V202429 == 1 & V201377 == 3) #& V203401 == 1
  ggplot(df, aes(x = as.factor(V202429), y = V201377)) +
  geom_bar(stat='identity', color = "light blue", fill = "light blue")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7, hjust = 0.75)) +
  scale_x_discrete(breaks = c(-9,-7,-6,-5,-1,1,5,7),
                   labels = c("Refused", "No post-election data", "No post-election interview",
                                "Interview Breakoff", "Inapplicable",
                                "Democratic", "Republican", "Other")) +
  labs(x = "Categories",
         y = "Count",
         title = "Count of Respondents Having a Great Deal of Trust in the Media") 
  
  
  
  scale_y_discrete(breaks = c(-9,-8,1,2,3,4,5),
                   labels = c("Refused", "Don't Know", "None",
                              "A little", "A moderate amount",
                              "A lot", "A great deal")) +
