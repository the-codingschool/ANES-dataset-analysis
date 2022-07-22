anes = read.csv("data/anes_timeseries_2020.csv")
anes_finance = select(anes, V201018, V201502, V201503, V201594) %>%
  rename("registration_party" = V201018, "one_year_ago" = V201502, "next_year" = V201503, "current_situation" = V201594)

anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "-9"] <- "Refused"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "-8"] <- "Don't know"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "1"] <- "Much better off"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "2"] <- "Somewhat better off"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "3"] <- "About the same"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "4"] <- "Somewhat worse off"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "5"] <- "Much worse off"

anes_finance["registration_party"][anes_finance["registration_party"] == "-9"] <- "Refused"
anes_finance["registration_party"][anes_finance["registration_party"] == "-8"] <- "Don't know"
anes_finance["registration_party"][anes_finance["registration_party"] == "-1"] <- "Inapplicable"
anes_finance["registration_party"][anes_finance["registration_party"] == "1"] <- "Democratic party"
anes_finance["registration_party"][anes_finance["registration_party"] == "2"] <- "Republican party"
anes_finance["registration_party"][anes_finance["registration_party"] == "4"] <- "None or ‘independent’"
anes_finance["registration_party"][anes_finance["registration_party"] == "5"] <- "Other"

ggplot(data = anes_finance, aes(x = one_year_ago, y = ..count.., fill = registration_party)) +
  geom_bar(position = "dodge", color = "black") +
  scale_y_log10() +
  labs(x = "Financial Situation Compared to 1 Year Ago",
       y = "Number of Respondants",
       fill = "Party of Registration")
