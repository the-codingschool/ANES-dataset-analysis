# Read the csv file and save the data set
anes = read.csv("data/anes_timeseries_2020.csv")

# Create a subset that contains columns important to my research question
anes_finance = select(anes, V201018, V201502, V201503, V201594) %>%
  rename("registration_party" = V201018, "one_year_ago" = V201502, "next_year" = V201503, "current_situation" = V201594)

# Give the values significant names
anes_finance["registration_party"][anes_finance["registration_party"] == "-9"] <- "Refused"
anes_finance["registration_party"][anes_finance["registration_party"] == "-8"] <- "Don't know"
anes_finance["registration_party"][anes_finance["registration_party"] == "-1"] <- "Inapplicable"
anes_finance["registration_party"][anes_finance["registration_party"] == "1"] <- "Democratic party"
anes_finance["registration_party"][anes_finance["registration_party"] == "2"] <- "Republican party"
anes_finance["registration_party"][anes_finance["registration_party"] == "4"] <- "None or 'independent'"
anes_finance["registration_party"][anes_finance["registration_party"] == "5"] <- "Other"

anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "-9"] <- "Refused"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "-8"] <- "Don't know"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "1"] <- "Much better off"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "2"] <- "Somewhat better off"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "3"] <- "About the same"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "4"] <- "Somewhat worse off"
anes_finance["one_year_ago"][anes_finance["one_year_ago"] == "5"] <- "Much worse off"

anes_finance["next_year"][anes_finance["next_year"] == "-9"] <- "Refused"
anes_finance["next_year"][anes_finance["next_year"] == "-8"] <- "Don't know"
anes_finance["next_year"][anes_finance["next_year"] == "1"] <- "Much better off"
anes_finance["next_year"][anes_finance["next_year"] == "2"] <- "Somewhat better off"
anes_finance["next_year"][anes_finance["next_year"] == "3"] <- "About the same"
anes_finance["next_year"][anes_finance["next_year"] == "4"] <- "Somewhat worse off"
anes_finance["next_year"][anes_finance["next_year"] == "5"] <- "Much worse off"

anes_finance["current_situation"][anes_finance["current_situation"] == "-9"] <- "Refused"
anes_finance["current_situation"][anes_finance["current_situation"] == "-8"] <- "Don't know"
anes_finance["current_situation"][anes_finance["current_situation"] == "1"] <- "Extremely worried"
anes_finance["current_situation"][anes_finance["current_situation"] == "2"] <- "Very worried"
anes_finance["current_situation"][anes_finance["current_situation"] == "3"] <- "Moderately worried"
anes_finance["current_situation"][anes_finance["current_situation"] == "4"] <- "A little worried"
anes_finance["current_situation"][anes_finance["current_situation"] == "5"] <- "Not at all worried"

# Set the order of the categories in each column
anes_finance$registration_party <- factor(anes_finance$registration_party, levels = c("Democratic party", "Republican party", "None or 'independent'", "Other", "Inapplicable", "Don't know", "Refused")) 
anes_finance$one_year_ago <- factor(anes_finance$one_year_ago, levels = c("Much worse off", "Somewhat worse off", "About the same", "Somewhat better off", "Much better off", "Refused")) 
anes_finance$next_year <- factor(anes_finance$next_year, levels = c("Much worse off", "Somewhat worse off", "About the same", "Somewhat better off", "Much better off", "Don't know", "Refused")) 
anes_finance$current_situation <- factor(anes_finance$current_situation, levels = c("Extremely worried", "Very worried", "Moderately worried", "A little worried", "Not at all worried", "Refused")) 

# Filter out rows with the value "Don't know" for columns "one_year_ago" and "current_situation"
anes_finance = filter(anes_finance, one_year_ago != "Don't know", current_situation != "Don't know")

# Create visualizations with the subset
ggplot(data = anes_finance, aes(x = one_year_ago, y = ..count.., fill = registration_party)) +
  geom_bar(position = "dodge", color = "black") +
  scale_y_log10() +
  labs(x = "Financial Situation Compared to 1 Year Ago",
       y = "Number of Respondants",
       fill = "Party of Registration")

ggplot(data = anes_finance, aes(x = next_year, y = ..count.., fill = registration_party)) +
  geom_bar(position = "dodge", color = "black") +
  scale_y_log10() +
  labs(x = "Financial Situation Next Year",
       y = "Number of Respondants",
       fill = "Party of Registration")

ggplot(data = anes_finance, aes(x = current_situation, y = ..count.., fill = registration_party)) +
  geom_bar(position = "dodge", color = "black") +
  scale_y_log10() +
  labs(x = "How Worried About Financial Situation",
       y = "Number of Respondants",
       fill = "Party of Registration")

anes_finance <- anes_finance %>% group_by(one_year_ago, registration_party) %>% summarise(n = n())

ggplot(data = anes_finance, aes(x = one_year_ago, y = registration_party, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), color = "white", size = 4) 
