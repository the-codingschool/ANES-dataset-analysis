# statistical analysis
chisq.test(anes_finance$registration_party, anes_finance$one_year_ago)
chisq.test(anes_finance$registration_party, anes_finance$next_year)
chisq.test(anes_finance$registration_party, anes_finance$current_situation)

# Split into training, test, and validation sets
anes_finance_len <- nrow(anes_finance)

anes_finance$label <- c(rep("training", ceiling(.6*anes_finance_len)),
                rep("test", ceiling(.2*anes_finance_len)),
                rep("validation", ceiling(.2*anes_finance_len))) %>%
  sample(anes_finance_len, replace = F)

anes_finance_train <- filter(anes_finance, label == "training")
anes_finance_test <- filter(anes_finance, label == "test")
anes_finance_valid <- filter(anes_finance, label == "validation")

# create the gradient boosting model

anes_finance_train <- mutate(anes_finance_train, party_num = ifelse(registration_party == "Democratic party", 0,1))

anes_finance_gbm <- gbm(party_num ~ one_year_ago + next_year + current_situation,
                data = anes_finance_train,
                n.trees = 500)

# select out only the x values we used from test and predict
anes_finance_gbm_preds <- anes_finance_test %>%
  select(one_year_ago, next_year, current_situation) %>%
  predict(object = anes_finance_gbm)

# save predictions back into test set
anes_finance_test$gbm_pred <- anes_finance_gbm_preds

anes_finance_test <- mutate(anes_finance_test, party_pred1 = ifelse(gbm_pred < 0, "Democratic party", "Republican party"))

# Create other models

# Model 2
anes_finance_gbm <- gbm(party_num ~ one_year_ago + current_situation,
                        data = anes_finance_train,
                        n.trees = 500)

anes_finance_gbm_preds <- anes_finance_test %>%
  select(one_year_ago, current_situation) %>%
  predict(object = anes_finance_gbm)

anes_finance_test$gbm_pred <- anes_finance_gbm_preds

anes_finance_test <- mutate(anes_finance_test, party_pred2 = ifelse(gbm_pred < 0, "Democratic party", "Republican party"))

# Model 3
anes_finance_gbm <- gbm(party_num ~ next_year + current_situation,
                        data = anes_finance_train,
                        n.trees = 500)

anes_finance_gbm_preds <- anes_finance_test %>%
  select(next_year, current_situation) %>%
  predict(object = anes_finance_gbm)

anes_finance_test$gbm_pred <- anes_finance_gbm_preds

anes_finance_test <- mutate(anes_finance_test, party_pred3 = ifelse(gbm_pred < 0, "Democratic party", "Republican party"))

# Model 4
anes_finance_gbm <- gbm(party_num ~ one_year_ago + next_year,
                        data = anes_finance_train,
                        n.trees = 500)

anes_finance_gbm_preds <- anes_finance_test %>%
  select(one_year_ago, next_year) %>%
  predict(object = anes_finance_gbm)

anes_finance_test$gbm_pred <- anes_finance_gbm_preds

anes_finance_test <- mutate(anes_finance_test, party_pred4 = ifelse(gbm_pred < 0, "Democratic party", "Republican party"))

# Model 5
anes_finance_gbm <- gbm(party_num ~ one_year_ago,
                        data = anes_finance_train,
                        n.trees = 500)

anes_finance_gbm_preds <- anes_finance_test %>%
  select(one_year_ago) %>%
  predict(object = anes_finance_gbm)

anes_finance_test$gbm_pred <- anes_finance_gbm_preds

anes_finance_test <- mutate(anes_finance_test, party_pred5 = ifelse(gbm_pred < 0, "Democratic party", "Republican party"))

# Model 6
anes_finance_gbm <- gbm(party_num ~ next_year,
                        data = anes_finance_train,
                        n.trees = 500)

anes_finance_gbm_preds <- anes_finance_test %>%
  select(next_year) %>%
  predict(object = anes_finance_gbm)

anes_finance_test$gbm_pred <- anes_finance_gbm_preds

anes_finance_test <- mutate(anes_finance_test, party_pred6 = ifelse(gbm_pred < 0, "Democratic party", "Republican party"))

# Model 7
anes_finance_gbm <- gbm(party_num ~ current_situation,
                        data = anes_finance_train,
                        n.trees = 500)

anes_finance_gbm_preds <- anes_finance_test %>%
  select(current_situation) %>%
  predict(object = anes_finance_gbm)

anes_finance_test$gbm_pred <- anes_finance_gbm_preds

anes_finance_test <- mutate(anes_finance_test, party_pred7 = ifelse(gbm_pred < 0, "Democratic party", "Republican party"))

## Evaluate performance of models
model1_accuracy <- accuracy(anes_finance_test$registration_party, anes_finance_test$party_pred1) # one_year_ago, next_year, current_situation
model2_accuracy <- accuracy(anes_finance_test$registration_party, anes_finance_test$party_pred2) # one_year_ago, current_situation, most accurate
model3_accuracy <- accuracy(anes_finance_test$registration_party, anes_finance_test$party_pred3) # next_year, current_situation
model4_accuracy <- accuracy(anes_finance_test$registration_party, anes_finance_test$party_pred4) # one_year_ago, next_year
model5_accuracy <- accuracy(anes_finance_test$registration_party, anes_finance_test$party_pred5) # one_year_ago
model6_accuracy <- accuracy(anes_finance_test$registration_party, anes_finance_test$party_pred6) # next_year
model7_accuracy <- accuracy(anes_finance_test$registration_party, anes_finance_test$party_pred7) # current_situation

accuracies <- c(model1_accuracy, model2_accuracy, model3_accuracy, model4_accuracy, model5_accuracy, model6_accuracy, model7_accuracy)
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7")

accuracy_data <- data.frame(accuracies, model_names)

ggplot(data = accuracy_data, aes(x = model_names, y = accuracies, fill = model_names)) +
  geom_bar(stat = "identity") +
  theme(legend.position= "none")

