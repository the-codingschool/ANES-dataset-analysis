# statistical analysis
chisq.test(anes_finance$registration_party, anes_finance$one_year_ago)

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
anes_finance_gbm <- gbm(unclass(registration_party) ~ one_year_ago + next_year + current_situation,
                data = anes_finance_test,
                n.trees = 500)

# select out only the x values we used from test and predict
anes_finance_gbm_preds <- anes_finance_test %>%
  select(one_year_ago, next_year, current_situation) %>%
  predict(object = anes_finance_gbm)

# save predictions back into test set
anes_finance_test$gbm_pred <- anes_finance_gbm_preds

## Evaluate performance of models

accuracy(anes_finance_test$registration_party, anes_finance_test$gbm_pred)
