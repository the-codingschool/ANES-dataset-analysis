#loading stuff ib
library(ggplot2)
library(dplyr)
library(gbm)
library(Metrics)

anes <- read.csv("anesplease.csv")
View(anes)

#setting up dataset
anesClean <- select(anes, V201336, V201345x, V201200, V201435, V201549x, V201601)
anesClean <- rename(anesClean, abortion = V201336, deathPenalty = V201345x, libCons = V201200, religion = V201435, race = V201549x, sexuality = V201601)
anesClean <- filter(anesClean, abortion > 0, abortion < 5, deathPenalty > 0, libCons > 0, libCons < 99, religion > 0, race > 0, sexuality > 0)

#splitting into subsets
anes_Clen <- nrow(anesClean)

anesClean$label <- c(rep("training", ceiling(.6*anes_Clen)),
                rep("test", ceiling(.2*anes_Clen)),
                rep("validation", ceiling(.2*anes_Clen))) %>%
  sample(anes_Clen, replace = F)

anes_Ctrain <- filter(anesClean, label == "training")
anes_Ctest <- filter(anesClean, label == "test")
anes_Cvalid <- filter(anesClean, label == "validation")


## ALL VARIABLES

#relative influence and predictions
anes_Cgbm <- gbm(abortion ~ deathPenalty + libCons + religion + race + sexuality, data = anes_Ctrain, n.trees = 500)
anes_Cgbm
summary(anes_Cgbm)

anes_gbm_Cpreds <- anes_Ctest %>%
  select(deathPenalty, libCons, religion, race, sexuality) %>%
  predict(object = anes_Cgbm)

anes_Ctest$gbm_pred <- anes_gbm_Cpreds
View(anes_Ctest)

#calculating errors 
allRMSE <- rmse(anes_Ctest$abortion, anes_Ctest$gbm_pred)
allMAE <- mae(anes_Ctest$abortion, anes_Ctest$gbm_pred)
allRMSE
allMAE

#calculating accuracy
anes_Ctest <- anes_Ctest %>%
  mutate(glm_pred_cat = ifelse(round(gbm_pred, digits = 0) == 1, "never", ifelse(round(gbm_pred, digits = 0) == 2, "sometimes", ifelse(round(gbm_pred, digits = 0) == 3, "most times", "always"))),
         true_abort_cat = ifelse(abortion == 1, "never", ifelse(abortion == 2, "sometimes", ifelse(abortion == 3, "most times", "always"))))

true_vals <-sum(anes_Ctest$glm_pred_cat == anes_Ctest$true_abort_cat)
total_vals <- nrow(anes_Ctest)

allAccuracy <- true_vals/total_vals # 0.45 accuracy
allAccuracy


## libCons ONLY

#rpredictions
lc_Cgbm <- gbm(abortion ~ libCons, data = anes_Ctrain, n.trees = 500)
lc_Cgbm

lc_gbm_Cpreds <- anes_Ctest %>%
  select(libCons) %>%
  predict(object = lc_Cgbm)

anes_Ctest$lc_pred <- lc_gbm_Cpreds
View(anes_Ctest)

#calculating errors 
lcRMSE <- rmse(anes_Ctest$abortion, anes_Ctest$lc_pred)
lcMAE <- mae(anes_Ctest$abortion, anes_Ctest$lc_pred)
lcRMSE
lcMAE

#calculating accuracy
anes_Ctest <- anes_Ctest %>%
  mutate(lc_pred_cat = ifelse(round(lc_pred, digits = 0) == 1, "never", ifelse(round(lc_pred, digits = 0) == 2, "sometimes", ifelse(round(lc_pred, digits = 0) == 3, "most times", "always"))))

true_vals <-sum(anes_Ctest$lc_pred_cat == anes_Ctest$true_abort_cat)
total_vals <- nrow(anes_Ctest)

lcAccuracy <- true_vals/total_vals # 0.48 accuracy
lcAccuracy


