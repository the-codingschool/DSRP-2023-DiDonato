
library(parsnip)
library(rsample)
library(yardstick)

# first need to run code from "cleaningData.R" then "analyzingData.R"

### Hypothesis Testing ####

# null hypothesis: The age an individual is told they have a liver condition is the same age they are told they have high blood pressure.
# alt hypothesis: The age an individual is told they have a liver condition is different than the age they are told they have high blood pressure.

# t test

t.test(test3$agetold_hbp, test3$agetold_liver, paired = T) # p value: 0.12 (not significant), thus supports null hypothesis


### Machine Learning ####

# set a seed for reproducability
set.seed(72723)

# create a split
reg_split <- initial_split(test3, prop = .85)

# use the split to form testing and training sets
reg_train <- training(reg_split)
reg_test <- testing(reg_split)

# Linear Regression
lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(agetold_hbp ~ agetold_liver + .,
      data = reg_train)

lm_fit$fit
summary(lm_fit$fit) # rse: 8.5, p-value: 6.841e-13

# calculate errors for regression
reg_results <- reg_test
reg_results$lm_pred <- predict(lm_fit, reg_test)$.pred

mae(reg_results, agetold_hbp, lm_pred) # mae: 6.6
rmse(reg_results, agetold_hbp, lm_pred) # rmse: 8.7

# Boosted Decision Tree
boost_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(agetold_hbp ~ agetold_liver + .,
      data = reg_train)

boost_fit$fit$evaluation_log # rmse: 1.6

# Random Forest
forest_reg_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression") |>
  fit(agetold_hbp ~ agetold_liver + .,
      data = reg_train)

forest_reg_fit$fit # mse: 81
