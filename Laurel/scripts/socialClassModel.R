library(tidyr)
library(dplyr)
library(janitor)
library(ggplot2)
library(rsample)
library(parsnip)
library(yardstick)
library(Metrics)
library(caret)

set.seed(7262023)

class_split_social <- initial_split(social_factors, prop = 0.75)

## Train and test based on the data set
class_train_social <- training(class_split_social)
class_test_social <- testing(class_split_social)

## training the boosted tree classification model ####
boost_class_fit_social <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(depressed ~ ., data = class_train)

boost_class_fit_social$fit$evaluation_log

## training the forested tree classification model ####
forest_class_fit_social <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(depressed ~ ., data = class_train)

forest_class_fit_social$fit

#### evaluate model performance on test set ####
class_results_social <- class_test_social

class_results_social$boost_pred <- predict(boost_class_fit_social, class_test_social)$.pred_class
class_results_social$forest_pred <- predict(forest_class_fit_social, class_test_social)$.pred_class

## calculating the accuracy
accuracy(class_results_social$depressed, class_results_social$boost_pred) ## accuracy is 82.4%
accuracy(class_results_social$depressed, class_results_social$forest_pred) ## accuracy is 84.3%

## calculating the f1 score
boost_predicted <- class_results_social$boost_pred
forest_predicted <- class_results_social$forest_pred
actual <- class_results_social$depressed

# create confusion matrix
confusionMatrix(boost_predicted, actual,
                mode = "everything",
                positive="1")

confusionMatrix(forest_predicted, actual,
                mode = "everything",
                positive="1")

## modelling the distribution of what the model got right and wrong






