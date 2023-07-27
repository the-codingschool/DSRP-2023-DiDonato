library(tidyr)
library(dplyr)
library(janitor)
library(ggplot2)
library(reshape2)
library(rsample)
library(parsnip)
library(yardstick)
library(Metrics)
library(caret)

set.seed(7262023)

class_split_bio <- initial_split(bio_factors, prop = 0.75)

## Train and test based on the data set
class_train_bio <- training(class_split)
class_test_bio <- testing(class_split)

## training the boosted tree classification model ####
boost_class_fit_bio <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(depressed ~ ., data = class_train)

boost_class_fit_bio$fit$evaluation_log

## training the forested tree classification model ####
forest_class_fit_bio <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(depressed ~ ., data = class_train)

forest_class_fit_bio$fit

#### evaluate model performance on test set ####
class_results_bio <- class_test_bio

class_results_bio$boost_pred <- predict(boost_class_fit_bio, class_test_bio)$.pred_class
class_results_bio$forest_pred <- predict(forest_class_fit_bio, class_test_bio)$.pred_class

## determining the overall accuracy of the model
accuracy(class_results_bio$depressed, class_results_bio$boost_pred) ## accuracy is 79.8%
accuracy(class_results_bio$depressed, class_results_bio$forest_pred) ## accuracy is 79.6%

## determining the F1 score of the model
boost_predicted <- class_results_bio$boost_pred
forest_predicted <- class_results_bio$forest_pred
actual <- class_results_bio$depressed

confusionMatrix(boost_predicted, actual,
                mode = "everything",
                positive="1")

confusionMatrix(forest_predicted, actual,
                mode = "everything",
                positive="1")




