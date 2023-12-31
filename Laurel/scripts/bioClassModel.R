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
library(wesanderson)
library(vip)

set.seed(7262023)

class_split_bio <- initial_split(bio_factors, prop = 0.75)

## Train and test based on the data set
class_train_bio <- training(class_split_bio)
class_test_bio <- testing(class_split_bio)

## training the boosted tree classification model ####
boost_class_fit_bio <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(depressed ~ ., data = class_train_bio)

boost_class_fit_bio$fit$evaluation_log

## training the forested tree classification model ####
forest_class_fit_bio <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(depressed ~ ., data = class_train_bio)

forest_class_fit_bio$fit

#### evaluate model performance on test set ####
class_results_bio <- class_test_bio

class_results_bio$boost_pred <- predict(boost_class_fit_bio, class_test_bio)$.pred_class
class_results_bio$forest_pred <- predict(forest_class_fit_bio, class_test_bio)$.pred_class

## determining the overall accuracy of the model
accuracy(class_results_bio$depressed, class_results_bio$boost_pred) ## accuracy is 75.9%
accuracy(class_results_bio$depressed, class_results_bio$forest_pred) ## accuracy is 76.3%

## determining the F1 score of the model
boost_predicted <- class_results_bio$boost_pred
forest_predicted <- class_results_bio$forest_pred
actual <- class_results_bio$depressed

boost_matrix_bio <- confusionMatrix(boost_predicted, actual,
                mode = "everything",
                positive="1")

forest_matrix_bio <- confusionMatrix(forest_predicted, actual,
                mode = "everything",
                positive="1")

## modelling the distribution of what the models got right and wrong
select(class_results_bio, depressed, boost_pred, forest_pred)

## boosted tree model
boost_data_frame_bio <- data.frame(boost_matrix_bio$table)

## random forest model
forest_data_frame_bio <- data.frame(forest_matrix_bio$table)

## boost tree error graph
ggplot(boost_data_frame_bio, aes(fill=Prediction, y=Freq, x=Reference)) + 
  geom_bar(position="fill", stat="identity") +
  theme_bw() +
  labs(y = "Percentage") +
  scale_fill_manual(values=wes_palette(n=4, name="Moonrise1"))

## forest tree error graph
ggplot(forest_data_frame_bio, aes(fill=Prediction, y=Freq, x=Reference)) + 
  geom_bar(position="fill", stat="identity") +
  theme_bw() +
  labs(y = "Percentage") +
  scale_fill_manual(values=wes_palette(n=4, name="Moonrise1"))

## tells you the important factors
vip(boost_class_fit_bio) +
  theme_bw() +
  labs(title = "Important factors in the biological model")


