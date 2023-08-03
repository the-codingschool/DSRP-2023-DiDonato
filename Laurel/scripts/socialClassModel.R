library(tidyr)
library(dplyr)
library(janitor)
library(ggplot2)
library(rsample)
library(parsnip)
library(yardstick)
library(Metrics)
library(caret)
library(RColorBrewer)
library(wesanderson)
library(vip)

set.seed(7262023)

class_split_social <- initial_split(social_factors, prop = 0.75)

## Train and test based on the data set
class_train_social <- training(class_split_social)
class_test_social <- testing(class_split_social)

## training the boosted tree classification model ####
boost_class_fit_social <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(depressed ~ ., data = class_train_social)

boost_class_fit_social$fit$evaluation_log

## training the forested tree classification model ####
forest_class_fit_social <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(depressed ~ ., data = class_train_social)

forest_class_fit_social$fit

#### evaluate model performance on test set ####
class_results_social <- class_test_social

class_results_social$boost_pred <- predict(boost_class_fit_social, class_test_social)$.pred_class
class_results_social$forest_pred <- predict(forest_class_fit_social, class_test_social)$.pred_class

## calculating the accuracy
accuracy(class_results_social$depressed, class_results_social$boost_pred) ## accuracy is 80.1%
accuracy(class_results_social$depressed, class_results_social$forest_pred) ## accuracy is 79.5%

## calculating the f1 score
boost_predicted <- class_results_social$boost_pred
forest_predicted <- class_results_social$forest_pred
actual <- class_results_social$depressed

# create confusion matrix
boost_matrix_social <- confusionMatrix(boost_predicted, actual,
                mode = "everything",
                positive="1")

forest_matrix_social <- confusionMatrix(forest_predicted, actual,
                mode = "everything",
                positive="1")

## modelling the distribution of what the model got right and wrong
select(class_results_social, depressed, boost_pred, forest_pred)

## boosted tree model
boost_data_frame_social <- data.frame(boost_matrix_social$table)
boost_table_social <- pivot_wider(boost_data_frame_social,
            names_from = Reference,
            values_from = Freq)

## random forest model
forest_data_frame_social <- data.frame(forest_matrix_social$table)
forest_table_social <- pivot_wider(forest_data_frame_social,
            names_from = Reference,
            values_from = Freq)

## modelling the error of the boost tree
ggplot(boost_data_frame_social, aes(fill=Prediction, y=Freq, x=Reference)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_bw()

ggplot(boost_data_frame_social, aes(fill=Prediction, y=Freq, x=Reference)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw()

ggplot(boost_data_frame_social, aes(fill=Prediction, y=Freq, x=Reference)) + 
  geom_bar(position="fill", stat="identity") +
  theme_bw() +
  labs(y = "Percentage") +
  scale_fill_manual(values=wes_palette(n=4, name="Moonrise1"))

## modelling the error of the random forest
ggplot(forest_data_frame_social, aes(fill=Prediction, y=Freq, x=Reference)) + 
  geom_bar(position="fill", stat="identity") +
  theme_bw() +
  labs(y = "Percentage") +
  scale_fill_manual(values=wes_palette(n=4, name="Moonrise1"))

## modelling the importance of each of the social factors
vip(boost_class_fit_social) +
  theme_bw() +
  labs(title = "Important factors in the social model")
