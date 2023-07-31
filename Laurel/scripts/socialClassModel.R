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
boost_matrix_social <- confusionMatrix(boost_predicted, actual,
                mode = "everything",
                positive="1")

forest_matrix_social <- confusionMatrix(forest_predicted, actual,
                mode = "everything",
                positive="1")

## modelling the distribution of what the model got right and wrong
select(class_results_social, depressed, boost_pred, forest_pred)

## boosted tree model
'          Reference
Prediction    0    1    2    3
         0 2503  321   60   34
         1   92  275   44   24
         2    3    6   31    8
         3    2    9    6   51
0 % acc: 2503/2600 = 96%
1 % acc: 275/611 = 45%
2 % acc: 31/141 = 22%
3 % acc: 51/117 = 44%'
boost_data_frame_social <- data.frame(boost_matrix_social$table)
boost_table_social <- pivot_wider(boost_data_frame_social,
            names_from = Reference,
            values_from = Freq)

## random forest model
'          Reference
Prediction    0    1    2    3
         0 2554  326   62   37
         1   44  276   38   23
         2    0    5   39    2
         3    2    4    2   55
0 % acc: 2554/2600 = 98%
1 % acc: 276/611 = 45%
2 % acc: 39/141 = 28%
3 % acc: 55/117 = 47%'
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

## modelling the error of the random
ggplot(forest_data_frame_social, aes(fill=Prediction, y=Freq, x=Reference)) + 
  geom_bar(position="fill", stat="identity") +
  theme_bw() +
  labs(y = "Percentage") +
  scale_fill_manual(values=wes_palette(n=4, name="Moonrise1"))
