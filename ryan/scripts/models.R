library(keras)
library(parsnip)
library(caret)
library(yardstick)
library(modelsummary)
library(MLmetrics)

# taking small samples

CAD2 <- filter(NAtoMean, had_coronary_heart_disease == 2)
CAD1 <- filter(NAtoMean, had_coronary_heart_disease == 1)
CAD2sample <- sample_n(CAD2, 200)

# join sample with people with CAD
CAD <- list(CAD1, CAD2sample) %>%
  reduce(full_join)

# some cleaning
CAD <- filter(CAD, had_coronary_heart_disease != 9) # 9 = refused
CAD$had_coronary_heart_disease <- as.factor(CAD$had_coronary_heart_disease)


#### USING SMALL SAMPLE OF DATA ####
small_split <- initial_split(CAD, prop = 0.75)

small_train <- training(small_split)
small_test <- testing(small_split)

#### XGBOOST TREE ######

boost_small_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(had_coronary_heart_disease ~ age_at_screening_yrs +
        ldl_cholesterol_mg_d_l + had_discomfort_in_chest +
         mean_systolic + 
        had_heart_attack +
        had_thyroid_problem + told_to_reduce_calories +
        had_angina + country_of_birth + told_to_reduce_salt +
        told_to_increase_exercise + had_copd_emphysema_ch_b,
         
      data = small_train)


small_results <- small_test

small_results$boost_pred <- predict(boost_small_fit, new_data = small_test)$.pred_class


confusionMatrix(small_results$boost_pred, 
                as.factor(small_results$had_coronary_heart_disease),
                mode = "everything")



##### RANDOM FOREST ######

forest_small_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(had_coronary_heart_disease ~ age_at_screening_yrs +
         ldl_cholesterol_mg_d_l + had_discomfort_in_chest +
         mean_systolic + 
         had_heart_attack +
         had_thyroid_problem + told_to_reduce_calories +
         had_angina + country_of_birth + told_to_reduce_salt +
         told_to_increase_exercise + had_copd_emphysema_ch_b, 
      data = small_train)

small_results$forest_pred <- predict(forest_small_fit, new_data = small_test)$.pred_class


confusionMatrix(small_results$forest_pred, 
                as.factor(small_results$had_coronary_heart_disease),
                mode = "everything")





##### nerual network #####
train_data <- select(small_train, age_at_screening_yrs,
                     ldl_cholesterol_mg_d_l,had_discomfort_in_chest,
                     mean_systolic, had_heart_attack,
                     had_thyroid_problem, told_to_reduce_calories,
                     had_angina, country_of_birth, told_to_reduce_salt,
                     told_to_increase_exercise, had_copd_emphysema_ch_b)

rows <- nrow(train_data)
cols <- ncol(train_data)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 512, activation = 'relu', 
              input_shape = c(cols)) %>%
  layer_dense(units = 512, activation = 'relu') %>%
  # layer_dense(units = 32, activation = 'relu') %>%
  # layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

learning_rate <- 0.0001
model %>% compile(
  optimizer = optimizer_adam(learning_rate = learning_rate),
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)



train_data <- unlist(train_data)
dim(train_data) <- c(rows, cols)
train_data <- train_data %>% scale() %>% normalize()

train_label <- small_train$had_coronary_heart_disease
train_label <- ifelse(train_label == 2, 0, train_label)
train_label <- train_label %>% as.matrix() 


model %>% fit(
  train_data,
  train_label,
  epochs = 20,
  batch_size = 64,
  validation_split = 0.2
  
)


y_results <- train_label

test_data <- small_test %>% select(
                    age_at_screening_yrs,
                    ldl_cholesterol_mg_d_l,had_discomfort_in_chest,
                    mean_systolic, had_heart_attack,
                    had_thyroid_problem, told_to_reduce_calories,
                    had_angina, country_of_birth, told_to_reduce_salt,
                    told_to_increase_exercise, had_copd_emphysema_ch_b) %>%
  scale() %>% normalize()

probability_scores <- predict(model, test_data)

threshold <- 0.5

predicted_classes <- ifelse(probability_scores > threshold, 1, 0)

test_labels <- small_test$had_coronary_heart_disease
clean_test_labels <- ifelse(test_labels == 2, 0, test_labels)

accuracy <- mean(clean_test_labels == predicted_classes)

factor_test_labels <- as.factor(clean_test_labels)
factor_predicted_classes <- as.factor(predicted_classes)
f1_score_nn <- F1_Score(factor_predicted_classes, factor_test_labels)

nn_conf_matrix <- table(factor_test_labels, factor_predicted_classes)

nn_conf_matrix_df <- as.data.frame(nn_conf_matrix)

colnames(nn_conf_matrix_df) <- c("True_Class","Predicted_Class", "Frequency")

ggplot(nn_conf_matrix_df, aes(x = True_Class, y = Predicted_Class, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Confusion Matrix",
       x = "True Class",
       y = "Predicted Class",
       fill = "Frequency")

##### FULL DATASET ######

NAtoMean$had_coronary_heart_disease <- as.factor(NAtoMean$had_coronary_heart_disease)

class_split <- initial_split(NAtoMean, prop = .75) # use 75% of data for training

# Use the split to form testing and training sets
class_train <- training(class_split)
class_test <- testing(class_split)



boost_class_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(as.factor(had_coronary_heart_disease) ~ ldl_cholesterol_mg_d_l +
        bmi + mean_systolic + #min_sedentary_activities +
       age_at_screening_yrs + moderate_work_activity, 
      data = class_train)




boost_class_fit$fit$evaluation_log

library(Metrics)

class_results <- class_test

class_results$boost_pred <- NA

class_results$boost_pred <- predict(boost_class_fit, new_data = class_test)$.pred_class

# f1(class_results$ever_told_you_had_coronary_heart_disease, class_results$boost_pred)

confusionMatrix(class_results$boost_pred, 
                class_results$ever_told_you_had_coronary_heart_disease,
                mode = "everything")


forest_class_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(as.factor(ever_told_you_had_coronary_heart_disease) ~ ldl_cholesterol_friedewald_mg_d_l +
        bmi + systolic_2nd_reading + minutes_doing_sedentary_activities + systolic_2nd_reading +
        age_at_screening_yrs + moderate_work_activity, data = class_train)


class_results$forest_pred <- predict(forest_class_fit, new_data = class_test)$.pred_class

# f1(class_results$ever_told_you_had_coronary_heart_disease, class_results$boost_pred)

confusionMatrix(class_results$forest_pred, 
                class_results$ever_told_you_had_coronary_heart_disease,
                mode = "everything")

###### TAXI DATA #####
library(dplyr)
library(lubridate)
library(keras)
library(caret)

df <- read.csv("ryan/datasets/taxi-fares.csv")


# Assuming 'df' is the name of your data frame containing the relevant columns


df <- df[df$passenger_count == 1, ]
df <- df[, !(names(df) %in% c('key', 'passenger_count'))]

for (i in seq_len(nrow(df))) {
  old <- Sys.time()
  if (i %% 100 == 0) {
    cat("Iteration:", i, "\n")
    new <- Sys.time()- old
    old <- Sys.time()
    print(new)
  }
  dt <- ymd_hms(df[i, 'pickup_datetime'], tz = 'UTC')
  df[i, 'day_of_week'] <- wday(dt)
  df[i, 'pickup_time'] <- hour(dt)
  x <- (df[i, 'dropoff_longitude'] - df[i, 'pickup_longitude']) * 54.6
  y <- (df[i, 'dropoff_latitude'] - df[i, 'pickup_latitude']) * 69.0
  distance <- sqrt(x^2 + y^2)
  df[i, 'distance'] <- distance
  
}
# seq_len(nrow(df))

df <- df[, !(names(df) %in% c('pickup_datetime', 
                              'pickup_longitude', 'pickup_latitude', 
                              'dropoff_longitude', 'dropoff_latitude'))]

df <- df[df$distance > 1.0 & df$distance < 10.0, ]
df <- df[df$fare_amount > 0.0 & df$fare_amount < 50.0, ]
head(df)

x <- select(df, -fare_amount)
y <- select(df, fare_amount)


x_train <- unlist(x)
dim(x_train) <- c(23298,3)
x_train <- x_train %>% scale() %>% normalize()

y_train <- unlist(y)
y_train <- y_train %>% as.numeric() %>% as.matrix()

taxi_model <- keras_model_sequential()

taxi_model %>%
  layer_dense(units = 512, activation = 'relu',
              input_shape = c(3)) %>%
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dense(units = 1)


taxi_model %>% compile(
  optimizer = 'adam',
  loss = 'mae',
  metrics = list('mae')
)

summary(taxi_model)

taxi_model %>% fit(
  x_train,
  y_train,
  epochs = 100,
  batch_size = 100,
  validation_split = 0.2
)

y_results <- y
y_results$predictions <- predict(taxi_model, x_train)
R_squared <- R2(y_results$predictions, y_results$fare_amount)


