# taking small samples

CAD2 <- filter(NAtoMean, ever_told_you_had_coronary_heart_disease == 2)
CAD1 <- filter(NAtoMean, ever_told_you_had_coronary_heart_disease == 1)
CAD2sample <- sample_n(CAD2, 200)

CAD <- list(CAD1, CAD2sample) %>%
  reduce(full_join)

CAD <- filter(CAD, ever_told_you_had_coronary_heart_disease != 9)
CAD$ever_told_you_had_coronary_heart_disease <- as.factor(CAD$ever_told_you_had_coronary_heart_disease)

#### USING SMALL SAMPLE OF DATA ####
small_split <- initial_split(CAD, prop = 0.75)

small_train <- training(small_split)
small_test <- testing(small_split)

boost_small_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(as.factor(ever_told_you_had_coronary_heart_disease) ~ age_at_screening_yrs + 
        ldl_cholesterol_friedewald_mg_d_l + ever_had_pain_or_discomfort_in_chest +
        bmi + systolic_2nd_reading + minutes_doing_sedentary_activities + 
      moderate_work_activity, 
      data = small_train)



forest_small_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(as.factor(ever_told_you_had_coronary_heart_disease) ~ age_at_screening_yrs + 
        ldl_cholesterol_friedewald_mg_d_l + ever_had_pain_or_discomfort_in_chest +
        bmi + systolic_2nd_reading + minutes_doing_sedentary_activities + 
       diastolic_2nd_reading, 
      data = small_train)


### RESULTS ##

small_results <- small_test

small_results$boost_pred <- predict(boost_small_fit, new_data = small_test)$.pred_class


confusionMatrix(small_results$boost_pred, 
                as.factor(small_results$ever_told_you_had_coronary_heart_disease),
                mode = "everything")


small_results$forest_pred <- predict(forest_small_fit, new_data = small_test)$.pred_class


confusionMatrix(small_results$forest_pred, 
                as.factor(small_results$ever_told_you_had_coronary_heart_disease),
                mode = "everything")


NAtoMean$ever_told_you_had_coronary_heart_disease <- as.factor(NAtoMean$ever_told_you_had_coronary_heart_disease)

class_split <- initial_split(NAtoMean, prop = .75) # use 75% of data for training

# Use the split to form testing and training sets
class_train <- training(class_split)
class_test <- testing(class_split)

library(parsnip)

boost_class_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(as.factor(ever_told_you_had_coronary_heart_disease) ~ ldl_cholesterol_friedewald_mg_d_l +
        bmi + systolic_2nd_reading + minutes_doing_sedentary_activities + systolic_2nd_reading +
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
