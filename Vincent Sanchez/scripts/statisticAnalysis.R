
library(parsnip)
library(rsample)
library(yardstick)

# first need to run code from "cleaningData.R" then "analyzingData.R"

### Hypothesis Testing ####

# null hypothesis: The age an individual is told they have a liver condition is the same age they are told they have high blood pressure.

# alt hypothesis: The age an individual is told they have a liver condition is different than the age they are told they have high blood pressure.
t.test(test3$agetold_hbp, test3$agetold_liver, paired = T, alternative = "two.sided") # p value: 0.08 (not significant)

# alt hypothesis: The age an individual is told they have a liver condition is less than the age they are told they have high blood pressure.
t.test(test3$agetold_hbp, test3$agetold_liver, paired = T, alternative = "greater") # p value: 0.96 (not significant)

# alt hypothesis: The age an individual is told they have a liver condition is greater than the age they are told they have high blood pressure.
t.test(test3$agetold_hbp, test3$agetold_liver, paired = T, alternative = "less") # p value: 0.04 (SIGNIFICANT)

# graph hypothesis testing results
hypothesis_data <- data.frame(Test = c("Different","Less Than","Greater Than"),
                              P_Value = c(0.08,0.9,0.04))
ggplot(hypothesis_data, aes(x = Test, y = P_Value, fill = Test)) + 
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Test",
       y = "P-Value",
       title = "Hypothesis Testing",
       tag = "Figure 5",
       caption = "Purple threshold is at 0.05") +
  geom_hline(lty = "dashed", color = "purple", linewidth = 1, yintercept = 0.05) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color = "purple", face = "bold", hjust = 1.25))

### Machine Learning ####

# set a seed for reproducability
set.seed(72723)

# create a split
reg_split <- initial_split(test3, prop = .8)

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
summary(lm_fit$fit)

# Boosted Decision Tree
boost_fit <- boost_tree(trees = 50) |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(agetold_hbp ~ agetold_liver + age + height_cm + bmi + weight_kg + race + stillhave_liver + prescr_hbp,
      data = reg_train)

boost_fit$fit$evaluation_log

# most influencial variables in boosted model
library(vip)
vip(boost_fit) + theme_minimal()

# Random Forest
forest_reg_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression") |>
  fit(agetold_hbp ~ agetold_liver + age + height_cm + bmi + weight_kg + race + stillhave_liver + prescr_hbp,
      data = reg_train)

forest_reg_fit$fit

# calculate errors
reg_results <- reg_test
reg_results <- select(reg_results, SEQN, agetold_liver, agetold_hbp)

reg_results$lm_pred <- predict(lm_fit, reg_test)$.pred
reg_results$boost_pred <- predict(boost_fit, reg_test)$.pred
reg_results$forest_pred <- predict(forest_reg_fit, reg_test)$.pred

mae(reg_results, agetold_hbp, lm_pred) # 6.19 (mean absolute error)
mae(reg_results, agetold_hbp, boost_pred) # 7.49
mae(reg_results, agetold_hbp, forest_pred) # 6.92

rmse(reg_results, agetold_hbp, lm_pred) # 8.2 (root of the mean squared error)
rmse(reg_results, agetold_hbp, boost_pred) # 9.7
rmse(reg_results, agetold_hbp, forest_pred) # 9

# graph ML results
ml_models <- data.frame(Model = c("Linear","Boosted Decision Tree","Random Forest"),
                              MAE = c(6.19,7.49,6.92))
ggplot(ml_models, aes(x = Model, y = MAE, fill = Model)) + 
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Model",
       y = "MAE",
       title = "Machine Learning Mean Absolute Errors",
       tag = "Figure 9",
       caption = "Purple threshold is at 4.94 MAE (10% MAPE)") +
  theme_minimal() +
  geom_hline(lty = "dashed", color = "purple", linewidth = 1, yintercept = 4.94) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color = "purple", face = "bold", hjust = 1.48))
