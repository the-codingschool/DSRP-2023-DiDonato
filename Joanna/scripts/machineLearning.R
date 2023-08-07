####


# How does Gender and Age Affect High Blood Pressure, High Cholesterol and Kidney Health?
# How does birth country affect High Blood Pressure, High Cholesterol and Kidney Health?
# How does Average Daily Alcohol Intake Affect High Blood Pressure, High Cholesterol and Kidney Health?
# How does having 4/5 or more drinks daily affect  High Blood Pressure, High Cholesterol and Kidney Health?



newData <- select(cleanData, -c(SEQN:interviewStatus))
View(newData)

pcas <- prcomp(newData)
summary(pcas)
pcas$roation

pcas$rotation^2

pca_vals <-  as.data.frame(pcas$x)
pca_vals$HaveDrankAlcohol <- newData$HaveDrankAlcohol

ggplot(pca_vals, aes(PC1, PC2)) +
  geom_point()+
  theme_minimal()



data <- na.omit(newData)

library(reshape2)
library(ggplot2)

cor(data)
corB <- data |>
  cor() |>
  melt() |>
  as.data.frame()


ggplot(corB, aes(x = Var1, y= Var2, fill= value)) +
  geom_tile()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



library(rsample)
library(parsnip)
library(yardstick)
library(rsample)
library(MLmetrics)

set.seed(72723)



dataAllFactor <- cleanData |>
  select(gender, `4orMoreDrinksDaily`:FailingKidneys ) |>
  mutate(gender = as.factor(gender),
         `4orMoreDrinksDaily` = as.factor(`4orMoreDrinksDaily`),
         HighBloodPressure = as.factor(HighBloodPressure),
         HighCholesterol = as.factor(HighCholesterol),
         FailingKidneys = as.factor(FailingKidneys))
  
View(dataAllFactor)
         
                                          
                                          
# Classification data set split
class_split <- reg_split <- (initial_split(dataAllFactor ,prop = .75))
class_train <- training(class_split)
class_test <- testing(class_split)

class_test




## Choose suitable model 
# Boost Tree 


boost_class_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(gender ~ ., data = class_train)


boost_class_fit$fit


dataPred <- class_test
dataPred$logReg <- predict(boost_class_fit, class_test)$.pred_class

F1_Score(dataPred$logReg, dataPred$gender)
#F1 Score: 0.8475452




# random forest
random_forest_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(gender ~ ., data = class_train)

random_forest_fit$fit

dataPred2 <- class_test
dataPred2$logReg <- predict(random_forest_fit, class_test)$.pred_class

F1_Score(dataPred2$logReg, dataPred2$gender)
#F1 Score: 0.8475452









random_forest_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(`4orMoreDrinksDaily` ~ ., data = class_train)

random_forest_fit$fit

dataPred2 <- class_test
dataPred2$logReg <- predict(random_forest_fit, class_test)$.pred_class

F1_Score(dataPred2$logReg, dataPred2$`4orMoreDrinksDaily`)

#F1_Score : 0.5182724 





boost_class_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(gender ~ ., data = class_train)


boost_class_fit$fit


dataPred <- class_test
dataPred$logReg <- predict(boost_class_fit, class_test)$.pred_class

F1_Score(dataPred$logReg, dataPred$`4orMoreDrinksDaily`)



results <- class_test

results$forest_pred <- predict(random_forest_fit, class_test)$.pred_class
resutls$boost_pred <- predict(boost_class_fit, class_test)$.pred_class

actual <- results$actual
pred_fr <- results$pred_fr
pred_bp <- results$pred_bp


actual <- factor(results$gender, levels = c("1", "2"))
pred <- facotr(results$logReg, levels = c("1", "2"))
confusionMatrix(pred, actual, mode = "everything", positive = "1")









# Regression Model
# linear regression 

reg_split <- initial_split(cleanData, prop = .8)
reg_train <- training(reg_split)
reg_test <- testing(reg_split)

lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(`4orMoreDrinksDaily` ~ HighBloodPressure + .,
      data = reg_train)

lm_fit$fit
summary(lm_fit$fit)


dataPredReg <- reg_test
dataPredReg$linReg <- predict(lm_fit, reg_test)$.pred

yardstick::mae(dataPredReg, truth = `4orMoreDrinksDaily`, estimate = linReg)
yardstick::rmse(dataPredReg, truth = `4orMoreDrinksDaily`, estimate = linReg)





# boosted decision tree

boost_tree_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(`4orMoreDrinksDaily` ~ HighBloodPressure + .,
      data = reg_train)

boost_tree_fit$fit

dataPredReg2 <- reg_test
dataPredReg2$logReg <- predict(boost_tree_fit, reg_test)$.pred

yardstick::mae(dataPredReg, truth = `4orMoreDrinksDaily`, estimate = logReg)
yardstick::rmse(dataPredReg, truth = `4orMoreDrinksDaily`, estimate = logReg)



