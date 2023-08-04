install.packages("haven")
install.packages("caret") 
library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(rsample)
library(parsnip)
library(yardstick)
library(Metrics)
library(caret)

#demographic data
data_demo <- read.csv("data/data.csv")
View(data_demo)

#physical activity data
data_PA <- read_xpt("Disha/dataDT/P_PAQ.XPT")
View(data_PA)

#Weight history data
data_WH <- read_xpt("Disha/dataDT/P_WHQ.XPT")
View(data_WH)

#diet behavior and nutrition data
data_DB <- read_xpt("Disha/dataDT/P_DBQ.XPT")
View(data_DB)

#diabetes data
data_DIB <- read_xpt("Disha/dataDT/P_DIQ.XPT")
View(data_DIB)

#alcohol data
data_ALC <- read_xpt("Disha/dataDT/P_ALQ.XPT")
View(data_ALC)

#blood pressure data
data_BP <- read_xpt("Disha/dataDT/P_BPQ.XPT")
View(data_BP)

#cholestrol data
data_CHOL <- read_xpt("Disha/dataDT/P_TCHOL.XPT")
View(data_CHOL)



join_data <- inner_join(data_DIB, data_WH)
View(join_data)

clean_data <- filter(join_data, DIQ010 != 7, DIQ010 != 9, DIQ010 != 3, WHD020 != 7777, WHD020 != 9999)
clean_data2 <- filter(join_data, DID040 != 777, DID040 != 999, DID040 != 666, !is.na(DID040))
charDIA <- mutate(clean_data, CHARDIA = as.character(DIQ010))
View(charDIA)

#graph relating age of diabetes ####
ggplot(clean_data2, aes(x = DID040)) +
  geom_histogram(fill = "#ffb8b1", color = "#993441") +
  labs(x = "Age diagnosed with diabetes",
       y = "frequency")


#null: there is no correlation between weight and diabetes
#alternative: greater weights have more of a chance to have diabetes
diabYes <- filter(charDIA, DIQ010 == "1")
View(diabYes)
diabNo <- filter(charDIA, DIQ010 == "2")
View(diabNo)
t.test(diabYes$WHD020, diabNo$WHD020, paired = F)

#null: gender is not associated with diabetes
#alternative: gender is associated with diabetes
join_data2 <- inner_join(data_demo, data_DIB)
join_data3 <- inner_join(join_data2, data_WH)
View(join_data3)
clean_data3 <- filter(clean_data4, DIQ010 != 7, DIQ010 != 9, DIQ010 != 3, WHD010 != 7777, WHD010 != 9999, !is.na(WHD010), WHD020 != 7777, WHD020 != 9999)
clean_data4 <- select(join_data3, -DID310D, -DID310S, -DIQ300D, -DIQ300S, -DIQ291, -DID330, -DID341, -DID350, -DIQ350U)
View(clean_data4)
characters <- mutate(clean_data3, Diabetes = as.factor(DIQ010), Gender = as.character(RIAGENDR))
View(characters)

ggplot(characters, aes(x = Gender, y = Diabetes)) +
  geom_count() +
  theme_minimal()

t <- table(characters$Diabetes, characters$Gender)
t

chi <- chisq.test(t)
chi
chi$p.value
chi$residuals
corrplot(chi$residuals, is.cor = F)

#correlation plots
dataClean <- select(clean_data3, DIQ010, RIAGENDR, RIDAGEYR, RIDRETH3, WHD010, WHD020)

dataCors <- dataClean |>
  cor() |>
  melt() |>
  as.data.frame()
dataCors

ggplot(dataCors, aes(x = Var1, y= Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0)

set.seed(100)
dataClean2 <- select(characters, Diabetes, RIDAGEYR, WHD010, WHD020)
View(dataClean2)

data_class_split <- initial_split(dataClean2, prop = 0.75)
train_class_data <- training(data_class_split)
test_class_data <- testing(data_class_split)


log_fit <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification") |>
  fit(Diabetes ~., data=train_class_data)
log_fit$fit
summary(log_fit$fit)

boost_tree_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(Diabetes ~., data = train_class_data)
boost_tree_fit$fit


dataPred <- test_class_data
dataPred$logReg <- predict(log_fit, test_class_data)$.pred_class

dataPred <- test_class_data
dataPred$boostPred <- predict(boost_tree_fit, test_class_data)$.pred_class

dataPred
dataPred$Diabetes

f1(dataPred$Diabetes == "1", dataPred$logReg == "1")
f1(dataPred$Diabetes == "2", dataPred$logReg == "2")

actual <- factor(dataPred$Diabetes, levels = c("1","2"))
pred <- factor(dataPred$logReg, levels = c("1","2"))
confusionMatrix(pred, actual, mode = "everything", positive="1")

