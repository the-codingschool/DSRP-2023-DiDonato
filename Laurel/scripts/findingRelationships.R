## loading in the library
library(ggplot2)
library(dplyr)
library(Metrics)
library(parsnip)
library(tidyr)
library(rsample)
library(reshape2)
library(janitor)
library(yardstick)
library(caret)

## looking at the correlation between blood pressure and how they answer the depressed question
ggplot(data = complete_health_overview, aes(x = depressed, y = sleep_hours)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Hours slept", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

## looking at the correlation between bmi and how they feel
ggplot(data = complete_health_overview, aes(x = depressed, y = bmi)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Body mass index", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

## looking at the correlation between family income to poverty and how they feel
ggplot(data = complete_health_overview, aes(x = depressed, y = income_to_poverty)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Ratio of income to poverty", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

## looking at the correlation between monthly poverty index and how they feel
ggplot(data = complete_health_overview, aes(x = depressed, y = monthly_poverty_index)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Monthly poverty index", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

## looking at the correlation between education level and how they feel
ggplot(data = complete_health_overview, aes(x = depressed, y = education)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Education level", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

## looking at the correlation between age and how they feel
ggplot(data = complete_health_overview, aes(x = depressed, y = age)) +
  geom_jitter(width = 0.25, alpha = 0.1, color = "blue") +
  labs(y = "Age", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

## testing the correlation between all the variables
factorsCor <- complete_health_overview |>
  cor() |>
  melt() |>
  as.data.frame()

## plotting the correlation between all variables
ggplot(factorsCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkblue", low = "red", mid = "white", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Variable 1", y = "Variable 2")


#### testing smoking and alcohol ####
alcohol_use <- read_xpt("Laurel/data_process/alcohol.xpt")

## cleaning the alcohol data set
noNAsAlcohol <- alcohol_use |>
  select(SEQN, ALQ121) |>
  rename("id" = "SEQN",
         "alcohol_use_in_12_months" = "ALQ121") |>
  filter(!is.na(alcohol_use_in_12_months))


mental_health_screener_test <- mental_health_screener |>
  select(SEQN, DPQ010:DPQ070, DPQ090) |>
  rename("id" = "SEQN") |>
  filter(!is.na(DPQ010), !is.na(DPQ020), !is.na(DPQ030), !is.na(DPQ040),
         !is.na(DPQ050), !is.na(DPQ060), !is.na(DPQ070), !is.na(DPQ090))
  

completeLook <- inner_join(noNAsAlcohol, mental_health_screener_test, by = "id")

moreFactorsCor <- completeLook |>
  cor() |>
  melt() |>
  as.data.frame()

## plotting the correlation between all variables
ggplot(moreFactorsCor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkblue", low = "red", mid = "white", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data = completeLook, aes(x = DPQ090, y = alcohol_use_in_12_months)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Alcohol use", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()
