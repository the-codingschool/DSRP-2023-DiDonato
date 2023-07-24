#### loading in the necessary libraries ####
library(tidyr)
library(ggplot2)
library(dplyr)
library(janitor)
library(haven)

#### loading in the data ####
demographic <- read_xpt("Laurel/data_process/demographic.xpt")
income <- read_xpt("Laurel/data_process/income.xpt")
physical_activity <- read_xpt("Laurel/data_process/physical_activity.xpt")
blood_pressure <- read_xpt("Laurel/data_process/blood_pressure.xpt")
weight <- read_xpt("Laurel/data_process/weight.xpt")
sleep_disorders <- read_xpt("Laurel/data_process/sleep_disorders.xpt")
alcohol_use <- read_xpt("Laurel/data_process/alcohol_use.xpt")
depression <- read_xpt("Laurel/data_process/depression.xpt")
medical_history <- read_xpt("Laurel/data_process/medical.xpt")
occupation <- read_xpt("Laurel/data_process/occupation.xpt")
smoking_history <- read_xpt("Laurel/data_process/smoking_history.xpt")

## Idea: try to see if physical factors affect depression or if self image/social have more effect
## bmi, medical, physical activity, blood pressure, sleep disorder
## self perception of weight, occupation, income, demographic

## Link to the original website for data
'https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?Cycle=2017-2020'

#### Renaming the variables and removing variables for all data sets ####

##### documentation for demographics: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.htm
demographic <- demographic |>
  select(SEQN, RIDAGEYR, RIAGENDR, DMDBORN4, DMDMARTZ,
         DMDEDUC2, INDFMPIR, RIDRETH3) |>
  filter(RIDAGEYR > 21, !is.na(INDFMPIR)) |>
  rename("id" = "SEQN",
         "age" = "RIDAGEYR",
         "gender" = "RIAGENDR",
         "country_of_birth" = "DMDBORN4",
         "marital_status" = "DMDMARTZ",
         "education" = "DMDEDUC2",
         "income_to_poverty" = "INDFMPIR",
         "race" = "RIDRETH3")

##### documentation for income: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_INQ.htm
income <- income |>
  select(-INDFMMPC) |>
  rename("id" = "SEQN",
         "monthly_poverty_index" = "INDFMMPI") |>
  filter(!is.na(monthly_poverty_index))

##### documentation for physical_activity: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_PAQ.htm
physical_activity <- physical_activity |> 
  select(SEQN, PAD680) |>
  rename("sedentary_minutes" = "PAD680",
         "id" = SEQN) |>
  filter(!is.na(sedentary_minutes), sedentary_minutes != 7777, sedentary_minutes != 9999)

##### documentation for blood_pressure: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_BPQ.htm
blood_pressure <- blood_pressure |>
  select(SEQN, BPQ020, BPQ080) |>
  rename("high_blood_pressure" = "BPQ020",
         "high_cholesterol" = "BPQ080",
         "id" = SEQN) |>
  filter(high_blood_pressure != 0, high_cholesterol != 7, high_cholesterol != 9)

##### documentation for weight: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_WHQ.htm#WHQ030
weight <- weight |>
  select(SEQN, WHD010, WHD020, WHQ030) |>
  rename("height" = "WHD010",
         "weight" = "WHD020",
         "self_image" = "WHQ030",
         "id" = SEQN) |>
  filter(!is.na(height), !is.na(weight), height != 7777, height != 9999, 
         weight != 7777, weight != 9999, self_image != 7, self_image != 9)
  
weight$bmi <- trunc(weight$weight * 703 / weight$height ^ 2)

##### documentation for sleep_disorders: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_SLQ.htm
## for the sake of simplicity, I will assume that the time they sleep during the weekend
## and how much they sleep during the weekday will be relatively the same and doesn't
## have a big impact on how much their mood fluctuates
sleep_disorders <- sleep_disorders |>
  select(SEQN, SLD012) |>
  rename("id" = "SEQN",
         "sleep_hours" = "SLD012") |>
  filter(!is.na(sleep_hours), sleep_hours != 77777, sleep_hours != 99999)

##### documentation for depression: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DPQ.htm
depression <- depression |>
  select(SEQN, DPQ020, DPQ060) |>
  rename("depressed" = "DPQ020",
         "self_image" = "DPQ060",
         "id" = SEQN) |>
  filter(depressed >= 0 & depressed <= 3, !is.na(depressed))

##### documentation for medical_history: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_MCQ.htm
medical_history <- medical_history |>
  select(SEQN, MCQ053, MCQ080) |>
  rename("overweight" = "MCQ080",
         "anemia" = "MCQ053",
         "id" = SEQN) |>
  filter(overweight != 9, !is.na(overweight), anemia != 7, anemia != 9)

##### documentation for occupation: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_OCQ.htm
occupation <- occupation |>
  select(SEQN, OCD150) |>
  rename("type_of_work" = "OCD150",
         "id" = SEQN) |>
  filter(type_of_work != 7, type_of_work != 9)

'health_overview <- the master overview of their health after combining all the data'
health_overview <- demographic |>
  inner_join(blood_pressure, by = "id") |>
  inner_join(depression, by = "id") |>
  inner_join(income, by = "id") |>
  inner_join(medical_history, by = "id") |>
  inner_join(occupation, by = "id") |>
  inner_join(physical_activity, by = "id") |>
  inner_join(sleep_disorders, by = "id") |>
  inner_join(weight, by = "id")

health_overview_plot <- 
  filter(health_overview, weight != 9999, weight != 7777, height != 9999, height != 7777)

## data set of all biological factors
## data set of all social factors
