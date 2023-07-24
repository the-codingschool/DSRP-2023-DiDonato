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

'health_overview <- the master overview of their health after combining all the data'

## Link to the original website for data
'https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?Cycle=2017-2020'


#### Renaming the variables and removing variables for all data sets ####

## documentation for demographics: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.htm
demographic <- demographic |>
  select(SEQN, RIDAGEYR, RIAGENDR, DMDBORN4, DMDMARTZ,
         DMDEDUC2, INDFMPIR) |>
  filter(RIDAGEYR > 21, !is.na(income_to_poverty)) |>
  rename("id" = "SEQN",
         "age" = "RIDAGEYR",
         "gender" = "RIAGENDR",
         "country_of_birth" = "DMDBORN4",
         "marital_status" = "DMDMARTZ",
         "education" = "DMDEDUC2",
         "income_to_poverty" = "INDFMPIR")

## documentation for income: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_INQ.htm#INDFMMPI
income <- income |>
  select(-INDFMMPC) |>
  rename("id" = "SEQN",
         "monthly_poverty_index" = "INDFMMPI") |>
  filter(!is.na(INDFMMPI), id %in% demographic$id)

## documentation for physical_activity: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_PAQ.htm
physical_activity <- physical_activity |> 

  
  






