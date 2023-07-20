## loading in the necessary libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(janitor)
library(haven)

## loading in the data
demographics <- read.csv("data/data.csv")
income <- read_xpt("Laurel/data_process/income.xpt")




## Link to the original website for data
'https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?Cycle=2017-2020'

## Renaming the variables



