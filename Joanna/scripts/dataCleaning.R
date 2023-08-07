#### Load in libraries ####
#install.packages("haven")
library(haven)
library(tidyr)
library(janitor)
library(dplyr)
library(foreign)
library(tidyverse)
library(stringr)
library(reshape2)


#### Load in data ####

# 2017-2020 demographics data vs. 2015-2016 demographics data
demoP <- read.csv("data/data.csv")
#demoI <- read.xport("Joanna/data/DEMO_I.XPT")

# 2017-2020 health data
alcoholUse <- read.xport("Joanna/data/P_ALQ.XPT")
bloodPressure <- read.xport("Joanna/data/P_BPQ.XPT")
mentalHealth <- read.xport("Joanna/data/P_DPQ.XPT")
kidneyHealth <- read.xport("joanna/data/P_KIQ_U.XPT")


#### Data Code book ####
# `https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.htm#RIDRETH3` 2017-2020 demographics data
# `https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_ALQ.htm` 2017-2020 alcohol use data
# `https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_BPQ.htm` 2017-2020 blood pressure and cholesterol data
# `https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_KIQ_U.htm#KIQ022` 2017-2020 kidney health data

                   
#### Cleaning Data ####
# Cleaned Demo Data
demoCLEAN <- select(demoP, c(SEQN,SDDSRVYR,RIDSTATR,RIAGENDR, RIDAGEYR,DMDBORN4,DMDEDUC2, RIDRETH3))|>
  rename(releaseCycle = SDDSRVYR,
         interviewStatus = RIDSTATR,
         gender = RIAGENDR,
         age = RIDAGEYR,
         birthCountry = DMDBORN4,
         education = DMDEDUC2,
         race = RIDRETH3
         )

#View(demoCLEAN)



# Cleaned Alcohol Use Data
View(alcoholUse)
alcCLEAN <- select(alcoholUse, c(SEQN:ALQ151)) |>
  rename(HaveDrankAlcohol = ALQ111,
         HowOftenDrankAlcohol = ALQ121,
         AvgDrinksDaily = ALQ130,
         DaysDrink4or5 = ALQ142,
         TimesDrink4or5in2hrs = ALQ270,
         `8orMoreDrinksOneDay`= ALQ280,
         `12orMoreDrinksOneDay` = ALQ290,
         `4orMoreDrinksDaily` = ALQ151) |>
  filter(HowOftenDrankAlcohol != 77,
         AvgDrinksDaily != 999,
         )



#View(alcCLEAN)

# Cleaned Blood Pressure Data
bpCLEAN <- select(bloodPressure, c(SEQN, BPQ020, BPQ080))|>
  rename(HighBloodPressure = BPQ020,
         HighCholesterol = BPQ080) |>
  filter(HighBloodPressure != 9,
         HighCholesterol !=7,
         HighCholesterol !=9)

#View(bpCLEAN)



# Cleaned Kidney Health Data
khCLEAN <- select(kidneyHealth, c(SEQN, KIQ022)) |>
  rename(FailingKidneys = KIQ022)|>
  filter(
         FailingKidneys != 9)

#View(khCLEAN)

#### Join files ####

allData <- left_join(demoCLEAN, alcCLEAN, by = "SEQN") %>%
  left_join(., bpCLEAN, by = "SEQN") %>%
  left_join(., khCLEAN, by = "SEQN")
#View(allData)


#### Remove NA values ####
# Any NA values found in alcohol data set results in the rows being dropped. This is due to those participants in the demographic data did not take the alcohol use survey.
# Removing NA values for BP/CHL shortens the data set drastically. There are still ans for BP/CHL and alcUse data

cleanData <- allData %>% drop_na(HaveDrankAlcohol:FailingKidneys) 

View(cleanData)






