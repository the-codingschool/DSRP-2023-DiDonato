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


#### Data Code book ####
# `https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.htm#RIDRETH3` 2017-2020 demographics data
# `https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_ALQ.htm` 2017-2020 alcohol use data
# `https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_BPQ.htm` 2017-2020 blood pressure and cholesterol data

                   
#### Cleaning Data ####
# Cleaned Demo Data
demoCLEAN <- select(demoP, c(SEQN,SDDSRVYR,RIDSTATR,RIAGENDR, RIDAGEYR,DMDBORN4,DMDEDUC2,RIDRETH1, RIDRETH3))
demoCLEAN <- rename(demoCLEAN,releaseCycle = SDDSRVYR,interviewStatus = RIDSTATR,
                    gender = RIAGENDR,age = RIDAGEYR,birthCountry = DMDBORN4,
                    education = DMDEDUC2,race = RIDRETH1, raceWithNHAsian = RIDRETH3
                    )
# demoCLEAN["gender"][demoCLEAN["gender"] == "1"] <- "Male"
# demoCLEAN["gender"][demoCLEAN["gender"] == "2"] <- "Female"
View(demoCLEAN)


# Cleaned Alcohol Use Data
View(alcoholUse)

# Cleaned Blood Pressure Data
bpCLEAN <- select(bloodPressure, c(SEQN, BPQ020,BPQ030, BPD035))
#bpCLEAN <- rename(bpCLEAN, )
bpCLEAN$BPQ020 <- as.character(bpCLEAN$BPQ020)
# bpCLEAN[bpCLEAN == "1"] <- "Yes"
# bpCLEAN[bpCLEAN == "2"] <- "No"
# bpCLEAN[bpCLEAN == "9"] <- "Don't Know"
View(bpCLEAN)

# Cleaned Cholesterol Data
chlCLEAN <- select(bloodPressure, c(SEQN, BPQ080, BPQ060, BPQ070))
View(chlCLEAN)


#### Join files ####

allData <- left_join(demoCLEAN, alcoholUse, by = "SEQN") %>%
  left_join(., bpCLEAN, by = "SEQN") %>%
  left_join(., chlCLEAN, by = "SEQN")
View(allData)


#### Remove NA values ####
# Any NA values found in alcohol data set results in the rows being dropped. This is due to those participants in the demographic data did not take the alcohol use survey.
# Removing NA values for BP/CHL shortens the data set drastically. There are still ans for BP/CHL and alcUse data

cleanData <- allData %>% drop_na(ALQ111:ALQ170) 
View(cleanData)



#### Visualize the Data ####
## Alcohol Consumption Based on Gender ##
avg_alcConsum <- summarize(cleanData,
                           mean_alc = mean(ALQ130),
                           .by = gender)
View(avg_alcConsum)




ggplot(data = avg_alcConsum, aes(x= gender, y= mean_alc, fill = gender))+
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values=c('red', "blue"))
  labs(x = "Gender",
       y = "Mean Alcohol Consumption",
       title = "Average Alcohol Consumption Based on Gender") 



  




