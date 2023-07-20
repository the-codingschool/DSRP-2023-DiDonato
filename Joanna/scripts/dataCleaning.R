#### Load in libraries ####
#install.packages("haven")
library(haven)
library(tidyr)
library(janitor)
library(dplyr)

?tidyr

#### Load in data ####
data <- read.csv("data/data.csv")

View(data)

#### Data Codebook ####
`https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.htm#RIAGENDR`

#### Read XPT ####

read.xpt("P_DR1TOT.XPT")
