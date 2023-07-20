library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(foreign)
library(tidyverse)

a1ac <- read.xport("ryan/datasets/P_GHB.XPT")
insulin <- read.xport("ryan/datasets/P_INS.XPT") 
hdl_cholesterol <- read.xport("ryan/datasets/P_HDL.XPT")
ldl <- read.xport("ryan/datasets/P_TRIGLY.XPT")
demographics <- read.csv("data/data.csv")

# SDDSRVYR- all values 66 (data release cycle) 
# RIDSTATR - values are either 1 or 2
# RIDRETH3 - info about ethnicity 
#joined_data <- inner_join(a1ac, insulin)

# combine data by SEQN (id)
merged_data <- list(a1ac, insulin, hdl_cholesterol, ldl, demographics) %>% 
  reduce(inner_join, by = 'SEQN')


# remove NAs and replace with Mean for all columns and rows
NAtoMean <- merged_data
for (i in 1:ncol(merged_data)) {
  NAtoMean[,i][is.na(NAtoMean[,i])] <- mean(NAtoMean[,i], na.rm = TRUE)
}


ggplot(NAtoMean, aes(x= LBDHDD, y = LBXGH, color = RIDRETH3)) +
  geom_point()


