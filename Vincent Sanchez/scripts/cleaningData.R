
# read in data files

library(dplyr)
demographics <- read.csv("data/data.csv")

library(foreign)
body_measures <- read.xport("Vincent Sanchez/more-data/P_BMX.XPT")

bp_chol <- read.xport("Vincent Sanchez/more-data/P_BPQ.XPT")

med_conditions <- read.xport("Vincent Sanchez/more-data/P_MCQ.XPT")


### Variable Meanings ####

# SEQN: Respondent sequence number

# RIAGENDR: Gender of the participant
# RIDAGEYR: Age in years of the participant (max 80)
# RIDRETH1: Reported race and Hispanic origin information
# RIDRETH3: Reported race and Hispanic origin information, with Non-Hispanic Asian Category
# DMDMARTZ: Marital Status

# BMXWT: Weight in kg
# BMXHT: Standing height in cm
# BMXBMI: Body Mass Index (kg/m^2)

# BPQ030: Told had high blood pressure 2+ times
# BPD035: Age told had hypertension
# BPQ040A: Taking prescription for hypertension

# MCQ160L: Ever told you had any liver condition
# MCQ170L: Do you still have a liver condition
# MCD180L: Age when told you had a liver condition
# MCQ510A: Liver condition - Fatty liver
# MCQ510C: Liver condition - Liver cirrhosis
# MCQ510D: Liver condition - Viral hepatitis




### Data Cleaning ####

# get rid of columns containing mostly NAs, as well as variables with irrelevant technical data (like recording processes)
# rename variables for easier use

# demographics cleaning
head(demographics)
demo_clean <- select(demographics, SEQN, DMDMARTZ, RIAGENDR, RIDAGEYR, RIDRETH1, RIDRETH3)
demo_clean <- rename(demo_clean, gender = RIAGENDR, age = RIDAGEYR, marital_status = DMDMARTZ, 
                     race = RIDRETH1, race_withasian = RIDRETH3)

# body measurements cleaning
head(body_measures)
bm_clean <- select(body_measures, SEQN, BMXWT, BMXHT, BMXBMI)
bm_clean <- rename(bm_clean, weight_kg = BMXWT, height_cm = BMXHT, bmi = BMXBMI)

general_info <- inner_join(demo_clean, bm_clean)

# blood pressure & cholesterol cleaning (only want blood pressure)
head(bp_chol)
hbp <- select(bp_chol, SEQN, BPQ030, BPD035, BPQ040A)
hbp <- rename(hbp, told_hbp = BPQ030, agetold_hbp = BPD035, prescr_hbp = BPQ040A)

# medical conditions cleaning (only want liver conditions)
head(med_conditions)
liver_info <- select(med_conditions, SEQN, MCQ160L, MCQ170L, MCD180L, MCQ510A, MCQ510C, 
                              MCQ510D)
liver_info <- rename(liver_info, fatty_liver = MCQ510A, liver_cirrhosis = MCQ510C,
                           viral_hepa = MCQ510D, told_liver = MCQ160L, stillhave_liver = MCQ170L, agetold_liver = MCD180L)

# combine all datasets into one
everything <- inner_join(inner_join(general_info, hbp), liver_info)

# filter out NAs in important variables
livercond_hbp <- filter(everything, !is.na(told_liver))
livercond_hbp <- filter(livercond_hbp, !is.na(told_hbp), !is.na(prescr_hbp), !is.na(height_cm), !is.na(weight_kg))
livercond_hbp <- filter(livercond_hbp, told_hbp < 8, told_liver < 8)
View(livercond_hbp)

test <- filter(livercond_hbp, told_liver == 1)
test <- select(test, -told_liver)

# replace NAs with 0 to keep as much data as possible and not filter down to only 10 observations
test2 <- mutate(test, liver_cirrhosis = ifelse(is.na(liver_cirrhosis), 0, liver_cirrhosis))
test2 <- mutate(test2, fatty_liver = ifelse(is.na(fatty_liver), 0, fatty_liver))
test2 <- mutate(test2, viral_hepa = ifelse(is.na(viral_hepa), 0, viral_hepa))
test2 <- filter(test2, agetold_liver < 81, agetold_hbp < 81)
View(test2)
