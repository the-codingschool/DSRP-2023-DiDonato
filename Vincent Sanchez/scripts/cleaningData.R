
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
View(general_info)

# blood pressure & cholesterol cleaning (only want blood pressure)
head(bp_chol)
hbp <- select(bp_chol, SEQN, BPQ030, BPD035, BPQ040A)
hbp <- rename(hbp, told_hbp = BPQ030, agetold_hbp = BPD035, prescr_hbp = BPQ040A)

View(hbp)

# medical conditions cleaning (only want liver conditions)
head(med_conditions)
liver_info <- select(med_conditions, SEQN, MCQ160L, MCQ170L, MCD180L, MCQ510A, MCQ510C, 
                              MCQ510D)
liver_info <- rename(liver_info, fatty_liver = MCQ510A, liver_cirrhosis = MCQ510C,
                           viral_hepa = MCQ510D, told_liver = MCQ160L, stillhave_liver = MCQ170L, agetold_liver = MCD180L)
View(liver_info)

# combine all datasets into one
everything <- inner_join(inner_join(general_info, hbp), liver_info)

# filter out NAs in important variables
livercond_hbp <- filter(everything, !is.na(told_liver))
livercond_hbp <- filter(livercond_hbp, !is.na(told_hbp), !is.na(prescr_hbp), !is.na(height_cm), !is.na(bmi))
View(livercond_hbp)

# initial dataset is now ready for analyzing (livercond_hbp)
