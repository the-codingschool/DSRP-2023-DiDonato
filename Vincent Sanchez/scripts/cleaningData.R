library(dplyr)
demographics <- read.csv("data/data.csv")

library(foreign)
body_measures <- read.xport("Vincent Sanchez/more-data/P_BMX.XPT")

bp_chol <- read.xport("Vincent Sanchez/more-data/P_BPQ.XPT")

med_conditions <- read.xport("Vincent Sanchez/more-data/P_MCQ.XPT")


### Variable Meanings ####

# SEQN: Respondent sequence number

# SDDSRVYR: Data release cycle
# RIAGENDR: Gender of the participant
# RIDAGEYR: Age in years of the participant (max 80)
# RIDRETH1: Reported race and Hispanic origin information
# RIDRETH3: Reported race and Hispanic origin information, with Non-Hispanic Asian Category
# DMDBORN4: Birth country
# DMDEDUC2: Highest grade or level of school completed or the highest degree received
# DMDMARTZ: Marital Status

# BMXWT: Weight in kg
# BMXHT: Standing height in cm
# BMXBMI: Body Mass Index (kg/m^2)
# BMDBMIC: BMI Category (children/youth)

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 


### Data Cleaning ####

# get rid of columns containing mostly NAs, as well as variables with irrelevant technical data (like recording processes)
# rename variables for easier use

# demographics cleaning
head(demographics)
demo_clean <- select(demographics, SEQN, DMDBORN4, DMDEDUC2, DMDMARTZ, RIAGENDR, RIDAGEYR, RIDRETH1, RIDRETH3, SDDSRVYR)
demo_clean <- rename(demo_clean, cycle = SDDSRVYR, gender = RIAGENDR, age = RIDAGEYR, birth_country = DMDBORN4, education = DMDEDUC2, 
                     marital_status = DMDMARTZ, race = RIDRETH1, race_withasian = RIDRETH3)
View(demo_clean)

# body measurements cleaning
head(body_measures)
bm_clean <- select(body_measures, SEQN, BMXWT, BMXHT, BMXBMI, BMDBMIC)
bm_clean <- rename(bm_clean, weight_kg = BMXWT, height_cm = BMXHT, bmi = BMXBMI, bmi_category = BMDBMIC)
View(bm_clean)

general_info <- inner_join(demo_clean, bm_clean)
View(general_info)

# blood pressure & cholesterol cleaning
head(bp_chol)

# medical conditions cleaning
head(med_conditions)
medconditions_clean <- select(med_conditions, SEQN, MCQ160M, MCQ170M, MCD180M, MCQ160L, MCQ170L, MCD180L, MCQ500, MCQ510A, MCQ510B, MCQ510C, 
                              MCQ510D, MCQ510E, MCQ510F)

View(medconditions_clean)




