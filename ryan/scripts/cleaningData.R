library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(foreign)
library(tidyverse)
library(reshape2)
library(rsample)
library(caret)

# import each datasets and cleaning each one
# a1c <- read.xport("ryan/datasets/P_GHB.XPT")
# insulin <- read.xport("ryan/datasets/P_INS.XPT") 
hdl_cholesterol <- read.xport("ryan/datasets/P_HDL.XPT")
ldl <- read.xport("ryan/datasets/P_TRIGLY.XPT") # subsample weights
demographics <- read.csv("data/data.csv")
exerciseActivity <- read.xport("ryan/datasets/P_PAQ.XPT")
bloodpressure <- read.xport("ryan/datasets/P_BPXO.XPT")
medicalConditions <- read.xport("ryan/datasets/P_MCQ.XPT")
weight <- read.xport("ryan/datasets/P_WHQ.XPT")
# bloodPressureCholesterol <- read.xport("ryan/datasets/P_BPQ.XPT")
cardioHealth <- read.xport("ryan/datasets/P_CDQ.XPT")

#### Cleaning ####

# renaming columns and removing unnecessary columns
hdl_cholesterol <- hdl_cholesterol |> rename("Direct HDL-Cholesterol (mg/dL)" = "LBDHDD") |>
  select(-LBDHDDSI)

hdl_cholesterol <- clean_names(hdl_cholesterol, "snake")

ldl <- ldl |> rename("Fasting Subsample Weight" = "WTSAFPRP", "Triglyceride (mg/dL)" = "LBXTR",
                     "LDL-Cholesterol, Friedewald (mg/dL)" = "LBDLDL") |> 
  select(-LBDLDLSI, -LBDLDLM, -LBDLDMSI, -LBDLDLN, -LBDLDNSI)



ldl <- clean_names(ldl, "snake")

demographics <- demographics |> rename("Gender (Male: 1, Female :2)" = "RIAGENDR", 
                                       "Age at screening (yrs)" = "RIDAGEYR", 
                                       "Race" = "RIDRETH3",
                                       "Country of birth" = "DMDBORN4",
                                       "Time in US" = "DMDYRUSZ",
                                       "Education level" = "DMDEDUC2",
                                       "Martial status" = "DMDMARTZ",
                                    )
demographics <-  select(demographics, -SDDSRVYR, -RIDSTATR, -RIDAGEMN, -RIDRETH1, -RIDEXMON, -RIDEXPRG, 
         -SIALANG, -SIAPROXY, -FIALANG, -SIAINTRP, -FIAPROXY, -MIALANG, -MIAPROXY,
         -MIAINTRP, -AIALANGA, -WTINTPRP, -WTMECPRP, -SDMVPSU, -SDMVSTRA, -INDFMPIR)

demographics <- select(demographics, -FIAINTRP)

demographics <- clean_names(demographics, "snake")

exerciseActivity <- exerciseActivity |> 
  rename("Num of days Vigorous Work (weekly)" = "PAQ610",
         "Vigorous Work Activity?" = "PAQ605",
         "Moderate Work Activity?" = "PAQ620",
         "Typical Time spent doing Vigorous Activity a Day (min)" = "PAD615",
        "Num Days of Moderate Work (weekly)" = "PAQ625",
        "Typical Time spent doing Moderate Work a Day (min)" = "PAD630",
         "Walk or bike?" = "PAQ635",
        "Num of Days Walk or Bike (weekly)" = "PAQ640",
        "Typical Daily Minutes Walk/Bike for Transportation" = "PAD645",
        "Vigorous recreational activities?" = "PAQ650",
        "Num of Days doing Vigorous Recreational Activities" = "PAQ655",
        "Typical Daily Minutes doing Vigorous Recreational Activites" = "PAD660",
        "Moderate Recreational Activities?" = "PAQ665",
        "Num of Days doing Moderate Recreational Activities" = "PAQ670",
        "Typical Daily Minutes doing Moderate Recreational Activities" = "PAD675",
        "Minutes doing Sedentary Activities" = "PAD680") 

exerciseActivity <- clean_names(exerciseActivity, "snake")

exerciseActivity$typical_time_spent_doing_moderate_work_a_day_min <-
  replace_na(exerciseActivity$typical_time_spent_doing_moderate_work_a_day_min,
             0)

bloodpressure <- select(bloodpressure, -BPAOARM)

bloodpressure <- bloodpressure |> rename("Systolic (1st reading)" = "BPXOSY1",
                                         "Diastolic (1st reading)" = "BPXODI1",
                                         "Systolic (2nd reading)" = "BPXOSY2",
                                         "Diastolic (2nd reading)" = "BPXODI2",
                                         "Systolic (3rd reading)" = "BPXOSY3",
                                         "Diastolic (3rd reading)" = "BPXODI3",
                                         "Pulse (1st reading)" = "BPXOPLS1",
                                         "Pulse (2nd reading)" = "BPXOPLS2",
                                         "Pulse (3rd reading)" = "BPXOPLS3") |>
  select(-BPAOCSZ)

bloodpressure <- clean_names(bloodpressure, "snake")

medicalConditions <- medicalConditions |>
  rename("Ever been told you have asthma" = "MCQ010",
         "Taking treatment for anemia (during past 3 months)" = "MCQ053",
         "Ever told you had coronary heart disease?" = "MCQ160C",
         "Age when told had Coronary Heart Disease?" = "MCD180C",
         "Ever told you had angina?" = "MCQ160D",
         "Age when told you had angina pectoris" = "MCD180D",
        "Ever told you had heart attack" ="MCQ160E",
        "Ever told you had thyroid problem" = "MCQ160M",
        "Do you still have thyroid problem" = "MCQ170M",
        "Age when told you had thyroid problem" = "MCD180M",
        "Ever told you had COPD, emphysema, ChB" = "MCQ160P",
        "Close relative had asthma?" = "MCQ300B",
        "Close relative had heart attack?" = "MCQ300A",
        "Doctor told to increase exercise?" = "MCQ366B",
        "Doctor told you to reduce salt?" = "MCQ366C",
        "Doctor told you to reduce calories?" = "MCQ366D",
        "Are you now controlling or losing weight?" = "MCQ371A",
        "Are you now increasing exercise?" = "MCQ371B",
        "Are you now reducing salt?" = "MCQ371C",
        "Are you now reducing calories?" = "MCQ371D",
        "Ever told you had congestive heart failure" = "MCQ160B",
        "Age when told you had congestive heart failure" = "MCD180B",
        "Age when told you had heart attack" = "MCD180E"
        ) |>
  select(-MCQ025,-MCQ035, -MCQ040, -MCQ050, -AGQ030, -MCQ092, -MCD093, 
         -MCQ149, -MCQ151, -RHD018, -MCQ160A, -MCQ195, -MCQ160F, -MCD180F,
         -MCQ160L, -MCQ170L,-MCD180L, -MCQ500, -MCQ510A, -MCQ510B, -MCQ510C,
         -MCQ510D, -MCQ510E, -MCQ510F, -MCQ520, -MCQ530, -MCQ540, -MCQ550, -MCQ560,
         -MCQ570, -MCQ220, -MCQ230A, -MCQ230B, -MCQ230C, -MCQ230D, -MCQ300C, -MCQ366A,
         -OSQ230, -MCQ080)


medicalConditions <- clean_names(medicalConditions, "snake")

weight <- weight |> filter(WHD010 < 7777 & WHD020 < 7777) |>
  select(SEQN, WHD010, WHD020) |>
  mutate(BMI = 703 * WHD020 / (WHD010 * WHD010)) |>
  select(-WHD010) |> rename("Weight (lbs)" = "WHD020")

weight <- clean_names(weight, "snake")



cardioHealth <- cardioHealth |> 
  select(SEQN, CDQ001, CDQ010) |>
  rename("Ever had pain or discomfort in chest?" = "CDQ001",
         "Shortnes of breath on stairs?" = "CDQ010")
         
cardioHealth <- clean_names(cardioHealth, "snake")


#joined_data <- inner_join(a1ac, insulin)

# combine data by SEQN (id)
merged_data <- list(ldl, bloodpressure,
                    medicalConditions, weight, exerciseActivity, demographics, cardioHealth) %>% 
  reduce(inner_join, by = 'seqn')


merged_data <- filter(merged_data, is.na(merged_data$ever_told_you_had_coronary_heart_disease) == F)


# remove NAs and replace with Mean for all columns and rows
NAtoMean <- merged_data

for (i in 1:ncol(merged_data)) {
  NAtoMean[,i][is.na(NAtoMean[,i])] <- mean(NAtoMean[,i], na.rm = TRUE)
}


