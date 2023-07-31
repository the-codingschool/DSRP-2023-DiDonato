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


##### HDL CHOLESTEROL ######

hdl_cholesterol <- read.xport("ryan/datasets/P_HDL.XPT")

hdl_cholesterol <- hdl_cholesterol |> rename("Direct HDL-Cholesterol (mg/dL)" = "LBDHDD") |>
  select(-LBDHDDSI)

hdl_cholesterol <- clean_names(hdl_cholesterol, "snake")


##### LDL CHOLESTEROL #####

ldl <- read.xport("ryan/datasets/P_TRIGLY.XPT") # subsample weights

ldl <- ldl |> rename("Fasting Subsample Weight" = "WTSAFPRP", "Triglyceride (mg/dL)" = "LBXTR",
                     "LDL-Cholesterol (mg/dL)" = "LBDLDL") |> 
  select(-LBDLDLSI, -LBDLDLM, -LBDLDMSI, -LBDLDLN, -LBDLDNSI)


ldl <- clean_names(ldl, "snake")


##### DEMOGRAPHICS #####

demographics <- read.csv("data/data.csv")


demographics <- demographics |> rename("Gender (Male: 1, Female :2)" = "RIAGENDR", 
                                       "Age at screening (yrs)" = "RIDAGEYR", 
                                       "Race" = "RIDRETH3",
                                       "Country of birth" = "DMDBORN4",
                                       # "Time in US" = "DMDYRUSZ",
                                       # "Education level" = "DMDEDUC2",
                                       # "Martial status" = "DMDMARTZ",
)
demographics <-  demographics %>% 
  select(-SDDSRVYR, -RIDSTATR, -RIDAGEMN, -RIDRETH1, -RIDEXMON, -RIDEXPRG, 
                        -SIALANG, -SIAPROXY, -FIALANG, -SIAINTRP, -FIAPROXY, -MIALANG, -MIAPROXY,
                        -MIAINTRP, -AIALANGA, -WTINTPRP, -WTMECPRP, -SDMVPSU, -SDMVSTRA, -INDFMPIR,
                        -DMDYRUSZ, -DMDMARTZ, -DMDEDUC2, -FIAINTRP)


demographics <- clean_names(demographics, "snake")

demographics <- filter(demographics, country_of_birth < 77)

##### EXERCISE ACTIVITY ######

exerciseActivity <- read.xport("ryan/datasets/P_PAQ.XPT")

exerciseActivity <- exerciseActivity |> 
  rename("days Vigorous Work (weekly)" = "PAQ610",
         "Vigorous Work Activity?" = "PAQ605",
         "Moderate Work Activity?" = "PAQ620",
         "Daily Vigorous Activity (min)" = "PAD615",
         "Days of Moderate Work (weekly)" = "PAQ625",
         "Moderate Work a Day (min)" = "PAD630",
         "Walk or bike?" = "PAQ635",
         "Days Walk or Bike (weekly)" = "PAQ640",
         # "Daily Min Walk/Bike for Transport" = "PAD645",
         # "Vigorous recreational activities?" = "PAQ650",
         # "Days doing Vigorous Recreational Activities" = "PAQ655",
         # "Daily Min doing Vigorous Recreational Activites" = "PAD660",
         # "Moderate Recreational Activities?" = "PAQ665",
         # "Days doing Moderate Recreational Activities" = "PAQ670",
         # "Daily Min doing Moderate Recreational Activities" = "PAD675",
         "Min Sedentary Activities" = "PAD680") 

exerciseActivity <- select(exerciseActivity, -PAQ655, -PAQ670, -PAD675,
                           -PAQ650, -PAQ665, -PAD660, -PAD645)

exerciseActivity <- clean_names(exerciseActivity, "snake")

exerciseActivity <- exerciseActivity %>%
  mutate(
    daily_vigorous_activity_min = replace_na(daily_vigorous_activity_min, 0),
    moderate_work_a_day_min = replace_na(moderate_work_a_day_min, 0),
    days_of_moderate_work_weekly = replace_na(days_of_moderate_work_weekly, 0),
    vigorous_work_activity = replace_na(vigorous_work_activity, 2),
    days_vigorous_work_weekly = replace_na(days_vigorous_work_weekly, 0),
    days_walk_or_bike_weekly = replace_na(days_walk_or_bike_weekly, 0),
    min_sedentary_activities = replace_na(min_sedentary_activities, 0),
    moderate_work_activity = replace_na(moderate_work_activity, 2),
    walk_or_bike = replace_na(walk_or_bike, 2)
  ) %>%
  filter(
    days_of_moderate_work_weekly < 99 &
      daily_vigorous_activity_min < 7000 &
      days_vigorous_work_weekly < 8 &
      days_walk_or_bike_weekly < 8 &
      min_sedentary_activities < 7000 &
      moderate_work_a_day_min < 7000 &
      moderate_work_activity < 3 &
      vigorous_work_activity < 3 &
      walk_or_bike < 3
  )


##### BLOODPRESSURE #####

bloodpressure <- read.xport("ryan/datasets/P_BPXO.XPT")

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

column_means <- bloodpressure %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Replace NA values with column means
bloodpressure <- bloodpressure %>%
  mutate(across(everything(),~replace_na(., mean(., na.rm = TRUE))))

bloodpressure <- mutate(bloodpressure, 
                        "mean_systolic" = (systolic_1st_reading + systolic_2nd_reading +systolic_3rd_reading)/3,
                        "mean_diastolic" = (diastolic_1st_reading + diastolic_2nd_reading + diastolic_3rd_reading) / 3,
                        "mean_pulse" = (pulse_1st_reading + pulse_2nd_reading + pulse_3rd_reading) / 3)

bloodpressure <- select(bloodpressure,
                        -pulse_1st_reading, pulse_2nd_reading, -pulse_3rd_reading,
                        -systolic_1st_reading, -systolic_2nd_reading, -systolic_3rd_reading,
                        -diastolic_3rd_reading,diastolic_2nd_reading, -diastolic_1st_reading,
                        -diastolic_2nd_reading)

##### MEDICAL CONDITIONS #####

medicalConditions <- read.xport("ryan/datasets/P_MCQ.XPT")


medicalConditions <- medicalConditions |>
  rename("told you have asthma" = "MCQ010",
         "Taking treatment anemia" = "MCQ053",
         "had coronary heart disease?" = "MCQ160C",
         # "Age had Coronary Heart Disease?" = "MCD180C",
         "had angina?" = "MCQ160D",
         # "Age had angina pectoris" = "MCD180D",
         "had heart attack" ="MCQ160E",
         "had thyroid problem" = "MCQ160M",
         # "still have thyroid problem" = "MCQ170M",
         # "Age had thyroid problem" = "MCD180M",
         "had COPD, emphysema, ChB" = "MCQ160P",
         "Close relative had asthma?" = "MCQ300B",
         "Close relative had heart attack?" = "MCQ300A",
         "told to increase exercise?" = "MCQ366B",
         "told to reduce salt?" = "MCQ366C",
         "told to reduce calories?" = "MCQ366D",
         # "controlling or losing weight?" = "MCQ371A",
         # "increasing exercise?" = "MCQ371B",
         # "reducing salt?" = "MCQ371C",
         # "reducing calories?" = "MCQ371D",
         "had congestive heart failure" = "MCQ160B",
         # "Age had congestive heart failure" = "MCD180B",
         # "Age had heart attack" = "MCD180E"
  ) |>
  select(-MCQ025,-MCQ035, -MCQ040, -MCQ050, -AGQ030, -MCQ092, -MCD093, 
         -MCQ149, -MCQ151, -RHD018, -MCQ160A, -MCQ195, -MCQ160F, -MCD180F,
         -MCQ160L, -MCQ170L,-MCD180L, -MCQ500, -MCQ510A, -MCQ510B, -MCQ510C,
         -MCQ510D, -MCQ510E, -MCQ510F, -MCQ520, -MCQ530, -MCQ540, -MCQ550, -MCQ560,
         -MCQ570, -MCQ220, -MCQ230A, -MCQ230B, -MCQ230C, -MCQ230D, -MCQ300C, -MCQ366A,
         -OSQ230, -MCQ080, -MCQ170M, -MCQ371A, -MCQ371C, -MCQ371D, -MCQ371B, -MCD180M,
         -MCD180D, -MCD180C, -MCD180B, -MCD180E)



medicalConditions <- clean_names(medicalConditions, "snake")

medicalConditions <- medicalConditions %>%
  mutate(
    close_relative_had_asthma = replace_na(close_relative_had_asthma, 2),
    close_relative_had_heart_attack = replace_na(close_relative_had_heart_attack, 2),
    had_angina = replace_na(had_angina, 2),
    had_congestive_heart_failure = replace_na(had_congestive_heart_failure, 2),
    had_copd_emphysema_ch_b = replace_na(had_copd_emphysema_ch_b, 2),
    had_coronary_heart_disease = replace_na(had_coronary_heart_disease, 2),
    had_heart_attack = replace_na(had_heart_attack, 2),
    had_thyroid_problem = replace_na(had_thyroid_problem, 2),
    taking_treatment_anemia = replace_na(taking_treatment_anemia, 2),
    told_to_increase_exercise = replace_na(told_to_increase_exercise, 2),
    told_to_reduce_calories = replace_na(told_to_reduce_calories, 2),
    told_to_reduce_salt = replace_na(told_to_reduce_salt, 2),
    told_you_have_asthma = replace_na(told_you_have_asthma, 2)
  ) %>%
  filter(if_all(!matches("seqn"), ~. <=3))
 

##### WEIGHT #####

weight <- read.xport("ryan/datasets/P_WHQ.XPT")

weight <- weight |> filter(WHD010 < 7777 & WHD020 < 7777) |>
  select(SEQN, WHD010, WHD020) |>
  mutate(BMI = 703 * WHD020 / (WHD010 * WHD010)) |>
  select(-WHD010) |> rename("Weight (lbs)" = "WHD020")

weight <- clean_names(weight, "snake")


#### CARDIO HEALTH #####

cardioHealth <- read.xport("ryan/datasets/P_CDQ.XPT")

cardioHealth <- cardioHealth |> 
  select(SEQN, CDQ001, CDQ010) |>
  rename("had discomfort in chest?" = "CDQ001",
         "Shortness of breath stairs?" = "CDQ010")
         
cardioHealth <- clean_names(cardioHealth, "snake")
cardioHealth <- filter(cardioHealth, shortness_of_breath_stairs != 9 &
                         had_discomfort_in_chest != 9)



#joined_data <- inner_join(a1ac, insulin)
# bloodPressureCholesterol <- read.xport("ryan/datasets/P_BPQ.XPT")



##### combine data by SEQN (id) ######
merged_data <- list(ldl, bloodpressure,
                    medicalConditions, weight, exerciseActivity, demographics, cardioHealth) %>% 
  reduce(inner_join, by = 'seqn')


merged_data <- filter(merged_data, is.na(merged_data$had_coronary_heart_disease) == F)


# remove NAs and replace with Mean for all columns and rows
NAtoMean <- merged_data

for (i in 1:ncol(merged_data)) {
  NAtoMean[,i][is.na(NAtoMean[,i])] <- mean(NAtoMean[,i], na.rm = TRUE)
}


