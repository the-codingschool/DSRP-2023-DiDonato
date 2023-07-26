## loading in all the data sets for 2013-2014
demographic3 <- read_xpt("Laurel/data_process/2013-2014/DEMO_H.XPT")
income3 <- read_xpt("Laurel/data_process/2013-2014/INQ_H.XPT")
physical_activity3 <- read_xpt("Laurel/data_process/2013-2014/PAQ_H.XPT")
blood_pressure3 <- read_xpt("Laurel/data_process/2013-2014/BPQ_H.XPT")
weight3 <- read_xpt("Laurel/data_process/2013-2014/WHQ_H.XPT")
sleep_disorders3 <- read_xpt("Laurel/data_process/2013-2014/SLQ_H.XPT")
alcohol_use3 <- read_xpt("Laurel/data_process/2013-2014/ALQ_H.XPT")
mental_health_screener3 <- read_xpt("Laurel/data_process/2013-2014/DPQ_H.XPT")
medical_history3 <- read_xpt("Laurel/data_process/2013-2014/MCQ_H.XPT")
occupation3 <- read_xpt("Laurel/data_process/2013-2014/OCQ_H.XPT")

## Link to the original website for data
'https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015'

#### Renaming the variables and removing variables for all data sets ####

demographic3 <- demographic3 |>
  select(SEQN, RIDAGEYR, RIAGENDR,
         DMDEDUC2, INDFMPIR, RIDRETH3) |>
  filter(RIDAGEYR > 21, !is.na(INDFMPIR), DMDEDUC2 <= 5) |>
  rename("id" = "SEQN",
         "age" = "RIDAGEYR",
         "gender" = "RIAGENDR",
         "education" = "DMDEDUC2",
         "income_to_poverty" = "INDFMPIR",
         "race" = "RIDRETH3")

income3 <- income3 |>
  select(SEQN, INDFMMPI) |>
  rename("id" = "SEQN",
         "monthly_poverty_index" = "INDFMMPI") |>
  filter(!is.na(monthly_poverty_index))

physical_activity3 <- physical_activity3 |> 
  select(SEQN, PAD680) |>
  rename("sedentary_minutes" = "PAD680",
         "id" = SEQN) |>
  filter(!is.na(sedentary_minutes), sedentary_minutes != 7777, sedentary_minutes != 9999)

blood_pressure3 <- blood_pressure3 |>
  select(SEQN, BPQ020, BPQ080) |>
  rename("high_blood_pressure" = "BPQ020",
         "high_cholesterol" = "BPQ080",
         "id" = SEQN) |>
  filter(high_blood_pressure != 0, high_cholesterol != 7, high_cholesterol != 9)

weight3 <- weight3 |>
  select(SEQN, WHD010, WHD020, WHQ030) |>
  rename("height" = "WHD010",
         "weight" = "WHD020",
         "self_image" = "WHQ030",
         "id" = SEQN) |>
  filter(!is.na(height), !is.na(weight), height != 7777, height != 9999, 
         weight != 7777, weight != 9999, self_image != 7, self_image != 9)

weight3$bmi <- trunc(weight3$weight * 703 / weight3$height ^ 2)

sleep_disorders3 <- sleep_disorders3 |>
  select(SEQN, SLD010H) |>
  rename("id" = "SEQN",
         "sleep_hours" = "SLD010H") |>
  filter(!is.na(sleep_hours), sleep_hours != 77777, sleep_hours != 99999)

depression3 <- mental_health_screener3 |>
  select(SEQN, DPQ020, DPQ060) |>
  rename("depressed" = "DPQ020",
         "self_image" = "DPQ060",
         "id" = SEQN) |>
  filter(depressed >= 0 & depressed <= 3, !is.na(depressed))

medical_history3 <- medical_history3 |>
  select(SEQN, MCQ053, MCQ080) |>
  rename("overweight" = "MCQ080",
         "anemia" = "MCQ053",
         "id" = SEQN) |>
  filter(overweight != 9, !is.na(overweight), anemia != 7, anemia != 9)

occupation3 <- occupation3 |>
  select(SEQN, OCD150) |>
  rename("type_of_work" = "OCD150",
         "id" = SEQN) |>
  filter(type_of_work != 7, type_of_work != 9)

'health_overview <- the master overview of their health after combining all the data'
health_overview3 <- demographic3 |>
  inner_join(blood_pressure3, by = "id") |>
  inner_join(depression3, by = "id") |>
  inner_join(income3, by = "id") |>
  inner_join(medical_history3, by = "id") |>
  inner_join(occupation3, by = "id") |>
  inner_join(physical_activity3, by = "id") |>
  inner_join(sleep_disorders3, by = "id") |>
  inner_join(weight3, by = "id") |>
  filter(weight != 9999, weight != 7777, height != 9999, 
         height != 7777, !is.na(sleep_hours), !is.na(self_image.x),
         !is.na(sedentary_minutes))
