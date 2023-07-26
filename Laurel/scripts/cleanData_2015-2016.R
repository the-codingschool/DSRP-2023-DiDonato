## loading in all the data
demographic2 <- read_xpt("Laurel/data_process/2015-2016/DEMO_I.XPT")
income2 <- read_xpt("Laurel/data_process/2015-2016/INQ_I.XPT")
physical_activity2 <- read_xpt("Laurel/data_process/2015-2016/PAQ_I.XPT")
blood_pressure2 <- read_xpt("Laurel/data_process/2015-2016/BPQ_I.XPT")
weight2 <- read_xpt("Laurel/data_process/2015-2016/WHQ_I.XPT")
sleep_disorders2 <- read_xpt("Laurel/data_process/2015-2016/SLQ_I.XPT")
alcohol_use2 <- read_xpt("Laurel/data_process/2015-2016/ALQ_I.XPT")
mental_health_screener2 <- read_xpt("Laurel/data_process/2015-2016/DPQ_I.XPT")
medical_history2 <- read_xpt("Laurel/data_process/2015-2016/MCQ_I.XPT")
occupation2 <- read_xpt("Laurel/data_process/2015-2016/OCQ_I.XPT")

## Link to the original website for data
'https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015'

#### Renaming the variables and removing variables for all data sets ####

demographic2 <- demographic2 |>
  select(SEQN, RIDAGEYR, RIAGENDR,
         DMDEDUC2, INDFMPIR, RIDRETH3) |>
  filter(RIDAGEYR > 21, !is.na(INDFMPIR), DMDEDUC2 <= 5) |>
  rename("id" = "SEQN",
         "age" = "RIDAGEYR",
         "gender" = "RIAGENDR",
         "education" = "DMDEDUC2",
         "income_to_poverty" = "INDFMPIR",
         "race" = "RIDRETH3")

income2 <- income2 |>
  select(SEQN, INDFMMPI) |>
  rename("id" = "SEQN",
         "monthly_poverty_index" = "INDFMMPI") |>
  filter(!is.na(monthly_poverty_index))

physical_activity2 <- physical_activity2 |> 
  select(SEQN, PAD680) |>
  rename("sedentary_minutes" = "PAD680",
         "id" = SEQN) |>
  filter(!is.na(sedentary_minutes), sedentary_minutes != 7777, sedentary_minutes != 9999)

blood_pressure2 <- blood_pressure2 |>
  select(SEQN, BPQ020, BPQ080) |>
  rename("high_blood_pressure" = "BPQ020",
         "high_cholesterol" = "BPQ080",
         "id" = SEQN) |>
  filter(high_blood_pressure != 0, high_cholesterol != 7, high_cholesterol != 9)

weight2 <- weight2 |>
  select(SEQN, WHD010, WHD020, WHQ030) |>
  rename("height" = "WHD010",
         "weight" = "WHD020",
         "self_image" = "WHQ030",
         "id" = SEQN) |>
  filter(!is.na(height), !is.na(weight), height != 7777, height != 9999, 
         weight != 7777, weight != 9999, self_image != 7, self_image != 9)

weight2$bmi <- trunc(weight2$weight * 703 / weight2$height ^ 2)

sleep_disorders2 <- sleep_disorders2 |>
  select(SEQN, SLD012) |>
  rename("id" = "SEQN",
         "sleep_hours" = "SLD012") |>
  filter(!is.na(sleep_hours), sleep_hours != 77777, sleep_hours != 99999)

depression2 <- mental_health_screener2 |>
  select(SEQN, DPQ020, DPQ060) |>
  rename("depressed" = "DPQ020",
         "self_image" = "DPQ060",
         "id" = SEQN) |>
  filter(depressed >= 0 & depressed <= 3, !is.na(depressed))

medical_history2 <- medical_history2 |>
  select(SEQN, MCQ053, MCQ080) |>
  rename("overweight" = "MCQ080",
         "anemia" = "MCQ053",
         "id" = SEQN) |>
  filter(overweight != 9, !is.na(overweight), anemia != 7, anemia != 9)

occupation2 <- occupation2 |>
  select(SEQN, OCD150) |>
  rename("type_of_work" = "OCD150",
         "id" = SEQN) |>
  filter(type_of_work != 7, type_of_work != 9)

'health_overview <- the master overview of their health after combining all the data'
health_overview2 <- demographic2 |>
  inner_join(blood_pressure2, by = "id") |>
  inner_join(depression2, by = "id") |>
  inner_join(income2, by = "id") |>
  inner_join(medical_history2, by = "id") |>
  inner_join(occupation2, by = "id") |>
  inner_join(physical_activity2, by = "id") |>
  inner_join(sleep_disorders2, by = "id") |>
  inner_join(weight2, by = "id") |>
  filter(weight != 9999, weight != 7777, height != 9999, 
         height != 7777, !is.na(sleep_hours), !is.na(self_image.x),
         !is.na(sedentary_minutes))
