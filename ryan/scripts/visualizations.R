Cors <- NAtoMean |>
  cor() |>
  melt() |>
  as.data.frame()


ggplot(Cors, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0)
# 
# 
# #ggplot(NAtoMean, aes(x= LBDHDD, y = LBXGH, color = RIDRETH3)) +
# #  geom_point()
# 
highCorrelation <- filter(Cors, (value >= 0.1 | value <= -0.1) & 
                            Var1 == "ever_told_you_had_coronary_heart_disease")
# 
# numDaysOfExercise <- select(exerciseActivity, PAQ610)
# 
# ggplot(merged_data, aes(x= MCQ160C)) +
#   geom_bar()
# 
# ggplot(merged_data, aes(x= PAQ610, fill = as.factor(MCQ160C))) +
#   geom_histogram(bins=10, binwidth = 1) + labs(x= "Days of Vigorous Exercise a Week", 
#                                                fill= "Diagonsed with Heart Disease? (1 = yes, 2 = no, 9= refused)")
# 
# ggplot(merged_data, aes(x=PAQ610)) +
#   geom_histogram()

ggplot(NAtoMean, aes(x = ldl_cholesterol_friedewald_mg_d_l)) +
  geom_histogram()

ggplot(NAtoMean, aes(x=  age_at_screening_yrs, 
                     y= ever_told_you_had_coronary_heart_disease)) +
  geom_point()


ggplot(NAtoMean, aes(x=bmi, 
                     y= ever_told_you_had_coronary_heart_disease)) +
  geom_point()

ggplot(NAtoMean, aes(x= systolic_2nd_reading, 
                     y= ever_told_you_had_coronary_heart_disease))   +
  geom_point()

ggplot(CAD, aes(x= age_at_screening_yrs, fill = ever_told_you_had_coronary_heart_disease)) +
  geom_histogram(bins = 40)

ggplot(merged_data, aes(x= ever_had_pain_or_discomfort_in_chest, 
                        fill= as.factor(ever_told_you_had_coronary_heart_disease))) +
  geom_histogram()

ggplot(merged_data, aes(x= shortnes_of_breath_on_stairs, 
                        fill= as.factor(ever_told_you_had_coronary_heart_disease))) +
  geom_histogram()

ggplot(NAtoMean, aes(x= typical_time_spent_doing_moderate_work_a_day_min )) +
  geom_histogram(bins = 50)
