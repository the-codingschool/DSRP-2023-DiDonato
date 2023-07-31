Cors <- NAtoMean |>
  cor() |>
  melt() |>
  as.data.frame()


ggplot(Cors, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1))



  


highCorrelation <- filter(Cors, (value >= 0.05 | value <= -0.05) & 
                            Var1 == "had_coronary_heart_disease")


ggplot(gather(exerciseActivity, "Key", "Value", -seqn), aes(Value)) +
  geom_histogram() +
  facet_wrap(~Key, scales='free_x')

ggplot(gather(medicalConditions, "Key", "Value", -seqn), aes(Value)) +
  geom_histogram() +
  facet_wrap(~Key, scales='free_x')

ggplot(gather(demographics, "Key", "Value", -seqn), aes(Value)) +
  geom_histogram() +
  facet_wrap(~Key, scales='free_x')



ggplot(exerciseActivity, aes(x= walk_or_bike)) +
  geom_histogram(bins = 2)

ggplot(NAtoMean, aes(x = ldl_cholesterol_mg_d_l)) +
  geom_histogram()

ggplot(NAtoMean, aes(x=  age_at_screening_yrs, 
                     y= had_coronary_heart_disease)) +
  geom_point()


ggplot(NAtoMean, aes(x=bmi, 
                     y= had_coronary_heart_disease)) +
  geom_point()

ggplot(NAtoMean, aes(x= mean_systolic, 
                     y= had_coronary_heart_disease))   +
  geom_point()

ggplot(CAD, aes(x= age_at_screening_yrs, fill = had_coronary_heart_disease)) +
  geom_histogram(bins = 40)

ggplot(merged_data, aes(x= discomfort_in_chest, 
                        fill= as.factor(had_coronary_heart_disease))) +
  geom_histogram()

ggplot(merged_data, aes(x= shortnes_of_breath_stairs, 
                        fill= as.factor(had_coronary_heart_disease))) +
  geom_histogram()

ggplot(NAtoMean, aes(x= moderate_work_a_day_min )) +
  geom_histogram(bins = 50)
