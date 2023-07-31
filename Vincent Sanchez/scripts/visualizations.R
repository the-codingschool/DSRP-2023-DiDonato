
# first need to run code from "cleaningData.R" then "analyzingData.R" then "statisticAnalysis.R"

# overall correlations
ggplot(liverCors2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "orange", high = "blue", mid = "white", midpoint = 0)

corrplot(cor_told3, tl.srt = 30,
         type = "full")

# agetold_liver vs. agetold_hbp

ggplot(test3, aes(x = agetold_liver, y = agetold_hbp)) +
  geom_abline(lty = "dashed", color = "orange", linewidth = 1, slope = 0.5, intercept = 25) +
  xlim(21,78) +
  ylim(21,78) +
  geom_smooth(linewidth = 2) +
  labs(title = "Diagnosis Ages of Hypertension vs. Liver Conditions ",
       x = "Age When Individual Was Told They Had A Liver Condition",
       y = "Age When Individual Was Told They Had Hypertension",
       caption = "Orange reference line has slope of 1/2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color = "orange", face = "bold"))

ggplot(test3, aes(x = agetold_liver, y = agetold_hbp)) +
  geom_point()

# told_liver vs. told_hbp

ggplot(livercond_hbp, aes(x = told_hbp, fill = as.factor(told_liver))) +
  geom_bar() +
  labs(x = "Individual Told They Have Hypertension?",
       y = "Count",
       title = "Of Those With Hypertension, How Many Have A Liver Condition?") +
  scale_fill_discrete(name = "Have Liver Condition?") +
  theme_minimal()

ggplot(livercond_hbp, aes(x = told_liver, fill = as.factor(told_hbp))) +
  geom_bar() +
  labs(x = "Individual Told They Have A Liver Condition?",
       y = "Count",
       title = "Of Those With A Liver Condition, How Many Have Hypertension?") +
  scale_fill_discrete(name = "Have Hypertension?") +
  theme_minimal()

# race vs. told_liver
ggplot(livercond_hbp, aes(x = race, fill = as.factor(told_liver))) + 
  geom_bar() +
  labs(x = "Race",
       y = "Count",
       title = "Race vs. Liver Condition Prevalence") +
  scale_fill_discrete(name = "Have Liver Condition?") +
  theme_minimal()

# marital_status vs. told_liver
livercond_hbp <- filter(livercond_hbp, marital_status < 75)
ggplot(livercond_hbp, aes(x = marital_status, fill = as.factor(told_liver))) + 
  geom_bar() +
  labs(x = "Marital Status",
       y = "Count",
       title = "Marital Status vs. Having A Liver Condition") +
  scale_fill_discrete(name = "Have Liver Condition?") +
  theme_minimal()

# marital_status vs. currently taking meds for hbp
ggplot(livercond_hbp, aes(x = marital_status, fill = as.factor(prescr_hbp))) + 
  geom_bar() +
  labs(x = "Marital Status",
       y = "Count",
       title = "Marital Status vs. Prescription For Hypertension") +
  scale_fill_discrete(name = "Prescribed?") +
  theme_minimal()

# weight_kg vs. agetold_hbp
ggplot(test3, aes(x = weight_kg, y = agetold_hbp)) +
  geom_point() +
  theme_minimal()
