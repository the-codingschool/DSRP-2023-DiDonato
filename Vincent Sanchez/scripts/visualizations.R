
# first need to run code from "cleaningData.R" then "analyzingData.R" then "statisticAnalysis.R"

# change data values to words for easier comprehension in visualizations
livercond_hbp <- filter(livercond_hbp, marital_status < 75)
livercond_hbp <- mutate(livercond_hbp,
                        prescr_hbp = ifelse(prescr_hbp == 1,"Yes","No"))
livercond_hbp <- mutate(livercond_hbp,
                        told_hbp = ifelse(told_hbp == 1,"Yes","No"))
livercond_hbp <- mutate(livercond_hbp,
                        told_liver = ifelse(told_liver == 1,"Yes","No"))
livercond_hbp <- mutate(livercond_hbp,
                        race = case_when(race == 1 ~ "Mexican",
                                               race == 2 ~ "Other Hispanic",
                                               race == 3 ~ "White",
                                               race == 4 ~ "Black",
                                               race == 5 ~ "Other/Multi-Racial"))
livercond_hbp <- mutate(livercond_hbp,
                        marital_status = case_when(marital_status == 1 ~ "Married",
                                                   marital_status == 2 ~ "Divorced",
                                                   marital_status == 3 ~ "Never Married"))

# overall correlations
ggplot(liverCors2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "orange", high = "blue", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Overall Correlations",
       tag = "Figure 3") +
  theme(plot.title = element_text(hjust = 0.5))

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
       caption = "Orange reference line has slope of 1/2",
       tag = "Figure 4") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color = "orange", face = "bold"))

ggplot(test3, aes(x = agetold_liver, y = agetold_hbp)) +
  xlim(21,78) +
  ylim(21,78) +
  geom_smooth(linewidth = 2) +
  labs(title = "Diagnosis Ages of Hypertension vs. Liver Conditions ",
       x = "Age When Individual Was Told They Had A Liver Condition",
       y = "Age When Individual Was Told They Had Hypertension") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(test3, aes(x = agetold_liver, y = agetold_hbp)) +
  geom_point()

# told_liver vs. told_hbp

ggplot(livercond_hbp, aes(x = told_hbp, fill = as.factor(told_liver))) +
  geom_bar(position="dodge") +
  labs(x = "Individual Told They Have Hypertension?",
       y = "Count",
       title = "Of Those With Hypertension, How Many Have A Liver Condition?",
       tag = "Figure 1") +
  scale_fill_discrete(name = "Have Liver Condition?") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(livercond_hbp, aes(x = told_liver, fill = as.factor(told_hbp))) +
  geom_bar(position="dodge") +
  labs(x = "Individual Told They Have A Liver Condition?",
       y = "Count",
       title = "Of Those With A Liver Condition, How Many Have Hypertension?",
       tag = "Figure 2") +
  scale_fill_discrete(name = "Have Hypertension?") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# race vs. told_liver
ggplot(livercond_hbp, aes(x = race, fill = as.factor(told_liver))) + 
  geom_bar(position = "dodge") +
  labs(x = "Race",
       y = "Count",
       title = "Race vs. Liver Condition Prevalence",
       tag = "Figure 8") +
  scale_fill_discrete(name = "Have Liver Condition?") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# marital_status vs. told_liver
ggplot(livercond_hbp, aes(x = marital_status, fill = as.factor(told_liver))) + 
  geom_bar(position="dodge") +
  labs(x = "Marital Status",
       y = "Count",
       title = "Marital Status vs. Having A Liver Condition",
       tag = "Figure 7") +
  scale_fill_discrete(name = "Have Liver Condition?") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# marital_status vs. currently taking meds for hbp
ggplot(livercond_hbp, aes(x = marital_status, fill = as.factor(prescr_hbp))) + 
  geom_bar(position="dodge") +
  labs(x = "Marital Status",
       y = "Count",
       title = "Marital Status vs. Prescription For Hypertension",
       tag = "Figure 6") +
  scale_fill_discrete(name = "Prescribed?") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# weight_kg vs. agetold_hbp
ggplot(test3, aes(x = weight_kg, y = agetold_hbp)) +
  geom_point() +
  theme_minimal()

