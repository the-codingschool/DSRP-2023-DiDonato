## loading in the library
library(ggplot2)

## looking at the correlation between blood pressure and how they answer the depressed question
ggplot(data = health_overview, aes(x = depressed, y = sleep_hours)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Hours slept", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()


ggplot(data = health_overview_plot, aes(x = depressed, y = bmi)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Body mass index", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

ggplot(data = health_overview_plot, aes(x = depressed, y = income_to_poverty)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Ratio of income to poverty", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

ggplot(data = health_overview_plot, aes(x = depressed, y = monthly_poverty_index)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Monthly poverty index", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

ggplot(data = health_overview_plot, aes(x = depressed, y = education)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Education level", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()

ggplot(data = health_overview_plot, aes(x = depressed, y = age)) +
  geom_jitter(width = 0.25, alpha = 0.2, color = "blue") +
  labs(y = "Age", x = "Depressed", caption = 
         "0 = not at all, 1 = several days, 2 = more than half the days, 3 = nearly every day") +
  theme_bw()




