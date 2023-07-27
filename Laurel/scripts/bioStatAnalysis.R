## loading packages
library(dplyr)
library(tidyr)
library(corrplot)

## data set of all biological factors
bio_factors <- select(health_overview, high_blood_pressure, gender,
                      high_cholesterol, depressed, anemia, overweight,
                      sedentary_minutes, sleep_hours, bmi, depressed)

bio_factors$depressed <- as.factor(bio_factors$depressed)

#### Comparing all categorical variables with Chi squared test ####

## comparing how they feel with whether they have anemia
t1 <- table(bio_factors$depressed, bio_factors$anemia)
chisq_result <- chisq.test(t1)
chisq_result$p.value     ## p-value: 0.004252266
chisq_result$residuals

## comparing how they feel with whether they are overweight or not
t2 <- table(bio_factors$depressed, bio_factors$overweight)
chisq_result <- chisq.test(t2)
chisq_result$p.value    ## p-value: 7.097778e-12
chisq_result$residuals

## comparing how they feel with whether they have high blood pressure
t3 <- table(bio_factors$depressed, bio_factors$high_blood_pressure)
chisq_result <- chisq.test(t3)
chisq_result$p.value    ## p-value: 0.0009251049
chisq_result$residuals