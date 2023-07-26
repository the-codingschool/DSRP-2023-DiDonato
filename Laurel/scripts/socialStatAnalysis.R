## loading packages
library(dplyr)
library(tidyr)
library(corrplot)

## data set of all social factors
social_factors <- health_overview |>
  select(id, education, income_to_poverty, race, self_image.x, 
         monthly_poverty_index, type_of_work, self_image.y, depressed) |>
  filter(education <= 5)

#### Comparing all categorical variables with Chi squared test ####

## comparing how they feel with the way they perceive their weight
t1 <- table(social_factors$depressed, social_factors$self_image.y)
chisq_result <- chisq.test(t1)
chisq_result$p.value     ## p-value: 5.96331e-14
chisq_result$residuals

## comparing how they feel with their education
t2 <- table(social_factors$depressed, social_factors$education)
chisq_result <- chisq.test(t2)
chisq_result$p.value    ## p-value: 5.388063e-11
chisq_result$residuals

## comparing how they feel with their race
t3 <- table(social_factors$depressed, social_factors$race)
chisq_result <- chisq.test(t3)
chisq_result$p.value    ## p-value: 2.779173e-09
chisq_result$residuals


