# #### Research Question : How much alcohol consumed start to affect a person’s  ####
# How does Gender and Age Affect High Blood Pressure, High Cholesterol and Kidney Health?
# How does birth country affect High Blood Pressure, High Cholesterol and Kidney Health?
# How does Average Daily Alcohol Intake Affect High Blood Pressure, High Cholesterol and Kidney Health?
# How does having 4/5 or more drinks daily affect  High Blood Pressure, High Cholesterol and Kidney Health?




#### Hypothesis testing ####
# Compare if Frequency of Drinks a day affect high blood pressure, high cholesterol, kidney health



## Null Hypothesis: Frequency of Alcohol consumed in the past year does not affect blood pressure, cholesterol or kidney health 
## Alt Hypothesis: Frequency of Alcohol consumed in the past year does affect blood pressure, cholesterol or kidney health 


#Avg drinks vs. High Blood Pressure
t <- table(cleanData$HowOftenDrankAlcohol, cleanData$HighBloodPressure)
t

chisq_result <-chisq.test(t)
chisq_result
chisq_result$p.value
# p-value: 0.0677382
# REJECT



#Avg drinks vs. High Cholesterol
t2 <- table(cleanData$HowOftenDrankAlcohol, cleanData$HighCholesterol)
t2

chisq_result <- chisq.test(t2)
chisq_result
chisq_result$p.value
# p-value: 0.6012939



#Avg drinks vs. Kidney Health
t3 <- table(cleanData$HowOftenDrankAlcohol, cleanData$FailingKidneys)
t3 

chisq_result <- chisq.test(t3)
chisq_result
chisq_result$p.value
# p-value:0.9754154




## Null Hypothesis: Gender does not affect blood pressure, cholesterol or kidney health 
## Alt Hypothesis: Gender does affect blood pressure, cholesterol or kidney health 


#Gender vs. High Blood Pressure
tg <- table(cleanData$gender, cleanData$HighBloodPressure)
tg

chisq_result <- chisq.test(tg)
chisq_result
chisq_result$p.value
# p-value: 0.03167844
#REJECT


tg2 <- table(cleanData$gender, cleanData$HighCholesterol)
tg2

chisq_result <- chisq.test(tg2)
chisq_result
chisq_result$p.value
# p-value: 0.05643367
#REJECT



tg3 <- table(cleanData$gender, cleanData$FailingKidneys)
tg3

chisq_result <- chisq.test(tg3)
chisq_result
chisq_result$p.value
# p-value: 0.3691156





## Null Hypothesis: Having 4/5 drinks or more does not affect blood pressure, cholesterol or kidney health 
## Alt Hypothesis: Having 4/5 drinks or more does affect blood pressure, cholesterol or kidney health 


td <- table(cleanData$`4orMoreDrinksDaily`, cleanData$HighBloodPressure)
td 

chisq_result <- chisq.test(td)
chisq_result
chisq_result$p.value
# p-value:  0.0006144196
# REJECT





td2 <- table(cleanData$`4orMoreDrinksDaily`, cleanData$HighCholesterol)
td2 

chisq_result <- chisq.test(td2)
chisq_result
chisq_result$p.value
# p-value: 0.1470252




td3 <- table(cleanData$`4orMoreDrinksDaily`, cleanData$FailingKidneys)
td3

chisq_result <- chisq.test(td3)
chisq_result
chisq_result$p.value
# p-value:  0.7527857





