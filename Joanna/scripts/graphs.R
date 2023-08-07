library(ggplot2)


#### Visualize the Data ####
## Alcohol Consumption Based on Gender ##
avg_alcConsum <- summarize(cleanData,
                           mean_alc = mean(AvgDrinksDaily),
                           .by = gender)
View(avg_alcConsum)




ggplot(data = avg_alcConsum, aes(x= gender, y= mean_alc, fill = gender))+
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values=c('red', "blue"))
labs(x = "Gender",
     y = "Mean Alcohol Consumption Daily",
     title = "Average Alcohol Consumption Based on Gender") 
# On avg Males tend to drink more alcohol daily than Females > possibly can affect how data on affect on health 


#### Correlation Graph ####

    
dataCors <- cleanData |>
  cor() |>
  melt() |>
  as.data.frame()


ggplot(subset(dataCors, !is.na(value)), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#0B0033", mid = "#370031", high = "#832232", midpoint = 0)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))






#### 4 or More Drinks daily vs. Blood Pressure ####
cleanData <- cleanData |>
  filter(`4orMoreDrinksDaily` != 9,
         `4orMoreDrinksDaily` !=7)

cleanData["4orMoreDrinksDaily"][cleanData["4orMoreDrinksDaily"] == "1"] <- "Yes"
cleanData["4orMoreDrinksDaily"][cleanData["4orMoreDrinksDaily"] == "2"] <- "No"

cleanData["HighBloodPressure"][cleanData["HighBloodPressure"] == "1"] <- "Yes"
cleanData["HighBloodPressure"][cleanData["HighBloodPressure"] == "2"] <- "No"

ggplot(cleanData, aes(x = `4orMoreDrinksDaily`, fill = HighBloodPressure)) +
  geom_bar(position = "dodge") +
  labs(x = "Drink 4 or More Alcholic Bevrages Daily (12 mo.)",
       y = "Count",
       title = "High Blood Pressure vs. 4 or More Drinks Daily")















#### 4 or More Drinks daily vs. Cholestrerol ####
cleanData <- cleanData |>
  filter(`4orMoreDrinksDaily` != 9,
         `4orMoreDrinksDaily` !=7)

cleanData["4orMoreDrinksDaily"][cleanData["4orMoreDrinksDaily"] == "1"] <- "Yes"
cleanData["4orMoreDrinksDaily"][cleanData["4orMoreDrinksDaily"] == "2"] <- "No"

cleanData["HighCholesterol"][cleanData["HighCholesterol"] == "1"] <- "Yes"
cleanData["HighCholesterol"][cleanData["HighCholesterol"] == "2"] <- "No"

ggplot(cleanData, aes(x = `4orMoreDrinksDaily`, fill = HighCholesterol)) +
  geom_bar(position = "dodge") +
  labs(x = "Drink 4 or More Alcholic Bevrages Daily (12 mo.)",
       y = "Count",
       title = "High Cholesterol vs. 4 or More Drinks Daily")         







cleanData <- cleanData |>
  filter(`4orMoreDrinksDaily` != 9,
         `4orMoreDrinksDaily` !=7)

cleanData["4orMoreDrinksDaily"][cleanData["4orMoreDrinksDaily"] == "1"] <- "Yes"
cleanData["4orMoreDrinksDaily"][cleanData["4orMoreDrinksDaily"] == "2"] <- "No"

cleanData["gender"][cleanData["gender"] == "1"] <- "Male"
cleanData["gender"][cleanData["gender"] == "2"] <- "Female"

ggplot(cleanData, aes(x = `4orMoreDrinksDaily`, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(x = "Drink 4 or More Alcholic Bevrages Daily (12 mo.)",
       y = "Count",
       title = "Gender vs. 4 or More Drinks Daily")







cleanData["HighBloodPressure"][cleanData["HighBloodPressure"] == "1"] <- "Yes"
cleanData["HighBloodPressure"][cleanData["HighBloodPressure"] == "2"] <- "No"

cleanData["gender"][cleanData["gender"] == "1"] <- "Male"
cleanData["gender"][cleanData["gender"] == "2"] <- "Female"





cleanData["HighCholesterol"][cleanData["HighCholesterol"] == "1"] <- "Yes"
cleanData["HighCholesterol"][cleanData["HighCholesterol"] == "2"] <- "No"

cleanData["gender"][cleanData["gender"] == "1"] <- "Male"
cleanData["gender"][cleanData["gender"] == "2"] <- "Female"





cleanData["HighBloodPressure"][cleanData["HighBloodPressure"] == "1"] <- "Yes"
cleanData["HighBloodPressure"][cleanData["HighBloodPressure"] == "2"] <- "No"

cleanData["age"][cleanData["age"] == "1"] <- "Male"
cleanData["age"][cleanData["age"] == "2"] <- "Female"





ggplot(cleanData, aes(x = `gender`, fill = HighBloodPressure)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(name = "Gender",
                       limits = c("Male", "Female")) +
  scale_fill_manual(values = c('orange','purple')) +
  labs(title = "High Blood Pressure Based on Gender")


ggplot(cleanData, aes(x = `gender`, fill = HighCholesterol)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(name = "Gender",
                   limits = c("Male", "Female")) +
  scale_fill_manual(values = c('orange','purple')) +
  labs(title = "Blood Pressure Based on Gender")



ggplot(cleanData, aes(x = `AvgDrinksDaily`, fill = HighBloodPressure)) +
  geom_bar(position = "dodge") +
  labs(x = "Average Drinks in One Day",
       y = "Count",
       title = "High Blood Pressure vs. Average Drinks in One Day")



ggplot(cleanData, aes(x = `AvgDrinksDaily`, fill = HighCholesterol)) +
  geom_bar(position = "dodge") +
  labs(x = "Average Drinks in One Day",
    y = "Count",
    title = "High Cholesterol vs. Average Drinks in One Day")






## chisq models

var_name2 <- c('High Blood Pressure', 'High Cholesterol')
p_value2 <- c(0.03167844, 0.04909763)

df_value2 <- data.frame(var_name2, p_value2)

ggplot(data = df_value2, aes(x = var_name2, y = p_value2)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  labs(x = "Variable",
       y = "P-Value",
       title = "Chisq Results for Gender") +
  geom_hline(yintercept = 0.05, linetype = "dashed")









var_name <- c('High Blood Pressure', 'High Cholesterol')
p_value <- c(0.0006144196, 0.1470252)

df_value <- data.frame(var_name, p_value)

ggplot(data = df_value, aes(x = var_name, y = p_value)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  labs(x = "Variable",
       y = "P-Value",
       title = "Chisq Results of 4 or More Drinks Daily") +
  geom_hline(yintercept = 0.05, linetype = "dashed")






