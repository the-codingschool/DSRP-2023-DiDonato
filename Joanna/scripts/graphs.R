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
     y = "Mean Alcohol Consumption",
     title = "Average Alcohol Consumption Based on Gender") 


#### Convert to Numeric ####

    
dataCors <- cleanData |>
  cor() |>
  melt() |>
  as.data.frame()


ggplot(dataCors, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#0B0033", mid = "#370031", high = "#832232", midpoint = 0)
