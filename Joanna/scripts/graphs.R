library(ggplot2)


#### Convert to Numeric ####

    
dataCors <- cleanData |>
  cor() |>
  melt() |>
  as.data.frame()


ggplot(dataCors, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#0B0033", mid = "#370031", high = "#832232", midpoint = 0)
