
# first need to run code from "cleaningData.R"

library(reshape2)
library(ggplot2)
library(corrplot)

# calculate correlations
liverCors <- livercond_hbp |>
  cor() |>
  melt() |>
  as.data.frame()
liverCors

liverCors2 <- test2 |>
  cor() |>
  melt() |>
  as.data.frame()
liverCors2

# plot correlations

ggplot(liverCors2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "orange", high = "blue", mid = "white", midpoint = 0)

test2 <- filter(test2, agetold_liver < 78, agetold_hbp < 78)
test3 <- filter(test2, agetold_liver > 21, agetold_hbp > 21)

cor_told3 <- cor(test3)
corrplot(cor_told3, tl.srt = 30,
         type = "full") # correlation between agetold_liver and agetold_hbp ?

# scatterplot (agetold_liver vs. agetold_hbp)

ggplot(test3, aes(x = agetold_liver, y = agetold_hbp)) +
  geom_abline(lty = "dashed", color = "red", linewidth = 1, slope = 0.5, intercept = 25) +
  xlim(21,78) +
  ylim(21,78) +
  geom_smooth(linewidth = 2) +
  theme_minimal()

# bar graphs between people with hbp or liver disease

ggplot(livercond_hbp, aes(x = told_hbp, fill = as.factor(told_liver))) +
  geom_bar() +
  theme_minimal()

ggplot(livercond_hbp, aes(x = told_liver, fill = as.factor(told_hbp))) +
  geom_bar() +
  theme_minimal()
