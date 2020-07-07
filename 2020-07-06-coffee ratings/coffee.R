
# ---- Load libraries
library(tidyverse)

# ---- Get data
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')



# ---- Analysis
# Regression model
model <- lm(total_cup_points ~ aroma + flavor + aftertaste + acidity + body + 
                               balance + uniformity + clean_cup + sweetness, 
   data = coffee_ratings)
