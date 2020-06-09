

# ---- Load libraries
library(tidyverse)
library(lubridate)
theme_set(theme_light())
# ---- Load data
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')


rankings %>% 
  ggplot(aes(year, points, color = gender)) + 
  geom_jitter(alpha = 0.7)


rankings %>% 
  count(artist, sort = TRUE, name = "n_songs")


polls %>% 
  count(artist, sort = TRUE, name = "n_songs")
