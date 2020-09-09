

# ---- Load libraries
library(tidyverse)

# ---- Read data
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

friends
friends_emotions
friends_info


install.packages("friends")
library(friends)
friends::friends_entities %>% 
  unnest()


friends_info %>% 
  group_by(season) %>% 
  mutate(median_rating = median(imdb_rating)) %>% 
  ggplot(aes(x = episode, y = imdb_rating, color = as_factor(season))) + 
  geom_point() + 
  geom_hline(aes(yintercept = median_rating)) + 
  geom_segment(aes(x = episode, 
                   xend = episode, 
                   y = median_rating, 
                   yend = imdb_rating)) + 
  facet_grid(.~as_factor(season)) + 
  guides(color = F) + 
  expand_limits(y = c(6.5, 10)) + 
  theme_minimal()
