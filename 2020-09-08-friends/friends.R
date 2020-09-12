

# ---- Load libraries
library(tidyverse)
library(friends)

extrafont::loadfonts('win')
windowsFonts()
extrafont::fonts()
# extrafont::font_import()

# Resource: https://github.com/jack-davison/TidyTuesday/blob/master/R/2020_09_08_Friends.R
library(showtext)
showtext_auto()
font_add(family = "friends", regular = "~/GABRWFFR.TTF")

friends_df <- friends_info %>%
  mutate(id = row_number()) %>%
  group_by(season) %>% 
  mutate(median_rating = median(imdb_rating), 
         season_start_id = min(id), 
         season_end_id = max(id)) %>% 
  ungroup()

friends_df %>% 
  ggplot(aes(x = id, y = imdb_rating, color = as_factor(season))) + 
  geom_segment(aes(x = season_start_id, 
                   xend = season_end_id, 
                   y = median_rating,
                   yend = median_rating)) + 
  # Season median rating line
  geom_segment(aes(x = id, 
                   xend = id, 
                   y = median_rating, 
                   yend = imdb_rating), color = "black", alpha = 0.4) + 
  geom_point() + 
  labs(x = "") + 
  guides(color = FALSE) + 
  expand_limits(y = c(6.5, 10)) + 
  theme_minimal() + 
  annotate(
    "text", 
    x = 110, 
    y = 7, 
    hjust = 0, 
    vjust = 1, 
    label = "Flashback episodes have\nthe lowest ratings", 
    family = "friends", 
    size = 2.5
    ) + 
  geom_curve(aes(x = 108, 
                 y = 6.9, 
                 xend = 95, 
                 yend = 7.1), 
             color = "grey", 
             curvature = -.5, arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_curve(aes(x = 158, 
                 y = 6.95, 
                 xend = 164, 
                 yend = 7.48), 
             color = "grey", 
             curvature = .3, arrow = arrow(length = unit(0.03, "npc"))) + 
  annotate(
    "text", 
    x = -5, 
    y = 7.75, 
    hjust = 0, 
    vjust = 1, 
    label = "Median season\nIMDB rating", 
    family = "friends", 
    size = 2.5
    ) + 
  scale_color_manual(values = c(
    "1" = "#FF4238",
    "2" = "#42A2D6",
    "3" = "#FFDC00", 
    "4" = "#FF4238", 
    "5" = "#42A2D6", 
    "6" = "#FFDC00", 
    "7" = "#FF4238", 
    "8" = "#42A2D6", 
    "9" = "#FFDC00", 
    "10" = "#FF4238"
  ))
