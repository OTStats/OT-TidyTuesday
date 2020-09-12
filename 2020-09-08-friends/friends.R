

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
  ggplot(aes(x = id, y = imdb_rating)) + 
  # vertical lines: Season median rating to episode rating line
  geom_segment(aes(x = id, 
                   xend = id, 
                   y = median_rating, 
                   yend = imdb_rating), color = "black", alpha = 0.4) + 
  # horizontal lines: Season median rating
  geom_segment(aes(x = season_start_id, 
                   xend = season_end_id, 
                   y = median_rating,
                   yend = median_rating, 
                   color = as_factor(season)), 
               size = 1.5) + 
  geom_point(aes(fill = as_factor(season)), color = "black", pch = 21) + 
  annotate(
    "text", 
    x = 110, 
    y = 7, 
    hjust = 0, 
    vjust = 1, 
    label = "Flashback episodes have\nthe lowest ratings", 
    family = "friends", 
    size = 2.5, 
    alpha = 0.7
    ) + 
  annotate(
    "text", 
    x = 110, 
    y = 9.7, 
    hjust = 0, 
    vjust = -1,
    label = "The One Where Everyone Finds Out", 
    family = "friends", 
    size = 2.5,
    alpha = 0.7
    ) + 
  annotate(
    "text", 
    x = 83, 
    y = 9.5, 
    hjust = 0, 
    vjust = -1,
    label = "The One With the Embryos", 
    family = "friends", 
    size = 2.5,
    alpha = 0.7
    ) + 
  geom_curve(aes(x = 12, 
                 y = 7.8, 
                 xend = 14, 
                 yend = 8.15), 
             color = "grey", 
             curvature = .18, arrow = arrow(length = unit(0.02, "npc"))) + 
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
    label = "Median season\nIMDb rating", 
    family = "friends", 
    size = 2.5, 
    alpha = 0.7
    ) + 
  scale_fill_manual(
    values = c(
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
    )) + 
  scale_color_manual(
    values = c(
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
  )) + 
  # Formatting
  scale_y_continuous(position = "right") + 
  labs(title = "F R I E N D S", 
       subtitle = "IMDb Ratings for all 236 Friends Episodes", 
       caption = "Data from {friends}\nCreated by Owen Thompson\nTiwtter/Github: @OTStats", 
       x = "", 
       y = "IMDb Rating") + 
  guides(color = FALSE, 
         fill = FALSE) + 
  expand_limits(y = c(6.5, 10)) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = .5, size = 35, face = "bold", family = "friends"), 
        plot.subtitle = element_text(hjust = .5, vjust = -1, size = 10, family = "sans", color = "#6b6a60"), 
        plot.caption = element_text(hjust = 0, size = 8, family = "sans", color = "#6b6a60"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = unit(c(1, .5, .5, .5), "cm"), 
        text = element_text(family = "friends"),
        # axis.text.y = element_text(family = "friends"), 
        axis.text.x = element_blank())
