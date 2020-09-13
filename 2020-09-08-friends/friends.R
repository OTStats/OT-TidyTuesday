# F R I E N D S
# Owen Thompson
# 2020-09-12

# ---- Load libraries
library(tidyverse)
library(friends)
library(extrafont)
# extrafont::loadfonts('win')
# windowsFonts()
# extrafont::fonts()
# extrafont::font_import()

# Download Friends font here: https://www.dafont.com/gabriel-weiss-friends.font
# Resource: https://github.com/jack-davison/TidyTuesday/blob/master/R/2020_09_08_Friends.R
library(showtext)
showtext_auto()
font_add(family = "friends", regular = here("2020-09-08-friends/GABRWFFR.TTF"))

# ---- Data prep
friends_df <- friends_info %>%
  mutate(id = row_number()) %>%
  group_by(season) %>% 
  mutate(median_rating = median(imdb_rating), 
         season_start_id = min(id), 
         season_end_id = max(id)) %>% 
  ungroup()

friends_season_text <- friends_df %>% 
  group_by(season) %>% 
  summarise(med_imdb_rating = median(imdb_rating), 
            start_id = min(id) + 1) %>% 
  mutate(season_text = if_else(season == 1, "Season 1", as.character(season)))

# ---- Data viz
friends_df %>% 
  ggplot(aes(x = id, y = imdb_rating)) + 
  geom_segment(aes(x = id, 
                   xend = id, 
                   y = median_rating, 
                   yend = imdb_rating), color = "black", alpha = 0.4) + 
  geom_segment(aes(x = season_start_id, 
                   xend = season_end_id, 
                   y = median_rating,
                   yend = median_rating, 
                   color = as_factor(season)), 
               size = 1.5) + 
  geom_point(aes(fill = as_factor(season)), color = "black", pch = 21) + 
  geom_text(data = friends_season_text, 
            aes(x = start_id, 
                y = med_imdb_rating, 
                label = season_text, 
                color = as_factor(season)), 
            vjust = -0.25, 
            hjust = 0, 
            size = 6, 
            alpha = .6) + 
  annotate(
    "text", 
    x = 120, 
    y = 7, 
    hjust = 0,
    vjust = 1, 
    label = "Flashback episodes have the lowest ratings", 
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
  annotate(
    "text", 
    x = 230, 
    y = 9.7, 
    hjust = 0, 
    vjust = -1,
    label = "The Last One(s)", 
    family = "friends", 
    size = 2.5,
    alpha = 0.7
    ) + 
  annotate(
    "text", 
    x = 38, 
    y = 9.4, 
    hjust = 0, 
    vjust = -1,
    label = "The One with the Prom Video", 
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
  geom_curve(aes(x = 118, 
                 y = 6.9, 
                 xend = 95, 
                 yend = 7.1), 
             color = "grey", 
             curvature = -.25, arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_curve(aes(x = 171, 
                 y = 6.97, 
                 xend = 188, 
                 yend = 7.45), 
             color = "grey", 
             curvature = .45, arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_curve(aes(x = 170, 
                 y = 6.9, 
                 xend = 203, 
                 yend = 7.45), 
             color = "grey", 
             curvature = .45, arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_curve(aes(x = 170, 
                 y = 7.1, 
                 xend = 167, 
                 yend = 7.45), 
             color = "grey", 
             curvature = .45, arrow = arrow(length = unit(0.025, "npc"))) + 
  geom_curve(aes(x = 147, 
                 y = 7.1, 
                 xend = 142, 
                 yend = 7.35), 
             color = "grey", 
             curvature = -.1, arrow = arrow(length = unit(0.025, "npc"))) + 
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
        plot.subtitle = element_text(hjust = .5, vjust = -1, size = 10, family = "sans", color = "#353831"), 
        plot.caption = element_text(hjust = 0, size = 8, family = "sans", color = "#353831"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = unit(c(1, .5, .5, .5), "cm"), 
        text = element_text(family = "friends"),
        # axis.text.y = element_text(family = "friends"), 
        axis.text.x = element_blank())

# ---- Save plot
ggsave(here("2020-09-08-friends/friends-episode-ratings.png"), width = 14, height = 9)
