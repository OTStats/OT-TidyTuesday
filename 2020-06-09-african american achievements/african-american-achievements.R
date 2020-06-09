# African American Achievements
# Owen Thompson
# 2020-06-09

# ---- Load libraries
library(tidyverse)
library(glue)
library(here)

# ---- Get data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

# ---- Data visualization
yearly_firsts <- firsts %>% 
  group_by(category) %>% 
  mutate(n = 1, 
         total = cumsum(n)) %>% 
  ungroup()

yearly_firsts %>% 
  ggplot(aes(x = year, y = total, color = category)) + 
  geom_step() + 
  scale_x_continuous(breaks = seq(1750, 2000, by = 50)) + 
  theme(plot.background = element_rect(fill = "#8d99ae"), 
        axis.text = element_text(color = "#2b2d42"), 
        panel.background = element_rect(fill = "#8d99ae"), 
        panel.gr) 
        panel.grid.major = element_line(color = "#14213d"))

theme_grey(base_size = base_size, base_family = base_family) %+replace% 
  theme(axis.text = element_text(size = rel(0.8)), 
        axis.ticks = element_line(colour = "black"), 
        legend.key = element_rect(colour = "grey80"), 
        panel.background = element_rect(fill = "white", colour = NA), 
        panel.border = element_rect(fill = NA, colour = "grey50"), 
        panel.grid.major = element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
        strip.background = element_rect(fill = "grey80", colour = "grey50", 
                                        size = 0.2))
                     