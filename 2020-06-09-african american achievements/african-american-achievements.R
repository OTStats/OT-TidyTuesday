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
firsts %>% 
  summarise(min_year = min(year), 
            max_year = max(year))

yearly_firsts <- firsts %>% 
  group_by(category, year) %>% 
  summarise(n = n()) %>% 
  group_by(category) %>% 
  mutate(total = cumsum(n)) %>% 
  ungroup() %>% 
  select(year, category, total)

years <- yearly_firsts %>% 
  expand(year = 1737:2019, category)

year_rank <- years %>% 
  left_join(yearly_firsts) %>% 
  mutate(total = if_else(year == 1737, 0, as.numeric(total))) %>% 
  group_by(category) %>% 
  fill(total, .direction = "down") %>% 
  ungroup() %>% 
  arrange(year, total) %>% 
  group_by(year) %>% 
  mutate(rank = 1:n())

install.packages("gganimate")
library(gganimate)
options(gganimate.nframes = 50)
plot <- year_rank %>% 
  filter(year > 2000) %>%
  ggplot() + 
  aes(xmin = 0, 
      xmax = total) + 
  aes(ymin = rank - .45, 
      ymax = rank + .45, 
      y = rank) + 
  # facet_wrap(~year) + 
  geom_rect() + 
  aes(fill = category) + 
  scale_fill_viridis_d() + 
  geom_text(col = "grey13", 
            hjust = "right", 
            aes(label = category), 
            x = -10) + 
  # scale_x_continuous(limits = c(-80, 120)) + 
  guides(color = F, fill = F) + 
  my_theme


# my_theme <- theme_classic(base_family = "Times") +
#   theme(axis.text.y = element_blank()) +
#   theme(axis.ticks.y = element_blank()) +
#   theme(axis.line.y = element_blank()) +
#   theme(legend.background = element_rect(fill = "gainsboro")) +
#   theme(plot.background = element_rect(fill = "gainsboro")) +
#   theme(panel.background = element_rect(fill = "gainsboro"))

plot + 
  # facet_null() + 
  transition_states(year) + 
  view_follow(fixed_x = T)
  # scale_x_continuous(limits = c(-75, 115), 
  #                    breaks = c(0, 25, 50, 75, 100)) + 
  # geom_text(x = 100 , y = 1,
  #           family = "Times",
  #           aes(label = as.character(year)),
  #           size = 6, col = "grey18") + 
  # aes(group = category) + 
  # gganimate::transition_time(year)

  

# Take 2 ####
plot2 <- year_rank %>% 
  filter(year > 2000) %>%
  ggplot(aes(rank, group = category, fill = category)) + 
  geom_tile(aes(y = total/2, height = total, width = 0.9), alpha = 0.9) + 
  geom_text(aes(y = 0, label = paste(category, " ")), vjust = 0.2, hjust = 1) + 
  geom_text(aes(y = total, label = total, hjust = 0)) + 
  coord_flip(clip = "off", expand = F)

plot2 + transition_states(year) +
  view_follow(fixed_x = TRUE)


  
yearly_firsts %>% 
  ggplot(aes(x = year, y = total, color = category)) + 
  geom_step()


firsts %>% 
  group_by(gender) %>% 
  mutate(n = 1, 
         total = cumsum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = total, color = gender)) + 
  geom_step()

  scale_x_continuous(breaks = seq(1750, 2000, by = 50)) + 
  theme(plot.background = element_rect(fill = "#8d99ae"), 
        axis.text = element_text(color = "#2b2d42"), 
        panel.background = element_rect(fill = "#8d99ae"), 
        panel.gr) 
        # panel.grid.major = element_line(color = "#14213d"))

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



science %>% 
  mutate(alive = if_else(is.na(death), "alive", "deceased")) %>% 
  replace_na(list(death = 2019)) %>% 
  mutate(occupation = str_remove(occupation_s, ";.+$") %>% fct_lump(n = 8), 
         name = fct_reorder(name, birth)) %>% 
  filter(occupation == "Inventor") %>% 
  ggplot() + 
  geom_segment(aes(x = birth, xend = death, y = name, yend = name))
