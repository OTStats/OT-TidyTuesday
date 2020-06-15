# American Slavery
# Owen Thompson
# 2020-06-16

# ---- Load libraries
library(tidyverse)

# ---- Get data
blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')




census %>% 
  pivot_longer(cols = c("total", "white", "black", "black_free", "black_slaves"), 
               names_to = "race", 
               values_to = "pop") %>% 
  # mutate(pop = pop / 1000000) %>% 
  filter(region == "USA Total", race %in% c("white", "black_free", "black_slaves")) %>% 
  ggplot(aes(x = year, y = pop / 1000000, color = race)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1790, 1870, by = 10))
