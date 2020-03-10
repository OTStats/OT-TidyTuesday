# College tuition, diversity, and pay
# Owen Thompson
# 2020-03-09

# ---- Load libraries
library(tidyverse)
library(glue)
library(here)

# ---- Get data
# tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
# tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 
# salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')
# historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')



diversity_school %>% 
  filter(state == "Wisconsin") %>% 
  filter(str_detect(name, "University of Wisconsin at")) %>% 
  filter(!category %in% c("Women", "Non-Resident Foreign", "Total Minority")) %>% 
  
  mutate(percent = enrollment / total_enrollment) %>% 
  filter(category == "White") %>% 
  mutate(name = fct_reorder(name, percent)) %>% 
  ggplot(aes(x = name, y = percent, fill = category)) + 
  geom_bar(stat = "identity") + 
  coord_flip()


schools_by_state <- diversity_school %>% 
  distinct(name, state) %>% 
  count(state) %>% 
  filter(!is.na(state))

schools_by_state %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  ggplot(aes(x = state, y = n)) + 
  geom_col() + 
  coord_flip() + 
  labs(title = "Number of universities by state")

# Wanted to look at state schools, but this is more difficult than anticipated
# diversity_school %>% 
#   filter(str_detect(name, "^University .+ at "))


# Plotting school diversity by state
diversity_school %>% 
  filter(category == "White") %>% 
  filter(!is.na(state)) %>% 
  # group_by(state) %>% 
  # filter(n() > 60) %>% 
  # ungroup() %>% 
  mutate(percent = enrollment / total_enrollment) %>% 
  mutate(state = fct_reorder(state, percent, .desc = T)) %>% 
  ggplot(aes(x = state, y = percent, size = total_enrollment)) + 
  geom_jitter(color = "grey", alpha = 0.5) + 
  geom_boxplot(alpha = 0) + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent_format()) + 
  coord_flip() + 
  labs(title = "How diverse are state's universities?", 
       x = "", 
       y = "Percent white students", 
       caption = "Created by: @OTStats") + 
  guides(size = F)

ggsave(filename = glue(here("2020-03-02-college tuition"), "states-college-diversity.png"), 
       width = 5, 
       height = 8.5)


