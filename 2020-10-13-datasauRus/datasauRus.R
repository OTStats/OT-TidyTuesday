
# ---- Load libraries
library(ggplot2)
library(datasauRus)
library(gganimate)

datasauRus::datasaurus_dozen %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  gghighlight::gghighlight(dataset == "dino")
  transition_states(dataset,
                    transition_length = 2,
                    state_length = 1)
