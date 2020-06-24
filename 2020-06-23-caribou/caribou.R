
# ---- Load libraries
library(tidyverse)

# ---- Read data
individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')


individuals
locations %>% 
  distinct(animal_id)

locations %>% 
  ggplot(aes(x = timestamp)) + 
  geom_bar()

library(ggmap)
register_google(key = "#######", write = TRUE)


mean_lat <- mean(locations$latitude)
mean_lon <- mean(locations$longitude)
get_map(location = c(lon = mean_lon, lat = mean_lat), zoom = 13, maptype = "terrain-background")
get_map("houston", zoom = 14)
get_googlemap("houston")

locations %>% 
  # filter(animal_id == "GR_C01") %>% 
  ggplot(aes(x = longitude, y = latitude)) + 
  stat_density2d(geom = "tile", aes(fill = ..density..), contour = FALSE)



get_map(maptyp)

qmplot(longitude, latitude, data = locations, geom = "blank", 
       zoom = 5, maptype = "terrain", darken = .7, legend = "topleft") +
  geom_path(aes(group = animal_id))
  
  # stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .8, color = NA)
  # scale_fill_gradient2("Robbery\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 650)


install.packages("moveVis")
library(moveVis)

test <- locations %>% 
  filter(animal_id == sample(unique(animal_id), 1))
data("move_data", package = "moveVis")
align_move(test, res = 4, unit = "hours")

locations %>% 
  group_by(animal_id) %>% 
  summarise(min_date = min(timestamp), 
            max_date = max(timestamp), 
            days = max_date - min_date, 
            n = n()) %>% 
  filter(n > 10) %>% 
  arrange(days)


test_caribou <- locations %>% 
  filter(animal_id %in% c("QU_car013", "QU_car015"))

m <- df2move(test_caribou, 
             proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
             x = "longitude", y = "latitude", 
             time = "timestamp", track_id = "animal_id") %>% 
  align_move(., unit = "days")

frames <- frames_spatial(m, 
                         path_colours = c("red", "blue"),
                         map_service = "osm", 
                         map_type = "hike", 
                         alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% 
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress() 

animate_frames(frames, out = "test-moveVis-01.gif")

# ------.
m <- df2move(test, 
        proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
        x = "longitude", y = "latitude", 
        time = "timestamp") %>% 
  align_move(., unit = "days")

frames <- frames_spatial(m, path_colours = c("red"),
               map_service = "osm", map_type = "hike", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress()

frames[[10]]

animate_frames(frames, out = "test-moveVis.gif")


# ----- Reference
m_ref <- align_move(move_data, res = 4, unit = "mins")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m_ref, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m_ref, type = "label") %>%
  add_progress()

frames[[1000]]
