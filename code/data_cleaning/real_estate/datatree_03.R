suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

#iowa <- read_csv("hidden/datatree/cleaned/datatree_ia02.csv") 
#wisconsin <- read_csv("hidden/datatree/cleaned/datatree_wi02.csv") 
#ohio <- read_csv("hidden/datatree/cleaned/datatree_oh02.csv") 
msp <- read_csv("data/csv/universe/msp_universe.csv") %>% 
  filter(st == "Ohio")

ohio_sf <- places(state = "39", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(msp %>% select(1), by = "city_fips") %>% 
  st_transform(2834)

oh_ctys <- counties("39", cb = T) %>% 
  st_transform(2834)

#crs <- crsuggest::suggest_crs(oh_ctys)

#ohio_dots <- ohio %>% 
#  filter(!is.na(SitusLatitude) & !is.na(SitusLongitude)) %>% 
#  distinct() %>% 
#  st_as_sf(., coords = c("SitusLongitude","SitusLatitude"), crs = 4326) %>% 
#  st_transform(.,crs = 2834) %>% 
#  st_centroid_xy() %>% 
#  st_intersection(.,ohio_sf) %>% 
#  write_csv("hidden/datatree/cleaned/datatree_oh02.csv")

oh_dots <- read_csv("hidden/datatree/cleaned/datatree_oh02.csv") 
ohio_dots <- oh_dots %>% 
  select(1:5,11,109:111) %>% 
  filter(!is.na(AssdTotalValue)) %>% 
  mutate(value_decile = ntile(AssdTotalValue,10)) %>% 
  st_as_sf(., coords = c("x","y"), crs = 2834) %>% 
  st_centroid_xy()

#num_of_NAs(ohio_dots)

ggplot() + 
  geom_sf(data = oh_ctys) +
  geom_point(data = ohio_dots, 
             size = .2, aes(x,y, color = factor(value_decile))) + 
  theme_void() + 
  ggsave("plot/maps/ohio_datatree_test2.png", width = 24, height = 16)

beepr::beep()