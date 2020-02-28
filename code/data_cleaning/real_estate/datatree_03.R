suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

#iowa <- read_csv("hidden/datatree/cleaned/datatree_ia01.csv") 
#wisconsin <- read_csv("hidden/datatree/cleaned/datatree_wi01.csv") 
ohio <- read_csv("hidden/datatree/cleaned/datatree_oh01.csv") 
msp <- read_csv("data/csv/universe/msp_universe.csv") %>% 
  filter(st == "Ohio",
         cty_fips == 39013) %>% 
  mutate(name = toupper(name))

ohio_sf <- places(state = "39", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(msp %>% select(1), by = "city_fips") %>% 
  st_transform(2835)

belmont <- counties("39", cb = T) %>% 
  filter(GEOID == 39013) %>% 
  st_transform(2835)

crs <- crsuggest::suggest_crs(belmont)
glimpse(crs)
city_list <- msp %>% pull(name)

ohio_belmont <- ohio %>% 
  filter(SitusCity %in% city_list,
         !is.na(SitusLatitude) & !is.na(SitusLongitude),
         !is.na(YearBuilt)) %>% 
  distinct() %>% 
  st_as_sf(., coords = c("SitusLongitude","SitusLatitude"), crs = 4326) %>% 
  st_transform(.,crs = 2835) %>% 
  st_centroid_xy() %>% 
  st_intersection(.,belmont)

ggplot() + 
  geom_sf(data = belmont) + 
  geom_point(data = ohio_belmont, size = .25, aes(x,y, color = YearBuilt)) + 
  viridis::scale_color_viridis() +
  theme_void() + 
  ggsave("plot/maps/ohio_datatree_test.png", width = 15, height = 10)
