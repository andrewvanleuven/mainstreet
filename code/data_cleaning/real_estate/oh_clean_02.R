suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

df <- read_fst("hidden/datatree/cleaned/datatree_oh01.fst") %>% 
  filter(!is.na(situs_latitude) & !is.na(situs_longitude),
         situs_latitude != 0 & situs_longitude != 0) %>% 
  st_as_sf(., coords = c("situs_longitude","situs_latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 2834)
oh_msp <- read_csv("data/csv/universe/oh_universe.csv")
beepr::beep()

ohio_sf <- places(state = "39", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(oh_msp %>% select(1,2), by = "city_fips") %>% 
  st_transform(2834) %>% 
  st_buffer(dist = 1500)

ohio_dots <- df %>% 
  st_intersection(.,ohio_sf) %>% 
  st_drop_geometry() %>% 
  write_fst("hidden/datatree/cleaned/datatree_oh02.fst")
beepr::beep()


# Test Map ----------------------------------------------------------------
oh_ctys <- counties("39", cb = T) %>% 
  st_transform(2834)
#suggest_crs(oh_ctys)[1,3]
ggplot() +
  geom_sf(data = oh_ctys) +
  geom_sf(data = ohio_sf, fill = "red", color = NA) +
  geom_sf(data = ohio_sf %>% filter(name == "Woodsfield"), fill = "black", color = NA) +
  theme_void()
