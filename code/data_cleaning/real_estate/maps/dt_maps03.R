suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(janitor)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Stuff I've Done Already -------------------------------------------------
df <- read_csv("hidden/datatree/ohio_feasible.csv")
oh_towns <- places(state = "39", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID),
         name = NAME) %>% 
  select(city_fips,name,geometry) %>% 
  st_transform(2834)
rds <- roads("39","Auglaize")
h2o <- area_water("39","Auglaize")

ohio <- df %>% 
  st_as_sf(., coords = c("situs_longitude","situs_latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 2834) %>% 
  st_centroid_xy()

# New Stuff ---------------------------------------------------------------
oh_downtowns <- st_read("data/shp/google_earth/ohio.kml") %>% 
  st_transform(2834) %>% 
  rename_all(tolower) %>% 
  select(-description) 

town <- oh_towns %>% filter(name == "Wapakoneta")
dots <- st_intersection(ohio,town)
trds <- st_intersection(rds %>% st_transform(.,crs = 2834),town)
th2o <- st_intersection(h2o %>% st_transform(.,crs = 2834),town)
tcbd <- st_intersection(st_buffer(oh_downtowns %>% filter(name == "Wapakoneta"),200,
                                  joinStyle = "MITRE", endCapStyle = "SQUARE"),town)

ggplot() +
  geom_sf(data = town) +
  geom_sf(data = tcbd, fill = "yellow", alpha = .5) +
  geom_sf(data = trds) +
  geom_sf(data = th2o, color = "blue", fill = "cyan") +
  geom_point(data = dots, aes(x,y), size = .5, color = "red") +
  theme_void() 
