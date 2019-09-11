library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Iowa data -------------------------------------------------------
iowa_crs <- "+proj=lcc +lat_1=43.26666666666667 +lat_2=42.06666666666667 +lat_0=41.5 +lon_0=-93.5 +x_0=1500000 +y_0=999999.9999898402 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
iowa_df <- read_csv("hidden/esri/iowa_geocoded.csv") %>% 
  st_as_sf(., coords = c("x","y"), crs = iowa_crs) %>% 
  mutate(x = sapply(geometry, "[[", 1), y = sapply(geometry, "[[", 2)) %>% 
  select(everything(),geometry)
iowa_places <- places(state = "19", cb = T) %>% 
  mutate(geoid = as.numeric(GEOID)) %>% select(geoid,geometry)
iowa_towns <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa", rucc > 2, pop2010 < 30000) %>% 
  left_join(.,iowa_places, by = "geoid") %>% 
  st_as_sf()  %>% select(geoid:rucc,geometry) %>% st_set_crs(iowa_crs)
iowa <- states(cb = T) %>% filter(STATEFP == "19") %>% st_set_crs(iowa_crs)

# Map ---------------------------------------------------------------------
ggplot() + 
  geom_sf(data = iowa) +
  geom_sf(data = iowa_towns, color = NA,fill = "yellow") +
  geom_point(data = (sample_frac((iowa_df %>% filter(year == 2000)),.05)), 
             size = 0.05,
             aes(x=x, y=y)) +
  theme_void() +
  ggsave("plot/cura.png", width = 20, height = 16)


