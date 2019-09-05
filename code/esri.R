library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
options(tigris_class = "sf")
# Map Projection ----------------------------------------------------------
iowa_crs <- "+init=epsg:3417"
# Read in Data ------------------------------------------------------------
iowa_places <- places(state = "19", cb = T) %>% 
  mutate(geoid = as.numeric(GEOID)) %>% select(geoid,geometry)
iowa_towns <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa", rucc > 2, pop2010 < 30000) %>% 
  left_join(.,iowa_places, by = "geoid") %>% 
  st_as_sf() %>% st_transform(., crs = iowa_crs) %>% select(geoid:rucc,geometry) 
esri1997 <- read_csv("hidden/esri/iowa/esri_iowa_1997.csv") %>% 
  filter(!is.na(Longitude)) %>% select(-(`SIC Code`:`SIC6_Descriptions(SIC4)`)) %>% 
  st_as_sf(., coords = c("Longitude","Latitude"), crs = iowa_crs) %>% 
  mutate(x = sapply(geometry, "[[", 1), y = sapply(geometry, "[[", 2))
# Make a map --------------------------------------------------------------
ggplot() + geom_sf(data = (states(cb = T) %>% filter(STATEFP == "19"))) +
  geom_sf(data = iowa_towns, color = NA,fill = "yellow") +
  geom_point(data = (sample_frac(esri1997,.01)), size = 0.2,
             aes(x=x, y=y)) +
  theme_void()

# Intersection of MSP Towns and ESRI data ---------------------------------
esri97 <- st_intersection(iowa_towns,esri1997)
