library(tidyverse)
library(sf)
library(rleuven)
library(tigris)

esri1997 <- read_csv("hidden/esri/iowa/esri_iowa_1997.csv") %>% 
  filter(!is.na(Longitude))
iowa_towns <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa", rucc > 2, pop2010 < 30000) %>% 
  right_join(.,(places(state = "19", cb = T) %>% 
                  mutate(geoid = as.numeric(GEOID)) %>% 
                  select(geoid,geometry)), by = "geoid")
iowa_crs <- "+proj=lcc +lat_1=41.78333333333333 +lat_2=40.61666666666667 +lat_0=40 +lon_0=-93.5 +x_0=500000.00001016 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

df_sf <- st_as_sf(esri1997, coords = c("Longitude", "Latitude"), crs = iowa_crs) %>%
  mutate(x = sapply(geometry, "[[", 1),
         y = sapply(geometry, "[[", 2))

ggplot() + geom_sf(data = (states(cb = T) %>% filter(STATEFP == "19"))) +
  geom_point(data = (sample_n(df_sf,100)),
             aes(x=x, y=y))
