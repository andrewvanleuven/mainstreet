library(tidyverse)
library(sf)
library(tigris)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

iowa_crs <- 26775
iowa <- states(cb = T) %>% 
  filter(STATEFP == "19") %>% 
  select(STATEFP,geometry) %>% 
  st_transform(.,crs = iowa_crs)
df <- read_csv("data/csv/sample_xy.csv") %>% 
  st_as_sf(., coords = c("x","y"), crs = iowa_crs) %>% 
  mutate(x = sapply(geometry, "[[", 1), y = sapply(geometry, "[[", 2))

df_intersect <- st_intersection(iowa,df)

iowa$geometry 
df$geometry

ggplot() +
  geom_sf(data = iowa) + 
  geom_point(data = df, 
             aes(x=x,y=y)) 


