library(tigris)
library(sf)
library(rgeos)
library(rgdal)
library(maptools)
library(units)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Load Michigan Counties and Places ---------------------------------------
mich <- counties(state = "MI", cb = T) %>% 
  filter(NAME == "St. Joseph")
riv3 <- places(state = "MI", cb = T) %>% 
  filter(NAME == "Three Rivers")
points <- read_csv("data/fake.csv")
# Read in Shapefile -------------------------------------------------------
riv3_msp <- st_read("data/shp/mich/Michigan-line.shp") %>% 
  filter(Name == "Three Rivers, MI") %>% 
  st_transform(., "+proj=utm +zone=16 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs")
#https://spatialreference.org/ref/sr-org/8009/proj4

# Create Buffer Around Main Street ----------------------------------------
riv3buff1 <- st_buffer(riv3_msp, 800)
riv3buff2 <- st_buffer(riv3_msp, 2000)
riv3buff3 <- st_buffer(riv3_msp, 3500)
#st_write(riv3buff1, "data/shp/buffers/ThreeRiversMI1.shp", driver = "ESRI Shapefile")

# Map It ------------------------------------------------------------------
ggplot() +
  #geom_sf(data = mich) +
  geom_sf(data = riv3,
          fill = "gray90") +
  geom_sf(data = riv3buff3,
          fill = "red",
          alpha = 0.5,
          lwd = 0) +
  geom_sf(data = riv3buff2,
          fill = "orange",
          alpha = 0.5,
          lwd = 0) +
  geom_sf(data = riv3buff1,
          fill = "yellow",
          alpha = 0.5,
          lwd = 0) +
  geom_sf(data = riv3_msp,
          color = "blue",
          size = 1) + 
  geom_point(data = points,
             aes(x = y, y = x),
             color = "black",
             fill = "white",
             shape = 21) +
  ggtitle("Three Rivers, Michigan") +
  theme_void() 

ggplot() +
  geom_sf(data = riv3,
          fill = "gray90") +
  geom_point(data = points,
             aes(x = y, y = x))

