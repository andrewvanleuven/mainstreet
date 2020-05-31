library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(leaflet)
library(mapview)
library(mapedit)
library(leafpm)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

oh_bldg <- st_read("hidden/microsoft/Ohio.geojson") %>% st_transform(2834)

line <- c(40.3075612,-83.0816224,40.2873177,-83.0486579)
delaware_polygon <- data.frame(lat = c(line[1],line[1],line[3],line[3],line[1]),
                               lon = c(line[2],line[4],line[4],line[2],line[2])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% st_transform(2834)
delaware_bldg <- st_intersection(oh_bldg,delaware_polygon) %>% 
  mutate(id = 1:n()) %>% 
  select(id,geometry) %>% 
  st_write("data/shp/bldgs/delaware_bldgs.kml", driver='kml')
beepr::beep()

line <- c(41.852692, -80.808156,41.884454, -80.757522)
ashtabula_polygon <- data.frame(lat = c(line[1],line[1],line[3],line[3],line[1]),
                                lon = c(line[2],line[4],line[4],line[2],line[2])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% st_transform(2834)
ashtabula_bldg <- st_intersection(oh_bldg,ashtabula_polygon) %>% 
  mutate(id = 1:n()) %>% 
  select(id,geometry) %>% 
  st_write("data/shp/bldgs/ashtabula_bldgs.kml", driver='kml')
beepr::beep()

line <- c(40.627401, -80.589882,40.612999, -80.566633)
eastliverpool_polygon <- data.frame(lat = c(line[1],line[1],line[3],line[3],line[1]),
                                    lon = c(line[2],line[4],line[4],line[2],line[2])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% st_transform(2834)
eastliverpool_bldg <- st_intersection(oh_bldg,eastliverpool_polygon) %>% 
  mutate(id = 1:n()) %>% 
  select(id,geometry) %>% 
  st_write("data/shp/bldgs/eastliverpool_bldgs.kml", driver='kml')
beepr::beep()

line <- c(41.057915, -83.679030,41.023304, -83.621105)
findlay_polygon <- data.frame(lat = c(line[1],line[1],line[3],line[3],line[1]),
                              lon = c(line[2],line[4],line[4],line[2],line[2])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% st_transform(2834)
findlay_bldg <- st_intersection(oh_bldg,findlay_polygon) %>% 
  mutate(id = 1:n()) %>% 
  select(id,geometry) %>% 
  st_write("data/shp/bldgs/findlay_bldgs.kml", driver='kml')
beepr::beep()

line <- c(39.550008, -83.454984,39.523624, -83.416632)
wch_polygon <- data.frame(lat = c(line[1],line[1],line[3],line[3],line[1]),
                                 lon = c(line[2],line[4],line[4],line[2],line[2])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% st_transform(2834)
wch_bldg <- st_intersection(oh_bldg,wch_polygon) %>% 
  mutate(id = 1:n()) %>% 
  select(id,geometry) %>% 
  st_write("data/shp/bldgs/wch_bldgs.kml", driver='kml')
beepr::beep()

line <- c(40.478878, -81.427500, 40.516479, -81.493614)
newphilly_polygon <- data.frame(lat = c(line[1],line[1],line[3],line[3],line[1]),
                                 lon = c(line[2],line[4],line[4],line[2],line[2])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% st_transform(2834)
newphilly_bldg <- st_intersection(oh_bldg,newphilly_polygon) %>% 
  mutate(id = 1:n()) %>% 
  select(id,geometry) %>% 
  st_write("data/shp/bldgs/newphilly_bldgs.kml", driver='kml')
beepr::beep()

line <- c(39.9572,-82.0403,39.9252,-81.9920)
zanesville_polygon <- data.frame(lat = c(line[1],line[1],line[3],line[3],line[1]),
                                 lon = c(line[2],line[4],line[4],line[2],line[2])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% st_transform(2834)
zanesville_bldg <- st_intersection(oh_bldg,zanesville_polygon) %>% 
  mutate(id = 1:n()) %>% 
  select(id,geometry) %>% 
  st_write("data/shp/bldgs/zanesville_bldgs.kml", driver='kml')
beepr::beep()
