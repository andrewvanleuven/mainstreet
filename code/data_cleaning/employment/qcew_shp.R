library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
library(RColorBrewer)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
# Read in Iowa data -------------------------------------------------------
iowa_crs <- 32615
clrs <- brewer.pal(7,"Purples")[2:7]
iowa_towns <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa", rucc > 2, pop2010 < 30000) %>% 
  left_join(.,(places(state = "19", cb = T) %>% mutate(geoid = as.numeric(GEOID)) %>%
                 select(geoid,geometry) %>% st_transform(.,crs = iowa_crs)), by = "geoid") %>% 
  select(geoid,name,geometry) %>% st_as_sf(crs = iowa_crs)
iowa_sf <- st_read("data/shp/google_earth/Iowa.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_remove_all(name, ", IA")) %>% st_transform(.,crs = iowa_crs)


iowa_downtowns <- st_intersection(iowa_towns,(iowa_sf %>% select(-name))) %>%  #%>% sample_n(.,1)
  st_buffer(., 200, joinStyle = "MITRE", endCapStyle = "SQUARE")


town_name <- iowa_downtowns %>% st_drop_geometry() %>% select(name) %>% pull()
town <- (iowa_towns %>% select(-geoid)) %>% filter(name == town_name)
cbd0 <- st_buffer(iowa_downtowns, 200, joinStyle = "MITRE", endCapStyle = "SQUARE")
cbd1 <- st_intersection(town,st_buffer(cbd0, 1*400))
cbd2 <- st_intersection(town,st_buffer(cbd0, 2*400))
cbd3 <- st_intersection(town,st_buffer(cbd0, 3*400))
cbd4 <- st_intersection(town,st_buffer(cbd0, 4*400))
buf5 <- st_difference(town,cbd4) %>% select(name,geometry) %>% mutate(buffer = 5)
buf4 <- st_difference(cbd4,cbd3) %>% select(name,geometry) %>% mutate(buffer = 4)
buf3 <- st_difference(cbd3,cbd2) %>% select(name,geometry) %>% mutate(buffer = 3)
buf2 <- st_difference(cbd2,cbd1) %>% select(name,geometry) %>% mutate(buffer = 2)
buf1 <- st_difference(cbd1,cbd0) %>% select(name,geometry) %>% mutate(buffer = 1)
buf0 <- cbd0 %>% select(name,geometry) %>% mutate(buffer = 0)
bind <- rbind(buf5,buf4,buf3,buf2,buf1,buf0) %>% select(name,buffer,geometry)

