library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
df <- read_csv("hidden/esri/iowa_geocoded.csv")
#df_sample <- df %>%
#  filter(year == 2000) %>% 
#  sample_n(.,1000) %>% 
#  select(id_yr:st,x,y) #%>% 
#  #write_csv("data/csv/sample_xy.csv")
# Read in Iowa data -------------------------------------------------------
iowa_crs <- 32615
iowa <- states(cb = T) %>% filter(STATEFP == "19") %>% st_transform(.,crs = iowa_crs)
iowa_df <- df %>%
  filter(year == 2000) %>% 
  sample_n(.,10000) %>% 
  select(id_yr:st,x,y) %>% 
  st_as_sf(., coords = c("x","y"), crs = 4326) %>%
  st_transform(.,crs = iowa_crs) %>% 
  mutate(utm_E = sapply(geometry, "[[", 1),
         utm_N = sapply(geometry, "[[", 2))
iowa_towns <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa", rucc > 2, pop2010 < 30000) %>% 
  left_join(.,(places(state = "19", cb = T) %>% mutate(geoid = as.numeric(GEOID)) %>%
                 select(geoid,geometry) %>% st_transform(.,crs = iowa_crs)), by = "geoid") %>% 
  select(geoid:rucc,geometry) %>% st_as_sf(crs = iowa_crs)
iowa_sf <- st_read("data/shp/google_earth/Iowa.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_remove_all(name, ", IA")) %>% st_transform(.,crs = iowa_crs)  
iowa_downtowns <- st_intersection(iowa_towns,iowa_sf)
iowa_roads <- map((counties("IA", cb = TRUE, resolution = "20m") %>% pull(COUNTYFP)), 
                  ~{roads(state = "19", county = .x)}) %>% 
  rbind_tigris() %>% st_transform(.,crs = iowa_crs)  

iowa$geometry
iowa_df$geometry
iowa_towns$geometry
iowa_sf$geometry
iowa_downtowns$geometry

# Map ---------------------------------------------------------------------
ggplot() + 
  geom_sf(data = iowa) +
  geom_sf(data = iowa_towns, color = NA,fill = "yellow") +
  geom_sf(data = iowa_downtowns, color = "red",size = .05) +
  geom_point(data = iowa_df, 
             aes(x = utm_E,
                 y = utm_N),
             size = .25) +
  theme_void() +
  coord_sf(ylim = c(4761146, 4816486), xlim = c(214657.5, 299804.1), expand = FALSE) + 
  ggsave("plot/cura.png", width = 20, height = 16)

# Start Buffering Around Towns --------------------------------------------
one <- iowa_towns %>% filter(name == "Webster City")
one_sf <- st_intersection(one,(st_buffer(iowa_downtowns, 200, joinStyle = "MITRE", endCapStyle = "SQUARE"))) %>% 
  mutate(buffer = 5) %>% select(geoid:rucc,buffer,geometry)
ring5 <- st_difference(one,(st_intersection(one,st_buffer(one_sf, 4*400)))) %>% 
  mutate(buffer = 5) %>% select(geoid:rucc,buffer,geometry)
ring4 <- st_difference((st_intersection(one,st_buffer(one_sf, 4*400))),(st_intersection(one,st_buffer(one_sf, 3*400)))) %>% 
  mutate(buffer = 4) %>% select(geoid:rucc,buffer,geometry)
ring3 <- st_difference((st_intersection(one,st_buffer(one_sf, 3*400))),(st_intersection(one,st_buffer(one_sf, 2*400)))) %>% 
  mutate(buffer = 3) %>% select(geoid:rucc,buffer,geometry)
ring2 <- st_difference((st_intersection(one,st_buffer(one_sf, 2*400))),(st_intersection(one,st_buffer(one_sf, 400)))) %>% 
  mutate(buffer = 2) %>% select(geoid:rucc,buffer,geometry)
ring1 <- st_difference((st_intersection(one,st_buffer(one_sf, 400))),(st_intersection(one,st_buffer(one_sf, 15)))) %>% 
  mutate(buffer = 1) %>% select(geoid:rucc,buffer,geometry)
ring0 <- st_intersection(one,st_buffer(one_sf, 15)) %>% 
  mutate(buffer = 0) %>% select(geoid:rucc,buffer,geometry)
ring_bind <- rbind(ring5,ring4,ring3,ring2,ring1,ring0) %>% arrange(geoid)
ring_dots <- st_intersection(ring_bind,iowa_df)
ring_roads <- st_intersection(one,iowa_roads)

ggplot() +
  geom_sf(data = ring5, fill = "red", alpha = .4) +
  geom_sf(data = ring4, fill = "yellow", alpha = .4) +
  geom_sf(data = ring3, fill = "orange", alpha = .4) +
  geom_sf(data = ring2, fill = "blue", alpha = .4) +
  geom_sf(data = ring1, fill = "green", alpha = .4) +
  geom_sf(data = ring0, fill = "magenta", alpha = .4) +
  geom_sf(data = ring_roads, color = "black", size = .2) + 
  geom_point(data = ring_dots, 
             aes(x = utm_E,
                 y = utm_N),
             size = .25) +
  theme_void() +
  ggsave("plot/buffertest.png", height = 7, width = 7)

freqTab(ring_dots,"buffer")

