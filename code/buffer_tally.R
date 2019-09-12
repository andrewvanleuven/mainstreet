library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
df <- read_csv("hidden/esri/iowa_geocoded.csv")
df_sample <- df %>%
  filter(year == 2000) %>% 
  sample_n(.,1000) %>% 
  select(id_yr:st,x,y) #%>% 
  #write_csv("data/csv/sample_xy.csv")
# Read in Iowa data -------------------------------------------------------
iowa_crs <- "+init=epsg:3857"
iowa <- states(cb = T) %>% filter(STATEFP == "19") %>% st_transform(.,crs = iowa_crs)
iowa_df <- df_sample %>% 
  st_as_sf(., coords = c("x","y"), crs = iowa_crs) %>% 
  #mutate(x = sapply(geometry, "[[", 1), y = sapply(geometry, "[[", 2)) %>% 
  select(everything(),geometry) %>% st_transform(.,crs = iowa_crs,use_gdal = TRUE)
iowa_towns <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa", rucc > 2, pop2010 < 30000) %>% 
  left_join(.,(places(state = "19", cb = T) %>% mutate(geoid = as.numeric(GEOID)) %>%
                 select(geoid,geometry) %>% st_transform(.,crs = iowa_crs)), by = "geoid") %>% 
  select(geoid:rucc,geometry) %>% st_as_sf(crs = iowa_crs)
iowa_sf <- st_read("data/shp/google_earth/Iowa.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_remove_all(name, ", IA")) %>% st_transform(.,crs = iowa_crs)  
iowa_downtowns <- st_intersection(iowa_towns,iowa_sf)

iowa$geometry
iowa_df$geometry
iowa_towns$geometry
iowa_sf$geometry
iowa_downtowns$geometry

# Map ---------------------------------------------------------------------
ggplot() + 
  geom_sf(data = iowa) +
  geom_sf(data = iowa_towns, color = NA,fill = "yellow") +
  geom_sf(data = iowa_downtowns, color = "red",fill = "red") +
  geom_point(data = (sample_frac((iowa_df %>% filter(year == 2000)),.05)), 
             size = 0.005,
             aes(x=x, y=y)) +
  theme_void() +
  ggsave("plot/cura.png", width = 20, height = 16)

# Start Buffering Around Towns --------------------------------------------
iowa_list <- sample_n(iowa_sf,1) %>% pull(name) 

one <- iowa_towns %>% filter(name == iowa_list) 
one_sf <- st_intersection(one,(st_buffer((iowa_downtowns%>% filter(name == iowa_list)), 
                                         200, joinStyle = "MITRE", endCapStyle = "SQUARE"))) %>% 
  mutate(buffer = 5) %>% select(geoid:rucc,buffer,geometry)
one_buf0 <- st_intersection(one,(st_buffer(one_sf, 50))) %>% mutate(buffer = 0) %>% 
  select(geoid:rucc,buffer,geometry)
one_buf1 <- st_intersection(one,(st_buffer(one_sf, 1320))) %>% mutate(buffer = 1) %>% 
  select(geoid:rucc,buffer,geometry)
one_buf2 <- st_intersection(one,(st_buffer(one_sf, 2*1320))) %>% mutate(buffer = 2) %>% 
  select(geoid:rucc,buffer,geometry)
one_buf3 <- st_intersection(one,(st_buffer(one_sf, 3*1320))) %>% mutate(buffer = 3) %>% 
  select(geoid:rucc,buffer,geometry)
one_buf4 <- st_intersection(one,(st_buffer(one_sf, 4*1320))) %>% mutate(buffer = 4) %>% 
  select(geoid:rucc,buffer,geometry)
one_bind <- rbind(one_sf,one_buf0,one_buf1,one_buf2,one_buf3,one_buf4) %>% st_set_crs(iowa_crs)

df_sf<-st_transform(x = iowa_df, crs = 3417)

one_dots <- st_intersection(one_bind,iowa_df)

ggplot() + 
  geom_sf(data = one) + 
  geom_sf(data = one_buf4, fill = "#e31a1c",
          alpha = 0.25, lwd = 0) +
  geom_sf(data = one_buf3, fill = "#fd8d3c",
          alpha = 0.5, lwd = 0) +
  geom_sf(data = one_buf2, fill = "#fecc5c",
          alpha = 0.5, lwd = 0) +
  geom_sf(data = one_buf1, fill = "#ffffb2",
          alpha = 0.5, lwd = 0) +
  geom_sf(data = one_buf0, fill = "black",
          alpha = 0.25, color = "black") + 
  #geom_point(data = (sample_frac((iowa_df %>% filter(year == 2000)),.05)), 
  #           size = 0.05,
  #           aes(x=x, y=y)) +
  theme_void() +
  ggsave("plot/buffertest2.png", height = 5, width = 7)
  











