suppressMessages({library(tidyverse)
library(sf)
library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

csv <- "https://raw.githubusercontent.com/andrewvanleuven/mainstreet/master/data/csv/sample_xy.csv"

df <- read_csv(csv) %>% 
  st_as_sf(., coords = c("x","y"), crs = 4326) %>%
  st_transform(.,crs = 32610) %>% 
  mutate(utm_E = sapply(geometry, "[[", 1),
         utm_N = sapply(geometry, "[[", 2)) #%>%
  #st_transform(.,crs = 4326) %>% 
  #mutate(lon = sapply(geometry, "[[", 1),
  #       lat = sapply(geometry, "[[", 2))
  
iowa <- states(cb = T) %>% 
  filter(STATEFP == "19") %>% 
  select(STATEFP,geometry) %>% 
  st_transform(.,crs = 32610)

df_intersect <- st_intersection(iowa,df) # no intersection

iowa$geometry # shows geometry in UTM (northing & easting) format
df$geometry # still shows geometry in lat/long format

ggplot() +
  geom_sf(data = iowa) + 
  geom_point(data = df_intersect, 
             aes(x = utm_E,
                 y = utm_N),
             size = .25) 
