library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(leaflet)
library(mapview)
library(mapedit)
library(RColorBrewer)
library(rleuven)
library(extrafont) 
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)
msp <- read_csv("data/csv/universe/msp_universe.csv") %>% filter(st == "Ohio")
line <- c(40.478878, -81.427500, 40.516479, -81.493614)
polygon <- data.frame(lat = c(line[1],line[1],line[3],line[3],line[1]),
                      lon = c(line[2],line[4],line[4],line[2],line[2])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% st_transform(2834)
bldg <- st_read("data/shp/bldgs/newphilly_bldgs.kml") %>% 
  st_transform(2834) %>% select(geometry) %>% mutate(id = 1:n())
town <- places("39", cb = T) %>% filter(NAME == "New Philadelphia") %>% 
  st_transform(2834) %>% st_intersection(polygon)
cnty <- counties("39", cb = T) %>% st_transform(2834) %>% 
  filter(st_intersects(x = ., y = town, sparse = FALSE)) %>% pull(NAME)
cty_rds <- roads("39",cnty) %>% st_transform(2834) %>% st_intersection(polygon)
cty_h2o <- area_water("39",cnty) %>% st_transform(2834) %>% st_intersection(polygon)
ctyh2o <- linear_water("39",cnty) %>% st_transform(2834) %>% st_intersection(polygon)
ohio <- states(cb = T) %>% filter(STUSPS == "OH") %>% st_transform(2834) %>% st_intersection(polygon)
downtown <- st_read("data/shp/google_earth/ohio.kml") %>% 
  st_transform(2834) %>% 
  rename_all(tolower) %>% select(-description) %>% 
  st_buffer(.,200,joinStyle = "MITRE", endCapStyle = "SQUARE") %>% 
  filter(name == town %>% pull(NAME))
inner_circle <- st_buffer(downtown, dist = 1600)
outer_circle <- st_buffer(downtown, dist = 2400)
inner_ring <- st_difference(inner_circle,downtown) %>% st_intersection(polygon)
outer_ring <- st_difference(outer_circle,inner_circle) %>% st_intersection(polygon)
greys <- brewer.pal(5,"Greys")
colors <- c("Downtown District" = greys[4], "1 Mile Radius" = greys[3], "1.5 Mile Radius" = greys[2])

ggplot() + 
  geom_sf(data = ohio, fill = greys[1], color = NA) + 
  geom_sf(data = town, fill = "white", color = NA) + 
  geom_sf(data = downtown, aes(fill = 'Downtown District'), color = NA, alpha = .9) +
  geom_sf(data = outer_ring, aes(fill = '1.5 Mile Radius'), color = NA, alpha = .85) +
  geom_sf(data = inner_ring, aes(fill = '1 Mile Radius'), color = NA, alpha = .8) +
  geom_sf(data = ctyh2o, color =  greys[4], size = 3, fill = greys[4], alpha = .75) +
  geom_sf(data = cty_h2o, color = greys[4], size = 0, fill = greys[4], alpha = .75) +
  geom_sf(data = cty_rds, color = "black", size = .5) +
  geom_sf(data = cty_rds %>% filter(RTTYP %in% c('U','I')), color = "black", size = 1.35) +
  geom_sf(data = bldg, fill = "black", color = NA) +
  theme_void() +
  scale_fill_manual(name = "",
                    breaks = c('Downtown District','1 Mile Radius','1.5 Mile Radius'),
                    values = colors,
                    guide = guide_legend(override.aes = list(shape = 19, size = 8))) + 
  theme(legend.title = element_text(size=16, face="bold", hjust = 0.5),
        legend.text = element_text(size=16),
        legend.key = element_rect(fill = NA, color = NA),
        legend.position = "bottom", #c(0.85, 0.17),
        text=element_text(family="LM Roman 10")) + 
  coord_sf(crs = 4326) +
  ggsave("plot/maps/ring_buffer.png", height = 8, width = 12)

