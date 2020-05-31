library(tidyverse)
library(tidycensus)
library(sf)
library(smoothr)
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
greys <- brewer.pal(5,"Greys")
msp <- read_csv("data/csv/universe/msp_universe.csv") %>% filter(st == "Ohio")
line <- c(39.550008, -83.454984,39.523624, -83.416632)
polygon <- data.frame(lat = c(line[1],line[1],line[3],line[3],line[1]),
                      lon = c(line[2],line[4],line[4],line[2],line[2])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% st_transform(2834) %>% 
  st_buffer(.,-550,joinStyle = "MITRE", endCapStyle = "SQUARE")
bldg <- st_read("data/shp/bldgs/wch_bldgs.kml") %>% 
  st_transform(2834) %>% select(geometry) %>% mutate(id = 1:n()) %>% st_intersection(polygon)
town <- places("39", cb = T) %>% filter(NAME == "Washington Court House") %>% 
  st_transform(2834) %>% st_intersection(polygon)
cnty <- counties("39", cb = T) %>% st_transform(2834) %>% 
  filter(st_intersects(x = ., y = town, sparse = FALSE)) %>% pull(NAME)
cty_rds <- roads("39",cnty) %>% st_transform(2834) %>% st_intersection(polygon)
cty_h2o <- area_water("39",cnty) %>% st_transform(2834) %>% st_intersection(polygon)
ctyh2o <- linear_water("39",cnty) %>% st_transform(2834) %>% st_intersection(polygon)
smooth_h2o <- smooth(ctyh2o, method = "chaikin")
ohio <- states(cb = T) %>% filter(STUSPS == "OH") %>% st_transform(2834) %>% st_intersection(polygon)
downtown <- st_read("data/shp/google_earth/ohio.kml") %>% 
  st_transform(2834) %>% 
  rename_all(tolower) %>% select(-description) %>% 
  st_buffer(.,200,joinStyle = "MITRE", endCapStyle = "SQUARE") %>% 
  st_buffer(-25) %>% filter(name == town %>% pull(NAME))
colors <- c("Downtown District" = greys[2])
centroid <- st_centroid(downtown)
center <- st_centroid(downtown) %>% st_coordinates()
in_house <- bldg %>% filter(id == 3614) %>% st_centroid() %>% st_coordinates()
out_house <- bldg %>% filter(id == 5022) %>% st_centroid() %>% st_coordinates()
line_in = st_sfc(st_linestring(rbind(center,in_house))) %>% st_as_sf(crs = 2834) %>%  # 275 m
  mutate(lngth = round(as.numeric(st_length(.))*3.281,digits = 0),
         length = paste(lngth,"feet",sep = " "))
line_out = st_sfc(st_linestring(rbind(center,out_house))) %>% st_as_sf(crs = 2834) %>%  # 960 m
  mutate(lngth = round(as.numeric(st_length(.))*3.281,digits = 0),
         length = paste(lngth,"feet",sep = " "))
lines <- rbind(line_in,line_out) %>% rename(geometry = x) %>% select(-lngth)
houses <- rbind(bldg %>% filter(id == 3614) %>% st_centroid(),
                bldg %>% filter(id == 5022) %>% st_centroid()) %>% 
  cbind(lines %>% st_drop_geometry())

ggplot() + 
  geom_sf(data = ohio, fill = greys[1], color = NA) + 
  geom_sf(data = town, fill = "white", color = NA) + 
  geom_sf(data = downtown, aes(fill = 'Downtown District'), color = NA, alpha = 1) +
  geom_sf(data = smooth_h2o, color =  greys[4], size = 3, fill = greys[4], alpha = .75) +
  geom_sf(data = cty_rds, color = greys[3], size = .5) +
  geom_sf(data = cty_rds %>% filter(RTTYP %in% c('U','I')), color = greys[3], size = 1) +
  geom_sf(data = bldg, fill = "black", color = NA, alpha = .4) +
  geom_sf(data = lines, size = 1.5, color = "black") +
  #geom_sf(data = lines, size = 1, color = "black") +
  geom_sf(data = bldg %>% filter(id == 3614) %>% st_buffer(3), fill = "red", color = NA) +
  geom_sf(data = bldg %>% filter(id == 5022) %>% st_buffer(3), fill = "blue", color = NA) +
  geom_sf(data = centroid, size = 3, shape = 15) +
  ggrepel::geom_label_repel(
    data = houses %>% slice(1),
    aes(label = length, geometry = geometry),
    point.padding = .65,
    min.segment.length = Inf,
    family = "LM Roman 10",
    fontface = "bold",
    stat = "sf_coordinates"
  ) +
  ggrepel::geom_label_repel(
    data = houses %>% slice(2),
    aes(label = length, geometry = geometry),
    point.padding = .55,
    min.segment.length = Inf,
    family = "LM Roman 10",
    fontface = "bold",
    stat = "sf_coordinates"
  ) +
  theme_void() +
  scale_fill_manual(name = "",
                    values = colors,
                    guide = guide_legend(override.aes = list(shape = 19, size = 8))) + 
  theme(legend.title = element_text(size=16, face="bold", hjust = 0.5),
        legend.text = element_text(size=16),
        legend.key = element_rect(fill = NA, color = NA),
        legend.position = "bottom", #c(0.85, 0.17),
        text=element_text(family="LM Roman 10")) + 
  coord_sf(crs = 4326) +
  ggsave("plot/maps/property_dist.png", height = 8, width = 12)
