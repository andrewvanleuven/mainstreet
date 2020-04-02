suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(janitor)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

df <- read_fst("hidden/datatree/cleaned/datatree_oh02.fst") %>% as_tibble()

# Filtering ---------------------------------------------------------------
feasible <- df %>% filter(
  property_class_id == "R",                  # residential
  is.na(owner1corp_ind),                     # not a corporation
  current_sales_price > 1000,                # not sold for pennies
  year_built > 1776,                         # has year built
  bedrooms > 0,                              # has bedrooms
  lot_size_sq_ft > 0,                        # has lot size
  building_area > 0,                         # has sqft
  bath_total_calc > 0,                       # has bathrooms
  situs_geo_status_code %in% c('A','B'),     # geocoded correctly
  #stories_nbr_code > 0,                     # has number of stories
  situs_state == "OH"                        # in ohio
) 
write_fst(feasible,"hidden/datatree/cleaned/datatree_oh03.fst")
beepr::beep()
# Load map components -----------------------------------------------------
dots <- feasible %>% 
  st_as_sf(., coords = c("situs_longitude","situs_latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 2834) %>% 
  st_centroid_xy()
oh_rds <- map(counties("39", cb = T) %>% pull(COUNTYFP), ~{
  roads(state = "39", county = .x)}) %>%
  rbind_tigris()
oh_h2o <- map(counties("39", cb = T) %>% pull(COUNTYFP), ~{
  area_water(state = "39", county = .x)}) %>%
  rbind_tigris()
oh_ctys <- counties("39", cb = T) %>% 
  st_transform(2834)
town_name <- read_csv("data/csv/universe/oh_universe.csv") %>% sample_n(1) %>% pull(name) #random town in 215 municipality sample
town <- places(state = "39", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID),
         name = NAME) %>% 
  select(city_fips,name,geometry) %>% 
  filter(name == town_name) %>% 
  st_transform(2834)
xybin <- st_bbox(town)
xybox <- st_as_sfc(st_bbox(town), crs = 4326) %>% 
  st_transform(2834)
cnty <- oh_ctys %>%
  filter(st_intersects(x = ., y = town, sparse = FALSE)) %>% 
  pull(NAME)
rds <- roads("39",cnty) %>% st_transform(.,crs = 2834) %>% 
  #st_intersection(.,xybox) %>% 
  st_collection_extract(type = "LINESTRING")
h2o <- area_water("39",cnty) %>% st_transform(.,crs = 2834) %>% 
  st_intersection(.,xybox) %>% 
  st_collection_extract(type = "POLYGON")
pnts <- st_intersection(dots,town)
downtown <- st_read("data/shp/google_earth/ohio.kml") %>% 
  st_transform(2834) %>% 
  rename_all(tolower) %>% 
  select(-description) %>% 
  st_buffer(.,200,joinStyle = "MITRE", endCapStyle = "SQUARE") %>% 
  st_intersection(.,town)

# Plot the map ------------------------------------------------------------
ggplot() +
  geom_sf(data = town, fill = "#eaeaea", color = NA) +
  geom_sf(data = downtown,color = NA, fill = "#fef6dd", alpha = 1) +
  geom_sf(data = rds %>% filter(RTTYP != "U"), color = "black", size = 1) +
  geom_sf(data = rds %>% filter(RTTYP != "U"), color = "white", size = .8) +
  geom_sf(data = rds %>% filter(RTTYP == "U"), color = "#f8cf70", size = 1.2) +
  geom_sf(data = rds %>% filter(RTTYP == "U"), color = "#f7e9a5", size = .8) +
  geom_sf(data = h2o, color = NA, fill = "#9dd3fd") +
  geom_point(data = pnts, aes(x,y), size = .5, shape = 20,
             #stroke = .2, shape = 21, fill = "#4D1979",
             color = "#4D1979") +
  geom_sf(data = town, fill = "#eaeaea", color = "#eaa39e", alpha = 0) +
  theme_void() +
  labs(title = sprintf("Residential Property Parcels in %s, OH",town_name),
       subtitle = "Filtered Prior to Hedonic Price Model",
       caption = "Source: First American DataTree") +
  theme(plot.caption = element_text(size = 16),
        text=element_text(family="Futura Medium"),
        plot.title = element_text(hjust = 0.5, family="Futura Bold", size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        panel.background = element_rect(fill = "#d7d5d1")) +
  coord_sf(xlim = c(xybin[1]-750,xybin[3]+750), 
           ylim = c(xybin[2]-150,xybin[4]+150),
           expand = FALSE)  +
  ggsave("plot/maps/ohio_feasible.png", width = 12, height = 8)

beepr::beep()
