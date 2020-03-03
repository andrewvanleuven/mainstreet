suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in data ------------------------------------------------------------
df <- read_fst("hidden/datatree/datatree_04.fst") %>% as_tibble()
msp <- read_csv("data/csv/universe/msp_universe.csv") %>% filter(st == "Iowa")

iowa_sf <- st_read("data/shp/google_earth/Iowa.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_remove_all(name, ", IA")) %>% st_transform(.,crs = 2794)  

iowa_towns <- places(state = "19", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(msp %>% select(1:3), by = "city_fips") %>% 
  st_transform(2794)

iowa_downtowns <- st_intersection(iowa_towns,(iowa_sf %>% select(-name))) %>% 
  select(name,geometry) 

iowa <- df %>% filter(situs_state == "IA") %>% 
  st_as_sf(., coords = c("situs_longitude","situs_latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 2794) %>% 
  st_centroid_xy()

webster_sf <- iowa_downtowns %>% filter(name == "Webster City") %>% 
  st_buffer(., 200, joinStyle = "MITRE", endCapStyle = "SQUARE")
webst_town <- iowa_towns %>% filter(name == "Webster City")
webst_dots <- iowa %>% filter(city_fips == 1983145) %>% 
  mutate(land_use_code = as.numeric(land_use_code)) %>% 
  filter(current_sales_price > 0,
         land_use_code > 999 & land_use_code < 1009,
         bedrooms > 0, year_built > 1700, building_area > 150,
         bath_total_calc > 0, building_condition_code != 0,
         lot_size_sq_ft > 0, stories_nbr_code != 0) %>% 
  select(1:11,current_sales_price,year_built,bedrooms,bath_total_calc,building_area,
         lot_size_sq_ft,stories_nbr_code,building_condition_code,x,y,geometry)
webst_dots_commercial <- iowa %>% filter(city_fips == 1983145) %>% 
  mutate(land_use_code = as.numeric(land_use_code)) %>% 
  filter(!is.na(current_sales_price) | !is.na(prev_sales_price),
         land_use_code > 1999 & land_use_code < 5000)
webst_road <- roads("IA", "Hamilton") %>% 
  st_transform(.,crs = 2794) %>% 
  st_intersection(.,webst_town)
webst_h2o <- area_water("IA", "Hamilton") %>% 
  st_transform(.,crs = 2794) %>% 
  st_intersection(.,webst_town) 
# Maps --------------------------------------------------------------------
ggplot() + 
  geom_sf(data = webst_town) +
  geom_sf(data = webster_sf, fill = "yellow", color = NA) +
  geom_sf(data = webst_h2o, fill = "cyan", color = "blue") +
  geom_sf(data = webst_road) +
  geom_point(data = webst_dots, aes(x,y), color = "black", size = .15) +
  geom_point(data = webst_dots_commercial, aes(x,y), color = "red", size = .25) +
  theme_void()

years <- df %>% select(1,current_sale_recording_date,prev_sale_recording_date) %>% 
  mutate(current_sale_yr = str_sub(current_sale_recording_date,end = 4))

freqTab(years,"current_sale_yr")

hedonic_webster <- lm(current_sales_price ~ ., data = webst_dots %>% select(-(1:11),-x,-y) %>% st_drop_geometry())
