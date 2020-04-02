suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(beepr)
  library(crsuggest)
  library(janitor)
  library(units)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

df <- read_fst("hidden/datatree/cleaned/datatree_oh03.fst") %>% 
  st_as_sf(., coords = c("situs_longitude","situs_latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 2834)
beep()
msp <- read_csv("data/csv/universe/oh_universe.csv") %>% filter(st == "Ohio")
towns <- msp %>% pull(name)
town_fips <- msp %>% #filter(!name %in% c("Portsmouth")) %>%
  pull(city_fips) 
downtown <- st_read("data/shp/google_earth/ohio.kml") %>% 
  st_transform(2834) %>% 
  rename_all(tolower) %>% 
  mutate(name = str_replace(name,"Waverly City","Waverly")) %>% 
  select(-description) %>% 
  st_buffer(.,200,joinStyle = "MITRE", endCapStyle = "SQUARE") %>% 
  filter(name %in% towns)
oh_places <- places(state = "39", cb = T) %>% 
  rename(city_fips = GEOID,
         name = NAME) %>% filter(city_fips %in% town_fips) %>% 
  select(city_fips,name) %>% st_transform(2834)

for (i in town_fips) {
  town <- oh_places %>% filter(city_fips == i)
  fat_town <- town %>% st_buffer(dist = 1500)
  i_name <- town %>% pull(name)
  muni <- oh_places %>% filter(city_fips == i) %>% pull(name) %>% str_to_lower() %>% 
    str_replace_all(" ", "_") %>% str_replace_all("'", "")%>% str_replace_all("\\.", "")
  sf_downtown <- downtown %>% filter(name == i_name) %>% st_buffer(dist = 50) 
  downtown_edge <- sf_downtown %>% st_cast(to = 'LINESTRING')
  town_dots <- df %>% 
    st_intersection(fat_town) %>% 
    mutate(distance_to = city_fips,
           raw_crow_dist = st_distance(.,downtown_edge),
           inside = if_else(st_intersects(.,sf_downtown, sparse = F),-1,1),
           dist_ft = (raw_crow_dist*inside) %>% 
             set_units(., ft) %>%
             as.vector() %>% 
             round(digits = 2)) %>% 
    select(property_id,dist_ft,distance_to) %>% 
    arrange(dist_ft) %>% 
    st_drop_geometry()
  name <- paste0("loop_",muni)
  assign(name,town_dots) 
  rm(i,town,muni,sf_downtown,downtown_edge,town_dots,name,i_name,fat_town)
}
beep()
distances <- mget(ls(pattern="loop")) %>% bind_rows() 

write_fst(distances,"hidden/datatree/cleaned/datatree_oh04.fst")

for (i in town_fips) {
  muni <- oh_places %>% filter(city_fips == i) %>% pull(name) %>% str_to_lower() %>% 
    str_replace_all(" ", "_") %>% str_replace_all("'", "")%>% str_replace_all("\\.", "")
  rm(list=paste0("loop_",muni))}
rm(muni,i) 


# Test Map ----------------------------------------------------------------
#oh_rds <- roads(state = "39", county = "Washington") %>% 
#  st_transform(2834) %>% st_intersection(.,town) %>% 
#  st_collection_extract(type = "LINESTRING")
#oh_h2o <- area_water(state = "39", county = "Washington") %>% 
#  st_transform(2834) %>% st_intersection(.,town) %>% 
#  st_collection_extract(type = "POLYGON")
ggplot() +
  geom_sf(data = town, fill = "#eaeaea", color = NA) +
  #geom_sf(data = downtown_edge, color = "black", size = 1) +
  #geom_sf(data = oh_h2o, color = NA, fill = "#9dd3fd") +
  #geom_sf(data = oh_rds, color = "black") +
  geom_sf(data = sf_downtown %>% st_buffer(dist = 100) %>% st_intersection(.,town), 
          fill = "#FFCD00", color = NA, alpha = .75) +
  #geom_sf(data = town_dots, size = .3, color = "#BB0000") +
  theme_void()
