suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

msp <- read_csv("data/csv/universe/msp_universe.csv")

# Iowa --------------------------------------------------------------------
ia_ctys <- counties("19", cb = T) %>% 
  st_transform(2794)
#suggest_crs(ia_ctys)[1,3]

iowa_sf <- places(state = "19", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(msp %>% select(1), by = "city_fips") %>% 
  st_transform(2794)

iowa <- read_csv("hidden/datatree/cleaned/datatree_ia02.csv") 

iowa_dots <- iowa %>% 
  filter(!is.na(situs_latitude) & !is.na(situs_longitude),
         situs_latitude != 0 & situs_longitude != 0) %>% 
  distinct() %>% 
  st_as_sf(., coords = c("situs_longitude","situs_latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 2794) %>% 
  st_centroid_xy() %>% 
  st_intersection(.,iowa_sf) %>% 
  st_drop_geometry() %>% select(-x,-y) %>% 
  write_fst("hidden/datatree/cleaned/datatree_ia03.fst")
beepr::beep()

rm(ia_ctys,iowa_sf,iowa,iowa_dots)

# Ohio --------------------------------------------------------------------
oh_ctys <- counties("39", cb = T) %>% 
  st_transform(2834)
#suggest_crs(oh_ctys)[1,3]

ohio_sf <- places(state = "39", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(msp %>% select(1), by = "city_fips") %>% 
  st_transform(2834)

ohio <- read_csv("hidden/datatree/cleaned/datatree_oh02.csv") 

ohio_dots <- ohio %>% 
  filter(!is.na(situs_latitude) & !is.na(situs_longitude),
         situs_latitude != 0 & situs_longitude != 0) %>% 
  distinct() %>% 
  st_as_sf(., coords = c("situs_longitude","situs_latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 2834) %>% 
  st_centroid_xy() %>% 
  st_intersection(.,ohio_sf) %>% 
  st_drop_geometry() %>% select(-x,-y) %>% 
  write_fst("hidden/datatree/cleaned/datatree_oh03.fst")
beepr::beep()

rm(oh_ctys,ohio_sf,ohio,ohio_dots)

# Wisconsin ---------------------------------------------------------------
wi_ctys <- counties("55", cb = T) %>% 
  st_transform(3069)
#suggest_crs(wi_ctys)[1,3]

wisconsin_sf <- places(state = "55", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(msp %>% select(1), by = "city_fips") %>% 
  st_transform(3069)

wisconsin <- read_csv("hidden/datatree/cleaned/datatree_wi02.csv") 

wisconsin_dots <- wisconsin %>% 
  filter(!is.na(situs_latitude) & !is.na(situs_longitude),
         situs_latitude != 0 & situs_longitude != 0) %>% 
  distinct() %>% 
  st_as_sf(., coords = c("situs_longitude","situs_latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 3069) %>% 
  st_centroid_xy() %>% 
  st_intersection(.,wisconsin_sf) 

wisconsin_dots %>% st_drop_geometry() %>% select(-x,-y) %>% 
  write_fst("hidden/datatree/cleaned/datatree_wi03.fst")
str(wisconsin_dots)
beepr::beep()
