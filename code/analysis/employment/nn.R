suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(beepr)
  library(rleuven)
  library(nngeo)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in data ------------------------------------------------------------
msp <- read_csv("data/csv/universe/msp_universe.csv")
downtown_ia <- st_read("data/shp/google_earth/iowa.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_remove_all(name, ", IA"), st = "Iowa")
downtown_mi <- st_read("data/shp/google_earth/michigan.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(st = "Michigan")
downtown_oh <- st_read("data/shp/google_earth/ohio.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_replace(name, "Waverly City","Waverly")) %>% 
  mutate(st = "Ohio")
downtown_wi <- st_read("data/shp/google_earth/wisconsin.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(st = "Wisconsin")
downtown_sf <- rbind(downtown_ia,downtown_mi,downtown_oh,downtown_wi) %>% 
  inner_join(msp, by = c('name','st')) %>% 
  select(city_fips,everything()) %>% 
  arrange(city_fips) %>% 
  st_transform(crs = 3174)
rm(downtown_ia,downtown_mi,downtown_oh,downtown_wi,msp)

# Calculate Nearest Neighbors ---------------------------------------------
treatment <- downtown_sf %>% filter(msp == 1) %>% st_centroid() %>% select(city_fips,name,st,geometry)
control <- downtown_sf %>% filter(msp == 0) %>% st_centroid() %>% select(city_fips,name,st,geometry)

# Find matches ------------------------------------------------------------
matches_mtx <- st_nn(treatment, control, k = 10, maxdist = 120000, returnDist = T)  # 75 miles
big_matches_mtx <- st_nn(treatment, control, k = 100, maxdist = 500000, returnDist = T)  # 75 miles
matches <- st_join(treatment, control, st_nn, k = 10, maxdist = 120000) %>%
  mutate(distance = round((unlist(matches_mtx[["dist"]]))/1609.34,1)) %>% 
  st_drop_geometry() %>% 
  rename(city_fips = city_fips.x,
         name = name.x,
         st = st.x,
         match_city_fips = city_fips.y,
         match_name = name.y,
         match_st = st.y) 

big_matches <- st_join(treatment, control, st_nn, k = 100, maxdist = 500000) %>%
  mutate(distance = round((unlist(big_matches_mtx[["dist"]]))/1609.34,1)) %>% 
  st_drop_geometry() %>% 
  rename(city_fips = city_fips.x,
         name = name.x,
         st = st.x,
         match_city_fips = city_fips.y,
         match_name = name.y,
         match_st = st.y) %>% 
  write_csv("data/csv/universe/all_nn_matches.csv")

list_of_matches <- matches %>% 
  filter(distance >= 5) %>% 
  group_by(match_name) %>% 
  slice(which.min(distance)) %>% 
  arrange(city_fips) %>% 
  write_csv("data/csv/universe/msp_nn_matches.csv")

matches_mtx0 <- st_nn(treatment, control, k = 10, maxdist = 80000, returnDist = T)  # 50 miles 
matches0 <- st_join(treatment, control, st_nn, k = 10, maxdist = 80000) %>%
  mutate(distance = round((unlist(matches_mtx0[["dist"]]))/1609.34,1)) %>% 
  st_drop_geometry() %>% 
  rename(city_fips = city_fips.x,
         name = name.x,
         st = st.x,
         match_city_fips = city_fips.y,
         match_name = name.y,
         match_st = st.y) 

list_of_matches0 <- matches0 %>% 
  filter(distance >= 5) %>% 
  group_by(match_name) %>% 
  slice(which.min(distance)) %>% 
  arrange(city_fips) %>% 
  write_csv("data/csv/universe/msp_nn_matches_50.csv")

#matches_mtx <- matches_mtx[["dist"]]
#matches_mtx <- do.call(rbind.data.frame, matches_mtx)
#names(matches_mtx) <- paste0("match_",1:10)
#rownames(matches_mtx) <- treatment$name
#match_dist <- tibble::rownames_to_column(matches_mtx/1609.34 , "treated_name") %>% 
#  mutate_if(is.numeric, round, 1) 

#dist_matrix <- st_distance(treatment, control)
#dist_matrix <- data.frame((dist_matrix/1609.34))
#names(dist_matrix) <- control$name
#rownames(dist_matrix) <- treatment$name
#dist <- tibble::rownames_to_column(dist_matrix, "treated_name") %>% 
#  mutate_if(is.numeric, round, 1) %>% 
#  units::drop_units() %>% 
#  arrange(treated_name)