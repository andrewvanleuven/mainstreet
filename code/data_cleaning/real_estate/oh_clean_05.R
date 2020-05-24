suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(beepr)
  library(crsuggest)
  library(janitor)
  library(units)
  library(googleway)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
#file.edit("~/.Renviron") # to copy/paste Google API key

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

test_fips <- c('3977588','3962848','3988084','3976778','3927048',
               '3972424','3955216','3981718','3921308','3932340')
for (i in test_fips) {
  town <- oh_places %>% filter(city_fips == i)
  fat_town <- town %>% st_buffer(dist = 1500)
  i_name <- town %>% pull(name)
  muni <- oh_places %>% filter(city_fips == i) %>% pull(name) %>% str_to_lower() %>% 
    str_replace_all(" ", "_") %>% str_replace_all("'", "")%>% str_replace_all("\\.", "")
  sf_downtown <- downtown %>% filter(name == i_name) %>% st_buffer(dist = 50) 
  downtown_edge <- sf_downtown %>% st_transform(4326) %>% st_centroid_xy()
  town_dots <- df %>% 
    st_intersection(fat_town) %>% 
    st_drop_geometry() %>% 
    select(property_id,city_fips,situs_longitude,situs_latitude)
  google_list <- data.frame()
  for (i in 1:nrow(town_dots)) {
    id <- town_dots$property_id[i]
    distance_to <- town_dots$city_fips[i]
    walk_dir <- google_directions(origin = c(town_dots$situs_latitude[i], town_dots$situs_longitude[i]),
                                  destination = c(downtown_edge$y, downtown_edge$x),
                                  mode = "walking")
    w_dist <- round(walk_dir$routes$legs[[1]][["distance"]][["value"]]*0.000621371,3)
    w_dura <- round(walk_dir$routes$legs[[1]][["duration"]][["value"]]/60,1)
    list <- data.frame(id,distance_to,w_dist,w_dura)
    google_list <- rbind(google_list,list)
  }
  name <- paste0("loop_",muni)
  assign(name,(google_list %>% distinct())) 
  }
beep()

google_dist <- mget(ls(pattern="loop")) %>% bind_rows() %>% distinct() %>% 
  write_csv("hidden/datatree/cleaned/datatree_piqua_troy.csv")
