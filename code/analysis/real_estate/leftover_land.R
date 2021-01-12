suppressMessages({
  library(tidyverse)
  library(sf)
  library(beepr)
  library(janitor)
  library(units)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

msp <- read_csv("data/csv/universe/oh_universe.csv") %>% filter(st == "Ohio")
towns <- msp %>% pull(name)
town_fips <- msp %>%
  pull(city_fips) 
downtown <- st_read("data/shp/google_earth/ohio.kml") %>% 
  st_transform(2834) %>% 
  rename_all(tolower) %>% 
  mutate(name = str_replace(name,"Waverly City","Waverly")) %>% 
  select(-description) %>% 
  st_buffer(.,200,joinStyle = "MITRE", endCapStyle = "SQUARE") %>% 
  filter(name %in% towns)
oh_places <- tigris::places(state = "39", cb = T) %>% 
  rename(city_fips = GEOID,
         name = NAME) %>% filter(city_fips %in% town_fips) %>% 
  select(city_fips,name) %>% st_transform(2834) %>% 
  arrange(name)


downtown_dots <- downtown %>% st_centroid() %>% 
  st_buffer(1609.34) %>% 
  arrange(name) %>% select(-name)
downtown_dots2 <- downtown %>% st_centroid() %>% 
  st_buffer(1609.34*1.5) %>% 
  arrange(name) %>% select(-name)
for (i in 1:213) {
  leftover <- st_difference(oh_places[i,],downtown_dots[i,])
  name <- paste0("loop_",i)
  assign(name,leftover) 
}
leftovers <- mget(ls(pattern="loop")) %>% bind_rows() 

for (i in 1:213) {
  leftover <- st_difference(oh_places[i,],downtown_dots2[i,])
  name <- paste0("knot_",i)
  assign(name,leftover) 
}
leftovers <- mget(ls(pattern="loop")) %>% bind_rows() 
leftovers2 <- mget(ls(pattern="knot")) %>% bind_rows() 


subtract <- st_difference(oh_places[1,],downtown_dots[1,])

plot(downtown_dots)

area <- oh_places %>% mutate(area = st_area(.)) %>% as_tibble() %>% 
  mutate(area = as.numeric(area*0.0000003861)) %>% 
  select(-geometry)
area_net <- leftovers %>% mutate(area_l1 = st_area(.)) %>% as_tibble() %>% 
  mutate(area_l1 = as.numeric(area_l1*0.0000003861)) %>% 
  select(-geometry,-name)
area_net2 <- leftovers2 %>% mutate(area_l2 = st_area(.)) %>% as_tibble() %>% 
  mutate(area_l2 = as.numeric(area_l2*0.0000003861)) %>% 
  select(-geometry,-name)

area_calc <- left_join(area,area_net, by = "city_fips") %>% 
  left_join(area_net2, by = "city_fips") %>% 
  mutate(remainder_1mi = round(area_l1/area,2),
         remainder_1_5mi = round(area_l2/area,2)) %>% 
  replace(is.na(.), 0) %>% 
  select(-4,-5)

mean(area_calc$remainder_1_5mi)

# 1 mile radius is 3.14 sqmi, 1.5 mile radius is 7.07 sqmi
166/213 # 78% of towns are smaller than a 1.5 mile radius 
121/213 # 57% of towns are smaller than a 1 mile radius
