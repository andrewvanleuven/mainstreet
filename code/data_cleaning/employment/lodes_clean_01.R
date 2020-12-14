library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

dfxw <- read_csv("defunct/lodes/ia_xwalk.csv") %>% 
  janitor::clean_names() %>% select(1:6,15,16) %>% 
  rename(blk_fips = 1)
df02 <- read_csv("defunct/lodes/ia02.csv") %>% 
  janitor::clean_names() %>% select(1,2,15) %>% 
  rename(blk_fips = 1) 
df03 <- read_csv("defunct/lodes/ia03.csv") %>% 
  janitor::clean_names() %>% select(1,2,15) %>% 
  rename(blk_fips = 1) %>% 
  left_join(dfxw, by = "blk_fips")

ia_blks <- blocks("IA") %>% 
  select(GEOID10,geometry) %>% 
  st_transform(crs = 3174)
msp <- read_csv("data/csv/universe/analytical_universe.csv")
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
  arrange(city_fips) %>% 
  st_transform(crs = 3174)
rm(downtown_ia,downtown_mi,downtown_oh,downtown_wi)

cbd <- st_buffer(downtown_sf, 200, joinStyle = "MITRE", endCapStyle = "SQUARE")

ia_cbd <- st_intersection(ia_blks,cbd)

test <- ia_cbd %>% st_drop_geometry() %>% 
  rename(blk_fips = GEOID10) %>% 
  mutate(blk_fips = as.numeric(blk_fips)) %>% 
  left_join(df02, by = "blk_fips") %>% 
  replace_na(replace = list('c000' = 0, 'cns07' = 0)) %>% 
  group_by(city_fips) %>% 
  summarise(jobs = sum(c000),
            retail_jobs = sum(cns07)) %>% 
  inner_join(msp, by = "city_fips") %>% 
  select(1,4:9,2,3) %>% 
  write_csv("defunct/lodes/ia_02_jobs.csv")
