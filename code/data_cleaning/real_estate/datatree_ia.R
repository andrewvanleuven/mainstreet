suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

county_fips <- counties(state = "19", cb = T) %>% 
  rename(cty_fips = GEOID) %>% 
  select(cty_fips) %>% 
  arrange(cty_fips) %>% 
  st_drop_geometry() %>% pull()

ia001 <- read_delim(paste0("hidden/datatree/Prop",county_fips[1],".txt.zip"), delim = "|")

