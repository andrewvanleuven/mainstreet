suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(tidycensus)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

df <- read_csv("data/csv/universe/universe.csv")

# Add Main Street Program details (year joined, accredited and/or affiliate membership status)
msp <- read_csv("data/csv/universe/msp.csv") %>% 
  select(city_fips,msp,msp_yr,msp_accr,msp_affl)
# Add county seat binary
seats <- read_csv("data/csv/universe/cty_seats.csv") %>% 
  mutate(city_fips = as.numeric(paste0(STATE_FIPS,FIPS55)),
         cty_seat = 1) %>%
  select(city_fips,cty_seat)
# Add CBSA FIPS code (helpful for distinguishing micropolitan areas)
cbsa <- read_csv("data/csv/universe/cbsa_xw.csv") %>% 
  select(1:2)

# Finish Building Universe ------------------------------------------------

msp_universe <- df %>% 
  left_join(msp, by = "city_fips") %>% 
  left_join(seats, by = "city_fips") %>% 
  left_join(cbsa, by = "cty_fips") %>% 
  mutate_at(vars(8:13), ~replace_na(., 0)) %>% 
  select(city_fips,name,st,cty_fips,cbsa_fips,everything()) %>% 
  write_csv("data/csv/universe/msp_universe.csv")