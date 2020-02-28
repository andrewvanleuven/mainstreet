suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(tidycensus)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
trim_census <- dget("code/functions/trim_census.R")

# Get Data ----------------------------------------------------------------

xw <- read_csv("data/csv/universe/xw.csv") %>% 
  mutate(st = toupper(st)) %>% 
  filter(st %in% c("IA","MI","OH","WI")) %>% 
  select(1,2,7)
rucc <- read_csv("data/csv/universe/rucc.csv") %>% 
  select(cty_fips,rucc)
cz <- read_csv("data/csv/universe/cz.csv") %>% 
  rename(cz = cz2000) %>% 
  select(cty_fips,cz)
historical <- read_csv("data/csv/universe/historical.csv") %>% 
  select(-pop_2010)

# Create Universe ---------------------------------------------------------

universe <- get_decennial(geography = "place", 
                        state = c("IA","MI","OH","WI"),  # Four states I focus on
                        variables = "P001001", 
                        year = 2010) %>%
  separate(NAME,c("name","ST"),sep = ",") %>%
  rename_all(tolower) %>% 
  trim_census() %>% 
  mutate(city_fips = as.numeric(geoid)) %>% 
  select(-variable,-geoid) %>% 
  rename(pop_2010 = value) %>% 
  left_join(xw, by = "city_fips") %>%     # Attaches county to city/place ID
  left_join(rucc, by = "cty_fips") %>%     # Attaches rural-urban continuum code
  left_join(cz, by = "cty_fips") %>%     # Attaches commute zone ID
  filter(rucc > 2 & afact > 0.5,    # Omits cities from the two MOST URBAN counties
         pop_2010 > 750 & pop_2010 < 75000) %>%     # Current population filter
  select(city_fips,name,st,cty_fips,cz,rucc,pop_2010,-afact) %>% 
  inner_join(historical, by = c("name","st")) %>% 
  filter(pop_1940 > 999 | pop_1930 > 999 | pop_1920 > 999) %>%     # Historical population filter
  select(1:7) %>% 
  distinct() %>% 
  arrange(city_fips) %>% 
  write_csv("data/csv/universe/universe.csv")
  
