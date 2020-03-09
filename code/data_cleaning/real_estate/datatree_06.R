suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(janitor)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

df <- read_fst("hidden/datatree/datatree_04.fst") %>% as_tibble()
vars <- colnames(read_csv("code/data_cleaning/real_estate/variables/vars_06.csv") %>% clean_names())

all <- df %>% select(all_of(vars)) %>% 
  write_fst("hidden/datatree/datatree_06.fst")

nimble <- read_fst("hidden/datatree/datatree_06.fst") %>% as_tibble()

feasible <- nimble %>% filter(
  property_class_id == "R",                  # residential
  is.na(owner1corp_ind),                     # not a corporation
  current_sales_price > 1000,                # not sold for pennies
  year_built > 1776,                         # has year built
  bedrooms > 0,                              # has bedrooms
  lot_size_sq_ft > 0,                        # has lot size
  building_area > 0,                         # has sqft
  bath_total_calc > 0,                       # has bathrooms
  situs_geo_status_code %in% c('A','B'),     # geocoded correctly
  stories_nbr_code > 0,                      # has number of stories
  situs_state == "OH"                        # in ohio
)

write_csv(feasible,"hidden/datatree/ohio_feasible.csv")

#freqTab(feasible,"situs_city")
