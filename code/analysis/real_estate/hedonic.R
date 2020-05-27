library(tidyverse)
library(rleuven)
library(lubridate)
library(stargazer)

# Read in data ------------------------------------------------------------
msp <- read_csv("data/csv/universe/oh_universe.csv") %>% filter(st == "Ohio")
df <- read_csv("hidden/datatree/cleaned/datatree_clean.csv") %>% 
  left_join(msp, by = c("distance_to" = "city_fips")) %>% 
  select(1:4,name:pop_2010,everything())

# Final data cleaning -----------------------------------------------------
df_clean <- df %>% 
  filter(owner_occupied == "Y",                                                       # removes any rentals or vacant properties
         current_sale_document_type %in% c(71,27,67,35,49,68,55,20,36,44,69,61,4,22), # removes foreclosures, death transfers, other rare deeds
         !is.na(current_sale_contract_date),                                          # removes any missing values for sale date
         !is.na(current_sale_document_type),                                          # removes any missing values for deed type
         !is.na(stories_nbr_code)                                                     # removes any missing values for number of stories
         ) %>% 
  mutate(basement = if_else(basement_code %in% c(1,2,3,4,6,7),1,0),                   # has basement binary
         pool = if_else(pool_code %in% c(1,2,4,5,6,7,8,9,10,11),1,0),                 # has pool binary
         garage = if_else(garage %in% c(1,2,3,4,5,7,10,17,19),1,0),                   # has garage binary
         brick = if_else(exterior_walls_code == 2,1,0),                               # has brick ext. walls binary
         poor_condition = if_else(building_condition_code %in% c(1,2,3,6),0,1),       # binary: if condition is "poor" or "unsound"
         stories = stories_nbr_code/100,                                              # transform stories variable
         dist_ft = dist_ft/660,                                                       # replace ft with 1/8 mile
         deck_ind = replace_na(deck_ind, 0),                                          # has deck, replace NA values
         brick = replace_na(brick, 0),                                                # brick, replace NA values
         current_sale_contract_date = lubridate::ymd(current_sale_contract_date)      # format date properly
         ) %>% 
  select(-(situs_latitude:owner1ownership_rights),-building_class_code,               # unneeded variables
         -site_influence_code,-basement_code,-pool_code,-style_code,
         -exterior_walls_code,-interior_walls_code,lot_size_sq_ft,
         -building_quality_code,-building_condition_code,
         -stories_nbr_code,-current_sales_price_code,
         -(prev_sale_contract_date:prev_sales_price_code),
         -current_sale_document_type,-sum_building_sq_ft) %>% 
  rename(sale_price = current_sales_price,                                            # give variables more intuitive names
         in_downtown = inside,
         city_fips = distance_to,
         city_name = name,
         address = situs_full_street_address,
         city = situs_city,
         distance = dist_ft,
         state = situs_state,
         zip = situs_zip5,
         sale_date = current_sale_contract_date,
         baths = bath_total_calc,
         recent_construction = effective_year_built,
         sqft = building_area,
         lot_size = lot_size_sq_ft)

# Hedonic price model -----------------------------------------------------
names(df)

modeldf <- df_clean %>% 
  filter(sale_date > as.Date("2014-12-31"))                                           # filter date...only transactions since 2015

model <- lm(log(sale_price) ~ distance*in_downtown + log(lot_size) + log(sqft) + year_built + bedrooms + baths + stories + basement + pool, 
            data = modeldf)

stargazer(model, out = "results/real_estate/base_model.html", digits = 1)