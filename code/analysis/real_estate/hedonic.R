library(tidyverse)
library(rleuven)
library(lubridate)
library(fredr)
library(stargazer)

# Read in data ------------------------------------------------------------
msp <- read_csv("data/csv/universe/msp_universe.csv") %>% 
  filter(st == "Ohio") %>% select(-pop_2010) 
df <- read_csv("hidden/datatree/cleaned/datatree_clean.csv") %>% 
  left_join(msp, by = c("distance_to" = "city_fips")) %>% 
  select(1:4,name:cty_seat,everything())
neighborhood_df <- read_csv("hidden/datatree/cleaned/datatree_oh06.csv")
cpi <- fredr(series_id = "CPIAUCSL", # maybe try CPIHOSSL --- Consumer Price Index for All Urban Consumers: Housing in U.S. City Average
             observation_start = as.Date("2000-01-01"),
             observation_end = as.Date("2019-12-31")) %>% 
  mutate(inflator = 258.444/value) %>% 
  rename(rdate = date) %>% select(rdate,inflator)

# Final data cleaning -----------------------------------------------------
df_clean_0 <- df %>% 
  inner_join(neighborhood_df, by = "property_id") %>% 
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
         deck = deck_ind,
         distance = dist_ft,
         state = situs_state,
         zip = situs_zip5,
         sale_date = current_sale_contract_date,
         baths = bath_total_calc,
         recent_construction = effective_year_built,
         sqft = building_area,
         lot_size = lot_size_sq_ft)

df_clean <- df_clean_0 %>% 
  filter(between(sale_date, as.Date("2000-01-01"), as.Date("2019-12-31"))) %>%        # filter date...only transactions since 2000
  mutate(d2000 = if_else(between(sale_date, as.Date("2000-01-01"), as.Date("2009-12-31")),1,0),
         d2010 = if_else(between(sale_date, as.Date("2010-01-01"), as.Date("2019-12-31")),1,0),
         nbhood_pop = ((d2000*pop_2000)+(d2010*pop_2010)),
         nbhood_age = ((d2000*median_age_2000)+(d2010*median_age_2010)),
         nbhood_nonwhite = ((d2000*pct_nonwhite_2000)+(d2010*pct_nonwhite_2010)),
         nbhood_renting = ((d2000*pct_renting_2000)+(d2010*pct_renting_2010)),
         nbhood_bachelors = ((d2000*pct_bachelors_2000)+(d2010*pct_bachelors_2010)),
         nbhood_unempl = ((d2000*civ_unempl_2000)+(d2010*civ_unempl_2010)),
         nbhood_income = ((d2000*median_income_2000)+(d2010*median_income_2010)),
         msp_yr = (ymd(sprintf("%d-01-01",msp_yr))),
         msp_at_sale = if_else(sale_date >= msp_yr,1,0),
         rdate = round_date(sale_date, "month"),
         msp_at_sale = replace_na(msp_at_sale, 0),
         age = year(sale_date)-year_built,
         construction_yn = if_else(recent_construction == year_built,0,1),
         construction_before_sale = if_else(recent_construction < year(sale_date),1,1),
         yrs_since_construction = (year(sale_date)-recent_construction)*construction_yn*construction_before_sale,
         recent_remodel = if_else(between(yrs_since_construction,1,10),1,0)) %>% 
  filter(!is.na(city_name)) %>% 
  left_join(.,cpi, by = "rdate") %>% 
  mutate(inflator = replace_na(inflator, 1),
         real_sale_price = round(sale_price*inflator,digits = 0)) %>% #arrange(real_sale_price) 
  select(property_id,msp_at_sale,msp_accr,msp_affl,distance,in_downtown,              # unique ID, MSP, distance
         address,city_name,st,cz,rucc,cty_seat,                                       # location
         sqft,lot_size,age,recent_remodel,bedrooms,total_rooms,                       # structural
         baths,deck,garage,pool,brick,stories,poor_condition,basement,                # structural
         sale_date,sale_price,real_sale_price,nbhood_pop:nbhood_income) %>%           # transaction, neighborhood
  write_csv("hidden/datatree/cleaned/datatree_model.csv")

rm(neighborhood_df,msp,df_clean_0,cpi,df)

# Hedonic price model -----------------------------------------------------
modeldf <- df_clean
#modeldf_nonmetro <- df_clean %>% filter(rucc>3) 

names(modeldf)

model_0 <- lm(log(sale_price) ~ log(lot_size) + log(sqft) + age + bedrooms + baths + stories + basement + pool,
              data = modeldf)

model_1 <- lm(log(real_sale_price) ~ log(lot_size) + log(sqft) + age + bedrooms + baths + stories + basement + pool,
              data = modeldf)

model_2 <- lm(log(real_sale_price) ~ log(lot_size) + log(sqft) + age + bedrooms + baths + stories + basement + pool +
              nbhood_age + nbhood_nonwhite + nbhood_renting + nbhood_bachelors + nbhood_unempl + nbhood_income,
              data = modeldf)

model_3 <- lm(log(real_sale_price) ~ log(lot_size) + log(sqft) + age + bedrooms + baths + stories + basement + pool +
              #nbhood_age + nbhood_nonwhite + nbhood_renting + nbhood_bachelors + nbhood_unempl + nbhood_income +
              distance + in_downtown,
              data = modeldf)

model_3.1 <- lm(log(real_sale_price) ~ log(lot_size) + log(sqft) + age + bedrooms + baths + stories + basement + pool +
              nbhood_age + nbhood_nonwhite + nbhood_renting + nbhood_bachelors + nbhood_unempl + nbhood_income +
              distance + in_downtown,
              data = modeldf)

model_4 <- lm(log(real_sale_price) ~ log(lot_size) + log(sqft) + age + bedrooms + baths + stories + basement + pool +
              #nbhood_age + nbhood_nonwhite + nbhood_renting + nbhood_bachelors + nbhood_unempl + nbhood_income +
              distance + in_downtown + msp_at_sale,
              data = modeldf)

model_4.1 <- lm(log(real_sale_price) ~ log(lot_size) + log(sqft) + age + bedrooms + baths + stories + basement + pool +
              nbhood_age + nbhood_nonwhite + nbhood_renting + nbhood_bachelors + nbhood_unempl + nbhood_income +
              distance + in_downtown + msp_at_sale,
              data = modeldf)


stargazer(model_0,model_1, out = "results/real_estate/hedonic.html", digits = 2)
