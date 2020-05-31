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
         nbhood_pop_density = ((d2000*(pop_2000/bg_size_sqmi))+(d2010*(pop_2010/bg_size_sqmi))),
         nbhood_age = ((d2000*median_age_2000)+(d2010*median_age_2010)),
         nbhood_nonwhite = ((d2000*pct_nonwhite_2000)+(d2010*pct_nonwhite_2010)),
         nbhood_renting = ((d2000*pct_renting_2000)+(d2010*pct_renting_2010)),
         nbhood_bachelors = ((d2000*pct_bachelors_2000)+(d2010*pct_bachelors_2010)),
         nbhood_unempl = ((d2000*civ_unempl_2000)+(d2010*civ_unempl_2010)),
         nbhood_income = ((d2000*median_income_2000*1.496491)+(d2010*median_income_2010*1.187675)), # respective CPI-U inflators
         msp_lag_year1 = msp_yr+1,
         msp_lag_year2 = msp_yr+2,
         msp_lag_year3 = msp_yr+3,
         msp_lag_year5 = msp_yr+5,
         msp_yr = (ymd(sprintf("%d-01-01",msp_yr))),
         msp_lag_yr1 = (ymd(sprintf("%d-01-01",msp_lag_year1))),
         msp_lag_yr2 = (ymd(sprintf("%d-01-01",msp_lag_year2))),
         msp_lag_yr3 = (ymd(sprintf("%d-01-01",msp_lag_year3))),
         msp_lag_yr5 = (ymd(sprintf("%d-01-01",msp_lag_year5))),
         msp_at_sale = if_else(sale_date >= msp_yr,1,0),
         msp_at_sale = replace_na(msp_at_sale, 0),
         msp_lag1 = if_else(sale_date >= msp_lag_yr1,1,0),
         msp_lag2 = if_else(sale_date >= msp_lag_yr2,1,0),
         msp_lag3 = if_else(sale_date >= msp_lag_yr3,1,0),
         msp_lag5 = if_else(sale_date >= msp_lag_yr5,1,0),
         msp_lag1 = replace_na(msp_lag1, 0),
         msp_lag2 = replace_na(msp_lag2, 0),
         msp_lag3 = replace_na(msp_lag3, 0),
         msp_lag5 = replace_na(msp_lag5, 0),
         lot_size = lot_size/43560, # acres
         rdate = round_date(sale_date, "month"),
         age = year(sale_date)-year_built,
         age = if_else(age<0,0,age),
         construction_yn = if_else(recent_construction == year_built,0,1),
         construction_before_sale = if_else(recent_construction < year(sale_date),1,1),
         yrs_since_construction = (year(sale_date)-recent_construction)*construction_yn*construction_before_sale,
         recent_remodel = if_else(between(yrs_since_construction,1,10),1,0)) %>% 
  filter(!is.na(city_name)) %>% 
  left_join(.,cpi, by = "rdate") %>% 
  mutate(inflator = replace_na(inflator, 1),
         real_sale_price = round(sale_price*inflator,digits = 0)) %>% 
  left_join(.,(read_csv("data/csv/universe/msp_universe.csv") %>% filter(st == "Ohio") %>% 
                 select(city_fips,pop_2010) %>% rename(population = pop_2010)), by = "city_fips") %>% 
  select(property_id,msp_at_sale,msp_lag1,msp_lag2,msp_lag3,msp_lag5,msp_accr,msp_affl,    # unique ID, MSP, distance
         distance,in_downtown,address,city_name,st,city_fips,cz,rucc,cty_seat,population,  # distance, location
         sqft,lot_size,age,recent_remodel,bedrooms,total_rooms,                            # structural
         baths,deck,garage,pool,brick,stories,poor_condition,basement,                     # structural, cont.
         sale_date,sale_price,real_sale_price,nbhood_pop:nbhood_income) %>%                # transaction, neighborhood
  write_csv("hidden/datatree/cleaned/datatree_model.csv")

#rm(neighborhood_df,msp,df_clean_0,cpi,df)

# Summary Stats -----------------------------------------------------------
dfsum <- df_clean %>% 
  select(property_id,msp_at_sale,msp_lag1,msp_lag2,msp_lag3,msp_lag5,msp_accr,msp_affl,    
         distance,in_downtown,address,city_name,st,city_fips,cz,rucc,cty_seat,population,  
         sqft,lot_size,age,recent_remodel,bedrooms,baths,total_rooms,stories,                            
         deck,garage,pool,brick,poor_condition,basement,sale_date,sale_price,real_sale_price,nbhood_pop:nbhood_income) %>% 
  select(msp_at_sale,distance,in_downtown,sqft:nbhood_unempl) %>% 
  select(-recent_remodel,-brick,-poor_condition,-sale_price,-sale_date,-nbhood_pop,-nbhood_pop_density,-nbhood_renting) %>%
  filter(real_sale_price < 5000000,
         lot_size < 10,
         lot_size >= 0.001) %>% 
  as.data.frame()
stargazer(dfsum,
          font.size = "footnotesize",
          omit.summary.stat = c("p25", "p75", "n"),
          digits = 1,
          covariate.labels = c("MSP Adopted?","Downtown Distance","Propterty Located Downtown?",
                               "Square Footage","Lot Size (Acres)","Home Age","Bedrooms","Bathrooms",
                               "Total Rooms","Number of Stories","Has Deck","Has Garage",
                               "Has Pool","Has Basement","Real Sale Price","Neighborhood Median Age",
                               "Neighborhood Pct. Non-White","Neighborhood Pct. w/Bachlelors+",
                               "Neighborhood Unemployment Rate"),
          out = "results/real_estate/summary.html")


# Hedonic price model -----------------------------------------------------
modeldf <- df_clean %>% filter(real_sale_price < 5000000,
                               lot_size < 10,
                               lot_size >= 0.001)
clr_cons()
names(modeldf)

model_1 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown,
              data = modeldf)

model_2 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories,
              data = modeldf)

model_3 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + 
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl, 
              data = modeldf)

model_4 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + pool + basement + deck +
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl + 
                msp_lag2, 
              data = modeldf)

model_5 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + pool + basement + deck +
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl + 
                msp_lag2*log(distance), 
              data = modeldf)

model_6 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                msp_lag2*log(distance), 
              data = modeldf)

stargazer(
  model_1,model_2,model_3,model_4,model_5,#model_6,
  omit = c("Constant","pool","basement","deck"),
  omit.stat = c("f", "ser", "adj.rsq"),
  font.size = "footnotesize",#no.space = T,
  covariate.labels = c("Downtown Distance","Property Located Downtown?","Lot Size (log)","Square Footage (log)","Home Age",
                       "Total Rooms","Stories","Neighborhood Median Age","Neighborhood Pct. Non-White","Neighborhood Pct. w/Bachlelors+",
                       "Neighborhood Unemployment Rate","MSP Adopted?","Distance*MSP interaction"),
  digits = 2,
  dep.var.labels = c("Natural Log of Property Sale Price (real USD)"),
  add.lines = list(c("Additional Structure Characteristics?", "No","No","No","Yes","Yes")),
  out = "results/real_estate/hedonic.html"
  ) 

# Models focusing only on main effect -------------------------------------
clr_cons()
names(modeldf)

model_00 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + pool + basement +
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl + 
                msp_at_sale*log(distance), 
              data = modeldf)

model_01 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + pool + basement +
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl + 
                msp_lag1*log(distance), 
              data = modeldf)

model_02 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + pool + basement +
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl + 
                msp_lag2*log(distance), 
              data = modeldf)

model_03 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + pool + basement +
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl + 
                msp_lag3*log(distance), 
              data = modeldf)

model_04 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + pool + basement +
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl + 
                msp_lag5*log(distance), 
              data = modeldf)

model_05 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + pool + basement +
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl + 
                msp_lag2*log(distance), 
              data = (modeldf %>% filter(distance <= 12))) # only properties within 1.5 miles of downtown)

model_06 <- lm(log(real_sale_price) ~ 
                log(distance) + 
                in_downtown +
                log(lot_size) + 
                log(sqft) + 
                age +
                total_rooms +
                stories + pool + basement +
                nbhood_age +
                nbhood_nonwhite + 
                nbhood_bachelors + 
                nbhood_unempl + 
                msp_lag2*log(distance), 
              data = (modeldf %>% filter(distance <= 8))) # only properties within 1 mile of downtown)

stargazer(
  model_00,model_01,model_02,model_03,model_04,model_05,model_06,
  omit = c("Constant","lot_size","sqft","age","total_rooms","stories","pool","basement",
           "nbhood_age","nbhood_bachelors","nbhood_nonwhite","nbhood_unempl"),
  omit.stat = c("f", "ser", "adj.rsq"),
  digits = 2,
  dep.var.labels = c("Natural Log of Property Sale Price (real USD)"),
  add.lines = list(c("Structure Characteristics?", "Yes","Yes","Yes","Yes","Yes"),
                   c("Neighborhood Characteristics?", "Yes","Yes","Yes","Yes","Yes")),
  out = "results/real_estate/hedonic_main.html"
) 


# Tables Formatted for Paper ----------------------------------------------
names(model_00$coefficients)[names(model_00$coefficients) == "log(distance):msp_at_sale"] <- "interaction"
names(model_01$coefficients)[names(model_01$coefficients) == "log(distance):msp_lag1"] <- "interaction"
names(model_02$coefficients)[names(model_02$coefficients) == "log(distance):msp_lag2"] <- "interaction"
names(model_03$coefficients)[names(model_03$coefficients) == "log(distance):msp_lag3"] <- "interaction"
names(model_04$coefficients)[names(model_04$coefficients) == "log(distance):msp_lag5"] <- "interaction"
names(model_05$coefficients)[names(model_05$coefficients) == "log(distance):msp_lag2"] <- "interaction"
names(model_06$coefficients)[names(model_06$coefficients) == "log(distance):msp_lag2"] <- "interaction"
names(model_00$coefficients)[names(model_00$coefficients) == "msp_at_sale"] <- "msp"
names(model_01$coefficients)[names(model_01$coefficients) == "msp_lag1"] <- "msp"
names(model_02$coefficients)[names(model_02$coefficients) == "msp_lag2"] <- "msp"
names(model_03$coefficients)[names(model_03$coefficients) == "msp_lag3"] <- "msp"
names(model_04$coefficients)[names(model_04$coefficients) == "msp_lag5"] <- "msp"
names(model_05$coefficients)[names(model_05$coefficients) == "msp_lag2"] <- "msp"
names(model_06$coefficients)[names(model_06$coefficients) == "msp_lag2"] <- "msp"

stargazer(
  model_00,model_01,model_02,model_03,model_04,
  title = "Estimated coefficients for Distance & MSP variables, by time of adoption",
  omit = c("Constant","lot_size","sqft","age","total_rooms","stories","pool","basement",
           "nbhood_age","nbhood_bachelors","nbhood_nonwhite","nbhood_unempl"),
  omit.stat = c("f", "ser", "adj.rsq"),
  font.size = "footnotesize", no.space = T,
  digits = 2,
  covariate.labels = c("Downtown Distance (log)","Propterty Located Downtown?","Active MSP Program?","Distance*MSP Interaction"),
  dep.var.labels = c("Natural Log of Property Sale Price (real USD)"),
  column.labels   = c("Time of Sale", "1-Year Lag", "2-Year Lag", "3-Year Lag", "5+ Year Lag"),
  add.lines = list(c("Structure Characteristics?", "Yes","Yes","Yes","Yes","Yes"),
                   c("Neighborhood Characteristics?", "Yes","Yes","Yes","Yes","Yes"))
  ) 

stargazer(
  model_03,model_05,model_06,
  title = "Estimated coefficients for Distance & MSP variables, by downtown proximity",
  omit = c("Constant","lot_size","sqft","age","total_rooms","stories","pool","basement",
           "nbhood_age","nbhood_bachelors","nbhood_nonwhite","nbhood_unempl"),
  omit.stat = c("f", "ser", "adj.rsq"),
  font.size = "footnotesize", no.space = T,
  digits = 2,
  covariate.labels = c("Downtown Distance (log)","Propterty Located Downtown?","Active MSP Program?","Distance*MSP Interaction"),
  dep.var.labels = c("Natural Log of Property Sale Price (real USD)"),
  column.labels   = c("Entire Municipality Radius","Within 1.5 Miles","Within a Mile"),
  add.lines = list(c("Structure Characteristics?", "Yes", "Yes", "Yes"),
                   c("Neighborhood Characteristics?", "Yes", "Yes", "Yes"))
) 

