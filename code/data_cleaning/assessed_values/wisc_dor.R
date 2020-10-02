suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(fst)
  library(tigris)})

msp <- read_csv("data/csv/universe/msp_universe.csv") %>% 
  filter(st == "Wisconsin") %>% mutate_if(is.character, str_to_upper)

df <- read_csv("data/csv/wisc_dor/assessed_value.csv") %>% ### SOURCE: https://www.revenue.wi.gov/Pages/RA/AssessedPropValues.aspx
  janitor::clean_names() %>% 
  separate(municipality, into = c("assessor", "name"), sep = "\\s", extra = "merge") %>% 
  separate(name, into = c("name", "mun_type"), sep = "\\(", extra = "merge") %>% 
  mutate(mun_type = str_replace_all(mun_type, "\\)", ""),
         name = str_replace_all(name, "SAINT CROIX", "ST. CROIX"),
         name = str_trim(name)) %>% 
  rename(year = year_of_year) %>% 
  group_by(year,name) %>% 
  summarise(value = sum(assessed_value))

df2 <- read_csv("data/csv/wisc_dor/land_parcels.csv") %>% 
  janitor::clean_names() %>% 
  separate(municipality, into = c("assessor", "name"), sep = "\\s", extra = "merge") %>% 
  separate(name, into = c("name", "mun_type"), sep = "\\(", extra = "merge") %>% 
  mutate(mun_type = str_replace_all(mun_type, "\\)", ""),
         name = str_replace_all(name, "SAINT CROIX", "ST. CROIX"),
         name = str_trim(name)) %>% 
  rename(year = year_number) %>% 
  group_by(year,name) %>% 
  summarise(parcels = sum(land_par))

df3 <- read_csv("data/csv/wisc_dor/parcel_acres.csv") %>% 
  janitor::clean_names() %>% 
  separate(municipality, into = c("assessor", "name"), sep = "\\s", extra = "merge") %>% 
  separate(name, into = c("name", "mun_type"), sep = "\\(", extra = "merge") %>% 
  mutate(mun_type = str_replace_all(mun_type, "\\)", ""),
         name = str_replace_all(name, "SAINT CROIX", "ST. CROIX"),
         name = str_trim(name)) %>% 
  rename(year = year_number) %>% 
  group_by(year,name) %>% 
  summarise(acres = sum(acres))

msp2 <- read_csv("data/csv/universe/msp_universe.csv") %>% 
  filter(st == "Wisconsin")

joined <- df %>% 
  left_join(df2, by = c("year", "name")) %>% 
  left_join(df3, by = c("year", "name")) %>% 
  left_join(.,(msp %>% select(1,2)), by = "name") %>% 
  filter(!is.na(city_fips)) %>% 
  select(city_fips,year,value,parcels,acres) %>% 
  left_join(msp2, by = "city_fips") %>% 
  select(1:2,6:17,3:5) %>% 
  mutate(val_per_acre = round(value/acres,2)) %>% 
  write_csv("data/csv/wisc_dor/wisc_dor.csv")

