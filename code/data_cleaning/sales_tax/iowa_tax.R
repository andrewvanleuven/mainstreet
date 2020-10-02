suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(fst)
  library(lubridate)
  library(readxl)
  library(janitor)
  library(tigris)})

msp <- read_csv("data/csv/universe/msp_universe.csv") %>% filter(st == "Iowa") %>% 
  filter(city_fips != 1950935)
msp_key <- msp %>% select(1:2)

# Import/clean nominal taxable retail sales -------------------------------
# Nominal Total Taxable Retail Sales by City
# Units: Total taxable sales in nominal dollars (not adjusted for inflation)
# Iowa Department of Revenue Annual Sales and Use Tax Reports 
df <- read_csv("data/csv/iowa_dor/iowa_city_nominal.csv") %>% ### SOURCE: https://www.icip.iastate.edu/tables/business/city-sales-pc
  clean_names() %>% 
  mutate(city_fips = as.numeric(fips)) %>% 
  inner_join(msp) %>% 
  filter(city_fips != 1950935) %>% # This removes "Melcher-Dallas, Iowa" which had some years of missing data
  select(city_fips:cty_seat,fy_1980:fy_2014)

ia2014 <- read_excel("data/csv/iowa_dor/manual_years/iowa_dor_2014.xls", sheet = 2) %>% 
  slice(-(1:3)) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  group_by(city) %>% 
  summarise(sales = sum(as.numeric(taxable_sales))) %>% 
  mutate(city = str_replace_all(city, "Lemars", "Le Mars"),
         city = str_replace_all(city, "Dewitt", "De Witt"),
         city = str_replace_all(city, "Laporte City", "La Porte City"),
         city = str_replace_all(city, "Mcgregor", "McGregor"),
         city = str_trim(city)) %>% 
  inner_join(msp_key, by = c("city" = "name")) %>% 
  rename(name = city,
         fy_2014 = sales) %>% 
  select(city_fips,name,everything())

ia2015 <- read_excel("data/csv/iowa_dor/manual_years/iowa_dor_2015.xlsx", sheet = 2) %>% 
  slice(-(1:3)) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  group_by(city) %>% 
  summarise(sales = sum(as.numeric(taxable_sales))) %>% 
  mutate(city = str_replace_all(city, "Lemars", "Le Mars"),
         city = str_replace_all(city, "Dewitt", "De Witt"),
         city = str_replace_all(city, "Laporte City", "La Porte City"),
         city = str_replace_all(city, "Mcgregor", "McGregor"),
         city = str_trim(city)) %>% 
  inner_join(msp_key, by = c("city" = "name")) %>% 
  rename(name = city,
         fy_2015 = sales) %>% 
  select(city_fips,name,everything())

ia2016 <- read_excel("data/csv/iowa_dor/manual_years/iowa_dor_2016.xlsx", sheet = 2) %>% 
  slice(-(1:3)) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  group_by(city) %>% 
  summarise(sales = sum(as.numeric(taxable_sales))) %>% 
  mutate(city = str_replace_all(city, "Lemars", "Le Mars"),
         city = str_replace_all(city, "Dewitt", "De Witt"),
         city = str_replace_all(city, "Laporte City", "La Porte City"),
         city = str_replace_all(city, "Mcgregor", "McGregor"),
         city = str_trim(city)) %>% 
  inner_join(msp_key, by = c("city" = "name")) %>% 
  rename(name = city,
         fy_2016 = sales) %>% 
  select(city_fips,name,everything())

ia2017 <- read_excel("data/csv/iowa_dor/manual_years/iowa_dor_2017.xlsx", sheet = 2) %>% 
  slice(-(1:5)) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  group_by(city) %>% 
  summarise(sales = sum(as.numeric(taxable_sales))) %>% 
  mutate(city = str_replace_all(city, "Lemars", "Le Mars"),
         city = str_replace_all(city, "Dewitt", "De Witt"),
         city = str_replace_all(city, "Laporte City", "La Porte City"),
         city = str_replace_all(city, "Mcgregor", "McGregor"),
         city = str_trim(city)) %>% 
  inner_join(msp_key, by = c("city" = "name")) %>% 
  rename(name = city,
         fy_2017 = sales) %>% 
  select(city_fips,name,everything())

ia2018 <- read_excel("data/csv/iowa_dor/manual_years/iowa_dor_2018.xlsx", sheet = 2) %>% 
  slice(-(1:5)) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  group_by(city) %>% 
  summarise(sales = sum(as.numeric(taxable_sales))) %>% 
  mutate(city = str_replace_all(city, "Lemars", "Le Mars"),
         city = str_replace_all(city, "Dewitt", "De Witt"),
         city = str_replace_all(city, "Laporte City", "La Porte City"),
         city = str_replace_all(city, "Mcgregor", "McGregor"),
         city = str_trim(city)) %>% 
  inner_join(msp_key, by = c("city" = "name")) %>% 
  rename(name = city,
         fy_2018 = sales) %>% 
  select(city_fips,name,everything())

ia2019 <- read_excel("data/csv/iowa_dor/manual_years/iowa_dor_2019.xlsx", sheet = 2) %>% 
  slice(-(1:5)) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  group_by(city) %>% 
  summarise(sales = sum(as.numeric(taxable_sales))) %>% 
  mutate(city = str_replace_all(city, "Lemars", "Le Mars"),
         city = str_replace_all(city, "Dewitt", "De Witt"),
         city = str_replace_all(city, "Laporte City", "La Porte City"),
         city = str_replace_all(city, "Mcgregor", "McGregor"),
         city = str_trim(city)) %>% 
  inner_join(msp_key, by = c("city" = "name")) %>% 
  rename(name = city,
         fy_2019 = sales) %>% 
  select(city_fips,name,everything())

manual_vals <- ia2014 %>% 
  left_join(ia2015, by = c("city_fips", "name")) %>% 
  left_join(ia2016, by = c("city_fips", "name")) %>% 
  left_join(ia2017, by = c("city_fips", "name")) %>% 
  left_join(ia2018, by = c("city_fips", "name")) %>% 
  left_join(ia2019, by = c("city_fips", "name"))

nominal_taxable_sales <- left_join(df %>% select(-fy_2014),manual_vals, by = c("city_fips", "name"))
rm(ia2014,ia2015,ia2016,ia2017,ia2018,ia2019,manual_vals)

# Real per-capita ---------------------------------------------------------

df2 <- read_csv("data/csv/iowa_dor/iowa_cities.csv") %>% 
  clean_names() %>% 
  mutate(city_fips = as.numeric(fips)) %>% 
  inner_join(msp) %>% 
  filter(city_fips != 1950935) %>% # This removes "Melcher-Dallas, Iowa" which had some years of missing data
  select(city_fips:cty_seat,fy1980:fy2014) %>% 
  mutate(fy1980 = as.numeric(sub(",", "", fy1980, fixed = TRUE)),
         fy1981 = as.numeric(sub(",", "", fy1981, fixed = TRUE)))

# Population weights 1980-2014 --------------------------------------------
cpi <- fredr::fredr(series_id = "CPIAUCSL", # maybe try CPIHOSSL --- Consumer Price Index for All Urban Consumers: Housing in U.S. City Average
                    observation_start = as.Date("1980-01-01"),
                    observation_end = as.Date("2020-01-31")) %>% 
  mutate(now = last(value),
         inflator = now/value) %>% 
  rename(yr = date) %>% select(yr,inflator)

populations <- df %>% select(1,14:48) %>% pivot_longer(!city_fips,names_to = "year", values_to = "nominal") %>% 
  mutate(year = as.numeric(str_replace_all(year, "fy_", ""))) %>% 
  inner_join(df2 %>% select(1,14:48) %>% 
               pivot_longer(!city_fips,names_to = "year", values_to = "real_pk") %>% 
               mutate(year = as.numeric(str_replace_all(year, "fy", ""))),
             by = c("city_fips", "year")) %>% 
  mutate(yr = ymd(year, truncated = 2L)) %>% 
  left_join(cpi, by = "yr") %>% 
  mutate(real = nominal*inflator,
         population = round(real/real_pk,0)) %>% 
  select(city_fips,year,population) %>% 
  pivot_wider(names_from = "year", values_from = "population", names_prefix = "pop_")
  

pop_1980to2019 <- rio::import("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/cities/totals/sub-est2019_19.csv") %>% 
  clean_names() %>% 
  mutate(city_fips = as.numeric(paste0(state,str_pad(place,5,"left","0")))) %>% 
  select(city_fips,popestimate2015:popestimate2019) %>% 
  inner_join(msp_key, by = "city_fips") %>% 
  group_by(city_fips) %>% 
  slice(which.max(popestimate2015)) %>% 
  select(-name) %>% 
  rename_with(~paste0("pop", sub("popestimate*", "_", .)), -1) %>% 
  inner_join(populations, by = "city_fips") %>% 
  select(city_fips,pop_1980:pop_2014,pop_2015:pop_2019) %>% 
  pivot_longer(!city_fips,names_to = "year", values_to = "population") %>% 
  mutate(year = as.numeric(str_replace_all(year, "pop_", "")))

# Real Per-Capita Taxable Sales -------------------------------------------

sales_pk <- nominal_taxable_sales %>% select(-(3:13)) %>% 
  pivot_longer(!city_fips:name,names_to = "year", values_to = "nominal_sales") %>% 
  mutate(year = as.numeric(str_replace_all(year, "fy_", "")),
         yr = ymd(year, truncated = 2L),
         nominal_sales = round(nominal_sales,0)) %>% 
  inner_join(pop_1980to2019, by = c("city_fips", "year")) %>% 
  left_join(cpi, by = "yr") %>% select(-yr) %>% 
  mutate(real_sales = round(nominal_sales*inflator,0),
         real_pk_sales = round(real_sales/population,2)) %>% 
  select(-inflator,-population) %>% 
  write_csv("data/csv/iowa_dor/real_taxable_sales_pk.csv")
