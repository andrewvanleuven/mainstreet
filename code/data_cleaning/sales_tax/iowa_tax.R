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
populations <- df %>% select(1,2,14:48) %>% #can make this code MUCH tighter with pivot_longer
  inner_join(df2 %>% select(1,2,14:48), by = c("city_fips", "name")) %>% 
  mutate(pop_1980 = round(fy_1980/fy1980,0),
         pop_1981 = round(fy_1981/fy1981,0),
         pop_1982 = round(fy_1982/fy1982,0),
         pop_1983 = round(fy_1983/fy1983,0),
         pop_1984 = round(fy_1984/fy1984,0),
         pop_1985 = round(fy_1985/fy1985,0),
         pop_1986 = round(fy_1986/fy1986,0),
         pop_1987 = round(fy_1987/fy1987,0),
         pop_1988 = round(fy_1988/fy1988,0),
         pop_1989 = round(fy_1989/fy1989,0),
         pop_1990 = round(fy_1990/fy1990,0),
         pop_1991 = round(fy_1991/fy1991,0),
         pop_1992 = round(fy_1992/fy1992,0),
         pop_1993 = round(fy_1993/fy1993,0),
         pop_1994 = round(fy_1994/fy1994,0),
         pop_1995 = round(fy_1995/fy1995,0),
         pop_1996 = round(fy_1996/fy1996,0),
         pop_1997 = round(fy_1997/fy1997,0),
         pop_1998 = round(fy_1998/fy1998,0),
         pop_1999 = round(fy_1999/fy1999,0),
         pop_2000 = round(fy_2000/fy2000,0),
         pop_2001 = round(fy_2001/fy2001,0),
         pop_2002 = round(fy_2002/fy2002,0),
         pop_2003 = round(fy_2003/fy2003,0),
         pop_2004 = round(fy_2004/fy2004,0),
         pop_2005 = round(fy_2005/fy2005,0),
         pop_2006 = round(fy_2006/fy2006,0),
         pop_2007 = round(fy_2007/fy2007,0),
         pop_2008 = round(fy_2008/fy2008,0),
         pop_2009 = round(fy_2009/fy2009,0),
         pop_2010 = round(fy_2010/fy2010,0),
         pop_2011 = round(fy_2011/fy2011,0),
         pop_2012 = round(fy_2012/fy2012,0),
         pop_2013 = round(fy_2013/fy2013,0),
         pop_2014 = round(fy_2014/fy2014,0)) %>% 
  select(1:2,pop_1980:pop_2014)

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

cpi <- fredr::fredr(series_id = "CPIAUCSL", # maybe try CPIHOSSL --- Consumer Price Index for All Urban Consumers: Housing in U.S. City Average
             observation_start = as.Date("1980-01-01"),
             observation_end = as.Date("2020-01-31")) %>% 
  mutate(now = last(value),
         inflator = now/value) %>% 
  rename(yr = date) %>% select(yr,inflator)

sales_pk <- nominal_taxable_sales %>% select(-(3:13)) %>% 
  pivot_longer(!city_fips:name,names_to = "year", values_to = "nominal_sales") %>% 
  mutate(year = as.numeric(str_replace_all(year, "fy_", ""))) %>% 
  mutate(yr = (as.Date(as.character(year), format = "%Y"))) %>% 
  inner_join(pop_1980to2019, by = c("city_fips", "year")) %>% 
  left_join(cpi, by = "yr") %>% select(-yr) %>% 
  mutate(real_sales = nominal_sales*inflator,
         real_pk_sales = round(real_sales/population,2)) %>% 
  select(-inflator) %>% 
  write_csv("data/csv/iowa_dor/real_taxable_sales_pk.csv")
