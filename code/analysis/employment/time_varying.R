library(tidyverse)
library(tidycensus)
library(lubridate)
library(sf)
library(tigris)
library(rleuven)
library(scales)
library(viridis)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999,"digits"=3)

msp <- read_csv("data/csv/universe/msp_universe.csv") %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),
         town = paste(name,st,sep = ", "),
         city_fips = as.numeric(str_replace(city_fips,"3981935","3981942"))) %>% 
  select(city_fips,town) 

# 1990s -------------------------------------------------------------------
df <- read_fwf("data/txt/sc2000f_us.txt", fwf_cols(
  block = 6,
  sum_level = 3,
  st_fips = 3,
  cty_fips = 7,
  place_fips = 9,
  st = 3,
  name = 38,
  census_2000 = 15,
  est_2000 = 11,
  est_1999 = 11,
  est_1998 = 11,
  est_1997 = 11,
  est_1996 = 11,
  est_1995 = 11
)) %>% slice(-(1:11)) %>% 
  filter(block == 1) %>% 
  select(-block)  
df2 <- read_fwf("data/txt/sc2000f_us.txt", fwf_cols(
  block = 4,
  sum_level = 5,
  st_fips = 3,
  cty_fips = 8,
  place_fips = 9,
  st = 3,
  name = 37,
  est_1994 = 15,
  est_1993 = 11,
  est_1992 = 11,
  est_1991 = 11,
  est_1990 = 11,
  census_1990 = 11
)) %>% slice(-(1:11)) %>% 
  filter(block == 2) %>% 
  select(-block) %>% 
  inner_join(df, by = c("sum_level", "st_fips", "cty_fips", "place_fips", "st", "name")) %>% 
  mutate(cty_fips = paste0(st_fips,cty_fips),
         city_fips = paste0(st_fips,place_fips))
city <- df2 %>% 
  filter(!is.na(place_fips)) %>% 
  select(city_fips,sum_level:name,est_1990,est_1991,est_1992,est_1993,
         est_1994,est_1995,est_1996,est_1997,est_1998,est_1999,est_2000) %>% 
  write_csv("data/csv/universe/nineties.csv")
rm(df,df2,city)

#81214 #81718
pop1990s <- read_csv("data/csv/universe/nineties.csv") %>% 
  select(-(sum_level:name)) %>% 
  group_by(city_fips) %>% 
  summarise_at(.vars = names(.)[2:12],
               .funs = c(pop="sum")) %>% 
  mutate(city_fips = str_replace(city_fips, "81214", "81718")) %>% 
  mutate(city_fips = as.numeric(city_fips)) %>% 
  right_join(msp, by = "city_fips") %>% 
  relocate(town, .after = city_fips)

# 2000s -------------------------------------------------------------------
df2k <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/cities/sub-est00int.csv") 

pop2000s <- df2k %>% 
  janitor::clean_names() %>% 
  filter(sumlev == 162) %>% 
  mutate(cty_fips = as.numeric(paste0(state,county)),
         city_fips = as.numeric(paste0(state,place))) %>%
  select(city_fips,popestimate2001:popestimate2009) %>% 
  right_join(msp, by = "city_fips")

# 2010s -------------------------------------------------------------------
df10 <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/cities/totals/sub-est2019_all.csv") 

pop2010s <- df10 %>% 
  janitor::clean_names() %>% 
  filter(sumlev == 162) %>% 
  mutate(place = str_replace(place, "81935", "81942")) %>% 
  mutate_all(.,function(x){gsub('[^ -~]', '', x)}) %>% 
  mutate(city_fips = as.numeric(paste0(state,place))) %>% 
  select(city_fips,census2010pop,popestimate2011:popestimate2019) %>% 
  mutate_all(.,function(x){as.numeric(x)}) %>% 
  right_join(msp, by = "city_fips")

pop_1990to2019 <- inner_join(pop1990s,pop2000s, by = c("city_fips", "town")) %>% 
  inner_join(pop2010s, by = c("city_fips", "town")) %>% 
  rename(p1990 = 03, p1991 = 04, p1992 = 05, p1993 = 06, p1994 = 07, p1995 = 08, 
         p1996 = 09, p1997 = 10, p1998 = 11, p1999 = 12, p2000 = 13, p2001 = 14, 
         p2002 = 15, p2003 = 16, p2004 = 17, p2005 = 18, p2006 = 19, p2007 = 20, 
         p2008 = 21, p2009 = 22, p2010 = 23, p2011 = 24, p2012 = 25, p2013 = 26, 
         p2014 = 27, p2015 = 28, p2016 = 29, p2017 = 30, p2018 = 31, p2019 = 32) %>% 
  select(-p1990,-p2000,-p2010)
num_of_NAs(pop_1990to2019)


# Census ------------------------------------------------------------------

dec_pop <- read_csv("data/csv/universe/decennial_pop.csv") %>% 
  select(-st,-place) %>% 
  right_join(msp, by = "city_fips") %>% 
  select(city_fips,town,everything()) %>% 
  inner_join(pop_1990to2019, by = c("city_fips", "town"))

# Merge All ---------------------------------------------------------------


df_all <- dec_pop %>% 
  pivot_longer(!c(city_fips,town), names_to = "year", values_to = "population") %>% 
  mutate(year = str_replace_all(year,"p",""),
         city_fips = str_trim(city_fips, side = "both")) %>% 
  filter(year > 1980) %>% 
  arrange(city_fips,year) %>% 
  write_csv("data/csv/universe/pop_1990to2019.csv")

msp2 <- msp <- read_csv("data/csv/universe/msp_universe.csv") %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),
         town = paste(name,st,sep = ", ")) %>% 
  select(-pop_2010)

msp_pops <- dec_pop %>% 
  pivot_longer(!c(city_fips,town), names_to = "year", values_to = "population") %>% 
  mutate(year = str_replace_all(year,"p",""),
         city_fips = str_trim(city_fips, side = "both")) %>%
  arrange(city_fips,year) %>% 
  pivot_wider(names_from = year,  values_from = population, names_prefix = "pop_") %>% 
  select(-city_fips) %>% 
  inner_join(msp2, by = "town") %>% 
  write_csv("data/csv/universe/msp_universe_histpops.csv")


# County LAUS Controls ----------------------------------------------------
df <- read_fwf("/Users/andrew/Documents/GitHub/legacyR/hidden/too_big/pop.txt", 
               fwf_widths(c(4,2,2,3,2,1,1,1,2,8),
                          c("year","st","st_fips","cty_fips","registry",
                            "race","origin","sex","age","population")))

cty_pops <- df %>% 
  mutate(cty_fips = as.numeric(paste0(st_fips,cty_fips)),
         population = as.numeric(population)) %>% 
  group_by(cty_fips,year) %>% 
  summarize(cty_pop = sum(population)) %>% 
  rename(cal_yr = year) %>% 
  mutate(cal_yr= as.numeric(cal_yr)) %>% 
  write_csv("data/csv/universe/cty_populations.csv")

unemp <- laus::county_year %>% 
  select(2,7,11) %>% 
  rename(cal_yr = year,
         cty_fips = 1,
         cty_unemp_rt = 3) %>% 
  mutate(cty_fips = as.numeric(cty_fips),
         cal_yr = as.numeric(cal_yr)) 

cty_vary <- left_join(unemp,cty_pops, by = c("cty_fips", "cal_yr"))%>% 
  group_by(cty_fips) %>% 
  mutate(lag_unemp = lag(cty_unemp_rt)) %>% 
  filter(cal_yr > 1990) %>% 
  write_csv("data/csv/universe/cty_varying.csv")
beepr::beep()


# County GDP variables ----------------------------------------------------
cty_pops <- read_csv("data/csv/universe/cty_populations.csv")


gdp <- read_csv("data/csv/time_varying/real_gdp_county.csv", skip = 4) %>% 
  filter(!is.na(GeoName)) %>% 
  janitor::clean_names() %>% 
  pivot_longer(!c(geo_fips,geo_name), names_to = "cal_yr", values_to = "gdp") %>% 
  rename(name = geo_name,
         cty_fips = geo_fips) %>% 
  mutate(cal_yr = as.numeric(str_replace_all(cal_yr,"x","")),
         cty_fips = as.numeric(cty_fips)) %>% 
  inner_join(cty_pops, by = c("cty_fips", "cal_yr")) %>% 
  mutate(gdp_pk = gdp/cty_pop) %>% 
  select(1,2,3,5,6) %>% 
  write_csv("data/csv/time_varying/real_gdp_cty.csv")

gdp_retail <- read_csv("data/csv/time_varying/real_gdp_county_retail.csv", skip = 4) %>% 
  filter(!is.na(GeoName)) %>% 
  janitor::clean_names() %>% 
  select(-geo_name) %>% 
  mutate_all(funs(as.numeric(.))) %>% drop_na() %>% 
  pivot_longer(!geo_fips, names_to = "cal_yr", values_to = "gdp") %>% 
  rename(cty_fips = geo_fips) %>% 
  mutate(cal_yr = as.numeric(str_replace_all(cal_yr,"x","")),
         cty_fips = as.numeric(cty_fips)) %>% 
  inner_join(cty_pops, by = c("cty_fips", "cal_yr")) %>% 
  mutate(gdp_pk_rtl = gdp/cty_pop) %>% 
  select(cty_fips,cal_yr,gdp_pk_rtl)


gdp_mfg <- read_csv("data/csv/time_varying/real_gdp_county_mfg.csv", skip = 4) %>% 
  filter(!is.na(GeoName)) %>% 
  janitor::clean_names() %>% 
  select(-geo_name) %>% 
  mutate_all(funs(as.numeric(.))) %>% drop_na() %>% 
  pivot_longer(!geo_fips, names_to = "cal_yr", values_to = "gdp") %>% 
  rename(cty_fips = geo_fips) %>% 
  mutate(cal_yr = as.numeric(str_replace_all(cal_yr,"x","")),
         cty_fips = as.numeric(cty_fips)) %>% 
  inner_join(cty_pops, by = c("cty_fips", "cal_yr")) %>% 
  mutate(gdp_pk_mfg = gdp/cty_pop) %>% 
  select(cty_fips,cal_yr,gdp_pk_mfg)

gdp_cty <- inner_join(gdp,gdp_retail, by = c("cty_fips", "cal_yr")) %>% 
  inner_join(gdp_mfg, by = c("cty_fips", "cal_yr")) %>% 
  write_csv("data/csv/time_varying/gdp_cty.csv")



# BEA ---------------------------------------------------------------------
cpi <- fredr::fredr(series_id = "CPIAUCSL", # maybe try CPIHOSSL --- Consumer Price Index for All Urban Consumers: Housing in U.S. City Average
                    observation_start = as.Date("1980-01-01"),
                    observation_end = as.Date("2020-01-31")) %>% 
  mutate(now = last(value),
         inflator = now/value) %>% 
  rename(yr = date) %>% select(yr,inflator)

bea <- read_csv("data/csv/time_varying/cainc30.csv") %>% 
  janitor::clean_names() %>% 
  mutate(geo_fips = str_replace_all(geo_fips,"\"","")) %>% 
  filter(line_code %in% c(100,110,290)) %>% 
  select(geo_fips,line_code,x1980:x2018) %>% 
  pivot_longer(!c(geo_fips,line_code), names_to = "cal_yr", values_to = "value") %>% 
  mutate(cal_yr = as.numeric(str_replace_all(cal_yr,"x",""))) %>% 
  pivot_wider(names_from = line_code, names_prefix = "val", values_from = value) %>% 
  rename(cty_pop = val100,
         income_pc = val110,
         earnings_pj = val290) %>% 
  mutate(yr = ymd(cal_yr, truncated = 2L)) %>% 
  left_join(cpi, by = "yr") %>% 
  mutate(real_inc_pc = income_pc*inflator,
         real_earn_pj = earnings_pj*inflator) %>% 
  select(1:3,8,9) %>% 
  mutate(st_fips = str_sub(geo_fips,end = -4)) %>% 
  filter(st_fips %in% c(19,26,39,55)) %>% 
  select(-st_fips) %>% 
  write_csv("data/csv/time_varying/bea_profile.csv")

#bea <- read_csv("data/csv/time_varying/cainc30.csv") %>% 
#  janitor::clean_names() %>% 
#  mutate(geo_fips = str_replace_all(geo_fips,"\"","")) %>% 
#  filter(line_code %in% c(10,100,110,180,240,290)) %>% 
#  select(geo_fips,line_code,x1980:x2018) %>% 
#  pivot_longer(!c(geo_fips,line_code), names_to = "cal_yr", values_to = "value") %>% 
#  mutate(cal_yr = as.numeric(str_replace_all(cal_yr,"x",""))) %>% 
#  pivot_wider(names_from = line_code, names_prefix = "val", values_from = value) %>% 
#  rename(income = val10,
#         cty_pop = val100,
#         income_pc = val110,
#         earnings = val180,
#         jobs = val240,
#         earnings_pj = val290) %>% 
#  mutate(yr = ymd(cal_yr, truncated = 2L)) %>% 
#  left_join(cpi, by = "yr") %>% 
#  mutate(
#    real_inc_pc = income_pc*inflator,
#    real_inc_pc2 = (income*inflator)/cty_pop,
#    real_earn_pj = earnings_pj*inflator,
#    real_earn_pj2 = (earnings*inflator)/jobs
#  )   

bea_exp <- read_csv("data/csv/time_varying/cainc91.csv") %>% 
  janitor::clean_names() %>% 
  mutate(geo_fips = str_replace_all(geo_fips,"\"","")) %>% 
  filter(line_code %in% c(10,20)) %>% 
  select(geo_fips,line_code,x1990:x2018) %>% 
  pivot_longer(!c(geo_fips,line_code), names_to = "cal_yr", values_to = "value") %>% 
  mutate(cal_yr = as.numeric(str_replace_all(cal_yr,"x",""))) %>% 
  pivot_wider(names_from = line_code, names_prefix = "val", values_from = value) %>% 
  rename(inflow = val10,
         outflow = val20) %>% 
  mutate(yr = ymd(cal_yr, truncated = 2L),
         inflow = as.numeric(inflow),
         outflow = as.numeric(outflow)) %>% 
  left_join(cpi, by = "yr") %>% 
  mutate(real_inflow = inflow*inflator,
         real_outflow = outflow*inflator) %>% 
  select(1,2,7,8) %>% 
  mutate(st_fips = str_sub(geo_fips,end = -4)) %>% 
  filter(st_fips %in% c(19,26,39,55),
         !is.na(real_inflow)) %>% 
  select(-st_fips) %>% 
  inner_join(bea, by = c("geo_fips", "cal_yr")) %>% 
  rename(cty_fips = geo_fips) %>% 
  select(1,2,5,6,7,3,4) %>% 
  mutate(real_inflow_pc = real_inflow/cty_pop,
         real_outflow_pc = real_outflow/cty_pop) %>% 
  select(-real_inflow,-real_outflow) %>% 
  write_csv("data/csv/time_varying/bea_flow.csv")

               
