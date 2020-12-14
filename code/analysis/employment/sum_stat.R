library(foreign)
library(tidyverse)
library(tidycensus)

msp <- read_csv("data/csv/universe/msp_universe.csv") %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),
         town = paste(name,st,sep = ", ")) %>% 
  select(city_fips,town) 

census <- get_decennial(year = 2010,
                        variables = c("P010001","P003001","P003002","P013001"), 
                        state = c("19","26","39","55"),
                        geography = "place")

acs <- get_acs(year = 2014,
               variables = "B19013_001", 
               state = c("19","26","39","55"),
               geography = "place") %>% 
  select(1,4) %>%
  rename(city_fips = GEOID,
         median_inc_2010 = estimate) %>% 
  mutate(city_fips = as.numeric(city_fips)) 
  

demog <- census %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  mutate(city_fips = GEOID,
         pop_2010 = P010001,
         median_age_2010 = P013001,
         pct_nonwhite_2010 = round(100*(1-(P003002/P003001)),3)) %>% 
  select(city_fips, median_age_2010, pct_nonwhite_2010) %>% 
  mutate(city_fips = as.numeric(city_fips)) 

df <- read.dta("data/stata/nn.dta") %>% 
  left_join(msp, by = "town") %>% 
  select(city_fips,town,cty_seat,rucc,pop_2010,cal_yr,jobs,ests,jobs_p1k,ests_p10k,treated) %>% 
  left_join(acs, by = "city_fips") %>% 
  mutate(city_fips = str_replace_all(city_fips,"3981935","3981942"),
         city_fips = as.numeric(city_fips)) %>% 
  left_join(demog, by = "city_fips")

mean(df$cty_seat)

write.dta(df,"data/stata/sum.dta")
