library(tidyverse)
library(tidycensus)
library(tigris)
library(maptools)
library(maps)
library(mapview)
library(scales)
library(sf)
library(magrittr)
library(inlmisc)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
#####
hist <- read_csv("data/csv/hist_geo.csv") %>%
  filter(pop_1940 > 0 | pop_1930 > 0 | pop_1920 > 0) %>%
  select(-rucc,-pop_2010,-(pop_1910:pop_1900)) %>%
  `colnames<-`(c("GEOID","NAME","ST","CTY_FIPS","pop_1940","pop_1930","pop_1920"))
cz <- read_csv("data/csv/cz.csv") %>%
  mutate(CTY_FIPS = cty_fips,
         CZ = cz2000) %>%
  select(CTY_FIPS,CZ)
rucc <- read_csv("data/csv/rucc.csv") %>% 
  mutate(CTY_FIPS = FIPS,
         RUCC = RUCC_2013) %>%
  select(CTY_FIPS,RUCC)
msp <- read_csv("data/csv/msp.csv")  %>%
  mutate(GEOID = as.character(fips),
         fips = NULL) %>%
  select(GEOID,msp,msp_yr:msp_affl)
jobs <- read_csv("data/csv/jobs.csv")
places <- read_csv("data/csv/places.csv") %>%
  mutate(POP2010 = value) %>%
  select(GEOID,POP2010)
cbsa <- read_csv("data/csv/cbsa_xw.csv") %>%
  select(CTY_FIPS:CSA,METRO_TYPE)
  #####
msp.join <- inner_join(hist,cz, by = "CTY_FIPS") %>%
  inner_join(.,places, by = "GEOID") %>%
  inner_join(.,rucc, by = "CTY_FIPS") %>%
  full_join(.,cbsa, by = "CTY_FIPS") %>%
  mutate_at(vars(CBSA:METRO_TYPE), ~replace_na(., 0)) %>%
  left_join(.,msp, by = "GEOID") %>%
  mutate_at(vars(msp:msp_affl), ~replace_na(., 0)) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  inner_join(.,jobs, by = "GEOID") %>%
  distinct()
### Towns missing from jobs.csv: Santa Anna, TX; San Juan, TX; Waverly, OH
msp.data <- msp.join %>%
  filter(pop_1940 > 999 | pop_1930 > 999 | pop_1920 > 999) %>%
  select(-(pop_1940:pop_1920)) %>%
  janitor::clean_names() %>%
  filter(pop2010 > 750 & pop2010 < 75000) %>%
  arrange(rucc) %>%
  distinct() %>%
  write_csv("data/csv/msp_data.csv")


