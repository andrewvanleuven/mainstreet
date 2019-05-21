library(tidyverse)
library(tidycensus)
library(tigris)
library(openxlsx)
library(curl)
library(stargazer)
options(tigris_use_cache = TRUE)
###
setwd("~/Documents/GitHub/R/mainstreet")
###
neuterCensus <- function(x) {
  x %>% mutate_if(is.character, str_replace_all, pattern = " city", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " CDP", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " village", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " borough", replacement = "") %>%
    mutate_if(is.character, str_replace_all, pattern = " town", replacement = "")}
###
columns <- c("fips","name","st","county","rucc","pop_2010")
states <- c("08","19","23","26","27","37","39","48","51","55")
###
rucc <- read_csv("data/csv/rucc.csv") %>% 
  mutate(cty = FIPS,
         FIPS = NULL)
xw <- read_csv("data/csv/xw.csv") %>% 
  mutate(cty = county,
         GEOID = as.character(place),
         county = NULL,
         place = NULL)
historical <- read_csv("data/csv/historical.csv")  
places <- get_decennial(geography = "place", state = states, variables = "P001001", year = 2010) %>%
  separate(NAME,c("NAME","ST"),sep = ",") %>%
  mutate_if(is.character, str_trim) %>%
  neuterCensus()
###-------------------------------------IOWA
ia <- left_join(places,(xw %>% filter(stab == "ia")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "ia")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
###-------------------------------------OHIO
oh <- left_join(places,(xw %>% filter(stab == "oh")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "oh")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
###-------------------------------------WISCONSIN
wi <- left_join(places,(xw %>% filter(stab == "wi")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "wi")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
###-------------------------------------NORTH CAROLINA
nc <- left_join(places,(xw %>% filter(stab == "nc")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "nc")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
###-------------------------------------MICHIGAN
mi <- left_join(places,(xw %>% filter(stab == "mi")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "mi")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
###-------------------------------------VIRGINIA
va <- left_join(places,(xw %>% filter(stab == "va")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "va")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
###-------------------------------------MAINE
me <- left_join(places,(xw %>% filter(stab == "me")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "me")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
###-------------------------------------COLORADO
co <- left_join(places,(xw %>% filter(stab == "co")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "co")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
###-------------------------------------TEXAS
tx <- left_join(places,(xw %>% filter(stab == "tx")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "tx")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
###-------------------------------------MINNESOTA
mn <- left_join(places,(xw %>% filter(stab == "mn")),by = "GEOID") %>%
  filter(GEOID != 9999999)  %>%
  left_join(.,rucc, by = "cty") %>%
  select(-(State:Population_2010)) %>% 
  filter(RUCC_2013 > 2,
         afact > 0.45) %>%
  select(-variable,-(stab:afact)) %>%
  filter(value > 750 & value < 60000) %>%
  inner_join(.,(historical %>% filter(stabbr == "mn")),by = c("NAME","ST")) %>%
  filter(pop_1940 > 999 | pop_1930 > 999) %>%
  select(-value,-stabbr,-(pop_2000:pop_1900)) %>% 
  `colnames<-`(columns) %>%
  distinct()
####
muns <- rbind(ia,oh,wi,nc,mi,va,me,co,tx,mn) %>%
  arrange(fips) %>%
  write_csv("data/communities.csv")