suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(fs)
  library(haven)
  library(data.table)
  library(fst)
  library(tigris)})

# Merge all years ---------------------------------------------------------
dta_list <- "/Users/andrew/Downloads/upjohn" %>% dir_ls(regexp = "\\.dta$")

clean_wd <- function(dta) {
  read_dta(dta) %>% 
    janitor::clean_names() %>% 
    mutate(naics = as.numeric(naics)) %>% 
    filter(naics > 99 & naics < 1000,
           fipstate %in% c(0,19,26,39,55)) %>% 
    distinct() %>% 
    select(area,year,fipstate,fipscty,naics,emp)
}

for (i in dta_list) {
  st_yr <- i %>% 
    as.character() %>% str_to_lower() %>% 
    str_sub(start = -8, end = -5)
  nam <- paste0("df_",st_yr)
  assign(nam, clean_wd(i))}
beepr::beep()

rm(df)
wholedata <- bind_rows(lapply((ls(pattern='df_*')), get)) %>% 
  mutate(st = str_pad(fipstate,"0",width = 2,side = "left"),
         cty = str_pad(fipscty,"0",width = 3,side = "left"),
         cty_fips = as.numeric(paste0(st,cty)),
         year = year(as.Date(as.character(year), format = "%Y"))) %>% 
  select(cty_fips,year,naics,emp) %>% 
  write_csv("hidden/wholedata.csv")
rm(list=ls())
clr_cons()

# Calculate LQs -----------------------------------------------------------
cz <- read_csv("data/csv/universe/cz.csv") %>% 
  rename(cz = cz2000) %>% 
  select(cty_fips,cz)

df <- read_csv("hidden/wholedata.csv") 

big_e <- df %>% group_by(cty_fips,year) %>% 
  summarise(emp = sum(emp)) %>% 
  ungroup() %>% 
  mutate(naics = 0)

full <- bind_rows(df,big_e) %>% 
  arrange(cty_fips,year,naics) %>% 
  pivot_wider(names_from = naics, values_from = emp, names_prefix = "ind_") %>% 
  rename(total_emp = ind_0) %>% 
  select(1:3,ind_311:ind_339) %>% 
  replace(is.na(.), 0) %>% 
  mutate(mfg = rowSums(select(.,ind_311:ind_339)),
         hmfg = rowSums(select(.,ind_331:ind_339))) %>% 
  select(1,2,total_emp,mfg,hmfg) %>% 
  group_by(year) %>% 
  mutate(us_total = max(total_emp),
         us_mfg = max(mfg),
         us_hmfg = max(hmfg)) %>% 
  ungroup() 


cty_lqs <- full %>% 
  mutate(lq_mfg = round((mfg/total_emp)/(us_mfg/us_total),2),
         lq_hmfg = round((hmfg/total_emp)/(us_hmfg/us_total),2)) %>% 
  filter(cty_fips > 1) %>% 
  write_csv("hidden/wd_cty_lq.csv")


cz_lqs <- full %>% 
  left_join(cz, by = "cty_fips") %>% 
  replace_na(list(cz = 0)) %>% 
  group_by(cz,year) %>% 
  summarize(total_emp = sum(total_emp),
            mfg = sum(mfg),
            hmfg = sum(hmfg)) %>% 
  group_by(year) %>% 
  mutate(us_total = max(total_emp),
         us_mfg = max(mfg),
         us_hmfg = max(hmfg)) %>% 
  ungroup() %>% 
  mutate(lq_mfg = round((mfg/total_emp)/(us_mfg/us_total),2),
         lq_hmfg = round((hmfg/total_emp)/(us_hmfg/us_total),2)) %>% 
  filter(cz > 1) %>% 
  write_csv("hidden/wd_cz_lq.csv")
