suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------
msp <- read_csv("data/csv/universe/analytical_universe.csv")
df <- read_fst("hidden/esri/cleaned_05.fst")  %>% 
  mutate(unique_id = paste(id,year,sep = '_'))
df_dots <- read_fst("hidden/esri/cleaned_06.fst") 
  
df_buffers <- left_join(df_dots,df,by = "unique_id") %>% 
  distinct() %>% 
  separate(ubuff,c('fip','buf'),sep = "_") %>% 
  filter(fip == city_fips)
beepr::beep()

# Create Panel ------------------------------------------------------------
base_panel <- df_buffers %>% 
  select(fip,buffer,year,jobs,naics_code) %>% 
  rename(city_fips = fip) %>% 
  mutate(city_fips = as.numeric(city_fips)) %>% 
  filter(!is.na(jobs)) %>% 
  mutate(naics = str_sub((as.character(naics_code)), end = -3),
         naic2 = str_sub((as.character(naics_code)), end = -5),
         rtl = ifelse(naic2 %in% c(42,44,45,72),1,0),
         trn = ifelse(naic2 %in% c(48,49),1,0),
         mfg = ifelse(naic2 %in% c(11,21,22,23,31,32,33),1,0),
         svc = ifelse(naic2 %in% c(51,52,53,54,55,56,61,62,71,81,92),1,0)) %>%
  group_by(city_fips,year,buffer) %>% 
  summarise(job_total = sum(as.numeric(jobs)),
            rtl = sum(as.numeric(rtl)),
            trn = sum(as.numeric(trn)),
            mfg = sum(as.numeric(mfg)),
            svc = sum(as.numeric(svc)),
            estabs = n())
panel <- base_panel %>% 
  arrange(city_fips,year,buffer) %>% 
  write_csv("data/csv/employment/panel_long.csv")
jobs_panel <- base_panel %>% select(city_fips:job_total) %>% 
  pivot_wider(names_from = buffer, values_from = job_total, names_prefix = "buffer_") %>% 
  replace(is.na(.), 0) %>% 
  left_join(msp %>% select(1:3), by = "city_fips") %>% 
  select(1,9,10,2,buffer_0,buffer_1,buffer_2,buffer_3,buffer_4,buffer_5) %>% 
  write_csv("data/csv/employment/jobs_panel.csv")
ests_panel <- base_panel %>% select(city_fips:buffer,estabs) %>% 
  pivot_wider(names_from = buffer, values_from = estabs, names_prefix = "buffer_") %>% 
  replace(is.na(.), 0) %>% 
  left_join(msp %>% select(1:3), by = "city_fips") %>% 
  select(1,9,10,2,buffer_0,buffer_1,buffer_2,buffer_3,buffer_4,buffer_5) %>% 
  write_csv("data/csv/employment/ests_panel.csv")
