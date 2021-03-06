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
msp <- read_csv("data/csv/universe/msp_universe.csv")
#df <- read_fst("hidden/esri/cleaned_05.fst")  %>% 
#  mutate(unique_id = paste(id,year,sep = '_'))
#df_dots <- read_fst("hidden/esri/cleaned_06.fst") 
#  
#df_buffers <- left_join(df_dots,df,by = "unique_id") %>% 
#  distinct() %>% 
#  separate(ubuff,c('fip','buf'),sep = "_") %>% 
#  filter(fip == city_fips) %>% 
#  write_fst("hidden/esri/cleaned_07.fst")
df <- read_fst("hidden/esri/cleaned_07.fst")
beepr::beep()

# Create Panel ------------------------------------------------------------
base_panel <- df %>% 
  select(fip,buffer,year,jobs,naics_code) %>% 
  rename(city_fips = fip) %>% 
  mutate(city_fips = as.numeric(city_fips)) %>% 
  filter(!is.na(jobs)) %>% 
  mutate(naics = str_sub((as.character(naics_code)), end = -3),
         rtl = ifelse(naics %in% c(42,44,45,72),1,0),
         trn = ifelse(naics %in% c(48,49),1,0),
         mfg = ifelse(naics %in% c(11,21,22,23,31,32,33),1,0),
         svc = ifelse(naics %in% c(51,52,53,54,55,56,61,62,71,81,92),1,0)
         ) %>%
  group_by(city_fips,year,buffer) %>% 
  summarise(job_total = sum(as.numeric(jobs)),
            rtl_ests = sum(as.numeric(rtl)),
            trn_ests = sum(as.numeric(trn)),
            mfg_ests = sum(as.numeric(mfg)),
            svc_ests = sum(as.numeric(svc)),
            rtl_jobs = sum(as.numeric(rtl*jobs)),
            trn_jobs = sum(as.numeric(trn*jobs)),
            mfg_jobs = sum(as.numeric(mfg*jobs)),
            svc_jobs = sum(as.numeric(svc*jobs)),
            estabs = n()) %>% 
  arrange(city_fips,year,buffer) %>% 
  write_csv("data/csv/employment/panel_long.csv")
jobs_panel <- base_panel %>% select(city_fips:job_total) %>% 
  pivot_wider(names_from = buffer, values_from = job_total, names_prefix = "buffer_") %>% 
  replace(is.na(.), 0) %>% 
  left_join(msp, by = "city_fips") %>% 
  filter(!is.na(st))%>% 
  select(1,2,9:20,buffer_0,buffer_1,buffer_2,buffer_3,buffer_4,buffer_5) %>% 
  write_csv("data/csv/employment/jobs_panel.csv")
ests_panel <- base_panel %>% select(city_fips:buffer,estabs) %>% 
  pivot_wider(names_from = buffer, values_from = estabs, names_prefix = "buffer_") %>% 
  replace(is.na(.), 0) %>% 
  left_join(msp, by = "city_fips") %>% 
  filter(!is.na(st))%>% 
  select(1,2,9:20,buffer_0,buffer_1,buffer_2,buffer_3,buffer_4,buffer_5) %>% 
  write_csv("data/csv/employment/ests_panel.csv")
retail_panel <- base_panel %>% select(city_fips:buffer,rtl_ests,rtl_jobs) %>% 
  rename(ests = rtl_ests, jobs = rtl_jobs) %>% 
  pivot_wider(names_from = buffer, values_from = c(ests,jobs), names_prefix = "buffer_") %>% 
  replace(is.na(.), 0) %>% 
  left_join(msp, by = "city_fips") %>% 
  filter(!is.na(st)) %>% 
  select(1,2,jobs_buffer_0,jobs_buffer_1,jobs_buffer_2,jobs_buffer_3,jobs_buffer_4,jobs_buffer_5,
         ests_buffer_1,ests_buffer_2,ests_buffer_3,ests_buffer_4,ests_buffer_5,everything()) %>% 
  select(1,2,15:26,3:14) %>% 
  write_csv("data/csv/employment/retail_jobs_panel.csv")
