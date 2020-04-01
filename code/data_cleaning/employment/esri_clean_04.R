suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(fst)
  library(tigris)})
options(scipen = 999)

df <- read_fst("hidden/esri/cleaned_01.fst") %>% as_tibble() %>% 
  mutate(uid = paste(id,address,sep = "_"))
df_geo <- read_fst("hidden/esri/cleaned_03.fst") %>% as_tibble() %>% 
  rename_all(tolower) %>% 
  mutate(uid = paste(id,address,sep = "_")) %>% 
  select(uid,lat,lon,status) 
  
df_coded <- df %>% left_join(.,df_geo, by = "uid")
rm(df,df_geo)

didnt_need <- df_coded %>% filter(is.na(lon)) %>% 
  replace_na(list(status = "M")) %>% 
  select(-lat,-lon) 
did_need <- df_coded %>% filter(!is.na(lon)) %>% 
  select(-latitude,-longitude) %>% 
  rename(latitude = lat,
         longitude = lon) %>% 
  select(1:8,latitude,longitude,jobs:status)

final <- rbind(didnt_need,did_need) %>% 
  filter(status == "M")

100*(nrow(final)/nrow(df_coded)) # 96.1% of total observations are now geocoded

final %>% select(id:naics_code) %>% 
  write_fst("hidden/esri/cleaned_04.fst")
beepr::beep()