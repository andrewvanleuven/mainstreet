suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(fst)
  library(tigris)})
options(scipen = 999)

df1 <- st_read("hidden/esri/cura/Export_Output1.shp") %>% 
  select(Status:Match_type,id:geometry)
df2 <- st_read("hidden/esri/cura/Export_Output2.shp") %>% 
  select(Status:Match_type,id:geometry)
df3 <- st_read("hidden/esri/cura/Export_Output3.shp") %>% 
  select(Status:Match_type,id:geometry)
df4 <- st_read("hidden/esri/cura/Export_Output4.shp") %>% 
  select(Status:Match_type,id:geometry)

df <- rbind(df1,df2,df3,df4)

df %>% 
  mutate(lat = unlist(map(df$geometry,2)),
         lon = unlist(map(df$geometry,1))) %>% 
  st_drop_geometry() %>% 
  write_fst("hidden/esri/cleaned_03.fst")