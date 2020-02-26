suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Iowa Data ---------------------------------------------------------------
ia_fips <- counties(state = "19", cb = T) %>% 
  rename(cty_fips = GEOID) %>% 
  select(cty_fips) %>% 
  arrange(cty_fips) %>% 
  st_drop_geometry() %>% pull()

for (i in ia_fips) {
  data <- read_delim(paste0("hidden/datatree/Prop",i,".txt.zip"), delim = "|") %>% 
    filter(!is.na(PrevSaleRecordingDate))
  nam <- paste0("datatree_",i)
  assign(nam, data)
}

iowa <- do.call(rbind, lapply(paste0("datatree_",ia_fips), get))%>% 
  write_csv("hidden/datatree/cleaned/datatree_ia.csv")
rm(list=paste0("datatree_",ia_fips))
beepr::beep()
rm(iowa)

# Ohio Data ---------------------------------------------------------------
oh_fips <- counties(state = "39", cb = T) %>% 
  rename(cty_fips = GEOID) %>% 
  select(cty_fips) %>% 
  arrange(cty_fips) %>% 
  st_drop_geometry() %>% pull()

for (i in oh_fips) {
  data <- read_delim(paste0("hidden/datatree/Prop",i,".txt.zip"), delim = "|") %>% 
    filter(!is.na(PrevSaleRecordingDate))
  nam <- paste0("datatree_",i)
  assign(nam, data)
}

ohio <- do.call(rbind, lapply(paste0("datatree_",oh_fips), get))%>% 
  write_csv("hidden/datatree/cleaned/datatree_oh.csv")
rm(list=paste0("datatree_",oh_fips))
beepr::beep()
rm(ohio)

# Wisconsin Data ----------------------------------------------------------

wi_fips <- counties(state = "55", cb = T) %>% 
  rename(cty_fips = GEOID) %>% 
  select(cty_fips) %>% 
  arrange(cty_fips) %>% 
  st_drop_geometry() %>% pull()

for (i in wi_fips) {
  data <- read_delim(paste0("hidden/datatree/Prop",i,".txt.zip"), delim = "|") %>% 
    filter(!is.na(PrevSaleRecordingDate))
  nam <- paste0("datatree_",i)
  assign(nam, data)
}

wisconsin <- do.call(rbind, lapply(paste0("datatree_",wi_fips), get))%>% 
  write_csv("hidden/datatree/cleaned/datatree_wi.csv")
rm(list=paste0("datatree_",wi_fips))
beepr::beep()
