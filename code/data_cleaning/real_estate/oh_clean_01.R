suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

vars <- colnames(read_csv("code/data_cleaning/real_estate/vars_oh.csv") %>% janitor::clean_names())
oh_fips <- counties(state = "39", cb = T) %>% 
  rename(cty_fips = GEOID) %>% 
  select(cty_fips) %>% 
  arrange(cty_fips) %>% 
  st_drop_geometry() %>% pull()

for (i in oh_fips) {
  data <- read_delim(paste0("hidden/datatree/oh/Prop",i,".txt.zip"), delim = "|") %>% 
    janitor::clean_names() %>% 
    select(all_of(vars)) # only getting variables I want
  nam <- paste0("datatree_",i)
  assign(nam, data)
}

ohio <- do.call(rbind, lapply(paste0("datatree_",oh_fips), get))%>% 
  write_fst("hidden/datatree/cleaned/datatree_oh01.fst")
beepr::beep()