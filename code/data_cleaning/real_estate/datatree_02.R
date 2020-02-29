suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(janitor)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

vars <- colnames(read_csv("code/data_cleaning/real_estate/variables/vars_02.csv") %>% clean_names())

iowa <- read_csv("hidden/datatree/cleaned/datatree_ia01.csv") %>% select(vars) %>% 
  write_csv("hidden/datatree/cleaned/datatree_ia02.csv")
ohio <- read_csv("hidden/datatree/cleaned/datatree_oh01.csv") %>% select(vars) %>% 
  write_csv("hidden/datatree/cleaned/datatree_oh02.csv")
wisconsin <- read_csv("hidden/datatree/cleaned/datatree_wi01.csv") %>% select(vars) %>% 
  write_csv("hidden/datatree/cleaned/datatree_wi02.csv")

beepr::beep()