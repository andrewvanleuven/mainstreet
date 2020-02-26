suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

vars_1 <- read_csv("code/data_cleaning/real_estate/variables/first_pass.csv") %>% pull()

iowa <- read_csv("hidden/datatree/cleaned/datatree_ia.csv") %>% select(vars_1) %>% 
  write_csv("hidden/datatree/cleaned/datatree_ia01.csv")
ohio <- read_csv("hidden/datatree/cleaned/datatree_oh.csv") %>% select(vars_1) %>% 
  write_csv("hidden/datatree/cleaned/datatree_oh01.csv")
wisconsin <- read_csv("hidden/datatree/cleaned/datatree_wi.csv") %>% select(vars_1) %>% 
  write_csv("hidden/datatree/cleaned/datatree_wi01.csv")

beepr::beep()
