suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

iowa <- read_fst("hidden/datatree/cleaned/datatree_ia03.fst") 
ohio <- read_fst("hidden/datatree/cleaned/datatree_oh03.fst") 
wisc <- read_fst("hidden/datatree/cleaned/datatree_wi03.fst") 

everything <- rbind(iowa,ohio,wisc)
write_csv(everything,"hidden/datatree/cleaned/datatree_04.csv")
write_fst(everything,"hidden/datatree/cleaned/datatree_04.fst")
