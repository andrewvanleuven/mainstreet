suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(fst)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
df <- read_fst("hidden/esri/cleaned_02.fst")