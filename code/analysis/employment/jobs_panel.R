suppressMessages({library(tidyverse)
  library(sf)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------
df <- read_csv("data/csv/employment/jobs_panel.csv") 
