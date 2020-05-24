library(tidyverse)
library(rleuven)

# Read in data ------------------------------------------------------------
msp <- read_csv("data/csv/universe/oh_universe.csv") %>% filter(st == "Ohio")
df <- read_csv("hidden/datatree/cleaned/datatree_clean.csv") %>% 
  left_join(msp, by = c("distance_to" = "city_fips"))

num_of_NAs # Re-code binary variables (e.g., pool or basement) as 1 or 0
