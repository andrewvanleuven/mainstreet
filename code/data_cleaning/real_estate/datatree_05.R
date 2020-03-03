suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

df <- read_fst("hidden/datatree/datatree_04.fst")

single_sale <- df %>% 
  as_tibble() %>% 
  filter(!is.na(current_sales_price) | !is.na(prev_sales_price))

repeat_sale <- df %>% 
  as_tibble() %>% 
  filter(!is.na(current_sales_price),
         !is.na(prev_sales_price))  

freqTab(single_sale,"situs_state") # Distribution of repeat-sale obs. by state
freqTab(repeat_sale,"situs_state") # Distribution of repeat-sale obs. by state
