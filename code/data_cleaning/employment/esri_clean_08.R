suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------
df <- read_fst("hidden/esri/cleaned_07.fst") %>% 
  distinct()
beepr::beep()
test <- df %>% filter(
  #str_detect(company,"NOSTALGIC"),
  city_fips == 5572725,
  #city == "ALTOONA",
  year == "2018"
)

reshape <- df %>% filter(buffer < 4) %>% 
  select(-city_fips) %>% 
  distinct() %>% 
  pivot_wider()


