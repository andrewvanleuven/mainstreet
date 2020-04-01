suppressMessages({
  library(tidyverse)
  library(sf)
  library(data.table)
  library(rleuven)
  library(fst)
  library(tigris)})
df <- read_fst("hidden/esri/cleaned_01.fst")

find_bad_geocodes <- function(dataframe){
  dataframe %>% 
    arrange(id) %>% 
    group_by(id,address) %>% 
    slice(1) %>% 
    filter(str_detect(address, "PO BOX", negate = F) |
             str_detect(address, "BOX", negate = F) |
             str_detect(address, " MI ", negate = F) |
             str_detect(address, "MILE", negate = F) |
             str_detect(address, "RR ", negate = F) |
             match_code %in% c("2","4","X") |
             is.na(address))
}

needs_geo <- find_bad_geocodes(df) %>% 
  filter(match_code %in% c("2","4","X"),
         str_detect(address, "BOX ", negate = T),
         str_detect(address, "PO BOX", negate = T),
         str_detect(address, "P O BOX", negate = T),
         str_detect(address, "^RR ", negate = T),
         str_detect(address, "[:space:]MILES(?! RD| AVE|TONE)", negate = T),
         str_detect(address, " [NSEW] OF ", negate = T),
         address != "") 

beepr::beep()

needs_geo %>%  fwrite("to_geocode_all.csv") 
