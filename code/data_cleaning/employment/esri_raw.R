suppressMessages({library(tidyverse)
  library(sf)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------

oh1997 <- read_csv("hidden/infogroup/OH/Bus_OH_1997.csv") %>% 
  janitor::clean_names() %>% 
  rename(year = archive_version_year,
         jobs = employee_size_5_location,
         naics_code = primary_naics_code,
         naics = naics8_descriptions,
         address = address_line_1,
         id = abi) %>% 
  mutate(mailing_address = paste0(address,", ",city,", ",state," ",zip_code),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>% 
  select(id,year,company:zip_code,fips_code,latitude,longitude,jobs,naics_code,naics,match_code) %>%
  mutate(naics_code = str_sub(naics_code,end = -5)) %>% select(-naics,-zip_code)
  #filter(!is.na(longitude)) %>% 
  #st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  #st_centroid_xy()

