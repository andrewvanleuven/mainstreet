suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(fs)
  library(fst)
  library(tigris)})
df <- read_fst("hidden/esri/cleaned_01.fst")
csv_list <- "hidden/esri/raw/csv_2018" %>% 
  dir_ls(regexp = "\\.csv$")
clean_esri_18 <- function(csv) {
  read_csv(csv) %>% 
    janitor::clean_names() %>% 
    rename(year = archive_version_year,
           jobs = employee_size_5_location,
           naics_code = primary_naics_code,
           naics = naics8_descriptions,
           address = address_line1,
           id = abi) %>% 
    mutate(jobs = as.numeric(jobs),
           mailing_address = paste0(address,", ",city,", ",state," ",zipcode),
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>% 
    select(id,year,company:zipcode,fips_code,latitude,longitude,jobs,naics_code,naics,match_code) %>%
    mutate(naics_code = str_sub(naics_code,end = -5)) %>% select(-naics,-zipcode)
}

for (i in csv_list) {
  st_yr <- i %>% 
    as.character() %>% str_to_lower() %>% 
    str_sub(start = -11, end = -5)
  nam <- paste0("esridf_",st_yr)
  assign(nam, clean_esri_18(i))
}

combined <- bind_rows(lapply((ls(pattern='esridf*')), get)) 

merged <- bind_rows((df %>% mutate(id = as.numeric(id))),combined) %>%
  arrange(year,state,city) %>% 
  write_fst("hidden/esri/cleaned_02.fst")