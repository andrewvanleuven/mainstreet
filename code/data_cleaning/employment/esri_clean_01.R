suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(fs)
  library(fst)
  library(tigris)})

csv_list <- "hidden/esri/raw" %>% 
  dir_ls(regexp = "\\.csv$")

clean_esri <- function(csv) {
  read_csv(csv) %>% 
    janitor::clean_names() %>% 
    rename(year = archive_version_year,
           jobs = employee_size_5_location,
           naics_code = primary_naics_code,
           naics = naics8_descriptions,
           address = address_line_1,
           id = abi) %>% 
    mutate(jobs = as.numeric(jobs),
           mailing_address = paste0(address,", ",city,", ",state," ",zip_code),
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>% 
    select(id,year,company:zip_code,fips_code,latitude,longitude,jobs,naics_code,naics,match_code) %>%
    mutate(naics_code = str_sub(naics_code,end = -5)) %>% select(-naics,-zip_code)
}

for (i in csv_list) {
  st_yr <- i %>% 
    as.character() %>% str_to_lower() %>% 
    str_sub(start = -11, end = -5)
  nam <- paste0("esridf_",st_yr)
  assign(nam, clean_esri(i))
}
combined <- bind_rows(lapply((ls(pattern='esridf*')), get)) %>% 
  write_fst("hidden/esri/cleaned_01.fst")
beepr::beep()

for (i in 1997:2017) {for (j in c('ia','mi','oh','wi')) {rm(list=paste("esridf",j,i,sep = "_"))}}
rm(i,j,nam,st_yr,csv_list)


