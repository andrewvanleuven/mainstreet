suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(fs)
  library(data.table)
  library(fst)
  library(tigris)})
# Data for 1997 to 2017 ---------------------------------------------------
csv_list <- "hidden/esri/raw" %>% dir_ls(regexp = "\\.csv$")
clean_esri <- function(csv) {
  fread(csv) %>% 
    janitor::clean_names() %>% 
    rename(year = archive_version_year,
           jobs = employee_size_5_location,
           naics_code = primary_naics_code,
           naics = naics8_descriptions,
           address = address_line_1,
           zip = zip_code,
           id = abi) %>% 
    mutate(jobs = as.numeric(jobs),
           mailing_address = paste0(address,", ",city,", ",state," ",zip),
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>% 
    select(id,year,company:zip,fips_code,latitude,longitude,jobs,naics_code,naics,match_code) %>%
    mutate(naics_code = str_sub(naics_code,end = -5)) %>% select(-naics)
}
for (i in csv_list) {
  st_yr <- i %>% 
    as.character() %>% str_to_lower() %>% 
    str_sub(start = -11, end = -5)
  nam <- paste0("esridf_",st_yr)
  assign(nam, clean_esri(i))
}

# Data for 2018 -----------------------------------------------------------
csv_list_18 <- "hidden/esri/raw/csv_2018" %>% dir_ls(regexp = "\\.csv$")
clean_esri_18 <- function(csv) {
  fread(csv) %>% 
    janitor::clean_names() %>% 
    rename(year = archive_version_year,
           jobs = employee_size_5_location,
           naics_code = primary_naics_code,
           naics = naics8_descriptions,
           zip = zipcode,
           address = address_line1,
           id = abi) %>% 
    mutate(jobs = as.numeric(jobs),
           mailing_address = paste0(address,", ",city,", ",state," ",zip),
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>% 
    select(id,year,company:zip,fips_code,latitude,longitude,jobs,naics_code,naics,match_code) %>%
    mutate(naics_code = str_sub(naics_code,end = -5)) %>% select(-naics)
}
for (i in csv_list_18) {
  st_yr <- i %>% 
    as.character() %>% str_to_lower() %>% 
    str_sub(start = -11, end = -5)
  nam <- paste0("esridf_",st_yr)
  assign(nam, clean_esri_18(i))
}

beepr::beep()

combined <- bind_rows(lapply((ls(pattern='esridf*')), get)) %>% 
  write_fst("hidden/esri/cleaned_01.fst")

for (i in 1997:2018) {for (j in c('ia','mi','oh','wi')) {rm(list=paste("esridf",j,i,sep = "_"))}}
rm(i,j,nam,st_yr,csv_list,csv_list_18,clean_esri,clean_esri_18)


