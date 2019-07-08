library(tidycensus)
library(tidyverse)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
options(scipen = 999)
options(stringsAsFactors = FALSE)
# Layout Files -----------------------------------------------------
layoutZAsmt <- readxl::read_excel("hidden/Layout.xlsx", sheet = 1)
layoutZTrans <- readxl::read_excel("hidden/Layout.xlsx", sheet = 2)

# Import ZTRAX Function -----------------------------------------------------
read_ztrax <- function(ZType,TableType,STFips){
  dir <- "/Volumes/WD/ZTRAX"
  filename <- sprintf("%s/%s/%s/%s.txt", dir, ZType, STFips, TableType)
  layoutname <- eval(as.name(paste0("layout",ZType)))
  utname <- sprintf('ut%s', TableType)
  cnames <- layoutname %>% 
    filter(TableName == utname) %>% 
    pull(FieldName)
  read_delim(file = filename,
             delim = "|",
             n_max = nLoadRows,
             quote = '"',
             comment = "",
             na = "",
             col_names = cnames)
}
nLoadRows <- Inf

# Read in State Asmt ------------------------------------------------------
ia.zA.Main <- read_ztrax("ZAsmt","Main","19")
ia.zA.Bldg <- read_ztrax("ZAsmt","Building","19")
ia.zA.Sqft <- read_ztrax("ZAsmt","BuildingAreas","19")
# Filter out Rows and Variable --------------------------------------------
ia.A.Main <- ia.zA.Main %>%
  distinct(ImportParcelID, .keep_all = TRUE) %>%
  select(RowID,ImportParcelID,FIPS,State,County,
         AssessorParcelNumber,ParcelNumberTypeStndCode,
         RecordTypeStndCode,PropertyZoningDescription,
         PropertyZoningSourceCode,PropertyAddressLatitude,
         PropertyAddressLongitude,PropertyAddressCensusTractAndBlock) %>%
  write_csv("hidden/csv/ia.A.Main.csv")
rm(ia.zA.Main)
ia.A.Bldg <- ia.zA.Bldg %>%
  filter(str_detect(PropertyLandUseStndCode, 'RR')) %>%
  select(RowID, NoOfUnits, BuildingOrImprovementNumber, 
         YearBuilt, EffectiveYearBuilt, YearRemodeled,
         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
         HeatingTypeorSystemStndCode,TotalCalculatedBathCount,
         LoadID,PropertyLandUseStndCode)  %>%
  write_csv("hidden/csv/ia.A.Bldg.csv")
rm(ia.zA.Bldg)
ia.A.Sqft <- ia.zA.Sqft %>%
  filter(BuildingAreaStndCode %in% c('BAL','BAF','BAE','BAG','BAJ','BAT','BLF')) %>%
  select(RowID,BuildingOrImprovementNumber,BuildingAreaSequenceNumber,
         BuildingAreaStndCode,BuildingAreaSqFt) %>%
  group_by(RowID) %>% mutate(sqfeet = max(BuildingAreaSqFt)) %>%
  select(-(BuildingAreaSequenceNumber:BuildingAreaSqFt)) %>%
  distinct() %>%
  write_csv("hidden/csv/ia.A.Sqft.csv")
rm(ia.zA.Sqft)
ia.A <- inner_join(ia.A.Main, ia.A.Bldg, by = "RowID") %>%
  inner_join(.,ia.A.Sqft, by = c("RowID", "BuildingOrImprovementNumber")) %>%
  distinct() %>%
  write_csv("hidden/csv/ia_ZAsmt.csv")

# Read in State Asmt ------------------------------------------------------
wi.zA.Main <- read_ztrax("ZAsmt","Main","55")
wi.zA.Bldg <- read_ztrax("ZAsmt","Building","55")
wi.zA.Sqft <- read_ztrax("ZAsmt","BuildingAreas","55")
# Filter out Rows and Variable --------------------------------------------
wi.A.Main <- wi.zA.Main %>%
  distinct(ImportParcelID, .keep_all = TRUE) %>%
  select(RowID,ImportParcelID,FIPS,State,County,
         AssessorParcelNumber,ParcelNumberTypeStndCode,
         RecordTypeStndCode,PropertyZoningDescription,
         PropertyZoningSourceCode,PropertyAddressLatitude,
         PropertyAddressLongitude,PropertyAddressCensusTractAndBlock) %>%
  write_csv("hidden/csv/wi.A.Main.csv")
rm(wi.zA.Main)
wi.A.Bldg <- wi.zA.Bldg %>%
  filter(str_detect(PropertyLandUseStndCode, 'RR')) %>%
  select(RowID, NoOfUnits, BuildingOrImprovementNumber, 
         YearBuilt, EffectiveYearBuilt, YearRemodeled,
         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
         HeatingTypeorSystemStndCode,TotalCalculatedBathCount,
         LoadID,PropertyLandUseStndCode)  %>%
  write_csv("hidden/csv/wi.A.Bldg.csv")
rm(wi.zA.Bldg)
wi.A.Sqft <- wi.zA.Sqft %>%
  filter(BuildingAreaStndCode %in% c('BAL','BAF','BAE','BAG','BAJ','BAT','BLF')) %>%
  select(RowID,BuildingOrImprovementNumber,BuildingAreaSequenceNumber,
         BuildingAreaStndCode,BuildingAreaSqFt) %>%
  group_by(RowID) %>% mutate(sqfeet = max(BuildingAreaSqFt)) %>%
  select(-(BuildingAreaSequenceNumber:BuildingAreaSqFt)) %>%
  distinct() %>%
  write_csv("hidden/csv/wi.A.Sqft.csv")
rm(wi.zA.Sqft)
wi.A <- inner_join(wi.A.Main, wi.A.Bldg, by = "RowID") %>%
  inner_join(.,wi.A.Sqft, by = c("RowID", "BuildingOrImprovementNumber")) %>%
  distinct() %>%
  write_csv("hidden/csv/wi_ZAsmt.csv")
# Read in State Trans -----------------------------------------------------

# Join State Data ---------------------------------------------------------
st.join <- inner_join(., wi.properties, by = "TransId") %>%
  write_csv("hidden/csv/wisc.csv")

# Analyze the Data --------------------------------------------------------
freqTab(ia.A.Sqft,"BuildingAreaStndCode")
  
