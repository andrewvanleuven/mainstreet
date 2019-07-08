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
# ZTRAX Function ----------------------------------------------------------
ztrax_names <- function(ZType,TableType){
  dir <- "/Users/andrew/Downloads/ztrax"
  layoutname <- eval(as.name(paste0("layout",ZType)))
  utname <- sprintf('ut%s', TableType)
  layoutname %>% 
    filter(TableName == utname) %>% 
    pull(FieldName)
}
read_ztrax <- function(ZType,TableType,STFips){
  dir <- "/Users/andrew/Downloads/ztrax"
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
# Read In Data ------------------------------------------------------------
bff <- "/Users/andrew/Downloads/ztrax/ZAsmt/55/Main.txt"
kbff <- "/Users/andrew/Downloads/ztrax/ZAsmt/55/Building.txt"

Main <- ztrax_names("ZAsmt","Main")
Building <- ztrax_names("ZAsmt","Building")
nLoadRows <- Inf

# Roundabout Way to Read in 15 Mil+ in Main -------------------------------
wisc00 <- read_delim(file = bff, delim = "|", n_max = nLoadRows, quote = '"',comment = "", na = "", skip = 0, col_names = Main)
wisc.00 <- wisc00 %>%
  distinct(ImportParcelID, .keep_all = TRUE) %>%
  select(RowID,ImportParcelID,FIPS,State,County,
         AssessorParcelNumber,ParcelNumberTypeStndCode,
         RecordTypeStndCode,PropertyZoningDescription,
         PropertyZoningSourceCode,PropertyAddressLatitude,
         PropertyAddressLongitude,PropertyAddressCensusTractAndBlock)
rm(wisc00)
wisc01 <- read_delim(file = bff, delim = "|", n_max = nLoadRows, quote = '"',comment = "", na = "", skip = 5000000, col_names = Main)
wisc.01 <- wisc01 %>%
  distinct(ImportParcelID, .keep_all = TRUE) %>%
  select(RowID,ImportParcelID,FIPS,State,County,
         AssessorParcelNumber,ParcelNumberTypeStndCode,
         RecordTypeStndCode,PropertyZoningDescription,
         PropertyZoningSourceCode,PropertyAddressLatitude,
         PropertyAddressLongitude,PropertyAddressCensusTractAndBlock)
rm(wisc01)
wisc02 <- read_delim(file = bff, delim = "|", n_max = nLoadRows, quote = '"',comment = "", na = "", skip = 10000000, col_names = Main)
wisc.02 <- wisc02 %>%
  distinct(ImportParcelID, .keep_all = TRUE) %>%
  select(RowID,ImportParcelID,FIPS,State,County,
         AssessorParcelNumber,ParcelNumberTypeStndCode,
         RecordTypeStndCode,PropertyZoningDescription,
         PropertyZoningSourceCode,PropertyAddressLatitude,
         PropertyAddressLongitude,PropertyAddressCensusTractAndBlock)
rm(wisc02)
wisc.Main <- rbind(wisc.00,wisc.01,wisc.02) %>%
  distinct(ImportParcelID, .keep_all = TRUE) %>% 
  write_csv("hidden/csv/wiscMain.csv")

main1 <- read_csv("hidden/csv/wisc.Main.csv")
main2 <- read_csv("hidden/csv/wi.A.Main.csv")
wi.A.Main <- rbind(main1,main2) %>% distinct() %>% 
  write_csv("hidden/csv/wiMain.csv")

 # Resume Reading In -------------------------------------------------------
wisc00 <- read_delim(file = kbff, delim = "|", n_max = 5000000, quote = '"',comment = "", na = "", skip = 0, col_names = Building)
wisc.00 <- wisc00 %>%
  filter(str_detect(PropertyLandUseStndCode, 'RR')) %>%
  select(RowID, NoOfUnits, BuildingOrImprovementNumber, 
         YearBuilt, EffectiveYearBuilt, YearRemodeled,
         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
         HeatingTypeorSystemStndCode,TotalCalculatedBathCount,
         LoadID,PropertyLandUseStndCode) %>% distinct() %>%
  write_csv("hidden/csv/wisc00.csv")
rm(wisc00)
wisc01 <- read_delim(file = kbff, delim = "|", n_max = 5000000, quote = '"',comment = "", na = "", skip = 5000000, col_names = Building)
wisc.01 <- wisc01 %>%
  filter(str_detect(PropertyLandUseStndCode, 'RR')) %>%
  select(RowID, NoOfUnits, BuildingOrImprovementNumber, 
         YearBuilt, EffectiveYearBuilt, YearRemodeled,
         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
         HeatingTypeorSystemStndCode,TotalCalculatedBathCount,
         LoadID,PropertyLandUseStndCode) %>% distinct() %>%
  write_csv("hidden/csv/wisc01.csv")
rm(wisc01)
wisc02 <- read_delim(file = kbff, delim = "|", n_max = 5000000, quote = '"',comment = "", na = "", skip = 10000000, col_names = Building)
wisc.02 <- wisc02 %>%
  filter(str_detect(PropertyLandUseStndCode, 'RR')) %>%
  select(RowID, NoOfUnits, BuildingOrImprovementNumber, 
         YearBuilt, EffectiveYearBuilt, YearRemodeled,
         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
         HeatingTypeorSystemStndCode,TotalCalculatedBathCount,
         LoadID,PropertyLandUseStndCode) %>% distinct() %>%
  write_csv("hidden/csv/wisc02.csv")
rm(wisc02)
wisc03 <- read_delim(file = kbff, delim = "|", n_max = 5000000, quote = '"',comment = "", na = "", skip = 15000000, col_names = Building)
wisc.03 <- wisc03 %>%
  filter(str_detect(PropertyLandUseStndCode, 'RR')) %>%
  select(RowID, NoOfUnits, BuildingOrImprovementNumber, 
         YearBuilt, EffectiveYearBuilt, YearRemodeled,
         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
         HeatingTypeorSystemStndCode,TotalCalculatedBathCount,
         LoadID,PropertyLandUseStndCode) %>% distinct() %>%
  write_csv("hidden/csv/wisc03.csv")
rm(wisc03)
wisc04 <- read_delim(file = kbff, delim = "|", n_max = 5000000, quote = '"',comment = "", na = "", skip = 20000000, col_names = Building)
wisc.04 <- wisc04 %>%
  filter(str_detect(PropertyLandUseStndCode, 'RR')) %>%
  select(RowID, NoOfUnits, BuildingOrImprovementNumber, 
         YearBuilt, EffectiveYearBuilt, YearRemodeled,
         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
         HeatingTypeorSystemStndCode,TotalCalculatedBathCount,
         LoadID,PropertyLandUseStndCode) %>% distinct() %>%
  write_csv("hidden/csv/wisc04.csv")
 rm(wisc04)
wisc05 <- read_delim(file = kbff, delim = "|", n_max = 5000000, quote = '"',comment = "", na = "", skip = 25000000, col_names = Building)
wisc.Bldg <- rbind(wisc.00,wisc.01,wisc.02,wisc.03,wisc.04) %>% distinct()
write_csv(wisc.Bldg,"hidden/csv/wi.A.Bldg.csv")
#wi.zA.Bldg <- read_ztrax("ZAsmt","Building","55")
#wi.A.Bldg <- wi.zA.Bldg %>%
#  filter(str_detect(PropertyLandUseStndCode, 'RR')) %>%
#  select(RowID, NoOfUnits, BuildingOrImprovementNumber, 
#         YearBuilt, EffectiveYearBuilt, YearRemodeled,
#         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
#         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
#         HeatingTypeorSystemStndCode,TotalCalculatedBathCount,
#         LoadID,PropertyLandUseStndCode)  %>%
#  write_csv("hidden/csv/wi.A.Bldg.csv")
#rm(wi.zA.Bldg)


wi.zA.Sqft <- read_ztrax("ZAsmt","BuildingAreas","55")
wi.Sqft <- rbind(wi.zA.Sqft,wi.zA.Sqft)
wi.A.Sqft <- wi.Sqft %>%
  filter(BuildingAreaStndCode %in% c('BAL','BAF','BAE','BAG','BAJ','BAT','BLF')) %>%
  select(RowID,BuildingOrImprovementNumber,BuildingAreaSequenceNumber,
         BuildingAreaStndCode,BuildingAreaSqFt) %>%
  group_by(RowID) %>% mutate(sqfeet = max(BuildingAreaSqFt)) %>%
  select(-(BuildingAreaSequenceNumber:BuildingAreaSqFt)) %>%
  distinct() %>%
  write_csv("hidden/csv/wi.A.Sqft.csv")
rm(wi.zA.Sqft)
rm(wi.zA.Sqf2)
rm(wi.Sqft)



# Make Comparisons --------------------------------------------------------

wisc <- rbind(wisc00,wisc01) %>% unique()
rm(c(wisc00,wisc01))
comp <- arsenal::comparedf(wisc00,wisc,by = "RowID")
