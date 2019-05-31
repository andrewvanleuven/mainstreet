library(arsenal)
library(tidyverse)
library(tidycensus)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
# Frequency Table Function ------------------------------------------------
freqTab <- function(d,var) {
  df <- d %>% select(var)
  dft <- table(df)
  as.data.frame(dft) %>% 
    mutate(p = round((Freq/(sum(Freq)/100)), digits=1))%>%
    mutate(rank = rank(desc(p))) %>%
    filter(rank < 16) %>%
    arrange(desc(Freq)) %>%
    select(-rank) %>%
    `colnames<-`(c(var,"N","Percent"))
}
#freqTab(zIowaSales,"County")
# Layout Files -----------------------------------------------------
layoutZAsmt <- readxl::read_excel("hidden/Layout.xlsx", sheet = 1)
layoutZTrans <- readxl::read_excel("hidden/Layout.xlsx", sheet = 2)
dir <- "/Volumes/WD/ZTRAX"
# Import ZTRAX Function -----------------------------------------------------
read_ztrax <- function(ZType,TableType,STFips){
  filename <- sprintf("/Volumes/WD/ZTRAX/%s/%s/%s.txt", ZType, STFips, TableType)
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
         PropertyAddressLongitude,PropertyAddressCensusTractAndBlock)
  write_csv("hidden/csv/ia.A.Main.csv")
ia.A.Bldg <- ia.zA.Bldg %>%
  filter(str_detect(PropertyLandUseStndCode, 'RR')) %>%
  select(RowID, NoOfUnits, BuildingOrImprovementNumber, 
         YearBuilt, EffectiveYearBuilt, YearRemodeled,
         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
         HeatingTypeorSystemStndCode,TotalCalculatedBathCount,
         LoadID,PropertyLandUseStndCode)  %>%
  write_csv("hidden/csv/ia.A.Bldg.csv")
ia.A.Sqft <- ia.zA.Sqft %>%
  select(RowID,BuildingOrImprovementNumber,BuildingAreaSequenceNumber,
         BuildingAreaStndCode,BuildingAreaSqFt) %>%
  write_csv("hidden/csv/ia.A.Sqft.csv")


# Read in State Trans -----------------------------------------------------


# Join State Data ---------------------------------------------------------
st.join <- inner_join(., wi.properties, by = "TransId") %>%
  write_csv("hidden/csv/wisc.csv")

# Analyze the Data --------------------------------------------------------
freqTab(wi.Main,"AssessmentLandUseStndCode")


