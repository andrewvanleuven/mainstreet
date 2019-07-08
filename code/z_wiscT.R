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
nLoadRows <- Inf

# Read in State Asmt ------------------------------------------------------
wi.zA.Main <- read_ztrax("ZTrans","Main","55")
Prop <- read_ztrax("ZTrans","PropertyInfo","55")

wi.zT.Main <- wi.zA.Main %>% 
  filter(PropertyUseStndCode %in% c("SR","RR"),
         SalesPriceAmount != 0) %>% 
  distinct() %>% 
  select(TransId:RecordingDate,DocumentTypeStndCode:EffectiveDate,
         SalesPriceAmount,SalesPriceAmountStndCode) %>% 
  write_csv("hidden/csv/wi_ZTMain.csv")
wi.zT.Prop <- Prop %>% 
  select(TransId,AssessorParcelNumber,PropertyCity,PropertyAddressLatitude,
         PropertyAddressLongitude,PropertyAddressCensusTractAndBlock,
         LoadID,ImportParcelID,BatchID) %>% distinct() %>% 
  write_csv("hidden/csv/wi_ZTProp.csv")

wisconsin <- left_join(wi.zT.Main,wi.zT.Prop,by = "TransId") %>% 
  write_csv("hidden/csv/wi_ZTrans.csv")


freqTab(wisconsin,"PropertyCity",90)



