library(tidyverse)
library(rleuven)

wi.A.Main <- read_csv("hidden/csv/wi.A.Main.csv")
wi.A.Bldg <- read_csv("hidden/csv/wi.A.Bldg.csv")
wi.A.Sqft <- read_csv("hidden/csv/wi.A.Sqft.csv")
wis.xwalk <- read_csv("hidden/csv/wi_censblk.csv") %>%
  mutate(PropertyAddressCensusTractAndBlock = paste0(county,tract,block))

wi.A <- inner_join(wi.A.Main, wi.A.Bldg, by = "RowID") %>%
  inner_join(.,wi.A.Sqft, by = c("RowID", "BuildingOrImprovementNumber")) %>%
  distinct() %>%
  write_csv("hidden/csv/wi_ZAsmt.csv")

freqTab(wi.A,"County")

