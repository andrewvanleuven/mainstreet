library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
options(tigris_class = "sf")

# Read, Shrink, and Write Data (defunct)-----------------------------------
#remove <- c("zip4","county.code","area.code","idcode","primary.sic.code","yellow.page.code","industry.specific.first#.byte","year.established","office.size.code","company.holding.status","subsidiary.number","parent.number","parent#.actual.employee.size","parent.actual.sales.volume","parent.employee.size.code","parent.sales.volume.code","site#.number","address.type.indicator","cbsa.code","cbsa.level","csa.code","fips.code")
#for (i in 1997:2017) {
#  yr <- (str_sub((as.character(i)),-2))
#  csvname <- sprintf("hidden/esri/iowa/esri_ia%s.csv", yr)
#  csvsave <- sprintf("hidden/esri/iowa/esri_ia_%s.csv", i)
#  df <- read_csv(csvname) %>% rename_all(tolower) %>% 
#    select(-remove) %>% mutate(year = i, id = abi, 
#                               address = paste(address.line.1,city,state,zipcode, sep = " ")) %>% 
#    select(id,year,everything(),-abi) %>% write_csv(csvsave)
#  nam <- paste0("esri_",i)
#  assign(nam, df)
#  nm2 <- paste0("match_",i)
#  df2 <- freqTab(df, "match.code", maxvals = Inf, ties = T) %>% 
#    mutate(year = i)
#  assign(nm2, df2)
#}
#
#codemtch <- bind_rows(lapply((ls(pattern='match_*')), get)) 
#rm(list = ls(pattern='match_*'))
#codemtch %>% group_by(MATCH.CODE) %>% summarize(total = sum(N, na.rm=TRUE))
#beepr::beep()

# Shrink More, Bind, and Write --------------------------------------------
for (i in 1997:2017) {
  csvname <- sprintf("hidden/esri/iowa/esri_ia_%s.csv", i)
  df <- read_csv(csvname) %>% select(id:zipcode,address,match.code,x,y)
  nam <- paste0("esri_",i)
  assign(nam, df)}
rm(csvname,i,nam,df)

data <- rbind(esri_1997,esri_1998,esri_1999,esri_2000,esri_2001,esri_2002,esri_2003,
              esri_2004,esri_2005,esri_2006,esri_2007,esri_2008,esri_2009,esri_2010,
              esri_2011,esri_2012,esri_2013,esri_2014,esri_2015,esri_2016,esri_2017) 

rm(esri_1997,esri_1998,esri_1999,esri_2000,esri_2001,esri_2002,esri_2003,
   esri_2004,esri_2005,esri_2006,esri_2007,esri_2008,esri_2009,esri_2010,
   esri_2011,esri_2012,esri_2013,esri_2014,esri_2015,esri_2016,esri_2017)

parceldata <- data %>% filter(!match.code %in% c("2","4","X")) %>% write_csv("hidden/esri/iowa.csv")
to_geocode <- data %>% filter(match.code %in% c("2","4","X")) %>% write_csv("hidden/esri/ia_2geo.csv")

unique <- data %>% 
  mutate(po_box = ifelse(grepl("PO BOX", address), 1, 0),
         box = ifelse(grepl("BOX", address), 1, 0),
         mi = ifelse(grepl(" MI ", address), 1, 0),
         mile = ifelse(grepl(" MILE", address), 1, 0),
         exception = ifelse(address == "2528 MILE RD CHARLES CITY IA 50616", 1, 0),
         rr = ifelse(grepl("RR ", address), 1, 0),
         need_match = ifelse(match.code %in% c("2","4","X"), 1, 0),
         no_address = ifelse(is.na(address.line.1), 1, 0)) %>%
  select(id,address,match.code,po_box:no_address) %>% 
  arrange(address) %>% distinct() %>% 
  filter(need_match == 1, rr == 0, mi == 0,
         mile == 0 & exception == 0, no_address == 0) %>% 
  write_csv("data/csv/esri_geocode.csv") # 15.7 percent CANNOT be geocoded

#freqTab(unique,"po_box")
#freqTab(unique,"rr")
#freqTab(unique,"need_match")
#freqTab(unique,"no_address")
