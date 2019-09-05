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
rm(csvname,i,nam)

data <- rbind(esri_1997,esri_1998,esri_1999,esri_2000,esri_2001,esri_2002,esri_2003,
              esri_2004,esri_2005,esri_2006,esri_2007,esri_2008,esri_2009,esri_2010,
              esri_2011,esri_2012,esri_2013,esri_2014,esri_2015,esri_2016,esri_2017) %>%
  filter(match.code %in% c("2","4","X")) %>% write_csv("hidden/esri/iowa.csv")

unique <- data %>% select(id,address,match.code) %>% arrange(address) %>% 
  distinct() %>% #write_csv("hidden/esri/geocode_iowa.csv")
  mutate(po_box = ifelse(grepl("PO BOX", address), 1, 0))

freqTab(unique,"po_box")