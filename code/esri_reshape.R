library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
options(tigris_class = "sf")
iowa_crs <- "+proj=lcc +lat_1=43.26666666666667 +lat_2=42.06666666666667 +lat_0=41.5 +lon_0=-93.5 +x_0=1500000 +y_0=999999.9999898402 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

# Shrink, Bind, and Write -------------------------------------------------
for (i in 1997:2017) {
  csvname <- sprintf("hidden/esri/iowa/esri_ia_%s.csv", i)
  df <- read_csv(csvname) %>% select(id:zipcode,address,match.code,x,y) %>% 
    mutate(id_yr = paste(id,year,sep = "_"))
  nam <- paste0("esri_",i)
  assign(nam, df)}
rm(csvname,i,nam,df)

data <- rbind(esri_1997,esri_1998,esri_1999,esri_2000,esri_2001,esri_2002,esri_2003,
              esri_2004,esri_2005,esri_2006,esri_2007,esri_2008,esri_2009,esri_2010,
              esri_2011,esri_2012,esri_2013,esri_2014,esri_2015,esri_2016,esri_2017) 

rm(esri_1997,esri_1998,esri_1999,esri_2000,esri_2001,esri_2002,esri_2003,
   esri_2004,esri_2005,esri_2006,esri_2007,esri_2008,esri_2009,esri_2010,
   esri_2011,esri_2012,esri_2013,esri_2014,esri_2015,esri_2016,esri_2017)

# New Geocoded Points -----------------------------------------------------
df <- st_read("data/shp/cura/a0000000c.gdbtable") %>% rename_all(tolower) 

ia_gcode <- df %>% rename(geometry = shape) %>%
  filter(score > 2) %>% 
  select(uniqid, match_type, match_addr, geometry) %>% 
  mutate(x = sapply(geometry, "[[", 1), 
         y = sapply(geometry, "[[", 2)) %>% 
  st_as_sf(., coords = c("x","y"), crs = iowa_crs)

# Maps --------------------------------------------------------------------
iowa_places <- places(state = "19", cb = T) %>% 
  mutate(geoid = as.numeric(GEOID)) %>% select(geoid,geometry)
iowa_towns <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa", rucc > 2, pop2010 < 30000) %>% 
  left_join(.,iowa_places, by = "geoid") %>% 
  st_as_sf()  %>% select(geoid:rucc,geometry) %>% st_set_crs(iowa_crs)
iowa <- states(cb = T) %>% filter(STATEFP == "19")

ggplot() + geom_sf(data = iowa) +
  #geom_sf(data = iowa_towns, color = NA,fill = "yellow") +
  geom_point(data = (sample_frac(ia_gcode,.1)), size = 0.05,
             aes(x=x, y=y), color = "magenta") +
  theme_void() +
  ggsave("plot/cura.png", width = 20, height = 16)

# Graveyard ---------------------------------------------------------------
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

#unique <- data %>% 
#  mutate(po_box = ifelse(grepl("PO BOX", address), 1, 0),
#         box = ifelse(grepl("BOX", address), 1, 0),
#         mi = ifelse(grepl(" MI ", address), 1, 0),
#         mile = ifelse(grepl(" MILE", address), 1, 0),
#         rr = ifelse(grepl("RR ", address), 1, 0),
#         need_match = ifelse(match.code %in% c("2","4","X"), 1, 0),
#         no_address = ifelse(is.na(address.line.1), 1, 0)) %>%
#  select(id,address.line.1:address,match.code,po_box:no_address) %>% 
#  arrange(address) %>% distinct() %>% 
#  write_csv("hidden/esri_geocode.csv") # 15.7 percent CANNOT be geocoded
#
#freqTab(unique,"id")

#freqTab(unique,"po_box")
#freqTab(unique,"rr")
#freqTab(unique,"need_match")
#freqTab(unique,"no_address")

#parceldata <- data %>% filter(!match.code %in% c("2","4","X")) %>% write_csv("hidden/esri/iowa.csv")
#to_geocode <- data %>% filter(match.code %in% c("2","4","X")) %>% write_csv("hidden/esri/ia_2geo.csv")
#
#data_yr <- data %>% mutate(id_yr = paste(id,year,sep = "_")) %>% 
#  select(id_yr,id,year,address.line.1:zipcode,match.code,x,y,address) %>%
#  mutate(po_box = ifelse(grepl("PO BOX", address), 1, 0),
#         box = ifelse(grepl("BOX", address), 1, 0),
#         mi = ifelse(grepl(" MI ", address), 1, 0),
#         mile = ifelse(grepl(" MILE", address), 1, 0),
#         rr = ifelse(grepl("RR ", address), 1, 0),
#         need_match = ifelse(match.code %in% c("2","4","X"), 1, 0),
#         no_address = ifelse(is.na(address.line.1), 1, 0)) %>%
#  arrange(id_yr) %>% select(-address) %>%
#  write_csv("hidden/all_unique.csv") %>% select(-id_yr,year) %>%
#  filter(match.code %in% c("2","4","X")) %>% distinct() %>% 
#  write_csv("hidden/to_geocode.csv")
