suppressMessages({
  library(tidyverse)
  library(sf)
  library(rleuven)
  library(crsuggest)
  library(tidycensus)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
trim_census <- dget("code/functions/trim_census.R")

# Get Data ----------------------------------------------------------------
univ <- read_csv("data/csv/universe/universe.csv") %>% filter(st == "Ohio")
xw <- read_csv("data/csv/universe/xw.csv") %>% 
  mutate(st = toupper(st)) %>% 
  filter(st %in% c("IA","MI","OH","WI")) %>% 
  select(1,2,7)
rucc <- read_csv("data/csv/universe/rucc.csv") %>% 
  select(cty_fips,rucc)
cz <- read_csv("data/csv/universe/cz.csv") %>% 
  rename(cz = cz2000) %>% 
  select(cty_fips,cz)
historical <- read_csv("data/csv/universe/historical.csv") %>% 
  select(-pop_2010)

# Create Universe ---------------------------------------------------------

ohio_univ <- get_decennial(geography = "place", 
                           state = "OH",  # Just Ohio
                           variables = "P001001", 
                           year = 2010) %>%
  separate(NAME,c("name","ST"),sep = ",") %>%
  rename_all(tolower) %>% 
  trim_census() %>% 
  mutate(city_fips = as.numeric(geoid)) %>% 
  select(-variable,-geoid) %>% 
  rename(pop_2010 = value) %>% 
  left_join(xw, by = "city_fips") %>%     # Attaches county to city/place ID
  left_join(rucc, by = "cty_fips") %>%     # Attaches rural-urban continuum code
  left_join(cz, by = "cty_fips") %>%     # Attaches commute zone ID
  filter(afact > 0.5,    # NO urban-rural county filter
         pop_2010 > 750 & pop_2010 < 75000) %>%     # Current population filter
  select(city_fips,name,st,cty_fips,cz,rucc,pop_2010,-afact) %>% 
  inner_join(historical, by = c("name","st")) %>% 
  filter(pop_1940 > 999 | pop_1930 > 999 | pop_1920 > 999) %>%     # Historical population filter
  select(1:7) %>% 
  distinct() %>% 
  arrange(city_fips) 


# Buffer from MSA Principal Cities ----------------------------------------

univ_list <- univ %>% pull(name)
univ_cities <- ohio_univ %>% pull(name)

big_cities <- c("Akron","Canton","Massillon","Cincinnati","Cleveland","Elyria","Columbus",
                "Dayton","Kettering","Toledo","Boardman","Warren","Youngstown")

oh_urban <- places("39", cb = T) %>% 
  filter(NAME %in% big_cities) %>% 
  st_transform(crs = 2834) %>% 
  st_centroid_xy() %>% st_drop_geometry() %>% 
  select(5,6,10,11) %>% 
  st_as_sf(., coords = c("x","y"), crs = 2834, remove = F) 

oh_univers <- places("39", cb = T) %>% 
  filter(NAME %in% univ_cities) %>% 
  st_transform(crs = 2834) %>% 
  st_centroid_xy() %>% st_drop_geometry() %>% 
  select(5,6,10,11) %>% 
  st_as_sf(., coords = c("x","y"), crs = 2834, remove = F) 

oh_universe <- places("39", cb = T) %>% 
  filter(NAME %in% univ_list) %>% 
  st_transform(crs = 2834) %>% 
  st_centroid_xy() %>% st_drop_geometry() %>% 
  select(5,6,10,11) %>% 
  st_as_sf(., coords = c("x","y"), crs = 2834, remove = F) 

ohio <- counties("39", cb = T) %>% 
  st_transform(crs = 2834) 

urban_buff <- st_buffer(oh_urban,28968.2) %>% 
  st_dissolve() %>% 
  st_transform(crs = 2834) 

non_urban_univ <- st_difference(oh_univers,urban_buff)

non_urban_list <- non_urban_univ %>% pull(NAME)

notable <- non_urban_univ %>% 
  filter(NAME %in% c("Circleville","Chardon","Painesville","Newark","Delaware","Piqua","Troy"))

ggplot() +
  geom_sf(data = ohio, fill = "white") +
  #geom_sf(data = urban_buff, fill = "yellow") +
  #geom_point(data = oh_urban, aes(x,y), color = "red") +
  geom_point(data = oh_universe, aes(x,y), color = "blue") +
  geom_point(data = non_urban_univ, aes(x,y), color = "blue") +
  ggrepel::geom_label_repel(data = notable, aes(x,y,label = NAME)) +
  theme_void()

ohio_universe <- ohio_univ %>% 
  filter(name %in% non_urban_list | name %in% univ_list,
         !name %in% c("Oakwood")) %>% 
  mutate(city_fips = as.numeric(str_replace(city_fips,"3981942","3981935")),
         name = str_replace(name,"Waverly City","Waverly")) %>% 
  write_csv("data/csv/universe/oh_universe.csv")

# Merge with Existing Universe --------------------------------------------

non_oh_univ <- read_csv("data/csv/universe/universe.csv") %>% 
  filter(st != "Ohio")

univ_merge <- ohio_universe %>% 
  filter(!name %in% c("New Miami","Oxford","Sebring")) %>% 
  bind_rows(.,non_oh_univ) %>% 
  arrange(st,name) %>% 
  write_csv("data/csv/universe/analytical_universe.csv")

