suppressMessages({library(tidyverse)
  library(tidycensus)
  library(tigris)
  library(sf)
  library(magrittr)
  library(fst)
  library(inlmisc)
  library(RColorBrewer)
  library(rleuven)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)


# State Coverage ----------------------------------------------------------
us <- states(cb = T, resolution = "20m") %>% 
  mutate(msp = ifelse(STUSPS %in% c("IA","OH","WI","MI"), 1, 0)) %>% 
  filter(GEOID < 60, !STUSPS %in% c("AK","HI"))

ggplot() +
  geom_sf(data = us, aes(fill = factor(msp), color = factor(msp))) +
  scale_fill_manual(values = c("grey90","grey40")) +
  scale_color_manual(values = c("black","black")) +
  ggtitle("Main Street Program Data Coverage") +
  coord_sf(crs = st_crs(26916), ndiscr = 0) +
  guides(fill = F, color = F) +
  theme_void(base_size = 20, base_family = "LM Roman 10") +
  theme(plot.title = element_text(hjust = .5, face = "bold")) + 
  ggsave("plot/manuscripts/msp_states.png", height=6.5, width = 8)

# Downtown Map ------------------------------------------------------------
msp <- read_csv("data/csv/universe/analytical_universe.csv")
df <- read_fst("hidden/esri/cleaned_07.fst") %>% as_tibble() %>% 
  st_as_sf(., coords = c("longitude","latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 3174) %>% 
  select(id,year,city_fips,buffer,address:naics_code,longitude,latitude)
beepr::beep()
place_univ <- places(state = c('IA','MI','OH','WI'), cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(msp %>% select(1,2), by = "city_fips") %>% 
  st_transform(crs = 3174)
downtown_ia <- st_read("data/shp/google_earth/iowa.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_remove_all(name, ", IA"), st = "Iowa")
downtown_mi <- st_read("data/shp/google_earth/michigan.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(st = "Michigan")
downtown_oh <- st_read("data/shp/google_earth/ohio.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_replace(name, "Waverly City","Waverly")) %>% 
  mutate(st = "Ohio")
downtown_wi <- st_read("data/shp/google_earth/wisconsin.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(st = "Wisconsin")
downtown_sf <- rbind(downtown_ia,downtown_mi,downtown_oh,downtown_wi) %>% 
  inner_join(msp, by = c('name','st')) %>% 
  arrange(city_fips) %>% 
  st_transform(crs = 3174)
rm(downtown_ia,downtown_mi,downtown_oh,downtown_wi,msp)

biz_dotmap <- function(fips,year,colors){
  if(missing(colors)) {colors = "RdBu"}
  if(missing(year)) {year = 2005}
  n_yr <- year
  f_st <- str_sub(fips,end = 2)
  n_st <- states(cb = T, resolution = "20m") %>% filter(GEOID == f_st) %>% pull(STUSPS)
  ntwn <- downtown_sf %>% filter(city_fips == fips) %>% pull(name)
  titl <- sprintf("Business Establishments in %s, %s in %s", ntwn, n_st, n_yr)
  town <- place_univ %>% filter(city_fips == fips)
  cbd0 <- st_buffer((downtown_sf %>% filter(city_fips == fips)), 200, joinStyle = "MITRE", endCapStyle = "SQUARE")
  cbd1 <- st_intersection(town,st_buffer(cbd0, 1*400))
  cbd2 <- st_intersection(town,st_buffer(cbd0, 2*400))
  cbd3 <- st_intersection(town,st_buffer(cbd0, 3*400))
  cbd4 <- st_intersection(town,st_buffer(cbd0, 4*400))
  buf5 <- st_difference(town,cbd4) %>% select(name,geometry) %>% mutate(buffer = 5)
  buf4 <- st_difference(cbd4,cbd3) %>% select(name,geometry) %>% mutate(buffer = 4)
  buf3 <- st_difference(cbd3,cbd2) %>% select(name,geometry) %>% mutate(buffer = 3)
  buf2 <- st_difference(cbd2,cbd1) %>% select(name,geometry) %>% mutate(buffer = 2)
  buf1 <- st_difference(cbd1,cbd0) %>% select(name,geometry) %>% mutate(buffer = 1)
  buf0 <- cbd0 %>% select(name,geometry) %>% mutate(buffer = 0)
  bind <- rbind(buf5,buf4,buf3,buf2,buf1,buf0) %>% select(name,buffer,geometry)
  dots <- st_intersection(bind,(df %>% filter(year == n_yr)))
  pcty <- downtown_sf %>% filter(city_fips == fips) %>% pull(cty_fips)
  crds <- roads(state = str_sub(pcty,end = 2), county = as.character(str_sub(pcty,-3))) %>% st_transform(crs = 3174)
  road <- st_intersection(town,crds)
  tabl <- dots %>% group_by(buffer) %>% summarise(sum = n()) %>% pull(sum)
  nwth <- c("within downtown","within 1/4 mile","within 1/2 mile","within 3/4 mile","within 1 mile","beyond 1 mile")
  labs <- paste(tabl,nwth,sep = " ")
  clrs <- brewer.pal(7,colors)[2:7]
  ggplot() +
    geom_sf(data = bind, aes(fill = clrs), color = NA, alpha = 1) +
    geom_sf(data = road, size = .25, color = "black") +
    geom_sf(data = dots, shape = 21, color = "black", fill = "white", size = .75, stroke = .2) +
    geom_sf(data = town, alpha = 0, color = "black", size = .5) +
    scale_fill_manual(name = "# of Establishments\nin Each Buffer Zone",
                      values = rev(clrs),
                      labels = labs) +
    guides(fill = guide_legend(reverse = F,
                               override.aes = list(alpha = 1))) + 
    #ggtitle(titl, subtitle = "Concentric Buffers at Equal Distance Intervals from Downtown") +
    theme_void(base_family = "Helvetica") +
    theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5), 
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          legend.title=element_text(size=12 ,hjust = 0.5, face = "bold"),
          legend.text=element_text(size=12, family="Helvetica Light")) + 
    ggsave("plot/manuscripts/dot_buffers_edq.png", height = 7, width = 10)
    rm(n_yr,ntwn,titl,road,labs,nwth,tabl,clrs,town,cbd0,cbd1,cbd2,cbd3,cbd4,buf5,buf4,buf3,buf2,buf1,buf0,bind,fips,pcty,colors,crds,f_st,n_st)
}

biz_dotmap(fips = 2617020, year = 2000, colors = "Greys")