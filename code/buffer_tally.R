library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
library(RColorBrewer)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
# Read in Iowa data -------------------------------------------------------
df <- read_csv("hidden/esri/iowa_geocoded.csv")
iowa_crs <- 32615
iowa_df <- df %>%
  #filter(year == 2000) %>% 
  #sample_n(.,10000) %>% 
  select(id_yr:st,x,y) %>% 
  st_as_sf(., coords = c("x","y"), crs = 4326) %>%
  st_transform(.,crs = iowa_crs) %>% 
  mutate(utm_E = sapply(geometry, "[[", 1),
         utm_N = sapply(geometry, "[[", 2))
iowa_towns <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa", rucc > 2, pop2010 < 30000) %>% 
  left_join(.,(places(state = "19", cb = T) %>% mutate(geoid = as.numeric(GEOID)) %>%
                 select(geoid,geometry) %>% st_transform(.,crs = iowa_crs)), by = "geoid") %>% 
  select(geoid,name,geometry) %>% st_as_sf(crs = iowa_crs)
iowa_sf <- st_read("data/shp/google_earth/Iowa.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_remove_all(name, ", IA")) %>% st_transform(.,crs = iowa_crs)  
iowa_downtowns <- st_intersection(iowa_towns,(iowa_sf %>% select(-name))) %>% 
  select(name,geometry) 
rm(iowa_sf)
iowa_roads <- map((counties("IA", cb = TRUE, resolution = "20m") %>% pull(COUNTYFP)), 
                  ~{roads(state = "19", county = .x)}) %>% rbind_tigris() %>% st_transform(.,crs = iowa_crs)  
beepr::beep()

# Loop for everything -----------------------------------------------------
#for (i in (iowa_downtowns %>% pull(name))) {
#  town <- (iowa_towns %>% select(-geoid)) %>% filter(name == i)
#  cbd0 <- st_buffer((iowa_downtowns %>% filter(name == i)), 200, joinStyle = "MITRE", endCapStyle = "SQUARE")
#  cbd1 <- st_intersection(town,st_buffer(cbd0, 1*400))
#  cbd2 <- st_intersection(town,st_buffer(cbd0, 2*400))
#  cbd3 <- st_intersection(town,st_buffer(cbd0, 3*400))
#  cbd4 <- st_intersection(town,st_buffer(cbd0, 4*400))
#  buf5 <- st_difference(town,cbd4) %>% select(name,geometry) %>% mutate(buffer = 5)
#  buf4 <- st_difference(cbd4,cbd3) %>% select(name,geometry) %>% mutate(buffer = 4)
#  buf3 <- st_difference(cbd3,cbd2) %>% select(name,geometry) %>% mutate(buffer = 3)
#  buf2 <- st_difference(cbd2,cbd1) %>% select(name,geometry) %>% mutate(buffer = 2)
#  buf1 <- st_difference(cbd1,cbd0) %>% select(name,geometry) %>% mutate(buffer = 1)
#  buf0 <- cbd0 %>% select(name,geometry) %>% mutate(buffer = 0)
#  bind <- rbind(buf5,buf4,buf3,buf2,buf1,buf0) %>% select(name,buffer,geometry)
#  dots <- st_intersection(bind,(iowa_df %>% select(-(id:st))))
#  name <- paste0("loop_",tolower(str_replace_all(i, fixed(" "), "_")))
#  assign(name, dots)}
#beepr::beep()
#
#iowa_bound <- mget(ls(pattern="loop")) %>% bind_rows() %>% arrange(name,buffer) %>% 
#  separate(id_yr, sep = "_", into = c("id","yr")) %>% mutate(id_yr = paste(id,yr,sep = "_")) %>%
#  select(id_yr,buffer,utm_E,utm_N,geometry) %>% write_csv("hidden/esri/buffer_tally.csv")
#
#for (i in towns) {rm(list=paste0("loop_",tolower(str_replace_all(i, fixed(" "), "_"))))}
#rm(town,cbd0,cbd1,cbd2,cbd3,cbd4,buf5,buf4,buf3,buf2,buf1,buf0,bind,dots,name,i)
#
#iowa_buffered <- inner_join((iowa_bound %>% st_drop_geometry()),df, by = "id_yr") %>% 
#  write_csv("hidden/esri/iowa_buffered.csv")

# Make some maps ----------------------------------------------------------
random_map <- function(colors){
  if(missing(colors)) {
    colors = sample((c("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd","PuBu",
                       "PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd",
                       "BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")),1)}
  n_yr <- sample(1997:2017, 1)
  ntwn <- sample_n(iowa_downtowns,1) %>% pull(name)
  titl <- sprintf("%s, Iowa in %s", ntwn, n_yr)
  town <- (iowa_towns %>% select(-geoid)) %>% filter(name == ntwn)
  cbd0 <- st_buffer((iowa_downtowns %>% filter(name == ntwn)), 200, joinStyle = "MITRE", endCapStyle = "SQUARE")
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
  dots <- st_intersection(bind,(iowa_df %>% filter(year == 2008) %>% select(-(id:st))))
  road <- st_intersection(town,iowa_roads)
  tabl <- dots %>% group_by(buffer) %>% summarise(sum = n()) %>% pull(sum)
  nwth <- c("within downtown","within 1/4 mile","within 1/2 mile","within 3/4 mile","within 1 mile","beyond 1 mile")
  labs <- paste(tabl,nwth,sep = " ")
  clrs <- brewer.pal(7,colors)[2:7]
  
  gplt <- ggplot() +
    geom_sf(data = buf5, fill = clrs[1], color = NA, alpha = 1) +
    geom_sf(data = buf4, fill = clrs[2], color = NA, alpha = 1) +
    geom_sf(data = buf3, fill = clrs[3], color = NA, alpha = 1) +
    geom_sf(data = buf2, fill = clrs[4], color = NA, alpha = 1) +
    geom_sf(data = buf1, fill = clrs[5], color = NA, alpha = 1) +
    geom_sf(data = buf0, fill = clrs[6], color = NA, alpha = 1) +
    geom_sf(data = road, size = .25, color = "black") +
    geom_point(data = dots, shape = 21, color = "black", fill = "white", size = .75, stroke = .2,
               aes(x = utm_E,
                   y = utm_N)) +
    geom_point(data = dots, shape = 21, color = "black", size = 4, alpha = 0, 
               aes(x = utm_E,y = utm_N,
                   fill = factor(buffer))) +
    geom_sf(data = town, alpha = 0, color = "black", size = 1) +
    theme_void() +
    scale_fill_manual(name = "# of Establishments\nin Each Buffer Zone",
                      values = rev(clrs),
                      labels = labs) +
    guides(fill = guide_legend(reverse = F,
                               override.aes = list(alpha = 1))) + 
    ggtitle(titl, subtitle = "Concentric Buffers at Equal Distance Intervals from Downtown") +
    theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5), 
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          text = element_text(family = "IBM Plex Mono")) + 
    ggsave("plot/buffertest.png", height = 7, width = 10)
  
  rm(n_yr,ntwn,titl,road,labs,nwth,tabl,clrs)
  rm(town,cbd0,cbd1,cbd2,cbd3,cbd4,buf5,buf4,buf3,buf2,buf1,buf0,bind,dots,name,i)
  gplt
}
#random_map("YlGnBu") #display.brewer.all()
random_map()

