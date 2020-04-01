suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------
msp <- read_csv("data/csv/universe/analytical_universe.csv")
df <- read_fst("hidden/esri/cleaned_05.fst") %>% 
  mutate(unique_id = paste(id,year,sep = '_')) %>% 
  select(unique_id,everything()) %>% 
  st_as_sf(., coords = c("longitude","latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 3174)
beepr::beep()

# Build Downtown Geography ------------------------------------------------
#st_univ <- states(cb = T) %>% filter(STUSPS %in% c('IA','MI','OH','WI')) %>% 
#  st_transform(crs = 3174)
#cty_univ <- counties(state = c('IA','MI','OH','WI'), cb = T) %>% 
#  st_transform(crs = 3174)
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
  st_transform(crs = 3174) %>% 
  inner_join(msp, by = c('name','st')) %>% 
  arrange(city_fips)
#suggest_crs(downtown_sf)[1,3]
rm(downtown_ia,downtown_mi,downtown_oh,downtown_wi,msp)

# Loop through Iowa Municipalities -------------------------------------
df_ia <- df %>% filter(state == "IA") 
sf_ia <- downtown_sf %>% filter(st == "Iowa")
beepr::beep()

for (i in (sf_ia %>% pull(city_fips))) {
  muni <- sf_ia %>% filter(city_fips == i) %>% mutate(full_name = paste(name,st,sep = "_")) %>% 
    pull() %>% str_to_lower() %>% str_replace_all(" ", "_") %>%
    str_replace_all("'", "")%>% str_replace_all("\\.", "")
  town <- place_univ %>% filter(city_fips == i) %>% st_buffer(dist = 1500) %>% 
    st_transform(crs = 3174)
  cbd0 <- st_buffer((sf_ia %>% filter(city_fips == i) %>% select(1:3,geometry)), 200, 
                    joinStyle = "MITRE", endCapStyle = "SQUARE")
  cbd1 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 1*400))
  cbd2 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 2*400))
  cbd3 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 3*400))
  cbd4 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 4*400))
  buf5 <- st_difference(town,cbd4) %>% select(city_fips,geometry) %>% mutate(buffer = 5)
  buf4 <- st_difference(cbd4,cbd3) %>% select(city_fips,geometry) %>% mutate(buffer = 4)
  buf3 <- st_difference(cbd3,cbd2) %>% select(city_fips,geometry) %>% mutate(buffer = 3)
  buf2 <- st_difference(cbd2,cbd1) %>% select(city_fips,geometry) %>% mutate(buffer = 2)
  buf1 <- st_difference(cbd1,cbd0) %>% select(city_fips,geometry) %>% mutate(buffer = 1)
  buf0 <- cbd0 %>% select(city_fips,geometry) %>% mutate(buffer = 0)
  bind <- rbind(buf5,buf4,buf3,buf2,buf1,buf0) %>% select(city_fips,buffer,geometry)
  dots <- st_intersection(bind,(df_ia %>% select(unique_id))) %>% 
    st_drop_geometry() %>% 
    mutate(ubuff = paste(city_fips,buffer,sep = '_')) %>% 
    select(unique_id,buffer,ubuff)
  name <- paste0("loop_",muni)
  assign(name, dots)
  rm(town,cbd0,cbd1,cbd2,cbd3,cbd4,buf5,buf4,buf3,buf2,buf1,buf0,bind,dots,name,i,muni)
}
beepr::beep()

buffd_ia <- mget(ls(pattern="loop")) %>% bind_rows() %>% 
  write_csv("hidden/esri/cleaned_06_ia.csv")

for (i in (sf_ia %>% pull(city_fips))) {
  muni <- sf_ia %>% filter(city_fips == i) %>% mutate(full_name = paste(name,st,sep = "_")) %>% 
    pull() %>% str_to_lower() %>% str_replace_all(" ", "_") %>%
    str_replace_all("'", "")%>% str_replace_all("\\.", "")
  rm(list=paste0("loop_",muni))}
rm(df_ia,sf_ia,muni,i)

# Loop through Michigan Municipalities -------------------------------------
df_mi <- df %>% filter(state == "MI") 
sf_mi <- downtown_sf %>% filter(st == "Michigan")
beepr::beep()

for (i in (sf_mi %>% pull(city_fips))) {
  muni <- sf_mi %>% filter(city_fips == i) %>% mutate(full_name = paste(name,st,sep = "_")) %>% 
    pull() %>% str_to_lower() %>% str_replace_all(" ", "_") %>%
    str_replace_all("'", "")%>% str_replace_all("\\.", "")
  town <- place_univ %>% filter(city_fips == i) %>% st_buffer(dist = 1500) %>% 
    st_transform(crs = 3174)
  cbd0 <- st_buffer((sf_mi %>% filter(city_fips == i) %>% select(1:3,geometry)), 200, 
                    joinStyle = "MITRE", endCapStyle = "SQUARE")
  cbd1 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 1*400))
  cbd2 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 2*400))
  cbd3 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 3*400))
  cbd4 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 4*400))
  buf5 <- st_difference(town,cbd4) %>% select(city_fips,geometry) %>% mutate(buffer = 5)
  buf4 <- st_difference(cbd4,cbd3) %>% select(city_fips,geometry) %>% mutate(buffer = 4)
  buf3 <- st_difference(cbd3,cbd2) %>% select(city_fips,geometry) %>% mutate(buffer = 3)
  buf2 <- st_difference(cbd2,cbd1) %>% select(city_fips,geometry) %>% mutate(buffer = 2)
  buf1 <- st_difference(cbd1,cbd0) %>% select(city_fips,geometry) %>% mutate(buffer = 1)
  buf0 <- cbd0 %>% select(city_fips,geometry) %>% mutate(buffer = 0)
  bind <- rbind(buf5,buf4,buf3,buf2,buf1,buf0) %>% select(city_fips,buffer,geometry)
  dots <- st_intersection(bind,(df_mi %>% select(unique_id))) %>% 
    st_drop_geometry() %>% 
    mutate(ubuff = paste(city_fips,buffer,sep = '_')) %>% 
    select(unique_id,buffer,ubuff)
  name <- paste0("loop_",muni)
  assign(name, dots)
  rm(town,cbd0,cbd1,cbd2,cbd3,cbd4,buf5,buf4,buf3,buf2,buf1,buf0,bind,dots,name,i,muni)
}
beepr::beep()

buffd_mi <- mget(ls(pattern="_michigan")) %>% bind_rows() %>% 
  write_csv("hidden/esri/cleaned_06_mi.csv")

for (i in (sf_mi %>% pull(city_fips))) {
  muni <- sf_mi %>% filter(city_fips == i) %>% mutate(full_name = paste(name,st,sep = "_")) %>% 
    pull() %>% str_to_lower() %>% str_replace_all(" ", "_") %>%
    str_replace_all("'", "")%>% str_replace_all("\\.", "")
  rm(list=paste0("loop_",muni))}
rm(df_mi,sf_mi,muni,i)

# Loop through Ohio Municipalities -----------------------------------------
df_oh <- df %>% filter(state == "OH") 
sf_oh <- downtown_sf %>% filter(st == "Ohio")

for (i in (sf_oh %>% pull(city_fips))) {
  muni <- sf_oh %>% filter(city_fips == i) %>% mutate(full_name = paste(name,st,sep = "_")) %>% 
    pull() %>% str_to_lower() %>% str_replace_all(" ", "_") %>%
    str_replace_all("'", "")%>% str_replace_all("\\.", "")
  town <- place_univ %>% filter(city_fips == i) %>% st_buffer(dist = 1500) %>% 
    st_transform(crs = 3174)
  cbd0 <- st_buffer((sf_oh %>% filter(city_fips == i) %>% select(1:3,geometry)), 200, 
                    joinStyle = "MITRE", endCapStyle = "SQUARE")
  cbd1 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 1*400))
  cbd2 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 2*400))
  cbd3 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 3*400))
  cbd4 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 4*400))
  buf5 <- st_difference(town,cbd4) %>% select(city_fips,geometry) %>% mutate(buffer = 5)
  buf4 <- st_difference(cbd4,cbd3) %>% select(city_fips,geometry) %>% mutate(buffer = 4)
  buf3 <- st_difference(cbd3,cbd2) %>% select(city_fips,geometry) %>% mutate(buffer = 3)
  buf2 <- st_difference(cbd2,cbd1) %>% select(city_fips,geometry) %>% mutate(buffer = 2)
  buf1 <- st_difference(cbd1,cbd0) %>% select(city_fips,geometry) %>% mutate(buffer = 1)
  buf0 <- cbd0 %>% select(city_fips,geometry) %>% mutate(buffer = 0)
  bind <- rbind(buf5,buf4,buf3,buf2,buf1,buf0) %>% select(city_fips,buffer,geometry)
  dots <- st_intersection(bind,(df_oh %>% select(unique_id))) %>% 
    st_drop_geometry() %>% 
    mutate(ubuff = paste(city_fips,buffer,sep = '_')) %>% 
    select(unique_id,buffer,ubuff)
  name <- paste0("loop_",muni)
  assign(name, dots)
  rm(town,cbd0,cbd1,cbd2,cbd3,cbd4,buf5,buf4,buf3,buf2,buf1,buf0,bind,dots,name,i,muni)
  }
beepr::beep()

buffd_oh <- mget(ls(pattern="_ohio")) %>% bind_rows() %>% 
  write_csv("hidden/esri/cleaned_06_oh.csv")

for (i in (sf_oh %>% pull(city_fips))) {
  muni <- sf_oh %>% filter(city_fips == i) %>% mutate(full_name = paste(name,st,sep = "_")) %>% 
    pull() %>% str_to_lower() %>% str_replace_all(" ", "_") %>%
    str_replace_all("'", "")%>% str_replace_all("\\.", "")
  rm(list=paste0("loop_",muni))}
rm(df_oh,sf_oh,muni,i,finish)

# Loop through Wisconsin Municipalities -------------------------------------
df_wi <- df %>% filter(state == "WI") 
sf_wi <- downtown_sf %>% filter(st == "Wisconsin")
beepr::beep()

for (i in (sf_wi %>% pull(city_fips))) {
  muni <- sf_wi %>% filter(city_fips == i) %>% mutate(full_name = paste(name,st,sep = "_")) %>% 
    pull() %>% str_to_lower() %>% str_replace_all(" ", "_") %>%
    str_replace_all("'", "")%>% str_replace_all("\\.", "")
  town <- place_univ %>% filter(city_fips == i) %>% st_buffer(dist = 1500) %>% 
    st_transform(crs = 3174)
  cbd0 <- st_buffer((sf_wi %>% filter(city_fips == i) %>% select(1:3,geometry)), 200, 
                    joinStyle = "MITRE", endCapStyle = "SQUARE")
  cbd1 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 1*400))
  cbd2 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 2*400))
  cbd3 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 3*400))
  cbd4 <- st_intersection(town %>% select(-city_fips),st_buffer(cbd0, 4*400))
  buf5 <- st_difference(town,cbd4) %>% select(city_fips,geometry) %>% mutate(buffer = 5)
  buf4 <- st_difference(cbd4,cbd3) %>% select(city_fips,geometry) %>% mutate(buffer = 4)
  buf3 <- st_difference(cbd3,cbd2) %>% select(city_fips,geometry) %>% mutate(buffer = 3)
  buf2 <- st_difference(cbd2,cbd1) %>% select(city_fips,geometry) %>% mutate(buffer = 2)
  buf1 <- st_difference(cbd1,cbd0) %>% select(city_fips,geometry) %>% mutate(buffer = 1)
  buf0 <- cbd0 %>% select(city_fips,geometry) %>% mutate(buffer = 0)
  bind <- rbind(buf5,buf4,buf3,buf2,buf1,buf0) %>% select(city_fips,buffer,geometry)
  dots <- st_intersection(bind,(df_wi %>% select(unique_id))) %>% 
    st_drop_geometry() %>% 
    mutate(ubuff = paste(city_fips,buffer,sep = '_')) %>% 
    select(unique_id,buffer,ubuff)
  name <- paste0("loop_",muni)
  assign(name, dots)
  rm(town,cbd0,cbd1,cbd2,cbd3,cbd4,buf5,buf4,buf3,buf2,buf1,buf0,bind,dots,name,i,muni)
}
beepr::beep()

buffd_wi <- mget(ls(pattern="_wisconsin")) %>% bind_rows() %>% 
  write_csv("hidden/esri/cleaned_06_wi.csv")

for (i in (sf_wi %>% pull(city_fips))) {
  muni <- sf_wi %>% filter(city_fips == i) %>% mutate(full_name = paste(name,st,sep = "_")) %>% 
    pull() %>% str_to_lower() %>% str_replace_all(" ", "_") %>%
    str_replace_all("'", "")%>% str_replace_all("\\.", "")
  rm(list=paste0("loop_",muni))}
rm(df_wi,sf_wi,muni,i)

final <- rbind(buffd_ia,buffd_oh,buffd_mi,buffd_wi) %>% 
  write_fst("hidden/esri/cleaned_06.fst")
beepr::beep()


# Map Demo ----------------------------------------------------------------
##mun_bounds <- place_univ %>% filter(city_fips == i)
#clrs <- RColorBrewer::brewer.pal(7,"YlOrBr")[2:7]
#ggplot() +
#  geom_sf(data = buf5, fill = clrs[1], color = NA, alpha = 1) +
#  geom_sf(data = buf4, fill = clrs[2], color = NA, alpha = 1) +
#  geom_sf(data = buf3, fill = clrs[3], color = NA, alpha = 1) +
#  geom_sf(data = buf2, fill = clrs[4], color = NA, alpha = 1) +
#  geom_sf(data = buf1, fill = clrs[5], color = NA, alpha = 1) +
#  geom_sf(data = buf0, fill = clrs[6], color = NA, alpha = 1) +
#  geom_sf(data = dots %>% filter(str_detect(unique_id,"_2005")), color = "black", size = .35) +
#  geom_sf(data = mun_bounds, alpha = 0, color = "black", size = 1) +
#  theme_void() +
#  labs(title = "Business Establishments in Adrian, MI in 2005",
#       subtitle = "Demonstration of Why Buffers Should Extend Beyond Municipal Boundaries") +
#  theme(text = element_text(family = "Futura Medium"),
#        plot.title = element_text(hjust = 0.5, family="Futura Bold", size = 28),
#        plot.subtitle = element_text(hjust = 0.5, size = 16)) +
#  ggsave("plot/maps/buffering_demo2.png", height = 8.5, width = 11)
