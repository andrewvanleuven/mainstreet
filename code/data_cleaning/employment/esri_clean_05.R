suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(crsuggest)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data & subset state municipalities ------------------------------
msp <- read_csv("data/csv/universe/analytical_universe.csv")
df <- read_fst("hidden/esri/cleaned_04.fst")

# Iowa --------------------------------------------------------------------
ia_univ <- msp %>% filter(st == "Iowa")
ia_rucc <- ia_univ %>% select(cty_fips) %>% distinct() %>% pull()

df_ia <- df %>% filter(state == "IA", !is.na(longitude), !is.na(latitude), fips_code %in% ia_rucc)

#ia_ctys <- counties("19", cb = T) %>% 
#  st_transform(2794)
#suggest_crs(ia_ctys)[1,3]

ia_sf <- places(state = "19", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(ia_univ %>% select(1), by = "city_fips") %>% 
  st_transform(2794) %>% 
  st_buffer(dist = 1500) # a 1.5 KM buffer around the municipality (in case CBD is at edge of town)

ia_dots <- df_ia %>% #sample_n(10000) %>% 
  st_as_sf(., coords = c("longitude","latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 2794) %>% 
  st_intersection(.,ia_sf) %>% 
  st_drop_geometry() %>% 
  write_fst("hidden/esri/cleaned_05_ia.fst")
beepr::beep()

# Michigan --------------------------------------------------------------------
mi_univ <- msp %>% filter(st == "Michigan")
mi_rucc <- mi_univ %>% select(cty_fips) %>% distinct() %>% pull()

df_mi <- df %>% filter(state == "MI", !is.na(longitude), !is.na(latitude), fips_code %in% mi_rucc) 

#mi_ctys <- counties("26", cb = T) %>% 
#  st_transform(3078)
#suggest_crs(mi_ctys)[1,3]

mi_sf <- places(state = "26", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(mi_univ %>% select(1), by = "city_fips") %>% 
  st_transform(3078) %>% 
  st_buffer(dist = 1500) 

mi_dots <- df_mi %>% 
  st_as_sf(., coords = c("longitude","latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 3078) %>% 
  st_intersection(.,mi_sf) %>% 
  st_drop_geometry() %>% 
  write_fst("hidden/esri/cleaned_05_mi.fst")
beepr::beep()

# Ohio --------------------------------------------------------------------
oh_univ <- msp %>% 
  filter(st == "Ohio",
         !name %in% c("New Miami","Oakwood","Oxford","Sebring"))
oh_rucc <- oh_univ %>% select(cty_fips) %>% distinct() %>% pull()

df_oh <- df %>% filter(state == "OH", !is.na(longitude), !is.na(latitude), fips_code %in% oh_rucc)

oh_ctys <- counties("39", cb = T) %>% 
  st_transform(2834)
#suggest_crs(oh_ctys)[1,3]

oh_sf <- places(state = "39", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(oh_univ %>% select(1), by = "city_fips") %>% 
  st_transform(2834) %>% 
  st_buffer(dist = 1500) 

oh_dots <- df_oh %>% 
  st_as_sf(., coords = c("longitude","latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 2834) %>% 
  st_intersection(.,oh_sf) %>% 
  st_drop_geometry() %>% 
  write_fst("hidden/esri/cleaned_05_oh.fst")
beepr::beep()

# Wisconsin --------------------------------------------------------------------
wi_univ <- msp %>% filter(st == "Wisconsin")
wi_rucc <- wi_univ %>% select(cty_fips) %>% distinct() %>% pull()

df_wi <- df %>% filter(state == "WI", !is.na(longitude), !is.na(latitude), fips_code %in% wi_rucc) 

#wi_ctys <- counties("55", cb = T) %>% 
#  st_transform(3069)
#suggest_crs(wi_ctys)[1,3]

wi_sf <- places(state = "55", cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  select(city_fips,geometry) %>% 
  right_join(wi_univ %>% select(1), by = "city_fips") %>% 
  st_transform(3069) %>% 
  st_buffer(dist = 1500) 

wi_dots <- df_wi %>% 
  st_as_sf(., coords = c("longitude","latitude"), crs = 4326, remove = F) %>% 
  st_transform(.,crs = 3069) %>% 
  st_intersection(.,wi_sf) %>% 
  st_drop_geometry() %>% 
  write_fst("hidden/esri/cleaned_05_wi.fst")
beepr::beep()

# Join all dots -----------------------------------------------------------
all_dots <- rbind(ia_dots,mi_dots,oh_dots,wi_dots) %>% 
  write_fst("hidden/esri/cleaned_05.fst")

# Map Example -------------------------------------------------------------
ggplot() +
  geom_sf(data = ia_ctys, size = .25, alpha = 0, color = "black") +
  #geom_sf(data = ia_sf, fill = "blue", color = NA, alpha = .5) +
  geom_sf(data = ia_downtown_center, fill = "black", color = NA) +
  geom_sf(data = ia_dots, color = "#FFCD00", size = .05) +
  theme_void() +
  labs(title = "Example of Municipality Buffering Technique",
       subtitle = "Municipal Boundaries in Analytical Universe + 1-Mile Buffer Around Downtown",
       caption = "Sample: N = 4,437 (out of 10,000 arcross the state)") +
  theme(plot.caption = element_text(size = 12),
        text=element_text(family="Futura Medium"),
        plot.title = element_text(hjust = 0.5, family="Futura Bold", size = 28),
        plot.subtitle = element_text(hjust = 0.5, size = 16)) +
  ggsave("plot/maps/buffering_example.png", height = 8.5, width = 11)