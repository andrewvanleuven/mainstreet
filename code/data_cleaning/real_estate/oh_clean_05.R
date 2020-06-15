suppressMessages({
  library(tidyverse)
  library(sf)
  library(fst)
  library(beepr)
  library(crsuggest)
  library(janitor)
  library(units)
  library(mapboxapi)
  library(leaflet)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
#file.edit("~/.Renviron") # to copy/paste API key

# Read in data ------------------------------------------------------------
#df <- read_csv("hidden/datatree/cleaned/datatree_model.csv") %>% 
#  select(property_id,msp_at_sale,msp_lag1,msp_lag2,msp_lag3,msp_lag5,msp_accr,msp_affl,    
#         distance,in_downtown,address,city_name,st,city_fips,cz,rucc,cty_seat,population,  
#         sqft,lot_size,age,recent_remodel,bedrooms,baths,total_rooms,stories,                            
#         deck,garage,pool,brick,poor_condition,basement,sale_date,sale_price,real_sale_price,nbhood_pop:nbhood_income) %>% 
#  select(msp_at_sale,distance,in_downtown,sqft:nbhood_unempl) %>% 
#  select(-recent_remodel,-brick,-poor_condition,-sale_price,-sale_date,-nbhood_pop,-nbhood_pop_density,-nbhood_renting) %>%
#  filter(real_sale_price < 5000000,
#         lot_size < 10,
#         lot_size >= 0.001) 
#beep()
msp <- read_csv("data/csv/universe/oh_universe.csv") %>% filter(st == "Ohio")
towns <- msp %>% pull(name)
town_fips <- msp %>% #filter(!name %in% c("Portsmouth")) %>%
  pull(city_fips) 
downtown <- st_read("data/shp/google_earth/ohio.kml") %>% 
  st_transform(2834) %>% 
  rename_all(tolower) %>% 
  mutate(name = str_replace(name,"Waverly City","Waverly")) %>% 
  select(-description) %>% 
  st_buffer(.,200,joinStyle = "MITRE", endCapStyle = "SQUARE") %>% 
  filter(name %in% towns)
oh_places <- places(state = "39", cb = T) %>% 
  rename(city_fips = GEOID,
         name = NAME) %>% filter(city_fips %in% town_fips) %>% 
  select(city_fips,name) %>% st_transform(2834)
greys <- RColorBrewer::brewer.pal(5,"Greys")

# Map Data ----------------------------------------------------------------
i <- 3977588
town <- oh_places %>% filter(city_fips == i)
i_name <- town %>% pull(name)
muni <- oh_places %>% filter(city_fips == i) %>% pull(name) %>% str_to_lower() %>% 
  str_replace_all(" ", "_") %>% str_replace_all("'", "")%>% str_replace_all("\\.", "")
sf_downtown <- downtown %>% filter(name == i_name) %>% st_buffer(dist = 50) 
downtown_edge <- sf_downtown %>% st_centroid()
fat_town <- town %>% st_buffer(dist = 1500)
cnty <- counties("39", cb = T) %>% st_transform(2834) %>% 
  filter(st_intersects(x = ., y = town, sparse = FALSE)) %>% pull(NAME)
cty_rds <- roads("39",cnty) %>% st_transform(2834) %>% st_intersection(fat_town)
cty_h2o <- area_water("39",cnty) %>% st_transform(2834) %>% st_intersection(fat_town)

walk_05min <- mb_isochrone(downtown_edge,
                          profile = "walking",
                          time = 5)
walk_10min <- mb_isochrone(downtown_edge,
                          profile = "walking",
                          time = 10)
walk_15min <- mb_isochrone(downtown_edge,
                          profile = "walking",
                          time = 15)
walk_20min <- mb_isochrone(downtown_edge,
                          profile = "walking",
                          time = 20)
walk_25min <- mb_isochrone(downtown_edge,
                          profile = "walking",
                          time = 25)
walk_30min <- mb_isochrone(downtown_edge,
                          profile = "walking",
                          time = 30)
walk_45min <- mb_isochrone(downtown_edge,
                          profile = "walking",
                          time = 45)
walk_60min <- mb_isochrone(downtown_edge,
                          profile = "walking",
                          time = 60)
cpal <- rev(RColorBrewer::brewer.pal(9,"Oranges"))
colors <- c('5 Minute Walk' = cpal[2], '15 Minute Walk' = cpal[4],
            '30 Minute Walk' = cpal[6], '45 Minute Walk' = cpal[8])


ggplot() + 
  geom_sf(data = fat_town, fill = "white", color = NA) +
  geom_sf(data = town, fill = "grey90") +
  geom_sf(data = walk_45min, aes(fill = '45 Minute Walk'), alpha = 1, color = NA) +
  geom_sf(data = walk_30min, aes(fill = '30 Minute Walk'), alpha = 1, color = NA) +
  geom_sf(data = walk_15min, aes(fill = '15 Minute Walk'), alpha = 1, color = NA) +
  geom_sf(data = walk_05min, aes(fill = '5 Minute Walk'), alpha = 1, color = NA) +
  geom_sf(data = cty_h2o, color = greys[4], size = 0, fill = greys[4], alpha = .75) +
  geom_sf(data = cty_rds, color = greys[5], size = .5) +
  geom_sf(data = cty_rds %>% filter(RTTYP %in% c('U','I')), color = greys[5], size = 1) +
  geom_sf(data = cty_h2o, fill = "#AADAFF", color = "#AADAFF") +
  geom_sf(data = downtown_edge, color = "black", shape = 21, fill = "yellow", size = 3, stroke = 1) +
  theme_void() +
  scale_fill_manual(name = "",
                    breaks = c('5 Minute Walk','15 Minute Walk','30 Minute Walk','45 Minute Walk'),
                    values = colors,
                    guide = guide_legend(override.aes = list(shape = 19, size = 8))) +
  labs(title = 'Walking Distance from Downtown Center',
       subtitle = 'Troy, Ohio') +
  theme(legend.title = element_text(size=16, face="bold", hjust = 0.5),
        legend.text = element_text(size=16),
        plot.title = element_text(hjust = 0.5, size = 24, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        legend.key = element_rect(fill = NA, color = NA),
        text=element_text(family="LM Roman 10"),
        legend.position = "bottom") + 
  ggsave("plot/maps/walking_dist.png", height = 8, width = 12)

