suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(beepr)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------
msp <- read_csv("data/csv/universe/msp_universe.csv") %>%
  filter(rucc > 2 ,pop_2010 < 50000) %>% 
  select(city_fips,cz,msp,msp_yr)
df <- read_csv("data/csv/employment/jobs_panel.csv") %>% 
  inner_join(msp, by = "city_fips")

# Map the Universe --------------------------------------------------------
temp <- tempfile()
download.file("https://opendata.arcgis.com/datasets/6031c4fb8cac48649f2e0a98999d1248_0.kml",temp)
g_lakes <- st_read(temp) %>% st_transform(.,crs = 3174)
rm(temp)
univ_st <- states(cb = T) %>% filter(STUSPS %in% c('IA','MI')) %>% 
  st_transform(.,crs = 3174)
univ_cty <- counties(state = c('IA','MI','OH','WI'), cb = T) %>% 
  rename(cty_fips = GEOID) %>% 
  st_transform(.,crs = 3174)
univ_cz <- left_join(univ_cty,cbsaxw,by = "cty_fips") %>% 
  select(cty_fips,cz) %>% 
  st_transform(.,crs = 3174) %>% 
  st_dissolve(cz)
univ_town <- places(state = c('IA','MI','OH','WI'), cb = T) %>% 
  mutate(city_fips = as.numeric(GEOID)) %>% 
  st_transform(.,crs = 3174) %>% 
  inner_join(msp, by = "city_fips") %>% 
  select(city_fips,msp) %>% st_centroid()

ggplot() +
  geom_sf(data = g_lakes, fill = "#9dd3fd", color = "black") +
  geom_sf(data = univ_cty, alpha = 0, color = "gray", size = .3) +
  geom_sf(data = univ_st, color = NA, fill = "black", alpha = .1) +
  geom_sf(data = univ_cz, alpha = 0, color = "black") +
  geom_sf(data = univ_town, aes(fill = as.factor(msp)),
          shape = 21, size = 2, color = "black", alpha = .9) +
  scale_fill_manual(values = c('white','#bb0000'),
                    labels = c('Not Participating','Participating')) +
  labs(fill = "Main Street Program Status",
       title = "Analytical Universe for Dissertation Essay 1",
       subtitle = "Municipalities from Iowa, Michigan, Ohio, and Wisconsin") +
  theme_void() +
  theme(legend.position="bottom",
        text=element_text(family="Futura Medium",size = 18),
        legend.title = element_text(family="Futura Bold"),
        plot.title = element_text(hjust = 0.5, family="Futura Bold", size = 28),
        plot.subtitle = element_text(hjust = 0.5, size = 24)) +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  ggsave("plot/maps/msp_universe.png", width = 11, height = 8.5)
rm(g_lakes,univ_cty,univ_cz,univ_st,univ_town)

