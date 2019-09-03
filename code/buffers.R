library(tigris)
library(sf)
library(tidyverse)
library(rleuven)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Map Projections ---------------------------------------------------------
ia_north <- "+proj=lcc +lat_1=43.26666666666667 +lat_2=42.06666666666667 +lat_0=41.5 +lon_0=-93.5 +x_0=1500000 +y_0=999999.9999898402 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
ia_south <- "+proj=lcc +lat_1=41.78333333333333 +lat_2=40.61666666666667 +lat_0=40 +lon_0=-93.5 +x_0=500000.00001016 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

# Read in KMZ -------------------------------------------------------------
iowa_msp <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa", rucc > 2, pop2010 < 30000) 
iowa_sf <- st_read("data/shp/google_earth/Iowa.kml") %>% 
  rename_all(tolower) %>% select(-description) %>% 
  mutate(name = str_remove_all(name, ", IA")) %>% 
  inner_join(.,iowa_msp) %>% arrange(geoid) %>% select(-(jobs_02:jobs_15)) %>% 
  st_transform(.,ia_north)

# New Geography -----------------------------------------------------------
iowa_muncpl <- places("ia", cb = T) %>% 
  mutate(GEOID = as.numeric(GEOID)) %>% select(-NAME) %>% 
  rename_all(tolower) %>% right_join(., iowa_msp, by = "geoid") %>% 
  arrange(geoid) %>% 
  st_transform(.,ia_north)
iowa_cty <- counties("ia", cb = T) %>%
  rename(cty_fips = GEOID) %>% 
  left_join((cbsaxw %>% select(cty_fips,rucc,cz))) %>% 
  st_transform(.,ia_north)
iowa <- states(cb = T) %>% filter(STATEFP == "19") %>% 
  st_transform(.,ia_north)
iowa_roads <- map((counties("IA", cb = TRUE, resolution = "20m") %>% pull(COUNTYFP)), 
                  ~{roads(state = "19", county = .x)}) %>% 
  rbind_tigris() %>% st_transform(.,ia_north)

# Basic Plot --------------------------------------------------------------
ggplot() + 
  geom_sf(data = iowa_cty, alpha = 0) + 
  geom_sf(data = (st_dissolve(iowa_cty,cz)),
          color = "black", size = 1,
          aes(fill = factor(cz)), alpha = .5) +
  geom_sf(data = iowa_muncpl,
          color = NA, fill = "black") + 
  geom_sf(data = iowa, color = "black", alpha = 0, size = 2) +
  scale_fill_manual(values = rand_ncolors(st_dissolve(iowa_cty,cz)), guide = FALSE) + 
  theme_void()

# Single City Plot --------------------------------------------------------
one <- iowa_muncpl %>% filter(name == "Sanborn")
one_sf <- st_buffer(iowa_sf %>% filter(name == "Sanborn"),200,
                    joinStyle = "MITRE", endCapStyle = "SQUARE")
one_buf0 <- st_intersection(one,(st_buffer(one_sf, 50)))
one_buf1 <- st_intersection(one,(st_buffer(one_sf, 1320)))
one_buf2 <- st_intersection(one,(st_buffer(one_sf, 2*1320)))
one_buf3 <- st_intersection(one,(st_buffer(one_sf, 3*1320)))
one_buf4 <- st_intersection(one,(st_buffer(one_sf, 4*1320)))
one_rds <- st_intersection(one,iowa_roads)
ggplot() + 
  geom_sf(data = one) + 
  geom_sf(data = one_buf4, fill = "#e31a1c",
          alpha = 0.25, lwd = 0) +
  geom_sf(data = one_buf3, fill = "#fd8d3c",
          alpha = 0.5, lwd = 0) +
  geom_sf(data = one_buf2, fill = "#fecc5c",
          alpha = 0.5, lwd = 0) +
  geom_sf(data = one_buf1, fill = "#ffffb2",
          alpha = 0.5, lwd = 0) +
  geom_sf(data = one_rds, color = "black",
          size = .2) + 
  geom_sf(data = one_buf0, fill = "black",
          alpha = 0.25, color = "black") + 
  theme_void() +
  ggtitle("Sanborn, Iowa",
          subtitle = "Concentric Buffers at 1/4, 1/2, 3/4, and 1 mile intervals from downtown") +
  theme(plot.title = element_text(face="bold", size = 30, hjust = 0.5), 
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        text=element_text(family="IBM Plex Mono")) +
  ggsave("plot/buffertest2.png", height = 7, width = 7)

# For Loop for a Whole CZ -------------------------------------------------
cz_group <- iowa_sf %>% filter(cz == 304) %>% pull(name)
for (i in cz_group) {
  one <- iowa_muncpl %>% filter(name == i)
  one_sf <- st_buffer(iowa_sf %>% filter(name == i),200,
                      joinStyle = "MITRE", endCapStyle = "SQUARE")
  one_buf0 <- st_intersection(one,(st_buffer(one_sf, 50)))
  one_buf1 <- st_intersection(one,(st_buffer(one_sf, 1320)))
  one_buf2 <- st_intersection(one,(st_buffer(one_sf, 2*1320)))
  one_buf3 <- st_intersection(one,(st_buffer(one_sf, 3*1320)))
  one_buf4 <- st_intersection(one,(st_buffer(one_sf, 4*1320)))
  one_rds <- st_intersection(one,iowa_roads)
  title <- sprintf("%s, Iowa", i)
  filename <- sprintf("plot/cz_buffers/%s.png", i)
  ggplot() + 
    geom_sf(data = one) + 
    geom_sf(data = one_buf4, fill = "#e31a1c",
            alpha = 0.25, lwd = 0) +
    geom_sf(data = one_buf3, fill = "#fd8d3c",
            alpha = 0.5, lwd = 0) +
    geom_sf(data = one_buf2, fill = "#fecc5c",
            alpha = 0.5, lwd = 0) +
    geom_sf(data = one_buf1, fill = "#ffffb2",
            alpha = 0.5, lwd = 0) +
    geom_sf(data = one_rds, color = "black",
            size = .2) + 
    geom_sf(data = one_sf, fill = "black",
            alpha = 0.25, color = "black") + 
    theme_void() +
    ggtitle(title, subtitle = "Concentric Buffers at 1/4, 1/2, 3/4, and 1 mile intervals from downtown") +
    theme(plot.title = element_text(face="bold", size = 30, hjust = 0.5), 
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          text=element_text(family="IBM Plex Mono")) +
    ggsave(filename, height = 7, width = 7)
}

