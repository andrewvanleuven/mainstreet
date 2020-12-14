suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(beepr)
  library(ggnewscale)
  library(cowplot)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------
msp <- read_csv("data/csv/universe/msp_universe.csv") %>%
  filter(rucc > 2 ,pop_2010 < 50000) %>% 
  select(city_fips,cz,msp,msp_yr)

# Map the Universe --------------------------------------------------------
us <- states(cb = T, resolution = "20m") %>% 
  mutate(msp = ifelse(STUSPS %in% c("IA","OH","WI","MI"), 1, 0)) %>% 
  filter(GEOID < 60, !STUSPS %in% c("AK","HI"))
temp <- tempfile()
download.file("https://opendata.arcgis.com/datasets/6031c4fb8cac48649f2e0a98999d1248_0.kml",temp)
g_lakes <- st_read(temp) %>% st_transform(.,crs = 3174)
rm(temp)
st <- states(cb = T, resolution = "20m") %>% filter(!STUSPS %in% c("AK","HI","PR")) %>% st_transform(.,crs = 3174)
univ_st <- states(cb = T, resolution = "20m") %>% filter(STUSPS %in% c('IA','MI')) %>% 
  st_transform(.,crs = 3174)
univ_cty <- counties(state = c('IA','MI','OH','WI'), cb = T, resolution = "20m") %>% 
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
lim <- st_union(g_lakes,univ_cty) %>% st_buffer(-10000)

(ggm2 <- ggplot() +
  geom_sf(data = us,
          aes(fill = factor(msp)),
          color = "black",
          size = .25) +
  scale_fill_manual(values = c("gray90","#BB0000")) +
  coord_sf(crs = 2163,
           ndiscr = 0) +
  guides(fill=FALSE) +
  theme_void() +
  theme(panel.background = element_rect(color = NA, fill = "white"),
        panel.border = element_rect(colour = "black", size=1, fill = NA)))

(ggm1 <- ggplot() +
  geom_sf(data = g_lakes, fill = "#9dd3fd", color = "black") +
  #geom_sf(data = st, fill = "gray90", size = .3, color = "black") +
  geom_sf(data = univ_cty, color = "gray", size = .3, fill = "white") +
  #geom_sf(data = univ_st, color = NA, fill = "black", alpha = .05) +
  geom_sf(data = univ_cz, alpha = 0, color = "black") +
  new_scale_fill() +
  geom_sf(data = univ_town, aes(fill = as.factor(msp), size = as.factor(msp), alpha = as.factor(msp), color = as.factor(msp)),
          shape = 21, stroke = .5) +
  scale_fill_manual(values = c('grey70','#BB0000'),
                    labels = c('Not Participating','Participating')) +
  scale_size_manual(values = c(1.5,2),
                    guide = FALSE) +
  scale_color_manual(values = c('black','black'),
                    guide = FALSE) +
  scale_alpha_manual(values = c(.9,1),
                     guide = FALSE) +
  labs(fill = "Main Street Program Status") +
  theme_void(base_family = "LM Roman 10") +
  theme(plot.title = element_text(size=20, face="bold", hjust = 0.5),
        legend.title=element_text(size=12, face="bold", hjust = 0.5),
        legend.text=element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size=1, fill = NA),
        panel.background = element_rect(color = NA, fill = "gray95"),
        legend.position = c(0.2, 0.1),
        legend.direction = "horizontal",
        legend.margin=margin(t = 7, r = 11, b = 7, l = 7, unit = "pt"),
        legend.background = element_rect(color = "black", fill = "white")) +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5)))
  #ggsflabel::lims_bbox(lim))

ggdraw() +
  draw_plot(ggm1) +
  draw_plot(ggm2, x = 0.75, y = 0.75, width = 0.2, height = 0.2) +
  ggsave("plot/maps/universe.png", width = 11, height = 8.5)

