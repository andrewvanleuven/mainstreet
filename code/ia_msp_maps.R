library(tidyverse)
library(tidycensus)
library(tigris)
library(maptools)
library(maps)
library(mapview)
library(scales)
library(sf)
library(magrittr)
library(inlmisc)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
###
no_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.border = element_blank())
#####
cz <- read_csv("data/csv/cz.csv") %>%
  mutate(CTY_FIPS = cty_fips,
         CZ = paste0("CZ",cz2000)) %>%
  select(CTY_FIPS,CZ)
msp <- read_csv("data/csv/msp.csv")  %>%
  mutate(GEOID = as.character(fips),
         fips = NULL)
places <- read_csv("data/csv/places.csv") %>%
  mutate(POP2010 = value,
         value = NULL) %>%
  select(GEOID,POP2010)
hist <- read_csv("data/csv/hist_geo.csv")  %>% 
  mutate(CTY_FIPS = county,
         county = NULL,
         RUCC = rucc,
         rucc = NULL,
         GEOID = fips,
         fips = NULL) %>%
  select(GEOID,CTY_FIPS,pop_2010:RUCC)
rucc <- read_csv("data/csv/rucc.csv") %>% 
  mutate(CTY_FIPS = FIPS,
         FIPS = NULL,
         RUCC = as.numeric(RUCC_2013),
         RUCC_2013 = NULL) %>%
  select(CTY_FIPS,RUCC)
#####
ia.msp <- places(cb = TRUE, state = "IA") %>%
  inner_join(.,hist, by = "GEOID") %>%
  left_join(.,msp, by = "GEOID") %>%
  filter(pop_1940 > 999 | pop_1930 > 999 | pop_1920 > 999 | pop_1910 > 999 | pop_1900 > 999)%>%
  filter(pop_2010 > 750 & pop_2010 < 60000) %>%
  filter(RUCC > 0) %>%
  select(-cityst,-name,-st,-msp_accr,-msp_affl,-LSAD,-(STATEFP:AFFGEOID),-(pop_1940:pop_1900)) %>%
  mutate(centroids = st_centroid(geometry)) %>%
  unnest(centroids) %>% 
  group_by(GEOID) %>% 
  mutate(col=seq_along(GEOID)) %>% #add a column indicator
  spread(key=col, value=centroids) %>%
  mutate(X = `1`,Y = `2`,
         `1` = NULL,`2` = NULL,
         `3` = NULL,`4` = NULL) %>% drop_na(X) %>%
  select(GEOID,NAME,CTY_FIPS,pop_2010,RUCC,msp,msp_yr,everything()) %>%
  replace_na(list(msp = 0, msp_yr = 0)) %>%
  arrange(desc(pop_2010))
#####
ia.st <- states(cb = TRUE, resolution = "20m") %>% filter_state("Iowa")
ia <- counties(cb = TRUE, state = "IA") %>%
  mutate(CTY_FIPS = as.numeric(paste0(STATEFP,COUNTYFP))) %>%
  inner_join(.,cz, by = "CTY_FIPS") %>%
  inner_join(.,rucc, by = "CTY_FIPS") %>%
  select(CTY_FIPS,everything())
#####
ia_cz <- ia %>%
  mutate(MAX_X = map_dbl(geometry, ~ st_bbox(.x) %>% extract2("xmax")), # extract the bounding box's max x coordinate
         GRP = factor(ntile(MAX_X, 3)),
         STATUS = 'distinct') %>%
  group_by(CZ) %>%
  summarise(STATUS = 'dissolved') 
#####
#=======DISCRETE COLOR PALETTE w/SAME # OF COLORS AS # OF CZs
czcols <- gsub('.{2}$', '', (as.data.frame((inlmisc::GetColors((nrow(as.data.frame(table(ia$CZ)))), alpha = 0.2))) %>% pull() %>% as.character()))
#=======PLOT 
(ia_msp <- ggplot() +
    geom_sf(data = ia,
            color = "grey") +
    geom_sf(data = ia_cz,
            aes(fill = factor(CZ)),
            color = "black",
            alpha = 0.3) +
    scale_fill_manual(values = czcols) + 
    labs(fill = "Commute Zone") +
    ggtitle("MSP Programs in Iowa") +
    theme_bw() +
    no_axes + 
    theme(legend.position = "none") +
    geom_point(data = ia.msp, 
               aes(x = X, y = Y,
                   size = pop_2010),
               color='white', 
               fill = "black",
               alpha = 0.75,
               shape = 21) + 
    coord_sf(ndiscr = 0)) 
#=======PLOT MSP COMMUNITIES WITHIN CZs AND COUNTIES SHADED BY RUCC
(ia_msp_rucc <- ggplot() +
    geom_sf(data = ia,
            aes(fill = RUCC),
            lwd = 0.1) +
    scale_fill_gradient(low = "gray95", high = "gray5", 
                        guide = "colorbar",
                        limits = c(1,9), breaks=c(0,1,2,3,4,5,6,7,8,9),
                        labels=c("0","1","2","3","4","5","6","7","8","9")) +
    geom_sf(data = ia_cz,
            color = "white",
            alpha = 0) +
    labs(fill = "Rural-Urban\nContinuum Code",color = "Main Street\nProgram") +
    ggtitle("MSP Programs in Iowa") +
    theme_bw() +
    no_axes + 
    guides(fill = guide_colourbar(barheight = 10, reverse = T,
                                  ticks.colour = "black", ticks.linewidth = 2)) +
    geom_point(data = ia.msp, 
               aes(x = X, y = Y,
                   color = factor(msp)),
               alpha = 0.75, size = 2) +
    scale_color_manual(values = c("red","blue")) + 
    coord_sf(ndiscr = 0))       
#ggsave("plot/iowa_msp.png",ia_msp, height=6.5)
#ggsave("plot/iowa_msp_rucc.png",ia_msp_rucc, height=6.5)