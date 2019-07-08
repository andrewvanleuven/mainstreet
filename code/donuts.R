library(tidyverse)
library(tidycensus)
library(tigris)
library(rleuven)
library(sf)
options(tigris_class = "sf",tigris_use_cache = TRUE,scipen = 999)
# Read in data ------------------------------------------------------------
data <- read_csv("data/csv/msp_data.csv") %>%
  mutate(micro_sa = ifelse(metro_type == "Micropolitan Statistical Area", 1, 0),
         metro_sa = ifelse(metro_type == "Metropolitan Statistical Area", 1, 0)) %>%
  arrange(geoid)
nonmetro.cz <- data %>%
  group_by(cz) %>%
  mutate(metro_cz = any(rucc == 2 | rucc == 1)) %>%
  filter(metro_cz == FALSE)
nonmetro.cty <- data %>%
  filter(rucc > 2)
# Import US Geography -----------------------------------------------------
st.msp <- read_csv("data/csv/states.csv") %>%
  select(GEOID,msp)
st.xw <- read_csv("data/csv/stxw.csv") %>%
  mutate(STATEFP = as.character(str_pad(STATEFP, 2, pad = "0")))
us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK","PR","HI","DC")) %>%
  inner_join(.,st.msp, by = "GEOID")
# Import County Geography -------------------------------------------------
rucc <- read_csv("data/csv/rucc.csv") %>% 
  mutate(CTY_FIPS = FIPS, ST = State, RUCC = as.numeric(RUCC_2013)) %>%
  select(CTY_FIPS,ST,RUCC)
cz <- read_csv("data/csv/cz.csv") %>%
  mutate(CTY_FIPS = cty_fips, CZ = paste0("CZ",cz2000)) %>%
  select(CTY_FIPS,CZ)
usctys <- counties(cb = TRUE) %>%
  mutate(CTY_FIPS = as.numeric(paste0(STATEFP,COUNTYFP))) %>%
  inner_join(.,rucc,by = "CTY_FIPS") %>%
  inner_join(.,cz, by = "CTY_FIPS") %>%
  left_join(.,st.xw, by = "STATEFP") %>%
  filter(!ST_POSTAL %in% c("HI","AK","PR","DC")) %>%
  select(CTY_FIPS:CZ,everything()) %>%
  filter(CZ == "CZ304")
# Import City Geography -----------------------------------------------------
cities <- data %>%
  left_join(.,st.xw, by = c("st" = "STATE")) %>%
  mutate(metro_type = str_to_lower(str_sub(metro_type,1,5)),
         GEOID = as.character(geoid)) %>% filter(cz == 304) 
cities.msp <- cities %>% filter(msp == 1) %>% arrange(st)
# Make CZ Shapefile -------------------------------------------------------
us_cz <- usctys %>%
  mutate(MAX_X = map_dbl(geometry, ~ st_bbox(.x) %>% magrittr::extract2("xmax")), 
         GRP = factor(ntile(MAX_X, 3)),
         STATUS = 'distinct') %>%
  filter(CZ == "CZ304") %>%
  group_by(CZ) %>%
  summarise(STATUS = 'dissolved') 
# Lookup Code Function ----------------------------------------------------
lookup_st <- function (state) {
  if (is.null(state)) 
    stop("Invalid state", call. = FALSE)
  else {vals <- head(st.xw[st.xw$ST_POSTAL == state,], 1)
  return(vals$STATE)}}
lookup_st("IA")
# Mason City CZ -----------------------------------------------------------
city <- places(state = "IA", cb = TRUE) %>%
  left_join(.,st.xw, by = "STATEFP") %>%
  inner_join(.,cities, by = "GEOID") %>%
  mutate(centroids = st_centroid(geometry)) %>%
  unnest(centroids) %>% 
  group_by(GEOID) %>% 
  mutate(col=seq_along(GEOID)) %>% #add a column indicator
  spread(key=col, value=centroids) %>%
  mutate(X = `1`,Y = `2`)

ggplot() + 
  geom_sf(data = usctys, 
          color = "grey") +
  geom_sf(data = us_cz,
          color = "black",
          alpha = 0) +
  geom_sf(data = city, 
          aes(fill = factor(msp))) +
  geom_point(data = city, 
             aes(x = X, y = Y,
                 color = factor(msp)),
             size = 2,
             alpha = 1) +
  coord_sf(ndiscr = 0) +
  theme_void() +
  scale_fill_manual(values = c("black","yellow"),
                     labels = c("= Control","= Treatment"))
