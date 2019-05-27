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
library(mapproj)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
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
  select(CTY_FIPS:CZ,everything())
# Import City Geography -----------------------------------------------------
cities <- read_csv("data/csv/msp_data.csv") %>%
  left_join(.,st.xw, by = c("st" = "STATE")) %>%
  mutate(metro_type = str_to_lower(str_sub(metro_type,1,5)),
         GEOID = as.character(geoid))
cities.msp <- cities %>% filter(msp == 1) %>% arrange(st)
# Make CZ Shapefile -------------------------------------------------------
us_cz <- usctys %>%
  mutate(MAX_X = map_dbl(geometry, ~ st_bbox(.x) %>% extract2("xmax")), 
         GRP = factor(ntile(MAX_X, 3)),
         STATUS = 'distinct') %>%
  group_by(CZ) %>%
  summarise(STATUS = 'dissolved') 
# Lookup Code Function ----------------------------------------------------
lookup_st <- function (state) {
  if (is.null(state)) 
    stop("Invalid state", call. = FALSE)
  else {vals <- head(st.xw[st.xw$ST_POSTAL == state,], 1)
    return(vals$STATE)}}
stfips <- lookup_st("IA")
# Functions to make State Masp ----------------------------------------------
statemap <- function(xstate,msp = NULL) {
  xstate <- toupper(xstate)
  st_cty <- usctys %>%
    filter(ST == xstate)
  st_cz <- st_cty %>%
    mutate(MAX_X = map_dbl(geometry, ~ st_bbox(.x) %>% extract2("xmax")), 
           GRP = factor(ntile(MAX_X, 3)),
           STATUS = 'distinct') %>%
    group_by(CZ) %>%
    summarise(STATUS = 'dissolved')
  czcols <- inlmisc::GetColors((nrow(as.data.frame(table(st_cty$CZ)))), alpha = 0.2) %>%
    as.character() %>%
    str_sub(., end=-3)
  if(is.null(msp)) {
    st_city <- places(state = xstate, cb = TRUE) %>%
      left_join(.,st.xw, by = "STATEFP") %>%
      inner_join(.,cities, by = "GEOID") %>%
      mutate(centroids = st_centroid(geometry)) %>%
      unnest(centroids) %>% 
      group_by(GEOID) %>% 
      mutate(col=seq_along(GEOID)) %>% #add a column indicator
      spread(key=col, value=centroids) %>%
      mutate(X = `1`,Y = `2`)
  } else {
    st_city <- places(state = xstate, cb = TRUE) %>%
      left_join(.,st.xw, by = "STATEFP") %>%
      inner_join(.,cities, by = "GEOID") %>%
      filter(msp == 1) %>%
      mutate(centroids = st_centroid(geometry)) %>%
      unnest(centroids) %>% 
      group_by(GEOID) %>% 
      mutate(col=seq_along(GEOID)) %>% #add a column indicator
      spread(key=col, value=centroids) %>%
      mutate(X = `1`,Y = `2`)
  }
  stmap <- ggplot() +
    geom_sf(data = st_cty,
            color = "grey")
  if(is.null(msp)) {
    stmap.p <- stmap +
      geom_sf(data = st_cz,
            color = "black",
            alpha = 0) +
      geom_point(data = st_city, 
                 aes(x = X, y = Y,
                     fill = factor(msp)),
                 color = "black",
                 shape = 21,
                 size = 2,
                 alpha = 1) + 
      scale_fill_manual(values = c("white","black"),
                        labels = c("Control Group","Treatment Group"),
                        name = "Main Street Program Status") +
      coord_sf(ndiscr = 0) +
      theme_void()
  } else {
    stmap.p <- stmap +
      geom_sf(data = st_cz,
              aes(fill = factor(CZ)),
              color = "black",
              alpha = 0.2) +
      geom_point(data = st_city, 
                 aes(x = X, y = Y,
                     color = factor(msp)),
                 fill = "black",
                 shape = 21,
                 size = 2,
                 alpha = 1) + 
      scale_fill_manual(values = sample(czcols),
                        guide = FALSE) + 
      scale_color_manual(values = "white",
                         labels = "= Main Street Program Community") +
      coord_sf(ndiscr = 0) +
      theme_void() +
      guides(color = guide_legend(nrow = 1,title=NULL))
  }
  stmap.p +     
    theme(plot.title=element_text(family='', face='bold', size=18, hjust = .5),
          plot.subtitle=element_text(family='', size=12, hjust = .5,margin=margin(t=10, b = 10)),
          legend.position="bottom")
}
statemap("IA",msp = 1)
# US MSP Coverage Map -----------------------------------------------------
us_msp <- ggplot() +
  geom_sf(data = us,
          aes(fill = factor(msp)),
          color = "black") +
  scale_fill_manual(values = c("gray","navy")) +
  ggtitle("Main Street Program Data Coverage") +
  coord_sf(crs = st_crs(26916),
           ndiscr = 0) +
  guides(fill=FALSE) +
  theme_void()
ggsave("plot/us_msp.png",us_msp, height=6.5)
# Loop State Maps ---------------------------------------------------------
state_names <- unique(cities.msp$ST_POSTAL) %>% na.omit()
for (i in state_names) {
  temp_title <- lookup_st(i)
  temp_plot = statemap(i) + ggtitle("Main Street Program Communities by CZ",
                                    subtitle = temp_title)
  ggsave(temp_plot, file=paste0("plot/st_msp/map_", i,"_msp.png"), height = 6.5)}
for (i in state_names) {
  temp_title <- lookup_st(i)
  temp_plot = statemap.msp(i) + ggtitle("Main Street Program Communities",
                                    subtitle = temp_title)
  ggsave(temp_plot, file=paste0("plot/st_msp/map_", i,".png"), height = 6.5)}
# Graveyard ---------------------------------------------------------------
#st.cty <- usctys %>%
#  filter(ST == "IA")
#st.cty <- counties(cb = TRUE, state = "IA") %>%
#  mutate(CTY_FIPS = as.numeric(paste0(STATEFP,COUNTYFP))) %>%
#  inner_join(.,rucc,by = "CTY_FIPS") %>%
#  inner_join(.,cz, by = "CTY_FIPS") %>%
#  filter(!STATEFP %in% c("02","15","72")) %>%
#  select(CTY_FIPS:CZ,everything())
#ia.cities <- places(state = "ia", cb = TRUE) %>%
#  left_join(.,st.xw, by = "STATEFP") %>%
#  inner_join(.,cities, by = "GEOID")%>%
#  filter(msp == 1) %>%
#  mutate(centroids = st_centroid(geometry)) %>%
#  unnest(centroids) %>% 
#  group_by(GEOID) %>% 
#  mutate(col=seq_along(GEOID)) %>% #add a column indicator
#  spread(key=col, value=centroids) %>%
#  mutate(X = `1`,Y = `2`,
#         `1` = NULL,`2` = NULL)
#statemap("ia") # + ggtitle("Virginia")