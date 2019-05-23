library(tidyverse)
library(tidycensus)
library(tigris)
library(openxlsx)
library(curl)
library(maptools)
library(usmap)
library(sf)
library(tmap)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
###
'%!in%' <- function(x,y)!('%in%'(x,y))
###
st.msp <- read_csv("data/csv/states.csv")
communities <- read_csv("data/csv/communities.csv") %>%
  filter(st == "Iowa") %>% 
  mutate(GEOID = fips,
         fips = NULL)
us <- states(cb = TRUE, resolution = "20m")
usa <- geo_join(us, st.msp, by = "GEOID")
usa <- usa[usa$STUSPS %!in% c("HI", "AK", "PR"), ]
###
ia.places <- places(cb = TRUE, state = "IA")
ia.plc <- inner_join(communities,ia.places, by = "GEOID")
###-------------------------------------Show which states I am covering in my Analysis
us_msp <- tm_shape(usa, projection = 26916) +
  tm_fill("msp", title = "Study Area", style = "fixed",
          breaks = c(0, 1, Inf),
          palette = c("gray", "navy")) +
  tm_borders(col = "white") +
  tm_legend(show=FALSE) +
  tm_layout(panel.labels = c("Main Street Program Data Coverage"))
us_msp
tmap_save(us_msp, "plot/us_msp.png", height=6.5)










