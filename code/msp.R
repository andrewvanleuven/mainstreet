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
msp <- read_csv("data/csv/msp.csv")  %>% 
  mutate(GEOID = fips,
         fips = NULL)
communities <- read_csv("data/csv/communities.csv") %>%
  filter(st == "Iowa") %>% 
  mutate(GEOID = fips,
         fips = NULL)
###
ia.places <- places(cb = TRUE, state = "IA")
ia.plc <- inner_join(communities,ia.places, by = "GEOID")
###-------------------------------------
iowa.st <- states(cb = TRUE, resolution = "20m") %>% filter_state("Iowa")

ia_msp <- ggplot() +
  geom_sf(data = iowa.st) + 
  geom_sf(data = ia.plc)
ia_msp

#ggsave("plot/iowa_msp.png",ia_msp, height=6.5)




