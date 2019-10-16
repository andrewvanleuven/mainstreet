suppressMessages({library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(magrittr)
library(inlmisc)
library(rleuven)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

us <- states(cb = T) %>% 
  mutate(msp = ifelse(STUSPS %in% c("IA","OH","WI"), 1, 0)) %>% 
  filter(GEOID < 60, !STUSPS %in% c("AK","HI"))

ggplot() +
  geom_sf(data = us,
          aes(fill = factor(msp)),
          color = "black") +
  scale_fill_manual(values = c("gray","#BB0000")) +
  #ggtitle("Main Street Program Data Coverage") +
  coord_sf(crs = st_crs(26916),
           ndiscr = 0) +
  guides(fill=FALSE) +
  theme_void() +
  ggsave("plot/us_msp_slides.png", height=6.5)
