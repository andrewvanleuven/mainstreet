library(tidyverse)
library(tidycensus)
library(tigris)
library(maptools)
library(maps)
library(mapview)
library(scales)
library(sf)
library(inlmisc)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
###
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.border = element_blank()
)
cz <- read_csv("data/csv/cz.csv") %>%
  mutate(CTY_FIPS = cty_fips,
         CZ = paste0("CZ",cz2000)) %>%
  select(CTY_FIPS,CZ,cz2000)
msp <- read_csv("data/csv/msp.csv")  %>%
  mutate(GEOID = as.character(fips),
         fips = NULL)
###
ia.places <- places(cb = TRUE, state = "IA")
ia.msp <- inner_join(ia.places,msp, by = "GEOID") %>%
  mutate(centroids = st_centroid(geometry))
latlon <- data.frame(t(sapply((ia.msp$centroids),c)))
ia.dots <- cbind(as.data.frame(ia.msp),as.data.frame(latlon))%>%
  mutate(Y = X2,
         X = X1,
         X2 = NULL,
         X1 = NULL) %>%
  st_as_sf()
###-------------------------------------
ia.st <- states(cb = TRUE, resolution = "20m") %>% filter_state("Iowa")
ia.cty <- counties(cb = TRUE, state = "IA") %>%
  mutate(CTY_FIPS = as.numeric(paste0(STATEFP,COUNTYFP))) %>%
  inner_join(.,cz, by = "CTY_FIPS") %>%
  select(CTY_FIPS,everything())
###-------------------------------------
CZcolors <- gsub('.{2}$', '', (as.data.frame((inlmisc::GetColors((nrow(as.data.frame(table(ia.cty$CZ)))), alpha = 0.2))) %>% pull() %>% as.character()))

(ia_msp <- ggplot() +
    geom_sf(data = ia.cty,
            aes(fill = factor(CZ)),
            color = "black",
            alpha = 0.3) +
    scale_fill_manual(values = CZcolors) + 
    labs(fill = "Commute Zone") +
    ggtitle("MSP Programs in Iowa") +
    theme_bw() +
    ditch_the_axes + 
    theme(legend.position = "none") +
    geom_point(data = ia.dots, 
               aes(x = X, y = Y),
               color='white',
               fill = "black",
               shape = 21))

#ggsave("plot/iowa_msp.png",ia_msp, height=6.5)



