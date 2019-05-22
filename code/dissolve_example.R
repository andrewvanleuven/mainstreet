library(sf) 
library(tidyverse) 

nc <- st_read(system.file("shape/nc.shp", package="sf")) 

nc_grp <-
  nc %>%
  mutate(MAX_X = map_dbl(geometry, ~ st_bbox(.x) %>% extract2("xmax")), # extract the bounding box's max x coordinate
         GRP = factor(ntile(MAX_X, 3)),
         STATUS = 'distinct')

nc_grp_distinct <-
  nc_grp %>%
  group_by(NAME) %>%
  summarise(GRP = first(GRP),
            STATUS = 'distinct')  %>% # make a facetting variable for ggplot
  ungroup() %>%
  select(-NAME)

nc_grp_dissolved <-
  nc_grp %>%
  group_by(GRP) %>%
  summarise(STATUS = 'dissolved') # make a facetting variable for ggplot

nc_combined <- rbind(nc_grp_distinct, nc_grp_dissolved)

# Have a look
ggplot(data = nc_combined) +
  ggplot2::geom_sf(aes(fill = GRP)) +
  facet_wrap(~STATUS, ncol = 1)