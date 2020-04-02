suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(beepr)
  library(ggnewscale)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------
msp <- read_csv("data/csv/universe/msp_universe.csv") %>%
  filter(rucc > 2 ,pop_2010 < 50000) %>% 
  select(city_fips,cz,msp,msp_yr)
df <- read_csv("data/csv/employment/jobs_panel.csv") %>% 
  inner_join(msp, by = "city_fips")

# Map the Universe --------------------------------------------------------
temp <- tempfile()
download.file("https://opendata.arcgis.com/datasets/6031c4fb8cac48649f2e0a98999d1248_0.kml",temp)
g_lakes <- st_read(temp) %>% st_transform(.,crs = 3174)
rm(temp)
univ_st <- states(cb = T) %>% filter(STUSPS %in% c('IA','MI')) %>% 
  st_transform(.,crs = 3174)
univ_cty <- counties(state = c('IA','MI','OH','WI'), cb = T) %>% 
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

ggplot() +
  geom_sf(data = g_lakes, fill = "#9dd3fd", color = "black") +
  geom_sf(data = univ_cty, alpha = 0, color = "gray", size = .3) +
  geom_sf(data = univ_st, color = NA, fill = "black", alpha = .1) +
  geom_sf(data = univ_cz, alpha = 0, color = "black") +
  new_scale_fill() +
  geom_sf(data = univ_town, aes(fill = as.factor(msp)),
          shape = 21, size = 2, color = "black", alpha = .9) +
  scale_fill_manual(values = c('white','#bb0000'),
                    labels = c('Not Participating','Participating')) +
  labs(fill = "Main Street Program Status",
       title = "Analytical Universe for Dissertation Essay 1",
       subtitle = "Municipalities from Iowa, Michigan, Ohio, and Wisconsin") +
  theme_void() +
  theme(legend.position="bottom",
        text=element_text(family="Futura Medium",size = 18),
        legend.title = element_text(family="Futura Bold"),
        plot.title = element_text(hjust = 0.5, family="Futura Bold", size = 28),
        #plot.background = element_rect(fill = "#d7d5d1", color = "#d7d5d1"), 
        #panel.background = element_rect(fill = "#d7d5d1", color = "#d7d5d1"),
        plot.subtitle = element_text(hjust = 0.5, size = 24)) +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  ggsave("plot/maps/msp_universe.png", width = 11, height = 8.5)

st_intersection(univ_town,univ_cz) %>% 
  st_drop_geometry() %>% 
  group_by(cz) %>% 
  summarize(n_msp = sum(msp)) %>% 
  arrange(desc(n_msp)) %>% slice(1) %>% pull(n_msp)
# The most MSP participants that any CZ contains is 4

# Not Yet -----------------------------------------------------------------
beep()
beep()
beep()
# Stack Towns & Years -----------------------------------------------------
unstacked <- jobs_panel %>% select(-(buffer_0:buffer_5)) %>% 
  filter(msp == 1, year == 2000) %>% 
  group_by(cz) %>% 
  mutate(msp1 = nth(name,1),
         msp2 = nth(name,2),
         msp3 = nth(name,3),
         msp4 = nth(name,4)) %>% 
  summarise(num = n(),
            name_stack1 = first(msp1),
            name_stack2 = first(msp2),
            name_stack3 = first(msp3),
            name_stack4 = first(msp4)) %>% 
  arrange(desc(num)) %>% 
  right_join(.,(jobs_panel %>% select(-(buffer_0:buffer_5))), by = "cz") %>% 
  select(name:msp_yr,cz,everything())

freqTab(unstacked,"cz",Inf)

msp_yrs <- unstacked %>% select(name,msp_yr) %>% distinct()
stack_1 <- msp_yrs %>% rename(name_stack1 = name, mspyear_stack1 = msp_yr)
stack_2 <- msp_yrs %>% rename(name_stack2 = name, mspyear_stack2 = msp_yr)
stack_3 <- msp_yrs %>% rename(name_stack3 = name, mspyear_stack3 = msp_yr)
stack_4 <- msp_yrs %>% rename(name_stack4 = name, mspyear_stack4 = msp_yr)

unstacked_yrs <- left_join(unstacked,stack_1) %>% 
  left_join(.,stack_2) %>% 
  left_join(.,stack_3) %>% 
  left_join(.,stack_4) %>% 
  mutate(ryear_stack1 = year - mspyear_stack1,
         ryear_stack2 = year - mspyear_stack2,
         ryear_stack3 = year - mspyear_stack3,
         ryear_stack4 = year - mspyear_stack4) %>%
  rename(community = name) %>% 
  select(community,year,name_stack1:name_stack4,
         mspyear_stack1:mspyear_stack4,ryear_stack1:ryear_stack4)

stacked <- unstacked_yrs %>% 
  pivot_longer(
    -(community:year), 
    names_to = c(".value", "stack"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% arrange(community, stack) %>% 
  mutate(stack = str_replace_all(stack, "stack", "")) %>% 
  rename(stack_name = name,
         name = community) 

# Merge Values with Stacked Data ------------------------------------------
jobs_match <- jobs_panel %>% 
  select(name,year,cz:msp_yr,buffer_0:buffer_1)

panel <- left_join(stacked,jobs_match, by = c("name","year")) %>% 
  rename(downtown_jobs = buffer_0,
         treated = msp,
         treated_yr = msp_yr,
         match = stack_name,
         time = ryear,
         match_treat_yr = mspyear,
         town = name) %>% 
  mutate(townid = as.factor(paste0("S",(group_indices(.,town,stack)))),
         cal_year = as.factor(year)) %>% 
  filter(time >= -3 & time <= 5,
         match_treat_yr >= 2000 & match_treat_yr <= 2013,
         town != match) %>% 
  select(town,time,cal_year,downtown_jobs,treated,treated_yr,cz,match,match_treat_yr,townid) %>% 
  write_csv("data/csv/panel.csv")

# Panel Diagnostics -------------------------------------------------------
length(unique(panel$townid)) #number of 9-year stretches
length(unique(panel$town))   #number of unique towns in panel
length(unique(panel$match))   #number of unique TREATED towns in panel















