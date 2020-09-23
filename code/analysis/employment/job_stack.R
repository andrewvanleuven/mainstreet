suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(beepr)
  library(rleuven)
  library(fastDummies)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------
df <- read_csv("data/csv/employment/jobs_panel.csv") %>% 
  select(-(msp_accr:cty_seat),-(buffer_1:buffer_5)) %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  rename(cal_yr = year,
         jobs = buffer_0)

# Panel - no matches ------------------------------------------------------
panel_df_treated <- df %>% 
  mutate(post = ifelse(cal_yr >= msp_yr,1,0),
         rel_yr = ifelse(msp == 1,cal_yr - msp_yr,0)) %>% 
  filter(msp == 1) %>% 
  filter(rel_yr >= -3 & rel_yr <= 5,
         msp_yr >= 2000 & msp_yr <= 2013) %>% 
  select(1:4,7,10:14) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "-", "m")) %>% 
  dummy_cols(select_columns = c("rel_yr")) %>% 
  select(1:10,19,18,17,11:16) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "m", "-"))


# Panel - nearest neighbor ------------------------------------------------
panel_df <- read_csv("data/csv/universe/msp_nn_matches.csv") %>% 
  left_join(df,.,by = c("city_fips" = "match_city_fips")) %>% 
  select(-(15:17)) %>%
  rename(name = name.x,
         st = st.x,
         treated_match = name.y,
         treated_mfips = city_fips.y) %>% 
  filter(msp == 1 | !is.na(treated_match)) %>% 
  left_join(.,(df %>% filter(cal_yr == 2000) %>% select(1,11)), by = c("treated_mfips" = "city_fips")) %>% 
  replace_na(list(treated_mfips = 0, distance = 0, msp_yr.y = 0)) %>% 
  mutate(treated_match = coalesce(treated_match,name)) %>% 
  mutate(treatment_year = msp_yr.x + msp_yr.y) %>% 
  mutate(post = ifelse(cal_yr >= treatment_year,1,0),
         rel_yr = cal_yr - treatment_year) %>% 
  filter(rel_yr >= -3 & rel_yr <= 5,
         treatment_year >= 2000 & treatment_year <= 2013) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "-", "m")) %>% 
  dummy_cols(select_columns = c("rel_yr")) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "m", "-")) %>% 
  rename(treated = msp,
         match_distance = distance) %>% 
  mutate(town = paste(name,st,sep = ", ")) %>% 
  select(city_fips,town,cz,rucc,pop_2010,cal_yr,jobs,treated,treatment_year,treated_mfips,treated_match,match_distance,post,rel_yr,rel_yr_m3,rel_yr_m2,rel_yr_m1,rel_yr_0:rel_yr_5)

weights <- panel_df %>% 
  filter(treated == 0,
         rel_yr == 0) %>% 
  group_by(treated_mfips) %>% 
  summarise(matches = n())

panel_nn <- panel_df %>% 
  left_join(weights, by = c("city_fips" = "treated_mfips")) %>% 
  left_join(weights, by = "treated_mfips") %>% 
  mutate(matched = round(1/matches.y,3),
         matches = matches.x) %>% 
  select(-matches.x,-matches.y,-treated_mfips) 

nrow(panel_nn %>% select(town) %>% distinct()) # 226 towns in the data
nrow(panel_nn %>% filter(treated == 1) %>% select(town) %>% distinct()) # 43 treated towns (13 of which are RUCC = 3)

#foreign::write.dta(panel_nn,"data/stata/msp_nearest.dta")

# Panel - CZ stacks -------------------------------------------------------
df %>% 
  filter(msp == 1, cal_yr == 2000) %>% 
  group_by(cz) %>% 
  summarize(num = n()) %>% 
  arrange(desc(num)) %>% 
  slice(1) %>% pull(as.numeric(num)) # Shows that no CZ contains more than 5 active MSPs 

stack_setup <- df %>% filter(cal_yr == 2000) %>% 
  mutate(town = paste(name,st,sep = ", ")) %>% 
  select(1,13,7,10) %>% 
  group_by(cz) %>% 
  mutate(id = row_number(),
         treateds = sum(msp))
