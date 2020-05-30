suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(fst)
  library(beepr)
  library(janitor)
  library(units)
  library(rleuven)
  library(tigris)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in and geocode transactions by block group -------------------------
df <- read_fst("hidden/datatree/cleaned/datatree_oh03.fst") %>% 
  select(property_id,situs_longitude,situs_latitude) %>% 
  distinct() %>% 
  st_as_sf(., coords = c("situs_longitude","situs_latitude"), crs = 4326, remove = T) %>% 
  st_transform(.,crs = 2834)

bgs <- block_groups("39", cb = T) %>% st_transform(., crs = 2834)
geocoded <- df %>% st_intersection(.,bgs)

bg_clean <- geocoded %>% 
  st_drop_geometry() %>% 
  rename(bg_fips = GEOID) %>% 
  select(property_id,bg_fips)  

# Prep for Census & ACS downloads -----------------------------------------
county_fips <- counties("39",cb = T) %>% 
  mutate(GEOID = str_sub(GEOID, -3)) %>% 
  arrange(GEOID) %>% pull(GEOID)

v10 <- load_variables(2010, "sf1", cache = TRUE)
v10_5 <- load_variables(2014, "acs5", cache = TRUE)
v_00 <- load_variables(2000, "sf1", cache = TRUE)
v00 <- load_variables(2000, "sf3", cache = TRUE)

# 2010 Downloads ----------------------------------------------------------
bg_10 <- get_decennial(year = 2010,
                       variables = c("P010001","P003001","P003002","P013001","H004001","H004004"), 
                       state = "39", 
                       county = county_fips,
                       geography = "block group")
bg_10_2 <- get_acs(year = 2014,
                 variables = c("B19013_001","B23025_003","B23025_005","B17020_002","B17020_001"), 
                 state = "39", 
                 county = county_fips,
                 geography = "block group")
bg_10_3 <- get_acs(year = 2014,
                 variables = c("B15003_001","B15003_022","B15003_023","B15003_024","B15003_025"), 
                 state = "39", 
                 county = county_fips,
                 geography = "block group")
beep()

bg_2010 <- rbind(bg_10_2,bg_10_3) %>% rename(value = estimate) %>% select(-moe) %>% 
  rbind(bg_10) %>% 
  select(-NAME) %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  mutate(bg_fips = GEOID,
         pop_2010 = P010001,
         median_age_2010 = P013001,
         pct_nonwhite_2010 = P003002/P003001,
         pct_renting_2010 = H004004/H004001,
         pct_bachelors_2010 = (B15003_022+B15003_023+B15003_024+B15003_025)/B15003_001,
         pct_poverty_2010 = B17020_002/B17020_001,
         civ_unempl_2010 = B23025_005/B23025_003,
         median_income_2010 = B19013_001) %>% 
  select(bg_fips,pop_2010,median_age_2010,pct_nonwhite_2010,pct_renting_2010,
         pct_bachelors_2010,pct_poverty_2010,civ_unempl_2010,median_income_2010)


# 2000 Downloads ----------------------------------------------------------
bg_00 <- get_decennial(year = 2000,
                       variables = c("P010001","P003001","P003003","P013001","H004001","H004003"), 
                       state = "39", 
                       county = county_fips,
                       sumfile = "sf1",
                       geography = "block group") 
bg_00_2 <- get_decennial(year = 2000,
                       variables = c("P037001","P037015","P037016","P037017","P037018",
                                     "P037032","P037033","P037034","P037035"), 
                       state = "39", 
                       county = county_fips,
                       sumfile = "sf3",
                       geography = "block group")
bg_00_3 <- get_decennial(year = 2000,
                       variables = c("P087001","P087002","P053001",
                                     "P043005","P043007","P043012","P043014"), 
                       state = "39", 
                       county = county_fips,
                       sumfile = "sf3",
                       geography = "block group")

bg_geo <- get_decennial(year = 2000,
                       variables = "P010001",
                       state = "39", 
                       county = county_fips,
                       geography = "block group",
                       geometry = T) 

bg_xw <- bg_geo %>% 
  st_transform(.,crs = 2834) %>% 
  rename(bg_fips = GEOID) %>% 
  select(bg_fips) %>% 
  st_intersection(df,.)
beep()


bg_2000 <- rbind(bg_00,bg_00_2,bg_00_3) %>% 
  select(-NAME) %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  mutate(bg_fips = GEOID,
         pop_2000 = P010001,
         median_age_2000 = P013001,
         pct_nonwhite_2000 = P003003/P003001,
         pct_renting_2000 = H004003/H004001,
         pct_bachelors_2000 = (P037015+P037016+P037017+P037018+P037032+P037033+P037034+P037035)/P037001,
         pct_poverty_2000 = P087002/P087001,
         civ_unempl_2000 = (P043007+P043014)/(P043005+P043012),
         median_income_2000 = P053001) %>% 
  select(bg_fips,pop_2000,median_age_2000,pct_nonwhite_2000,pct_renting_2000,
         pct_bachelors_2000,pct_poverty_2000,civ_unempl_2000,median_income_2000)



# Neighborhood Data Checklist ---------------------------------------------
#     2010           |     2000            |
#--------------------|---------------------|
# [x] Population     | [x] Population      |
# [x] Race           | [x] Race            |
# [x] Age            | [x] Age             |
# [ ] Income         | [x] Income          |
# [ ] Employment     | [x] Employment      |
# [ ] Education      | [x] Education       |
# [x] Housing Tenure | [x] Housing Tenure  |
# [ ] Poverty        | [x] Poverty         |

# Calculate Block Group Population Density --------------------------------
bgsize <- bgs %>% 
  st_drop_geometry() %>% 
  mutate(bg_size_sqmi = (ALAND+AWATER)*0.000000386,
         bg_fips = as.numeric(GEOID)) %>% 
  select(bg_fips,bg_size_sqmi)
# Merge 2000 and 2010 with properties -------------------------------------
df_neighborhood_2000 <- bg_xw %>% 
  st_drop_geometry() %>% 
  left_join(bg_2000, by = "bg_fips") %>% 
  filter(!is.na(pct_bachelors_2000)) %>% 
  select(-bg_fips,-pct_poverty_2000)

df_neighborhood_2010 <- bg_clean %>% 
  left_join(bg_2010, by = "bg_fips") %>% 
  select(-pct_poverty_2010) 

df_neighborhood <- inner_join(df_neighborhood_2000,df_neighborhood_2010, by = "property_id") %>% 
  select(property_id,bg_fips,everything()) %>% 
  left_join(bgsize, by = "bg_fips") %>% 
  write_csv("hidden/datatree/cleaned/datatree_oh06.csv")

names(df_neighborhood)