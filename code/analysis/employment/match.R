suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(beepr)
  library(rleuven)
  library(fastDummies)
  library(tigris)})

# Read in Data ------------------------------------------------------------
df <- read_csv("data/csv/employment/jobs_panel.csv") %>% 
  select(-(msp_accr:cty_seat),-(buffer_1:buffer_5)) %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  rename(cal_yr = year,
         jobs = buffer_0) %>% 
  group_by(city_fips) %>%
  mutate(jobs_lead = lead(jobs, n = 1, default = NA))

# Panel - nearest neighbor ------------------------------------------------
panel_df <- read_csv("data/csv/universe/msp_nn_matches.csv") %>% 
  left_join(df,.,by = c("city_fips" = "match_city_fips")) %>% 
  select(-(16:18)) %>%
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
         treatment_year >= 2000 & treatment_year <= 2012) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "-", "m")) %>% 
  dummy_cols(select_columns = c("rel_yr")) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "m", "-")) %>% 
  rename(treated = msp,
         match_distance = distance) %>% 
  mutate(town = paste(name,st,sep = ", ")) %>% 
  select(city_fips,town,cz,rucc,pop_2010,cal_yr,jobs,jobs_lead,treated,treatment_year,treated_mfips,treated_match,match_distance,
         post,rel_yr,rel_yr_m3,rel_yr_m2,rel_yr_m1,rel_yr_0:rel_yr_5)

weights <- panel_df %>% 
  filter(treated == 0,
         rel_yr == 0) %>% 
  group_by(treated_mfips) %>% 
  summarise(matches = n())

panel_nn <- panel_df %>% 
  left_join(weights, by = c("city_fips" = "treated_mfips")) %>% 
  left_join(weights, by = "treated_mfips") %>% 
  mutate(matched = 1/matches.y,
         matches = matches.x) %>% 
  select(-matches.x,-matches.y,-treated_mfips) %>% 
  rename(id = city_fips) %>% 
  arrange(town,cal_yr) %>% 
  mutate(id = 1000+group_indices(., town, treated_match)) %>% arrange(id) %>% 
  mutate(cz_yr = group_indices(.,cz,cal_yr),
         weights = ifelse(is.na(matched),1,matched),
         weights = ifelse(is.na(weights),1,weights),
         weights = round(weights, 3)) %>% 
  select(-matched,-matches) %>% 
  write_csv("data/csv/employment/panel_nn.csv")

#foreign::write.dta(panel_nn,"data/stata/msp_nearest.dta")

nrow(panel_nn %>% select(town) %>% distinct()) # 226 towns in the data
nrow(panel_nn %>% filter(treated == 1) %>% select(town) %>% distinct()) # 43 treated towns (13 of which are RUCC = 3)
rm(panel_df,weights)

# Panel - CZ stacks -------------------------------------------------------
df %>% 
  filter(msp == 1, cal_yr == 2000) %>% 
  group_by(cz) %>% 
  summarize(num = n()) %>% 
  arrange(desc(num)) %>% 
  slice(1) %>% pull(as.numeric(num)) # Shows that no CZ contains more than 5 active MSPs 

stack_setup <- df %>% filter(cal_yr == 2000) %>% 
  mutate(town = paste(name,st,sep = ", ")) %>% 
  select(1,14,7,10) %>% 
  group_by(cz) %>% 
  mutate(id = row_number(),
         treateds = sum(msp))

unstacked <- df %>% filter(msp == 1, cal_yr == 2000) %>% 
  mutate(town = paste(name,st,sep = ", ")) %>% 
  select(1,14,7,10) %>% 
  group_by(cz) %>% 
  mutate(msp1 = nth(town,1),
         msp2 = nth(town,2),
         msp3 = nth(town,3),
         msp4 = nth(town,4),
         msp5 = nth(town,5)) %>% 
  summarise(num = n(),
            name_stack1 = first(msp1),
            name_stack2 = first(msp2),
            name_stack3 = first(msp3),
            name_stack4 = first(msp4),
            name_stack5 = first(msp5)) %>% 
  arrange(desc(num)) %>% 
  right_join(.,stack_setup, by = "cz") %>% 
  select(city_fips,town,cz,msp,name_stack1:name_stack5) %>% 
  filter(msp != 1, !is.na(name_stack1)) %>% 
  group_by(cz) %>% 
  select(-msp)

treated_stack <- stack_setup %>% 
  filter(msp == 1) %>% 
  select(1:3) %>% 
  mutate(match_number = 0,
         treated_match = town)

treated_yrs <- df %>% filter(cal_yr == 2000) %>% 
  mutate(town = paste(name,st,sep = ", ")) %>% 
  ungroup() %>% 
  select(town,msp_yr)

stacked <- unstacked %>%
  pivot_longer(cols = starts_with("name_stack"), names_to = "match_number",
               values_to = "treated_match",values_drop_na = TRUE) %>% 
  mutate(match_number = as.numeric(str_replace_all(match_number, "name_stack", ""))) %>%
  rbind(.,treated_stack) %>% 
  slice(rep(1:n(), each = 22)) %>% 
  group_by(town,match_number) %>% 
  mutate(cal_yr = row_number()+1996) %>% 
  select(city_fips,cal_yr,town,everything(),-cz) %>% 
  arrange(city_fips) %>% 
  ungroup() %>% 
  left_join(df, by = c("city_fips","cal_yr")) %>% 
  select(-(name:cbsa_fips)) %>% 
  left_join(treated_yrs, by = c("treated_match" = "town")) %>% 
  rename(treated = msp,
         treatment_year = msp_yr.y) %>% 
  mutate(id = paste(city_fips,match_number,sep = "_")) %>% 
  select(id,everything(),-msp_yr.x,-city_fips,-match_number) %>% 
  mutate(post = ifelse(cal_yr >= treatment_year,1,0),
         rel_yr = cal_yr - treatment_year) %>% 
  filter(rel_yr >= -3 & rel_yr <= 5,
         treatment_year >= 2000 & treatment_year <= 2012) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "-", "m")) %>% 
  dummy_cols(select_columns = c("rel_yr")) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "m", "-")) %>% 
  select(id,town,cz,rucc,pop_2010,cal_yr,jobs,jobs_lead,treated,treatment_year,treated_match,post,rel_yr,rel_yr_m3,rel_yr_m2,rel_yr_m1,rel_yr_0:rel_yr_5)

weights_stack <- stacked %>% 
  filter(rel_yr == 0) %>% 
  group_by(treated_match) %>% 
  summarise(x = n()) %>% 
  ungroup()

matches <- read_csv("data/csv/universe/all_nn_matches.csv") %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  mutate(match_st = str_replace_all(match_st, "Iowa", "IA"),
         match_st = str_replace_all(match_st, "Michigan", "MI"),
         match_st = str_replace_all(match_st, "Ohio", "OH"),
         match_st = str_replace_all(match_st, "Wisconsin", "WI"),) %>% 
  mutate(treated_match = paste(name,st,sep = ", "),
         town = paste(match_name,match_st,sep = ", ")) %>% 
  select(town,treated_match,distance)

panel_stack <- stacked %>% 
  left_join(.,weights_stack, by = "treated_match") %>% 
  mutate(matched = ifelse(treated == 0, 1/x, NA),
         matches = ifelse(treated == 1, x, NA),
         weights = ifelse(is.na(matched),1,matched),
         weights = round(weights, 3)) %>% 
  select(-x,-matched,-matches) %>% 
  arrange(town,cal_yr) %>% 
  mutate(id = 1000+group_indices(., town, treated_match)) %>% arrange(id) %>% 
  mutate(cz_yr = group_indices(.,cz,cal_yr)) %>% 
  left_join(matches, by = c("town","treated_match")) %>% 
  mutate(distance = ifelse(weights>=1,0,distance)) %>% 
  write_csv("data/csv/employment/panel_stack.csv")

#foreign::write.dta(panel_stack,"data/stata/msp_stack.dta")

rm(treated_stack,treated_yrs,stack_setup,matches,unstacked,stacked,weights_stack)
nrow(panel_stack %>% select(town) %>% distinct()) # 280 towns in the data
nrow(panel_stack %>% filter(treated == 1) %>% select(town) %>% distinct()) # 41 treated towns (13 of which are RUCC = 3)


# Establishments Panels ---------------------------------------------------
ef <- read_csv("data/csv/employment/ests_panel.csv") %>% 
  select(-(msp_accr:cty_seat),-(buffer_1:buffer_5)) %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  rename(cal_yr = year,
         ests = buffer_0) %>% 
  group_by(city_fips) %>%
  mutate(ests_lead = lead(ests, n = 1, default = NA),
         town = paste(name,st,sep = ", ")) %>% 
  ungroup() %>% 
  select(town,cal_yr,ests,ests_lead)

epanel_nn <- panel_nn %>% 
  left_join(ef, by = c('town','cal_yr')) %>% 
  relocate(ests, .after = jobs) %>% 
  relocate(ests_lead, .after = jobs_lead) %>% 
  write_csv("data/csv/employment/epanel_nn.csv")

epanel_stack <- panel_stack %>% 
  left_join(ef, by = c('town','cal_yr')) %>% 
  relocate(ests, .after = jobs) %>% 
  relocate(ests_lead, .after = jobs_lead) %>% 
  write_csv("data/csv/employment/epanel_stack.csv")


# First two buffers panels ------------------------------------------------

df_buff2 <- read_csv("data/csv/employment/jobs_panel.csv") %>% 
  mutate(jobs = buffer_0 + buffer_1) %>% 
  select(-(msp_accr:cty_seat),-(buffer_0:buffer_5)) %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  rename(cal_yr = year) %>% 
  group_by(city_fips) %>%
  mutate(jobs_lead = lead(jobs, n = 1, default = NA),
         town = paste(name,st,sep = ", ")) %>% 
  ungroup() %>% 
  select(town,cal_yr,jobs,jobs_lead)

edf_buff2 <- read_csv("data/csv/employment/ests_panel.csv") %>% 
  mutate(ests = buffer_0 + buffer_1) %>% 
  select(-(msp_accr:cty_seat),-(buffer_0:buffer_5)) %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  rename(cal_yr = year) %>% 
  group_by(city_fips) %>%
  mutate(ests_lead = lead(ests, n = 1, default = NA),
         town = paste(name,st,sep = ", ")) %>% 
  ungroup() %>% 
  select(town,cal_yr,ests,ests_lead)

panel_nn_buff2 <- panel_nn %>% select(-jobs,-jobs_lead) %>% 
  left_join(df_buff2, by = c('town','cal_yr')) %>% 
  left_join(edf_buff2, by = c('town','cal_yr')) %>% 
  relocate(c(jobs,jobs_lead), .after = cal_yr) %>% 
  relocate(ests, .after = jobs) %>% 
  relocate(ests_lead, .after = jobs_lead) %>% 
  write_csv("data/csv/employment/epanel_nn_buff2.csv")

panel_stack_buff2 <- panel_stack %>% select(-jobs,-jobs_lead) %>% 
  left_join(df_buff2, by = c('town','cal_yr')) %>% 
  left_join(edf_buff2, by = c('town','cal_yr')) %>% 
  relocate(c(jobs,jobs_lead), .after = cal_yr) %>% 
  relocate(ests, .after = jobs) %>% 
  relocate(ests_lead, .after = jobs_lead) %>% 
  write_csv("data/csv/employment/epanel_stack_buff2.csv")

# Retail jobs only --------------------------------------------------------
rdf_buff2 <- read_csv("data/csv/employment/retail_jobs_panel.csv") %>% 
  mutate(ests = ests_buffer_0 + ests_buffer_1,
         jobs = jobs_buffer_0 + jobs_buffer_1) %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  rename(cal_yr = year) %>% 
  group_by(city_fips) %>%
  mutate(jobs_lead = lead(jobs, n = 1, default = NA),
         ests_lead = lead(ests, n = 1, default = NA),
         town = paste(name,st,sep = ", ")) %>% 
  ungroup() %>% 
  select(town,cal_yr,jobs,ests,jobs_lead,ests_lead)
  
rpanel_nn_buff2 <- panel_nn %>% select(-jobs,-jobs_lead) %>% 
  left_join(rdf_buff2, by = c('town','cal_yr')) %>% 
  relocate(c(jobs,jobs_lead), .after = cal_yr) %>% 
  relocate(ests, .after = jobs) %>% 
  relocate(ests_lead, .after = jobs_lead) %>% 
  write_csv("data/csv/employment/rpanel_nn_buff2.csv")

rdf_buff1 <- read_csv("data/csv/employment/retail_jobs_panel.csv") %>% 
  mutate(ests = ests_buffer_0,
         jobs = jobs_buffer_0) %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  rename(cal_yr = year) %>% 
  group_by(city_fips) %>%
  mutate(jobs_lead = lead(jobs, n = 1, default = NA),
         ests_lead = lead(ests, n = 1, default = NA),
         town = paste(name,st,sep = ", ")) %>% 
  ungroup() %>% 
  select(town,cal_yr,jobs,ests,jobs_lead,ests_lead)

rpanel_nn_buff1 <- panel_nn %>% select(-jobs,-jobs_lead) %>% 
  left_join(rdf_buff1, by = c('town','cal_yr')) %>% 
  relocate(c(jobs,jobs_lead), .after = cal_yr) %>% 
  relocate(ests, .after = jobs) %>% 
  relocate(ests_lead, .after = jobs_lead) %>% 
  write_csv("data/csv/employment/rpanel_nn_buff1.csv")

rdf_buff3 <- read_csv("data/csv/employment/retail_jobs_panel.csv") %>% 
  mutate(ests = ests_buffer_0 + ests_buffer_1 + ests_buffer_2,
         jobs = jobs_buffer_0 + jobs_buffer_1 + jobs_buffer_2) %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  rename(cal_yr = year) %>% 
  group_by(city_fips) %>%
  mutate(jobs_lead = lead(jobs, n = 1, default = NA),
         ests_lead = lead(ests, n = 1, default = NA),
         town = paste(name,st,sep = ", ")) %>% 
  ungroup() %>% 
  select(town,cal_yr,jobs,ests,jobs_lead,ests_lead)

rpanel_nn_buff3 <- panel_nn %>% select(-jobs,-jobs_lead) %>% 
  left_join(rdf_buff3, by = c('town','cal_yr')) %>% 
  relocate(c(jobs,jobs_lead), .after = cal_yr) %>% 
  relocate(ests, .after = jobs) %>% 
  relocate(ests_lead, .after = jobs_lead) %>% 
  write_csv("data/csv/employment/rpanel_nn_buff3.csv")

rpanel_stack_buff2 <- panel_stack %>% select(-jobs,-jobs_lead) %>% 
  left_join(rdf_buff2, by = c('town','cal_yr')) %>% 
  relocate(c(jobs,jobs_lead), .after = cal_yr) %>% 
  relocate(ests, .after = jobs) %>% 
  relocate(ests_lead, .after = jobs_lead) %>% 
  write_csv("data/csv/employment/rpanel_stack_buff2.csv")

# Iowa 1980s to Present ---------------------------------------------------

df_ia <- read_csv("data/csv/iowa_dor/real_taxable_sales_pk.csv") %>% select(-4,-5) %>% 
  rename(cal_yr = year,
         sales = real_pk_sales) %>% 
  group_by(city_fips) %>%
  mutate(sales_lead = lead(sales, n = 1, default = NA),
         st = "IA") %>% 
  select(city_fips,name,st,cal_yr,sales,sales_lead)

nn_matches <- read_csv("data/csv/universe/msp_nn_matches.csv") %>% 
  rename(treated_match = name,
         treated_fips = city_fips,
         city_fips = match_city_fips,
         name = match_name,
         treated_st = st,
         st = match_st) %>% 
  select(-name,-st)

panel_dfi <- nn_matches %>% 
  left_join(df_ia,.,by = "city_fips") %>% 
  left_join(read_csv("data/csv/universe/msp_universe.csv") %>% select(-name,-st), by = "city_fips") %>% 
  filter(msp == 1 | !is.na(treated_match)) %>% 
  replace_na(list(distance = 0, treated_fips = 0)) %>% 
  mutate(treated_match = coalesce(treated_match,name),
         town = paste(name,"IA",sep = ", "),
         treated_match = paste(treated_match,"IA",sep = ", ")) %>% 
  select(city_fips,town,cal_yr,cty_fips:msp_yr,treated_fips,treated_match,distance,sales,sales_lead) %>% 
  rename(treatment_year = msp_yr,
         treated = msp) %>% 
  group_by(treated_match) %>% 
  mutate(treatment_year = max(treatment_year),
         treated_mfips = if_else(treated_fips == 0,city_fips,treated_fips)) %>% 
  ungroup() %>% 
  mutate(post = ifelse(cal_yr >= treatment_year,1,0),
         rel_yr = cal_yr - treatment_year) %>% 
  filter(rel_yr >= -3 & rel_yr <= 5,
         treatment_year >= 1984 & treatment_year <= 2012) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "-", "m")) %>% 
  dummy_cols(select_columns = c("rel_yr")) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "m", "-")) %>% 
  rename(match_distance = distance) %>% 
  select(city_fips,town,cz,rucc,pop_2010,cal_yr,treated,treatment_year,treated_mfips,treated_match,match_distance,
         post,rel_yr,rel_yr_m3,rel_yr_m2,rel_yr_m1,rel_yr_0:rel_yr_5)

iweights <- panel_dfi %>% 
  filter(treated == 0,
         rel_yr == 0) %>% 
  group_by(treated_mfips) %>% 
  summarise(matches = n())

ipanel_nn <- panel_dfi %>% 
  left_join(iweights, by = "treated_mfips") %>% 
  select(-treated_mfips) %>% 
  rename(id = city_fips) %>% 
  arrange(town,cal_yr) %>% 
  mutate(id = 1000+group_indices(., town, treated_match)) %>% arrange(id) %>% 
  mutate(cz_yr = group_indices(.,cz,cal_yr),
         weights = if_else(treated == 1,1,(1/matches)),
         weights = round(weights, 3)) %>% 
  select(-matches) %>% 
  write_csv("data/csv/employment/ipanel_nn.csv")

idf <- df_ia %>%
  left_join(read_csv("data/csv/universe/msp_universe.csv") %>% select(-name,-st), by = "city_fips")

istack_setup <- idf %>% filter(cal_yr == 2000) %>% 
  mutate(town = paste(name,st,sep = ", ")) %>% 
  select(city_fips,town,cz,msp) %>% 
  group_by(cz) %>% 
  mutate(id = row_number(),
         treateds = sum(msp))

iunstacked <- idf %>% filter(msp == 1, cal_yr == 2000) %>% 
  mutate(town = paste(name,st,sep = ", ")) %>% 
  select(city_fips,town,cz,msp) %>% 
  group_by(cz) %>% 
  mutate(msp1 = nth(town,1),
         msp2 = nth(town,2),
         msp3 = nth(town,3),
         msp4 = nth(town,4),
         msp5 = nth(town,5)) %>% 
  summarise(num = n(),
            name_stack1 = first(msp1),
            name_stack2 = first(msp2),
            name_stack3 = first(msp3),
            name_stack4 = first(msp4),
            name_stack5 = first(msp5)) %>% 
  arrange(desc(num)) %>% 
  right_join(.,istack_setup, by = "cz") %>% 
  select(city_fips,town,cz,msp,name_stack1:name_stack5) %>% 
  filter(msp != 1, !is.na(name_stack1)) %>% 
  group_by(cz) %>% 
  select(-msp)

itreated_stack <- istack_setup %>% 
  filter(msp == 1) %>% 
  select(1:3) %>% 
  mutate(match_number = 0,
         treated_match = town)

itreated_yrs <- idf %>% filter(cal_yr == 2000) %>% 
  mutate(town = paste(name,st,sep = ", ")) %>% 
  ungroup() %>% 
  select(town,msp_yr)

istacked <- iunstacked %>%
  pivot_longer(cols = starts_with("name_stack"), names_to = "match_number",
               values_to = "treated_match",values_drop_na = TRUE) %>% 
  mutate(match_number = as.numeric(str_replace_all(match_number, "name_stack", ""))) %>%
  rbind(.,treated_stack) %>% 
  slice(rep(1:n(), each = 40)) %>% 
  group_by(town,match_number) %>% 
  mutate(cal_yr = row_number()+1979) %>% 
  select(city_fips,cal_yr,town,everything(),-cz) %>% 
  arrange(city_fips) %>% 
  ungroup() %>% 
  left_join(idf, by = c("city_fips","cal_yr")) %>% 
  select(-(name:cbsa_fips)) %>% 
  left_join(itreated_yrs, by = c("treated_match" = "town")) %>% 
  rename(treated = msp,
         treatment_year = msp_yr.y) %>% 
  mutate(id = paste(city_fips,match_number,sep = "_")) %>% 
  select(id,everything(),-msp_yr.x,-city_fips,-match_number) %>% 
  mutate(post = ifelse(cal_yr >= treatment_year,1,0),
         rel_yr = cal_yr - treatment_year) %>% 
  filter(rel_yr >= -3 & rel_yr <= 5,
         treatment_year >= 1984 & treatment_year <= 2012) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "-", "m")) %>% 
  dummy_cols(select_columns = c("rel_yr")) %>% 
  mutate(rel_yr = str_replace_all(rel_yr, "m", "-")) %>% 
  select(id,town,cz,rucc,pop_2010,cal_yr,treated,treatment_year,treated_match,post,rel_yr,rel_yr_m3,rel_yr_m2,rel_yr_m1,rel_yr_0:rel_yr_5)

iweights_stack <- istacked %>% 
  filter(rel_yr == 0) %>% 
  group_by(treated_match) %>% 
  summarise(x = n()) %>% 
  ungroup()

imatches <- read_csv("data/csv/universe/all_nn_matches.csv") %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),) %>% 
  mutate(match_st = str_replace_all(match_st, "Iowa", "IA"),
         match_st = str_replace_all(match_st, "Michigan", "MI"),
         match_st = str_replace_all(match_st, "Ohio", "OH"),
         match_st = str_replace_all(match_st, "Wisconsin", "WI"),) %>% 
  mutate(treated_match = paste(name,st,sep = ", "),
         town = paste(match_name,match_st,sep = ", ")) %>% 
  select(town,treated_match,distance)

ipanel_stack <- istacked %>% 
  left_join(.,iweights_stack, by = "treated_match") %>% 
  mutate(cz_yr = group_indices(.,cz,cal_yr),
         weights = if_else(treated == 1,1,(1/x)),
         weights = round(weights, 3)) %>% 
  select(-x) %>% 
  arrange(town,cal_yr) %>% 
  mutate(id = 1000+group_indices(., town, treated_match)) %>% arrange(id) %>% 
  mutate(cz_yr = group_indices(.,cz,cal_yr)) %>% 
  left_join(imatches, by = c("town","treated_match")) %>% 
  mutate(match_distance = ifelse(weights>=1,0,distance)) %>% 
  relocate(match_distance, .after = treated_match) %>% 
  write_csv("data/csv/employment/ipanel_stack.csv")
