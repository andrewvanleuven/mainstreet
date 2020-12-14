library(foreign)
library(tidyverse)

# Import and write as .dta ------------------------------------------------
pops <- read_csv("data/csv/universe/pop_1990to2019.csv") %>% 
  select(-city_fips) %>% rename(cal_yr = year)
unemp <- read_csv("data/csv/universe/cty_varying.csv")
msp <- read_csv("data/csv/universe/msp_universe.csv") %>% 
  mutate(st = str_replace_all(st, "Iowa", "IA"),
         st = str_replace_all(st, "Michigan", "MI"),
         st = str_replace_all(st, "Ohio", "OH"),
         st = str_replace_all(st, "Wisconsin", "WI"),
         town = paste(name,st,sep = ", ")) %>% 
  select(town,cty_fips,cty_seat) 

dfn <- read_csv("data/csv/employment/epanel_nn.csv") %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(msp, by = "town") %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  separate(town, into = c('city','st'), sep = ", ", remove = F) %>% 
  filter(rucc > 3) %>% 
  mutate(jobs_p1k = (jobs/(population/1000)),
         ests_p10k = (ests/(population/1000)),
         jobsl_p1k = (jobs_lead/(population/1000)),
         estsl_p10k = (ests_lead/(population/1000)),
         st = st)
write.dta(dfn,"data/stata/nn.dta")

dfs <- read_csv("data/csv/employment/epanel_stack.csv") %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(msp, by = "town") %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  separate(town, into = c('city','st'), sep = ", ", remove = F) %>% 
  filter(distance > 5 | distance == 0, rucc > 3) %>% 
  mutate(jobs_p1k = (jobs/(population/1000)),
         ests_p10k = (ests/(population/1000)),
         jobsl_p1k = (jobs_lead/(population/1000)),
         estsl_p10k = (ests_lead/(population/1000)),
         st = st)
write.dta(dfs,"data/stata/stack.dta")

#Retail Jobs & Ests
dfen <- read_csv("data/csv/employment/rpanel_nn_buff2.csv") %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(msp, by = "town") %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  separate(town, into = c('city','st'), sep = ", ", remove = F) %>% 
  filter(rucc > 3) %>% 
  mutate(jobs_p1k = (jobs/(population/1000)),
         ests_p10k = (ests/(population/1000)),
         jobsl_p1k = (jobs_lead/(population/1000)),
         estsl_p10k = (ests_lead/(population/1000)),
         st = st)
write.dta(dfen,"data/stata/r_est_nn.dta")

dfes <- read_csv("data/csv/employment/rpanel_stack_buff2.csv") %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(msp, by = "town") %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  separate(town, into = c('city','st'), sep = ", ", remove = F) %>% 
  filter(distance > 5 | distance == 0, rucc > 3) %>% 
  mutate(jobs_p1k = (jobs/(population/1000)),
         ests_p10k = (ests/(population/1000)),
         jobsl_p1k = (jobs_lead/(population/1000)),
         estsl_p10k = (ests_lead/(population/1000)),
         st = st) 
write.dta(dfes,"data/stata/r_est_stack.dta")

#Buffers 0 & 1 (jobs/ests)
ldfn <- read_csv("data/csv/employment/epanel_nn_buff2.csv") %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(msp, by = "town") %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  separate(town, into = c('city','st'), sep = ", ", remove = F) %>% 
  filter(rucc > 3) %>% 
  mutate(jobs_p1k = (jobs/(population/1000)),
         ests_p10k = (ests/(population/1000)),
         jobsl_p1k = (jobs_lead/(population/1000)),
         estsl_p10k = (ests_lead/(population/1000)),
         st = st)
write.dta(ldfn,"data/stata/nn_buff2.dta")

ldfs <- read_csv("data/csv/employment/epanel_stack_buff2.csv") %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(msp, by = "town") %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  separate(town, into = c('city','st'), sep = ", ", remove = F) %>% 
  filter(distance > 5 | distance == 0, rucc > 3) %>% 
  mutate(jobs_p1k = (jobs/(population/1000)),
         ests_p10k = (ests/(population/1000)),
         jobsl_p1k = (jobs_lead/(population/1000)),
         estsl_p10k = (ests_lead/(population/1000)),
         st = st) 
write.dta(ldfs,"data/stata/stack_buff2.dta")

#Iowa Retail Sales

#ia_pops <- read_csv("data/csv/universe/cty_populations.csv")
#ia_pops <- read_csv("data/csv/iowa_dor/real_taxable_sales_pk.csv")
bea_ia <- read_csv("data/csv/time_varying/bea_profile.csv") %>% 
  rename(cty_fips = 1)

idfn <- read_csv("data/csv/employment/i_panel_nn.csv") %>% 
  left_join(msp, by = "town") %>% 
  left_join(bea_ia, by = c("cty_fips", "cal_yr")) %>% 
  separate(town, into = c('city','st'), sep = ", ", remove = F) #%>% filter(rucc > 3)
write.dta(idfn,"data/stata/iowa_nn.dta")

idfs <- read_csv("data/csv/employment/i_panel_stack.csv") %>% 
  left_join(msp, by = "town") %>% 
  left_join(bea_ia, by = c("cty_fips", "cal_yr")) %>% 
  separate(town, into = c('city','st'), sep = ", ", remove = F) %>% 
  filter(match_distance > 5 | match_distance == 0, rucc > 3)
write.dta(idfs,"data/stata/iowa_stack.dta")
 
idfnn <- idfn %>% 
left_join(unemp %>% select(-cty_pop), by = c("cty_fips","cal_yr")) %>% 
  filter(!is.na(lag_unemp)) %>% 
  group_by(id) %>% 
  mutate(gyrs = n()) %>% 
  filter(gyrs == 9) %>% 
  select(-gyrs)
write.dta(idfnn,"data/stata/iowa_nn_n.dta")
# CZ MFG LQs --------------------------------------------------------------
cz_lq <- read_csv("hidden/wd_cz_lq.csv") %>% 
  rename(cal_yr = year)

reg_nn <- dfn %>% left_join(cz_lq, by = c("cz","cal_yr")) %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  filter(!is.na(lq_mfg)) %>% 
  group_by(id) %>% 
  mutate(gyrs = n()) %>% 
  filter(gyrs == 9) %>% 
  select(-(total_emp:us_hmfg),-gyrs)
write.dta(reg_nn,"data/stata/nn_mfg.dta")

retail_nn <- dfen %>% left_join(cz_lq, by = c("cz","cal_yr")) %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  filter(!is.na(lq_mfg)) %>% 
  group_by(id) %>% 
  mutate(gyrs = n()) %>% 
  filter(gyrs == 9) %>% 
  select(-(total_emp:us_hmfg),-gyrs)
write.dta(retail_nn,"data/stata/r_est_nn_mfg.dta")


# County GDP --------------------------------------------------------------
gdp <- read_csv("data/csv/time_varying/gdp_cty.csv")
gdp_only <- read_csv("data/csv/time_varying/real_gdp_cty.csv")


gdp_reg_nn <- dfn %>% left_join(gdp_only, by = c("cty_fips","cal_yr")) %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  filter(!is.na(gdp_pk)) %>% 
  group_by(id) %>% 
  mutate(gyrs = n()) %>% 
  filter(gyrs == 9)  
write.dta(gdp_reg_nn,"data/stata/nn_gdp.dta")

gdp_retail_nn <- dfen %>% left_join(gdp_only, by = c("cty_fips","cal_yr")) %>% 
  left_join(pops, by = c("town", "cal_yr")) %>% 
  left_join(unemp, by = c("cty_fips", "cal_yr")) %>% 
  filter(!is.na(gdp_pk)) %>% 
  group_by(id) %>% 
  mutate(gyrs = n()) %>% 
  filter(gyrs == 9)  
write.dta(gdp_retail_nn,"data/stata/r_est_nn_gdp.dta")



# BEA Income --------------------------------------------------------------

bea <- read_csv("data/csv/time_varying/bea_flow.csv")

bea_reg_nn <- dfn %>% 
  select(-cty_pop) %>% 
  left_join(bea, by = c("cty_fips","cal_yr")) %>% 
  filter(!is.na(real_outflow_pc)) %>% 
  group_by(id) %>% 
  mutate(gyrs = n()) %>% 
  filter(gyrs == 9) %>% 
  select(-gyrs)
write.dta(bea_reg_nn,"data/stata/nn_bea.dta")

bea_retail_nn <- dfen %>% 
  select(-cty_pop) %>% 
  left_join(bea, by = c("cty_fips","cal_yr")) %>% 
  filter(!is.na(real_outflow_pc)) %>% 
  group_by(id) %>% 
  mutate(gyrs = n()) %>% 
  filter(gyrs == 9) %>% 
  select(-gyrs)
write.dta(bea_retail_nn,"data/stata/r_est_nn_bea.dta")

bea_reg_stk <- dfs %>% 
  select(-cty_pop) %>% 
  left_join(bea, by = c("cty_fips","cal_yr")) %>% 
  filter(!is.na(real_outflow_pc)) %>% 
  group_by(id) %>% 
  mutate(gyrs = n()) %>% 
  filter(gyrs == 9) %>% 
  select(-gyrs)
write.dta(bea_reg_stk,"data/stata/stack_bea.dta")

bea_retail_stk <- dfes %>% 
  select(-cty_pop) %>% 
  left_join(bea, by = c("cty_fips","cal_yr")) %>% 
  filter(!is.na(real_outflow_pc)) %>% 
  group_by(id) %>% 
  mutate(gyrs = n()) %>% 
  filter(gyrs == 9) %>% 
  select(-gyrs)
write.dta(bea_retail_stk,"data/stata/r_est_stack_bea.dta")
