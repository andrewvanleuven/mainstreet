suppressMessages({
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(beepr)
  library(rleuven)
  library(fastDummies)
  library(tigris)})

# Read in Data ------------------------------------------------------------
n_nei <- read_csv("data/csv/employment/epanel_nn.csv")

stack <- read_csv("data/csv/employment/epanel_stack.csv")

# Prep Wisconsin Residential Property Assessed Value ----------------------

wi_df <- read_csv("data/csv/wisc_dor/wisc_dor.csv") %>% 
  mutate(st = "WI",
         cal_yr = year,
         town = paste(name,st,sep = ", ")) %>% 
  select(20,19,15,18)

w_n_nei <- n_nei %>% 
  filter(str_detect(town, ', WI')) %>% 
  select(-(jobs:ests_lead)) %>% 
  left_join(wi_df, by = c("town","cal_yr")) %>% 
  relocate(c(value,val_per_acre), .after = cal_yr) %>% 
  group_by(id) %>% mutate(x = sum(val_per_acre)) %>% 
  ungroup() %>% filter(!is.na(x)) %>% select(-x) %>% 
  write_csv("data/csv/employment/wpanel_nn.csv")

w_stack <- stack %>% 
  filter(str_detect(town, ', WI')) %>% 
  select(-(jobs:ests_lead)) %>% 
  left_join(wi_df, by = c("town","cal_yr")) %>% 
  relocate(c(value,val_per_acre), .after = cal_yr) %>% 
  group_by(id) %>% mutate(x = sum(val_per_acre)) %>% 
  ungroup() %>% filter(!is.na(x)) %>% select(-x) %>% 
  write_csv("data/csv/employment/wpanel_stack.csv")


# Prep Iowa Taxable Retail Sales ------------------------------------------

n_nei80 <- read_csv("data/csv/employment/ipanel_nn.csv")
stack80 <- read_csv("data/csv/employment/ipanel_stack.csv")

ia_df <- read_csv("data/csv/iowa_dor/real_taxable_sales_pk.csv") %>% 
  mutate(st = "IA",
         cal_yr = year,
         town = paste(name,st,sep = ", "),
         sales = real_pk_sales,
         sales_lead = lead(sales, n = 1, default = NA)) %>% 
  select(town,cal_yr,sales,sales_lead) 

i_n_nei <- n_nei80 %>% 
  left_join(ia_df, by = c("town","cal_yr")) %>% 
  relocate(c(sales,sales_lead), .after = cal_yr) %>% 
  write_csv("data/csv/employment/i_panel_nn.csv")
  
i_stack <- stack80 %>% 
  left_join(ia_df, by = c("town","cal_yr")) %>% 
  relocate(c(sales,sales_lead), .after = cal_yr) %>% 
  write_csv("data/csv/employment/i_panel_stack.csv")

