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
  select(20,19,15:18)

w_n_nei <- n_nei %>% 
  filter(str_detect(town, ', WI')) %>% 
  select(-(jobs:ests_lead)) %>% 
  left_join(wi_df, by = c("town","cal_yr"))

# NEED TO FILTER OUT NA's & REPEAT FOR STACK

# Prep Iowa Taxable Retail Sales ------------------------------------------

ia_df <- read_csv("data/csv/iowa_dor/real_taxable_sales_pk.csv") %>% 
  mutate(st = "IA",
         cal_yr = year,
         town = paste(name,st,sep = ", ")) %>% 
  select(town,cal_yr,nominal_sales:real_pk_sales)

i_n_nei <- n_nei %>% 
  filter(str_detect(town, ', IA')) %>% 
  select(-(jobs:ests_lead)) %>% 
  left_join(ia_df, by = c("town","cal_yr"))

# NEED TO GO BACK AND ALLOW FOR MORE 9-YR RANGES
