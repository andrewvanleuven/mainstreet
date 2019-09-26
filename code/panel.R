suppressMessages({library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
library(panelr)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Read in Data ------------------------------------------------------------
colnames <- read_csv("hidden/esri/column_names.csv") %>% pull(names)
df <- read_csv("hidden/esri/iowa_buffered.csv") %>% `colnames<-`(colnames)
rm(colnames)
msp <- read_csv("data/csv/msp_data.csv") %>% 
  filter(st == "Iowa") %>% 
  select(-geoid,-st,-cty_fips,-(cbsa:metro_type),-(jobs_02:jobs_15))

# Create Panel ------------------------------------------------------------
base_panel <- df %>% select(-(st:zip),-(population_code:y)) %>% 
  filter(!is.na(employee_size)) %>% 
  mutate(naics = str_sub((as.character(naics)), end = -3),
         naic2 = str_sub((as.character(naics)), end = -5),
         rtl = ifelse(naic2 %in% c(42,44,45,72),1,0),
         trn = ifelse(naic2 %in% c(48,49),1,0),
         mfg = ifelse(naic2 %in% c(11,21,22,23,31,32,33),1,0),
         svc = ifelse(naic2 %in% c(51,52,53,54,55,56,61,62,71,81,92),1,0)) %>%
  group_by(name,year,buffer) %>% 
  summarise(jobs = sum(as.numeric(employee_size)),
            rtl = sum(as.numeric(rtl)),
            trn = sum(as.numeric(trn)),
            mfg = sum(as.numeric(mfg)),
            svc = sum(as.numeric(svc)),
            estabs = n())
jobs_panel <- base_panel %>% select(name:jobs) %>% 
  pivot_wider(names_from = buffer, values_from = jobs, names_prefix = "buffer_") %>% 
  replace(is.na(.), 0) %>% 
  left_join(.,msp, by = "name") %>% 
  select(-(msp_accr:cty_seat),-(pop2010:rucc)) %>% 
  write_csv("data/csv/jobs_panel.csv")

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
stack_1 <- msp_yrs %>% rename(name_stack1 = name, cyear_stack1 = msp_yr)
stack_2 <- msp_yrs %>% rename(name_stack2 = name, cyear_stack2 = msp_yr)
stack_3 <- msp_yrs %>% rename(name_stack3 = name, cyear_stack3 = msp_yr)
stack_4 <- msp_yrs %>% rename(name_stack4 = name, cyear_stack4 = msp_yr)

unstacked_yrs <- left_join(unstacked,stack_1) %>% 
  left_join(.,stack_2) %>% 
  left_join(.,stack_3) %>% 
  left_join(.,stack_4) %>% 
  mutate(ryear_stack1 = year - cyear_stack1,
         ryear_stack2 = year - cyear_stack2,
         ryear_stack3 = year - cyear_stack3,
         ryear_stack4 = year - cyear_stack4) %>%
  rename(community = name) #%>% 
  select(community,year,name_stack1:name_stack4,
         cyear_stack1:cyear_stack4,ryear_stack1:ryear_stack4)

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
jobs_stack_match <- jobs_panel %>% 
  rename(stack_name = name,
         cyear = year,
         buff_0 = buffer_0,
         buff_1 = buffer_1) %>% 
  select(stack_name,cyear,buff_0:buff_1)
jobs_match <- jobs_panel %>% 
  select(name,year,cz:msp_yr,buffer_0:buffer_1)
  
panel <- left_join(stacked,jobs_match, by = c("name","year")) %>% 
  select(name,year,cz:buffer_1,everything()) %>% 
  left_join(.,jobs_stack_match, by = c("stack_name","cyear"))


