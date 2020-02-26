library(tidyverse)
library(tidycensus)
library(tigris)
library(panelr)
library(rleuven)
library(haven)
library(skimr)
options(tigris_class = "sf",tigris_use_cache = TRUE,scipen = 999)
# Read in data ------------------------------------------------------------
data <- read_csv("data/csv/msp_data.csv") %>% arrange(geoid)
midwest <- data %>%
  filter(st %in% c("Iowa","Michigan","Wisconsin","Ohio"),
         metro_type != "Metropolitan Statistical Area") %>%
  gather(starts_with("jobs_"), key = "year", value = "jobs") %>%
  mutate(year = as.numeric(str_replace(year, 'jobs_', '20')),
         micro_sa = ifelse(metro_type == "Micropolitan Statistical Area", 1, 0)) %>%
  select(geoid:st,year,jobs,everything(),-(pop2010:metro_type),-cty_fips) %>%
  mutate(activemsp = ifelse(msp_yr <= year & msp_yr != 0, 1, 0))
# Convert to Panel --------------------------------------------------------
mwpanel <- panel_data(midwest, id = geoid, wave = year) 
# Run regression ----------------------------------------------------------
model <- wbm(jobs ~ activemsp + year | factor(cz), data = mwpanel)
summary(model)