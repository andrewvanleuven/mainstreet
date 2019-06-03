library(tidyverse)
library(tidycensus)
library(tigris)
library(panelr)
options(tigris_class = "sf",tigris_use_cache = TRUE,scipen = 999)
# Read in data ------------------------------------------------------------
data <- read_csv("data/csv/msp_data.csv") %>% arrange(geoid)
midwest <- data %>%
  filter(st %in% c("Iowa","Michigan","Wisconsin","Ohio"),
         metro_type != "Metropolitan Statistical Area") %>%
  gather(starts_with("jobs_"), key = "year", value = "jobs") %>%
  mutate(year = as.numeric(str_replace(year, 'jobs_', '20'))) %>%
  select(geoid:st,year,jobs,everything())
# Convert to Panel --------------------------------------------------------
midwest.panel <- panel_data(midwest, id = geoid, wave = year)
midwest.panel
  



