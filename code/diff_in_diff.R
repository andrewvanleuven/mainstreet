suppressMessages({library(tidyverse)
library(sf)
library(rleuven)
library(tigris)
library(plm)
library(did)
library(panelr)})
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

df <- read_csv("data/csv/panel.csv")

jobs <- panel_data(df, id = townid, wave = time)
jobs

model <- wbm(downtown_jobs ~ time + treated | town + as.factor(cal_year) | time*treated, data = jobs)
model
