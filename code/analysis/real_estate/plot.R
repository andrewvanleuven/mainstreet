library(tidyverse)

df <- read_csv("data/csv/real_estate/effect.csv")

library(ggplot2)

ggplot(df) +
  geom_line(aes(x = msp_age, y = dist_msp), size = 1L, colour = "#0c4c8a") +
  geom_line(aes(x = msp_age, y = dist), size = 1L, colour = "#0c4c8a") +
  geom_line(aes(x = msp_age, y = dist_msp + dist), size = 1L, colour = "#0c4c8a") +
  theme_minimal()
