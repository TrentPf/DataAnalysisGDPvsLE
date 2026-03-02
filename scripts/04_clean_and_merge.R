library(tidyverse)
library(readr)

wb_raw <- read_csv("data_raw/worldbank_gdp_life_raw.csv")

wb_clean <- wb_raw %>%
  filter(region != "Aggregates") %>%
  filter(!is.na(gdp_per_capita), !is.na(life_expectancy)) %>%
  filter(gdp_per_capita > 0) %>%
  mutate(log_gdp = log(gdp_per_capita)) %>%
  select(iso2c, country, year, region, income, gdp_per_capita_log_gdp, life_expectancy)

write_csv(wb_clean, "data_clean/wb_gdp_life_clean.csv")