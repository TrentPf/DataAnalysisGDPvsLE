library(WDI)
library(tidyverse)
library(readr)

wb_raw <- WDI(
  country = "all", # Fetch all countries in datasets
  indicator = c(
    gdp_per_capita = "NY.GDP.PCAP.CD", # Code to fetch GDP data set
    life_expectancy = "SP.DYN.LE00.IN" # Code to fetch life expectancy data set
  ),
  start = 2000, # Trying to keep time range reasonably small, but AI and experts suggest this range
  end = 2023,
  extra = TRUE,
  cache = NULL
)

write_csv(wb_raw, "data_raw/worldbank_gdp_life_raw.csv") # Write to file for use

glimpse(wb_raw) # Quick sanity check for dataset integrity
summary(wb_raw$gdp_per_capita)
summary(wb_raw$life_expectancy)