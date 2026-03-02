# Purely for validating patterns exist and those patterns exist within more than one source's data set

library(tidyverse)
library(readr)

owid <- read_csv("data_raw/owid_lifeexp_gdp.csv")

glimpse(owid)