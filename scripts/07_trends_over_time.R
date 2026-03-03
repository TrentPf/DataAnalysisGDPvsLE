library(tidyverse)
library(readr)

df <- read_csv("data_clean/wb_gdp_life_clean.csv")

years_keep <- c(2000, 2010, 2020, 2023)

df2 <- df %>% filter(year %in% years_keep)

p <- ggplot(df2, aes(x = log_gdp, y = life_expectancy)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~year) +
  labs(title = "Life Expectancy vs log GDP per Capita Over Time")

ggsave("outputs/figures/scatter_over_time_facets.png", p, width = 10, height = 6)