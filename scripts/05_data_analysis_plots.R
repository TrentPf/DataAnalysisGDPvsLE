library(tidyverse)
library(readr)

# OPTIONAL: Only run if wanting a minimalistic, clean theme for plots
theme_set(theme_minimal())

df <- read_csv("data_clean/wb_gdp_life_clean.csv")

# Summarize data

summary_table <- df %>%
  summarise(
    n = n(),
    gdp_mean = mean(gdp_per_capita),
    gdp_median = median(gdp_per_capita),
    gdp_sd = sd(gdp_per_capita),
    life_mean = mean(life_expectancy),
    life_median = median(life_expectancy),
    life_sd = sd(life_expectancy)
  )

write_csv(summary_table, "outputs/tables/summary_stats.csv")

# Make sure folders exist (prevents save errors)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Histogram (log GDP)
p1 <- ggplot(df, aes(x = log_gdp)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribution of Log GDP per Capita", x = "log(GDP per capita)", y = "Count")

ggsave("outputs/figures/hist_log_gdp.png", p1, width = 8, height = 5)

# Scatter: life expectancy vs log GDP (Primary data plot)

p2 <- ggplot(df, aes(x = log_gdp, y = life_expectancy)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Life Expectancy vs Log GDP per Capita",
    x = "log(GDP per capita)",
    y = "Life expectancy (years)"
  )

ggsave("outputs/figures/scatter_life_vs_loggdp.png", p2, width = 8, height = 5)

# Plot by income group
p3 <- ggplot(df, aes(x = log_gdp, y = life_expectancy, color = income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Life Expectancy vs Log GDP per Capita by Income Group",
    x = "log(GDP per capita)",
    y = "Life expectancy (years)"
  )

ggsave("outputs/figures/scatter_by_income.png", p3, width = 10, height = 6)