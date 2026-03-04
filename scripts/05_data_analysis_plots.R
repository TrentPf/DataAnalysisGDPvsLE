library(tidyverse)
library(readr)

# OPTIONAL: Only run if wanting a minimalistic, clean theme for plots
theme_set(theme_minimal())

# Make sure folders exist (prevents save errors)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

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

# -----------------------------
# R^2 helpers
# -----------------------------

# Moving R^2 labels to legend
r2_income_log <- df %>%
  group_by(income) %>%
  summarise(
    r_squared = summary(lm(life_expectancy ~ log_gdp))$r.squared,
    .groups = "drop"
  )

legend_labels_log <- r2_income_log %>%
  mutate(label = paste0(income, " (R² = ", round(r_squared, 3), ")")) %>%
  select(income, label)

legend_labels_log <- setNames(legend_labels_log$label, legend_labels_log$income)

r2_income_gdp <- df %>%
  group_by(income) %>%
  summarise(
    r_squared = summary(lm(life_expectancy ~ gdp_per_capita))$r.squared,
    .groups = "drop"
  )

legend_labels_gdp <- r2_income_gdp %>%
  mutate(label = paste0(income, " (R² = ", round(r_squared, 3), ")")) %>%
  select(income, label)

legend_labels_gdp <- setNames(legend_labels_gdp$label, legend_labels_gdp$income)
# Move labels to legend
legend_labels_log <- r2_income_log %>%
  mutate(label = paste0(income, " (R² = ", round(r_squared, 3), ")")) %>%
  select(income, label)

legend_labels_log


# Histogram: Log GDP vs Life Expectancy 
# more readable, graph scale is condensed while maintaining relative dispersion
p1 <- ggplot(df, aes(x = log_gdp)) +
  geom_histogram(
    bins = 35,
    fill = "steelblue",
    color = "black",
    alpha = 0.85
  ) +
  labs(
    title = "Distribution of Log GDP per Capita",
    x = "log(GDP per capita)",
    y = "Count"
  ) +
  theme_minimal()

ggsave("outputs/figures/hist_log_gdp.png", p1, width = 8, height = 5)

# Scatter: Log GDP vs Life Expectancy (Primary data plot)

p2 <- ggplot(df, aes(x = log_gdp, y = life_expectancy)) +
  geom_point(alpha = 0.35, size = 1.4) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  geom_text(
    aes(
      x = max(log_gdp) - 0.2,
      y = max(life_expectancy),
      label = paste0("R² = ", round(r2_log, 3))
    ),
    inherit.aes = FALSE,
    size = 5
  ) +
  labs(
    title = "Life Expectancy vs Log GDP per Capita",
    x = "log(GDP per capita)",
    y = "Life expectancy (years)"
  )

ggsave("outputs/figures/scatter_life_vs_loggdp.png", p2, width = 8, height = 5)

# Scatter: Log GDP vs Life Expectancy, plot by income group
p3 <- ggplot(df, aes(x = log_gdp, y = life_expectancy)) +
  # Points
  geom_point(aes(color = income), alpha = 0.25, size = 1.3) +
  
  # Black outline for regression lines
  geom_smooth(
    aes(group = income),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 2.6,
    linetype = "solid"
  ) +
  # Colored regression lines on top
  geom_smooth(
    aes(color = income),
    method = "lm",
    se = FALSE,
    linewidth = 1.4,
    linetype = "solid"
  ) +
  scale_color_brewer(
    palette = "Set1",
    labels = legend_labels_gdp
  ) +
  labs(
    title = "Life Expectancy vs Log GDP per Capita by Income Group",
    x = "log(GDP per capita)",
    y = "Life expectancy (years)",
    color = "Income Group"
  ) +
  
  theme_minimal()

ggsave("outputs/figures/scatter_by_income.png", p3, width = 10, height = 6, dpi = 300)

# Scatter: GDP vs Life Expectancy
# Unmodified, unscaled, as bare bones as the data set can be rendered
p4 <- ggplot(df, aes(x = gdp_per_capita, y = life_expectancy)) +
  geom_point(alpha = 0.35, size = 1.4) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  geom_text(
    aes(
      x = max(gdp_per_capita) * 0.8,
      y = max(life_expectancy),
      label = paste0("R² = ", round(r2_gdp, 3))
    ),
    inherit.aes = FALSE,
    size = 5
  ) +
  labs(
    title = "Life Expectancy vs GDP per Capita",
    x = "GDP per Capita (USD)",
    y = "Life Expectancy (years)"
  )

ggsave("outputs/figures/scatter_life_vs_gdp.png", p4, width = 8, height = 5, dpi = 300)

# Scatter: GDP vs Life Expectancy
p5 <- ggplot(df, aes(x = gdp_per_capita, y = life_expectancy)) +
  # Points
  geom_point(aes(color = income), alpha = 0.25, size = 1.3) +
  
  # Black outline for regression lines
  geom_smooth(
    aes(group = income),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 2.6,
    linetype = "solid"
  ) +
  # Colored regression lines on top
  geom_smooth(
    aes(color = income),
    method = "lm",
    se = FALSE,
    linewidth = 1.4,
    linetype = "solid"
  ) +
  scale_color_brewer(
    palette = "Set1",
    labels = legend_labels_gdp
    ) +
  labs(
    title = "Life Expectancy vs GDP per Capita by Income Group",
    x = "GDP per Capita (USD)",
    y = "Life Expectancy (years)",
    color = "Income Group",
    linetype = "Income Group"
  ) + 
  theme_minimal()

ggsave("outputs/figures/scatter_gdp_by_income.png", p5, width = 10, height = 6, dpi = 300)


# Recommended by AI 
# Scatter: GDP vs Life Expectancy, Log-scaled axis
# Log-scaling the axis, but not the GDP units, so GDP units of dollars can be maintained while being more readable
p6 <- ggplot(df, aes(x = gdp_per_capita, y = life_expectancy)) +
  geom_point(alpha = 0.35, size = 1.4) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_x_log10() +
  geom_text(
    aes(
      x = max(gdp_per_capita) * 0.8,
      y = max(life_expectancy),
      label = paste0("R² = ", round(r2_gdp, 3))
    ),
    inherit.aes = FALSE,
    size = 5
  ) +
  labs(
    title = "Life Expectancy vs GDP per Capita (Log Scale)",
    x = "GDP per Capita (USD, log scale)",
    y = "Life Expectancy (years)"
  )


ggsave("outputs/figures/scatter_gdp_logscale.png", p6, width = 8, height = 5, dpi = 300)