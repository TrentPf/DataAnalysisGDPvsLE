library(tidyverse)
library(readr)
library(broom)

df <- read_csv("data_clean/wb_gdp_life_clean.csv")

# Correlations
cor_raw <- cor(df$gdp_per_capita, df$life_expectancy, use = "complete.obs", method = "spearman")
cor_log <- cor(df$log_gdp, df$life_expectancy, use = "complete.obs", method = "pearson")

cor_table <- tibble(
  measure = c("Spearman (GDP vs LifeExp)", "Pearson (logGDP vs LifeExp)"),
  value = c(cor_raw, cor_log)
)

write_csv(cor_table, "outputs/tables/correlations.csv")

# Regression models
m1 <- lm(life_expectancy ~ gdp_per_capita, data = df)
m2 <- lm(life_expectancy ~ log_gdp, data = df)

# Save model outputs
write_csv(tidy(m1), "outputs/tables/model1_coefficients.csv")
write_csv(glance(m1), "outputs/tables/model1_fit.csv")

write_csv(tidy(m2), "outputs/tables/model2_coefficients.csv")
write_csv(glance(m2), "outputs/tables/model2_fit.csv")