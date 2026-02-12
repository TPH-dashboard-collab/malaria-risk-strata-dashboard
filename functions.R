# Functions for Malaria Risk Strata Dashboard 
# This script contains the core data processing engine, including,
# risk stratification thresholds and seed aggregation logic.

library(tidyverse)

# Malaria Risk Color Palette (https://www.colorhexa.com/)
get_risk_colors <- function() {
  c("Very Low" = "#26a69a",  # Teal
    "Low"      = "#d4e157",  # Lime
    "Moderate" = "#ffb300",  # Orange
    "High"     = "#d32f2f")  # Red
}

# Risk Stratum Calculation 
# Thresholds are based on PfPR_2-10
calculate_risk_strata <- function(data) {
  data %>%
    mutate(risk_stratum = case_when(
      prevalenceRate <= 0.019 ~ "Very Low",
      prevalenceRate > 0.019 & prevalenceRate <= 0.033 ~ "Low",
      prevalenceRate > 0.033 & prevalenceRate <= 0.11 ~ "Moderate",
      prevalenceRate > 0.11 ~ "High",
      TRUE ~ "Unknown"
    )) %>%
    mutate(risk_stratum = factor(risk_stratum, 
                                 levels = c("Very Low", "Low", "Moderate", "High")))
}

# Data Aggregation 
# Results are aggregated across multiple seeds and filtered for target age groups.
# The target age group is 2-10 (0-5 is used as a placeholder here)
aggregate_data <- function(df, age_filter = "0-5") {
  df %>%
    filter(age_group == age_filter) %>%
    group_by(admin_1, admin_2, year, plan) %>%
    summarise(prevalenceRate = mean(prevalenceRate, na.rm = TRUE), .groups = "drop") %>%
    calculate_risk_strata()
}