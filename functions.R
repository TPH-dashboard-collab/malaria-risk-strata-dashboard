# Functions for Malaria Risk Strata Dashboard 
# This script contains the analytical engine for data aggregation, 
# risk stratification, and sankey transition flow generation.


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

# prepare sankey data
prep_sankey_data <- function(df, selected_years) {
  req(length(selected_years) >= 2) # Need at least two years to show movement
  
  years <- sort(selected_years)
  
  wide_df <- df %>%
    filter(year %in% years) %>%
    select(admin_2, year, risk_stratum) %>%
    mutate(year = paste0("yr_", year)) %>%
    # Use distinct to prevent duplicates should the data not be perfectly clean
    distinct(admin_2, year, .keep_all = TRUE) %>% 
    pivot_wider(names_from = year, values_from = risk_stratum) %>%
    drop_na()
  
  return(wide_df)
}


# Generated Data for sankey plot
generate_sankey_data <- function(df, selected_years) {
  years <- sort(selected_years)
  req(length(years) >= 2)
  
  # Prepare Wide Data: One row per district, columns for each year's stratum
  wide_df <- df %>%
    filter(year %in% years) %>%
    select(admin_2, year, risk_stratum) %>%
    mutate(year_label = paste0("yr_", year)) %>%
    distinct(admin_2, year, .keep_all = TRUE) %>% 
    pivot_wider(id_cols = admin_2, names_from = year_label, values_from = risk_stratum) %>%
    drop_na() # Sankey flows must be continuous for each district
  
  # Create Nodes
  # We need a unique ID for each year stratt ("High-2026", "High-2027" for instance)
  nodes <- data.frame(label = character(), color = character(), stringsAsFactors = FALSE)
  risk_cols <- get_risk_colors()
  
  for (yr in years) {
    for (stratum in names(risk_cols)) {
      nodes <- rbind(nodes, data.frame(
        label = paste0(stratum, " (", yr, ")"),
        color = risk_cols[[stratum]],
        stratum = stratum,
        year = yr
      ))
    }
  }
  nodes$id <- 0:(nrow(nodes) - 1)
  
  # Create Links (Flows between adjacent years)
  links <- data.frame(source = numeric(), target = numeric(), value = numeric())
  
  for (i in 1:(length(years) - 1)) {
    yr_start <- years[i]
    yr_end <- years[i+1]
    col_start <- paste0("yr_", yr_start)
    col_end <- paste0("yr_", yr_end)
    
    # Count transitions between these two years
    transitions <- wide_df %>%
      group_by(across(all_of(c(col_start, col_end)))) %>%
      summarise(count = n(), .groups = "drop") %>%
      rename(source_lab = 1, target_lab = 2)
    
    # Map labels to the created  Node IDs 
    for (j in 1:nrow(transitions)) {
      s_id <- nodes$id[nodes$stratum == transitions$source_lab[j] & nodes$year == yr_start]
      t_id <- nodes$id[nodes$stratum == transitions$target_lab[j] & nodes$year == yr_end]
      
      links <- rbind(links, data.frame(
        source = s_id,
        target = t_id,
        value = transitions$count[j],
        # To easily track ink color is a transparent version of the source node color
        color = paste0(risk_cols[[as.character(transitions$source_lab[j])]], "80") 
      ))
    }
  }
  
  return(list(nodes = nodes, links = links))
}