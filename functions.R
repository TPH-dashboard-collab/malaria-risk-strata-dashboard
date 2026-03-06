# Functions for Malaria Risk Strata Dashboard 
# This script contains the analytical engine for data aggregation, 
# risk stratification, sankey transition flow generation, Map labels data
# Updated: Heat map data generation

library(tidyverse)

## Classic Malaria Risk Color Palette
get_risk_colors <- function() {
  c("Very Low" = "#26a69a",  # Teal/Green
    "Low"      = "#d4e157",  # Lime
    "Moderate" = "#ffb300",  # Amber/Orange
    "High"     = "#d32f2f")  # Red
}

## Strata Calculation 
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
                                 levels = c("Very Low", "Low",
                                            "Moderate", "High")))
}

## Aggregation Logic
# Results are aggregated across multiple seeds 
# and filtered for target age groups.
aggregate_data <- function(df, age_filter = "0-5") {
  df %>%
    filter(age_group == age_filter) %>%
    group_by(admin_1, admin_2, year, plan) %>%
    summarise(
      prevalenceRate = mean(prevalenceRate, na.rm = TRUE), 
      nHost = mean(nHost, na.rm = TRUE), # keep population for weighting
      .groups = "drop"
    ) %>%
    calculate_risk_strata()
}

# Function to prep data for sankey
prep_sankey_data <- function(df, selected_years) {
  req(length(selected_years) >= 2)
  years <- sort(selected_years)
  
  df_filtered <- df %>% filter(year %in% years)
  if (nrow(df_filtered) == 0) return(NULL)
  
  # Get population weights (use mean across selected years for stability)
  pop_weights <- df_filtered %>%
    group_by(admin_2) %>%
    summarise(nHost = mean(nHost, na.rm = TRUE), .groups = "drop")
  
  # Pivot the risk strata
  df_filtered %>%
    mutate(year_label = paste0("yr_", year)) %>%
    select(admin_2, year_label, risk_stratum) %>%
    
    # Use distinct to prevent duplicates if data isn't perfectly clean
    distinct(admin_2, year_label, .keep_all = TRUE) %>% 
    pivot_wider(names_from = year_label, values_from = risk_stratum) %>%
    left_join(pop_weights, by = "admin_2") %>%
    drop_na() 
}

generate_sankey_data <- function(df, selected_years, weight_type = "count") {
  years <- sort(selected_years)
  req(length(years) >= 2)
  
  # Prep Wide Data
  wide_df <- prep_sankey_data(df, selected_years)
  
  # If filters result in 0 districts, return a placeholder to prevent crash
  if (is.null(wide_df) || nrow(wide_df) == 0) return(NULL) 
  
  # Create nodes
  # We need a unique ID for each year stratt 
  nodes <- data.frame(label = character(), color = character(), total_val = numeric(), stringsAsFactors = FALSE)
  risk_cols <- get_risk_colors()
  
  for (yr in years) {
    col_name <- paste0("yr_", yr)
    if(!(col_name %in% colnames(wide_df))) next 
    
    for (stratum in names(risk_cols)) {
      node_subset <- wide_df[wide_df[[col_name]] == stratum, ]
      val <- if(weight_type == "nHost") sum(node_subset$nHost, na.rm = TRUE) else nrow(node_subset)
      
      nodes <- rbind(nodes, data.frame(
        label = paste0(stratum, " (", yr, ")"),
        color = unname(risk_cols[[stratum]]),
        total_val = val,
        stratum = stratum,
        year = yr
      ))
    }
  }
  
  if(nrow(nodes) == 0) return(NULL)
  nodes$id <- 0:(nrow(nodes) - 1)
  
  # Create links
  links <- data.frame(source = numeric(), target = numeric(), value = numeric())
  for (i in 1:(length(years) - 1)) {
    yr_start <- years[i]; yr_end <- years[i+1]
    col_start <- paste0("yr_", yr_start); col_end <- paste0("yr_", yr_end)
    
    # Ensure columns exist before grouping to prevent the "Elements don't exist" error
    if(!(col_start %in% colnames(wide_df)) || !(col_end %in% colnames(wide_df))) next
    
    transitions <- wide_df %>%
      group_by(across(all_of(c(col_start, col_end)))) %>%
      summarise(
        flow_value = if(weight_type == "nHost") sum(nHost, na.rm = TRUE) else n(),
        .groups = "drop"
      ) %>%
      rename(source_lab = 1, target_lab = 2)
    
    # Map labels to the created  Node IDs 
    for (j in 1:nrow(transitions)) {
      s_id <- nodes$id[nodes$stratum == transitions$source_lab[j] & nodes$year == yr_start]
      t_id <- nodes$id[nodes$stratum == transitions$target_lab[j] & nodes$year == yr_end]
      
      if(length(s_id) > 0 && length(t_id) > 0) {
        links <- rbind(links, data.frame(
          source = s_id, target = t_id, value = transitions$flow_value[j],
          #To easily track ink color is a transparent version of the source node color
          color = paste0(unname(risk_cols[[as.character(transitions$source_lab[j])]]), "80")
        ))
      }
    }
  }
  return(list(nodes = nodes, links = links))
}

prep_persistence_data <- function(df, selected_years) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  yrs <- range(selected_years)
  s_yr <- yrs[1]
  e_yr <- yrs[2]
  
  col_s_risk <- paste0("risk_stratum_", s_yr)
  col_e_risk <- paste0("risk_stratum_", e_yr)
  col_s_prev <- paste0("prevalenceRate_", s_yr)
  col_e_prev <- paste0("prevalenceRate_", e_yr)
  
  # Filter for districts that stayed High or Moderate
  wide_df <- df %>%
    filter(year %in% c(s_yr, e_yr)) %>%
    select(admin_1, admin_2, year, risk_stratum, prevalenceRate) %>%
    pivot_wider(names_from = year, values_from = c(risk_stratum, prevalenceRate))
  
  required_cols <- c(col_s_risk, col_e_risk, col_s_prev, col_e_prev)
  if (!all(required_cols %in% colnames(wide_df))) return(NULL)
  
  res <- wide_df %>%
    mutate(PfPR_Change = .data[[col_e_prev]] - .data[[col_s_prev]]) %>%
    filter(.data[[col_s_risk]] %in% c("High", "Moderate"),
           .data[[col_e_risk]] %in% c("High", "Moderate"))
  
  return(res)
}

## Heat Map Data
prep_heatmap_data <- function(df, selected_years) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  years     <- sort(selected_years)
  total_yrs <- length(years)
  end_yr    <- max(years)
  
  df_filtered <- df %>%
    filter(year %in% years) %>%
    select(admin_2, year, risk_stratum, prevalenceRate) %>%
    distinct(admin_2, year, .keep_all = TRUE) %>%
    mutate(
      risk_stratum = as.character(risk_stratum),
      is_high_mod  = risk_stratum %in% c("High", "Moderate")
    )
  
  if (nrow(df_filtered) == 0) return(NULL)
  
  # Years each district spent in High/Moderate (for tooltip)
  persistence_score <- df_filtered %>%
    group_by(admin_2) %>%
    summarise(persist_years = sum(is_high_mod, na.rm = TRUE), .groups = "drop")
  
  # Filter dominant stratum = most frequent stratum across the selected years
  dominant <- df_filtered %>%
    group_by(admin_2, risk_stratum) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(admin_2) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    select(admin_2, dominant_stratum = risk_stratum)
  
  # Years each district spent specifically in its dominant stratum
  dominant_years <- df_filtered %>%
    left_join(dominant, by = "admin_2") %>%
    group_by(admin_2) %>%
    summarise(
      dom_years = sum(risk_stratum == dominant_stratum, na.rm = TRUE),
      .groups = "drop"
    )
  
  # End-year prevalence from the already-filtered data (correct plan/age/region)
  end_prev <- df_filtered %>%
    filter(year == end_yr) %>%
    select(admin_2, end_prevalence = prevalenceRate) %>%
    distinct(admin_2, .keep_all = TRUE)
  
  # Stratum rank: High = 1 (top), Very Low = 4 (bottom)
  stratum_rank <- c("High" = 1, "Moderate" = 2, "Low" = 3, "Very Low" = 4)
  
  # Build ordering table:
  # stratum rank (High first)
  # years in that stratum (most years first)
  # end-year prevalence (highest first, tiebreaker)
  order_df <- dominant %>%
    left_join(dominant_years, by = "admin_2") %>%
    left_join(end_prev,       by = "admin_2") %>%
    mutate(stratum_rank = stratum_rank[dominant_stratum]) %>%
    arrange(stratum_rank, desc(dom_years), desc(end_prevalence))
  
  
  # plotly y-axis runs bottom-up, so we reverse for the factor
  ordered_levels <- rev(order_df$admin_2)
  
  df_out <- df_filtered %>%
    left_join(persistence_score, by = "admin_2") %>%
    left_join(dominant,          by = "admin_2") %>%
    left_join(end_prev,          by = "admin_2") %>%
    mutate(
      total_years = total_yrs,
      admin_2     = factor(admin_2, levels = ordered_levels)
    )
  
  return(df_out)
}

## Generate HTML Labels for the Leaflet Map 
create_map_labels <- function(geo_data, yr_start, yr_end) {
  labels <- sprintf(
    "<strong>%s</strong><br/>
    %s: %s (%.1f%% Prev.)<br/>
    %s: %s (%.1f%% Prev.)<br/>
    <em>Movement: %s to %s</em><br/>
    <small style='color: grey;'>*Percentages represent Prevalence Rates</small>",
    
    # Standardize the column names based on our selected years
    geo_data$admin_2,
    yr_start, geo_data[[paste0("risk_stratum_", yr_start)]], geo_data[[paste0("prevalenceRate_", yr_start)]] * 100,
    yr_end,   geo_data[[paste0("risk_stratum_", yr_end)]],   geo_data[[paste0("prevalenceRate_", yr_end)]] * 100,
    geo_data[[paste0("risk_stratum_", yr_start)]], geo_data[[paste0("risk_stratum_", yr_end)]]
  ) %>% lapply(htmltools::HTML)
  
  return(labels)
}