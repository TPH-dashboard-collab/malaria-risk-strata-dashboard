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
                                 levels = c("Very Low", "Low", "Moderate", "High")))
}

## Aggregation Logic
aggregate_data <- function(df, age_filter = "0-5") {
  df %>%
    filter(age_group == age_filter) %>%
    group_by(admin_1, admin_2, year, plan) %>%
    summarise(
      prevalenceRate = mean(prevalenceRate, na.rm = TRUE), 
      nHost = mean(nHost, na.rm = TRUE), # keep population
      .groups = "drop"
    ) %>%
    calculate_risk_strata()
}

# Function to prep data for sankey
prep_sankey_data <- function(df, selected_years) {
  req(length(selected_years) >= 2)
  years <- sort(selected_years)
  
  # Get population weights (use mean across selected years for stability)
  pop_weights <- df %>%
    filter(year %in% years) %>%
    group_by(admin_2) %>%
    summarise(nHost = mean(nHost, na.rm = TRUE), .groups = "drop")
  
  # Pivot the risk strata
  df %>%
    filter(year %in% years) %>%
    mutate(year_label = paste0("yr_", year)) %>%
    select(admin_2, year_label, risk_stratum) %>%
    pivot_wider(names_from = year_label, values_from = risk_stratum) %>%
    left_join(pop_weights, by = "admin_2") %>%
    drop_na() # Ensure consistent cohort across all selected years
}

## Function to generate sankey data
generate_sankey_data <- function(df, selected_years, weight_type = "count") {
  years <- sort(selected_years)
  req(length(years) >= 2)
  
  # Prepare Wide Data
  wide_df <- prep_sankey_data(df, selected_years)
  
  # If filters result in 0 districts, return a placeholder to prevent crash
  if (nrow(wide_df) == 0) return(NULL) 
  
  # Create nodes
  nodes <- data.frame(label = character(), color = character(), stringsAsFactors = FALSE)
  risk_cols <- get_risk_colors()
  for (yr in years) {
    for (stratum in names(risk_cols)) {
      nodes <- rbind(nodes, data.frame(label = paste0(stratum, " (", yr, ")"), 
                                       color = risk_cols[[stratum]], stratum = stratum, year = yr))
    }
  }
  nodes$id <- 0:(nrow(nodes) - 1)
  
  # Create links
  links <- data.frame(source = numeric(), target = numeric(), value = numeric())
  for (i in 1:(length(years) - 1)) {
    yr_start <- years[i]; yr_end <- years[i+1]
    col_start <- paste0("yr_", yr_start); col_end <- paste0("yr_", yr_end)
    
    # Ensure columns exist before grouping to prevent the "Elements don't exist" error
    req(col_start %in% names(wide_df), col_end %in% names(wide_df))
    
    transitions <- wide_df %>%
      group_by(across(all_of(c(col_start, col_end)))) %>%
      summarise(
        flow_value = if(weight_type == "nHost") sum(nHost, na.rm = TRUE) else n(),
        .groups = "drop"
      ) %>%
      rename(source_lab = 1, target_lab = 2)
    
    for (j in 1:nrow(transitions)) {
      s_id <- nodes$id[nodes$stratum == transitions$source_lab[j] & nodes$year == yr_start]
      t_id <- nodes$id[nodes$stratum == transitions$target_lab[j] & nodes$year == yr_end]
      
      if(length(s_id) > 0 && length(t_id) > 0) {
        links <- rbind(links, data.frame(
          source = s_id, target = t_id, value = transitions$flow_value[j],
          color = paste0(risk_cols[[as.character(transitions$source_lab[j])]], "80")
        ))
      }
    }
  }
  return(list(nodes = nodes, links = links))
}

## Function to prep data for persistence tables 
prep_persistence_data <- function(df, selected_years) {
  yrs <- range(selected_years)
  
  df %>%
    filter(year %in% yrs) %>%
    select(admin_1, admin_2, year, risk_stratum, prevalenceRate) %>%
    pivot_wider(names_from = year, 
                values_from = c(risk_stratum, prevalenceRate)) %>%
    mutate(PfPR_Change = .[[paste0("prevalenceRate_", yrs[2])]] - .[[paste0("prevalenceRate_", yrs[1])]]) %>%
    
    # Filter for districts that stayed High or Moderate
    filter(.[[paste0("risk_stratum_", yrs[1])]] %in% c("High", "Moderate"),
           .[[paste0("risk_stratum_", yrs[2])]] %in% c("High", "Moderate"))
}


## Generate HTML Labels for Leaflet Map 
create_map_labels <- function(geo_data, yr_start, yr_end) {
  
  # Standardize the column names based on selected years
  start_strata_col <- paste0("risk_stratum_", yr_start)
  end_strata_col   <- paste0("risk_stratum_", yr_end)
  start_prev_col   <- paste0("prevalenceRate_", yr_start)
  end_prev_col     <- paste0("prevalenceRate_", yr_end)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>
    %s: %s (%.1f%%)<br/>
    %s: %s (%.1f%%)<br/>
    <em>Movement: %s to %s</em>",
    geo_data$admin_2,
    yr_start, geo_data[[start_strata_col]], geo_data[[start_prev_col]] * 100,
    yr_end,   geo_data[[end_strata_col]],   geo_data[[end_prev_col]] * 100,
    geo_data[[start_strata_col]], geo_data[[end_strata_col]]
  ) %>% lapply(htmltools::HTML)
  
  return(labels)
}
