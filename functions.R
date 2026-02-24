library(tidyverse)

# Classic Malaria Risk Color Palette
get_risk_colors <- function() {
  c("Very Low" = "#26a69a",  # Teal/Green
    "Low"      = "#d4e157",  # Lime
    "Moderate" = "#ffb300",  # Amber/Orange
    "High"     = "#d32f2f")  # Red
}

# Strata Calculation 
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

# Aggregation Logic
aggregate_data <- function(df, age_filter = "0-5") {
  df %>%
    filter(age_group == age_filter) %>%
    group_by(admin_1, admin_2, year, plan) %>%
    summarise(prevalenceRate = mean(prevalenceRate, na.rm = TRUE), .groups = "drop") %>%
    calculate_risk_strata()
}


prep_sankey_data <- function(df, selected_years) {
  req(length(selected_years) >= 2) # Need at least two years to show movement
  
  years <- sort(selected_years)
  
  wide_df <- df %>%
    filter(year %in% years) %>%
    select(admin_2, year, risk_stratum) %>%
    mutate(year = paste0("yr_", year)) %>%
    # Use distinct to prevent duplicates if data isn't perfectly clean
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
  
  # 2. Create Nodes
  # We need a unique ID for "High-2026", "High-2027", etc.
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
  
  #Create Links (Flows between adjacent years)
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
    
    # Map labels to the Node IDs created in step 2
    for (j in 1:nrow(transitions)) {
      s_id <- nodes$id[nodes$stratum == transitions$source_lab[j] & nodes$year == yr_start]
      t_id <- nodes$id[nodes$stratum == transitions$target_lab[j] & nodes$year == yr_end]
      
      links <- rbind(links, data.frame(
        source = s_id,
        target = t_id,
        value = transitions$count[j],
        # Link color is a transparent version of the source node color
        color = paste0(risk_cols[[as.character(transitions$source_lab[j])]], "80") 
      ))
    }
  }
  
  return(list(nodes = nodes, links = links))
}


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


# Generate HTML Labels for Leaflet Map
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

