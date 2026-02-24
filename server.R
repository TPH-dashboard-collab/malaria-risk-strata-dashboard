# Server for Malaria Risk Strata Dashboard 
# This script contains the Reactive Logic spatial join operations and visaulisations.
# updated: Sankey plots fully rendered

library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(DT)

#source("functions.R")

server <- function(input, output, session) {
  ## DATA ARCHITECTURE ##
  
  # Load and aggregate simulation data across seeds
  req(file.exists("tza_sample_data.csv"))
  df_raw <- read.csv("tza_sample_data.csv")
  
  ## Populate Age Selector Dynamically
  observe({
    req(df_raw)
    
    age_choices <- sort(unique(df_raw$age_group))
    
    # Set default to 0-5 if it exists, otherwise the first available
    default_age <- if ("0-5" %in% age_choices) "0-5" else age_choices[1]
    
    updateSelectInput(
      session,
      "age_select",
      choices = age_choices,
      selected = default_age
    )
  })
  
  
  ## Reactive aggregated data 
  full_data <- reactive({
    req(input$age_select)
    aggregate_data(df_raw, age_filter = input$age_select)
  })
  
  # Load and transform shapefile
  tza_shape <- st_read("shapefiles/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp") %>%
    st_transform(4326)
  
  
  ## REACTIVE FILTERS For UI Framework ##
  
  # Dynamic year slider detects year range from data
  output$year_slider_ui <- renderUI({
    req(full_data())
    yrs <- sort(unique(full_data()$year))
    sliderInput("year_range", "Movement Period:", min = min(yrs), max = max(yrs), 
                value = c(min(yrs), max(yrs)), step = 1, sep = "")
  })
  
  # Populate the Region Dropdown
  observe({
    req(full_data())
    
    region_list <- sort(unique(full_data()$admin_1))
    
    current_region <- isolate(input$region_select)
    
    # If current region still exists in new age data, keep it
    selected_region <- if (!is.null(current_region) &&
                           current_region %in% region_list) {
      current_region
    } else {
      "All"
    }
    
    updateSelectInput(
      session,
      "region_select",
      choices = c("All", region_list),
      selected = selected_region
    )
  })
  
  
  # Populate the District Dropdown with a cascading logic
  observeEvent(list(input$region_select, full_data()), {
    req(full_data())
    
    if (input$region_select == "All") {
      dist_list <- sort(unique(full_data()$admin_2))
    } else {
      dist_list <- full_data() %>%
        filter(admin_1 == input$region_select) %>%
        pull(admin_2) %>%
        unique() %>%
        sort()
    }
    
    current_dist <- isolate(input$dist_select)
    
    selected_dist <- if (!is.null(current_dist) &&
                         current_dist %in% dist_list) {
      current_dist
    } else {
      "All"
    }
    
    updateSelectInput(
      session,
      "dist_select",
      choices = c("All", dist_list),
      selected = selected_dist
    )
  })
  
  
  ## DUAL DATA STREAMS ##
  
  # All BAU visual outputs will depend on this filtered dataset
  bau_filtered <- reactive({
    req(full_data(), input$year_range)
    df <- full_data() %>% filter(plan == "BAU", year >= input$year_range[1], year <= input$year_range[2])
    if (input$region_select != "All") df <- df %>% filter(admin_1 == input$region_select)
    if (input$dist_select != "All") df <- df %>% filter(admin_2 == input$dist_select)
    return(df)
  })
  
  # All NSP visual outputs will depend on this filtered dataset
  nsp_filtered <- reactive({
    req(full_data(), input$plan_select, input$year_range)
    df <- full_data() %>% filter(plan == input$plan_select, year >= input$year_range[1], year <= input$year_range[2])
    if (input$region_select != "All") df <- df %>% filter(admin_1 == input$region_select)
    if (input$dist_select != "All") df <- df %>% filter(admin_2 == input$dist_select)
    return(df)
  })
  
  output$nsp_sankey_title <- renderText({ paste("Strategic Plan:", input$plan_select) })
  
  # DUAL SANKEY plots
  # Function to generate the Sankey data structure and plot
  render_sankey <- function(data_reactive, yrs) {
    s_list <- generate_sankey_data(data_reactive(), yrs[1]:yrs[2])
    plot_ly(type = "sankey", orientation = "h",
            node = list(label = s_list$nodes$label, color = s_list$nodes$color, pad = 15, thickness = 20),
            link = list(source = s_list$links$source, target = s_list$links$target, 
                        value = s_list$links$value, color = s_list$links$color))
  }
  output$sankey_bau <- renderPlotly({ req(bau_filtered()); render_sankey(bau_filtered, input$year_range) })
  output$sankey_nsp <- renderPlotly({ req(nsp_filtered()); render_sankey(nsp_filtered, input$year_range) })
  
  
  ## MAP OUTPUTS 
  
  # Helper function to avoid code duplication for the initial render
  render_base_map <- function(data_reactive, shape_object, yrs) {
    req(data_reactive(), shape_object)
    yr_start <- yrs[1]; yr_end <- yrs[2]
    
    
    hover_data <- data_reactive() %>%
      filter(year %in% c(yr_start, yr_end)) %>%
      select(admin_2, year, risk_stratum, prevalenceRate) %>%
      pivot_wider(names_from = year, values_from = c(risk_stratum, prevalenceRate))
    
    geo_data <- shape_object %>% left_join(hover_data, by = "admin_2")
    labels <- create_map_labels(geo_data, yr_start, yr_end)
    
    pal <- colorFactor(palette = unname(get_risk_colors()), 
                       levels = names(get_risk_colors()), na.color = "#D3D3D3")
    
    leaflet(geo_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(get(paste0("risk_stratum_", yr_end))),
        weight = 1, opacity = 1, color = "white", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      # Initial legend
      addLegend(
        pal = pal, 
        values = names(get_risk_colors()), 
        position = "bottomright", 
        title = paste0("Risk Stratum (", yr_end, ")"),
        layerId = "map_legend" # Give it an ID to make it easier to manage
      )
  }
  
  output$map_bau <- renderLeaflet({ isolate(render_base_map(bau_filtered, tza_shape, input$year_range)) })
  output$map_nsp <- renderLeaflet({ isolate(render_base_map(nsp_filtered, tza_shape, input$year_range)) })
  
  observeEvent(list(input$region_select, input$dist_select), {
    
    if (input$dist_select != "All") {
      target_geom <- tza_shape %>% filter(admin_2 == input$dist_select)
    } else if (input$region_select != "All") {
      target_geom <- tza_shape %>% filter(Region_Nam == input$region_select)
    } else {
      target_geom <- tza_shape
    }
    
    bbox <- st_bbox(target_geom)
    
    leafletProxy("map_bau") %>%
      flyToBounds(bbox[["xmin"]], bbox[["ymin"]],
                  bbox[["xmax"]], bbox[["ymax"]])
    
    leafletProxy("map_nsp") %>%
      flyToBounds(bbox[["xmin"]], bbox[["ymin"]],
                  bbox[["xmax"]], bbox[["ymax"]])
    
  }, ignoreInit = TRUE)
  
  
  
  
  # Logic to update proxies (runs whenever data/years change)
  # Update Colors, Labels, and Legends - triggers on ANY filter change
  observe({
    req(bau_filtered(), nsp_filtered(), input$year_range)
    
    yr_start <- input$year_range[1]
    yr_end   <- input$year_range[2]
    
    risk_cols <- get_risk_colors()
    pal <- colorFactor(unname(risk_cols), levels = names(risk_cols), na.color = "#D3D3D3")
    
    # UPDATE BAU MAP PROXY
    hover_bau <- bau_filtered() %>% 
      filter(year %in% c(yr_start, yr_end)) %>%
      select(admin_2, year, risk_stratum, prevalenceRate) %>%
      pivot_wider(names_from = year, values_from = c(risk_stratum, prevalenceRate))
    
    geo_bau <- tza_shape %>% left_join(hover_bau, by = "admin_2")
    labels_bau <- create_map_labels(geo_bau, yr_start, yr_end)
    
    leafletProxy("map_bau", data = geo_bau) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(get(paste0("risk_stratum_", yr_end))),
        weight = 1, opacity = 1, color = "white", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = labels_bau
      ) %>%
      clearControls() %>% # Removes old legend
      addLegend(
        pal = pal, 
        values = names(risk_cols), 
        position = "bottomright", 
        title = paste0("Risk Stratum (", yr_end, ")")
      )
    
    # UPDATE NSP MAP PROXY 
    hover_nsp <- nsp_filtered() %>% 
      filter(year %in% c(yr_start, yr_end)) %>%
      select(admin_2, year, risk_stratum, prevalenceRate) %>%
      pivot_wider(names_from = year, values_from = c(risk_stratum, prevalenceRate))
    
    geo_nsp <- tza_shape %>% left_join(hover_nsp, by = "admin_2")
    labels_nsp <- create_map_labels(geo_nsp, yr_start, yr_end)
    
    leafletProxy("map_nsp", data = geo_nsp) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(get(paste0("risk_stratum_", yr_end))),
        weight = 1, opacity = 1, color = "white", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = labels_nsp
      ) %>%
      clearControls() %>% 
      addLegend(
        pal = pal, 
        values = names(risk_cols), 
        position = "bottomright", 
        title = paste0("Risk Stratum (", yr_end, ")")
      )
  })
  
  # Persistence Summary: BAU 
  output$sum_bau <- renderUI({
    req(bau_filtered(), input$year_range)
    
    df_wide <- prep_sankey_data(bau_filtered(), input$year_range[1]:input$year_range[2])
    first_col <- paste0("yr_", input$year_range[1])
    last_col <- paste0("yr_", input$year_range[2])
    
    total_districts <- nrow(df_wide)
    stuck_districts <- df_wide %>%
      filter(!!sym(first_col) %in% c("High", "Moderate"),
             !!sym(last_col) %in% c("High", "Moderate")) %>% nrow()
    
    perc <- round((stuck_districts / total_districts) * 100, 1)
    
    # Using 'alert-secondary' for a neutral baseline look
    HTML(paste0(
      "<div class='alert alert-secondary'>",
      "<h4 class='alert-heading'>BAU Persistence</h4>",
      "<b>", perc, "%</b> of districts (", stuck_districts,"/",total_districts,") ", 
      "remain in High/Moderate strata under the status quo.",
      "</div>"
    ))
  })
  
  # Persistence Summary
  output$sum_nsp <- renderUI({
    req(nsp_filtered(), input$year_range)
    
    df_wide <- prep_sankey_data(nsp_filtered(), input$year_range[1]:input$year_range[2])
    first_col <- paste0("yr_", input$year_range[1])
    last_col <- paste0("yr_", input$year_range[2])
    
    total_districts <- nrow(df_wide)
    stuck_districts <- df_wide %>%
      filter(!!sym(first_col) %in% c("High", "Moderate"),
             !!sym(last_col) %in% c("High", "Moderate")) %>% nrow()
    
    perc <- round((stuck_districts / total_districts) * 100, 1)
    
   
    HTML(paste0(
      "<div class='alert alert-danger'>",
      "<h4 class='alert-heading'>NSP Persistence</h4>",
      "<b>", perc, "%</b> of districts (", stuck_districts,"/",total_districts,") ",
      "remain in High/Moderate strata despite this strategic plan.",
      "</div>"
    ))
  })
  
  # Table: BAU 
  output$tab_bau <- renderDT({
    req(bau_filtered(), input$year_range)
    data <- prep_persistence_data(bau_filtered(), input$year_range[1]:input$year_range[2])
    
    datatable(data, 
              # Rename columns
              colnames = c("Region", "District", "Start Risk", "End Risk", "Start PfPR", "End PfPR", "Abs. Change"),
              options = list(pageLength = 10, scrollX = TRUE, dom = 'tp')) %>%
      formatPercentage(c(5, 6, 7), 1)
  })
  
  # Table: NSP 
  output$tab_nsp <- renderDT({
    req(nsp_filtered(), input$year_range)
    data <- prep_persistence_data(nsp_filtered(), input$year_range[1]:input$year_range[2])
    
    datatable(data, 
              colnames = c("Region", "District", "Start Risk", "End Risk", "Start PfPR", "End PfPR", "Abs. Change"),
              options = list(pageLength = 10, scrollX = TRUE, dom = 'tp')) %>%
      formatPercentage(c(5, 6, 7), 1)
  })
  
}