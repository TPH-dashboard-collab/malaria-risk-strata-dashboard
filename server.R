# Server for Malaria Risk Strata Dashboard 
# This script contains the Reactive Logic spatial join operations and visaulisations.


library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(DT)



server <- function(input, output, session) {
  
  ## DATA ARCHITECTURE ##
  
  # Load and aggregate simulation data across seeds
  full_data <- reactive({
    req(file.exists("tza_sample_data.csv"))
    df <- read.csv("tza_sample_data.csv")
    aggregate_data(df, age_filter = "0-5")
  })
  
  # Load and transform shapefile
  tza_shape <- reactive({
    shp <- st_read("shapefiles/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp") %>%
      st_transform(4326) # Transform shapefiles to WGS84 for Leaflet compatibility
    return(shp)
  })
  
  ## REACTIVE FILTERS For UI Framework ##
  
  # Dynamic year slider detects year range from data
  output$year_slider_ui <- renderUI({
    req(full_data())
    yrs <- sort(unique(full_data()$year))
    sliderInput("year_range", "Movement Period:", 
                min = min(yrs), max = max(yrs), 
                value = c(min(yrs), max(yrs)), step = 1, sep = "")
  })
  
  # Populate the Region Dropdown
  observe({
    req(full_data()) # Waits until CSV is loaded and aggregated
    
    # Get unique regions from admin_1
    region_list <- sort(unique(full_data()$admin_1))
    updateSelectInput(session, "region_select", choices = c("All", region_list))
  })
  
  # Populate the District Dropdown with a cascading logic
  observeEvent(input$region_select, {
    req(full_data())
    
    # If Region is "All", the District dropdown shows all districts
    if (input$region_select == "All") {
      dist_list <- sort(unique(full_data()$admin_2))
    } else {
      # If a specific region is selected, shows only districts in that region
      dist_list <- full_data() %>%
        filter(admin_1 == input$region_select) %>%
        pull(admin_2) %>% unique() %>% sort()
    }
    updateSelectInput(session, "dist_select", choices = c("All", dist_list))
  })
  
  ## Centralized Filtering ##
  
  # All visual outputs will depend on this filtered dataset
  filtered_df <- reactive({
    req(full_data(), input$plan_select, input$year_range)
    
    df <- full_data() %>%
      filter(plan == input$plan_select,
             year >= input$year_range[1],
             year <= input$year_range[2])
    
    if (input$region_select != "All") df <- df %>% filter(admin_1 == input$region_select)
    if (input$dist_select != "All") df <- df %>% filter(admin_2 == input$dist_select)
    
    return(df)
  })
  
  ## Placeholders for core Visualisations ##
  
  # Basic Map 
  
  # Focus on showing the correct strata for the latest year for now
  output$risk_map <- renderLeaflet({
    req(filtered_df(), tza_shape())
    
  # Extract data for the map (latest year selected on slider)
    latest_yr <- input$year_range[2]
    map_data <- filtered_df() %>% filter(year == latest_yr)
    
    
    # Join simulation data to shapefile using 'admin_2'
    # We use the shapefile as the base to ensure all polygons are drawn
    geo_data <- tza_shape() %>% left_join(map_data, by = "admin_2")
    
    risk_cols <- get_risk_colors()
    pal <- colorFactor(palette = unname(risk_cols), levels = names(risk_cols), na.color = "#D3D3D3")
    
    leaflet(geo_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(risk_stratum),
        weight = 1, color = "white", fillOpacity = 0.7,
        label = ~paste0(admin_2, ": ", risk_stratum)
      ) %>%
      addLegend(pal = pal, values = names(risk_cols), title = "Risk Stratum")
  })
  
  # Placeholder for Sankey plot
  output$sankey_plot <- renderPlotly({
    plotly_empty() %>% 
      layout(title = "Sankey Movement Diagram")
  })
  
  # Placeholder for Sunmary Table
  output$persistence_table <- renderDT({
    req(filtered_df())
    datatable(filtered_df() %>% select(admin_1, admin_2, year, risk_stratum, prevalenceRate),
              options = list(pageLength = 5, scrollX = TRUE))
  })
}