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
    sliderInput("year_range", "Movement Period:", min = min(yrs), max = max(yrs), 
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
  
  
  # DUAL DATA STREAMS 
  
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
  
  # Simple MAPS 
  render_simple_map <- function(data_reactive, shape_reactive, yrs) {
    req(data_reactive(), shape_reactive())
    # Extract data for the map
    map_data <- data_reactive() %>% filter(year == yrs[2]) # Latest year only
    
    # Join simulation data to shapefile using 'admin_2'
    geo_data <- shape_reactive() %>% left_join(map_data, by = "admin_2")
    pal <- colorFactor(unname(get_risk_colors()), levels = names(get_risk_colors()), na.color = "#D3D3D3")
    
    leaflet(geo_data) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(risk_stratum), weight = 1, color = "white", fillOpacity = 0.7,
                  label = ~paste0(admin_2, ": ", risk_stratum)) %>%
      addLegend(pal = pal, values = names(get_risk_colors()), position = "bottomright", title = "Risk Stratum")
  }
  output$map_bau <- renderLeaflet({ render_simple_map(bau_filtered, tza_shape, input$year_range) })
  output$map_nsp <- renderLeaflet({ render_simple_map(nsp_filtered, tza_shape, input$year_range) })
  
  # DUAL TABLES 
  output$tab_bau <- renderDT({
    req(bau_filtered())
    datatable(bau_filtered() %>% select(admin_1, admin_2, year, risk_stratum, prevalenceRate),
              options = list(pageLength = 5, scrollX = TRUE))
  })
  output$tab_nsp <- renderDT({
    req(nsp_filtered())
    datatable(nsp_filtered() %>% select(admin_1, admin_2, year, risk_stratum, prevalenceRate),
              options = list(pageLength = 5, scrollX = TRUE))
  })
}