# Server for Malaria Risk Strata Dashboard 
# This script contains the Reactive Logic spatial join operations and visaulisations.
# Updated: Summary boxes on all tabs, persistence heatmap, removed side-by-side logic

library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(DT)
library(htmlwidgets)


server <- function(input, output, session) {
  
  ## DATA ARCHITECTURE ##
  
  # Load and aggregate simulation data across seeds
  
  req(file.exists("tza_sample_data.csv"))
  df_raw <- read.csv("tza_sample_data.csv")
  
  observe({
    req(df_raw)
    updateSelectInput(session, "plan_select",
                      choices  = sort(unique(df_raw$plan)),
                      selected = unique(df_raw$plan)[1])
  })
  
  observe({
    req(df_raw)
    age_choices  <- sort(unique(df_raw$age_group))
    default_age  <- if ("0-5" %in% age_choices) "0-5" else age_choices[1]
    updateSelectInput(session, "age_select",
                      choices = age_choices, selected = default_age)
  })
  
  # Reactive aggregated data
  full_data <- reactive({
    req(input$age_select)
    aggregate_data(df_raw, age_filter = input$age_select)
  })
  
  # Load and transform shapefile
  tza_shape <- st_read("shapefiles/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp") %>%
    st_transform(4326) # Transform shapefiles to WGS84 for Leaflet compatibility
  
  ## REACTIVE UI FILTERS ##
  
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
    
    # Wait for the data to be aggregated
    req(full_data())
    
    region_list     <- sort(unique(full_data()$admin_1))
    
    # If current region still exists in new age data, keep it
    current_region  <- isolate(input$region_select)
    
    selected_region <- if (!is.null(current_region) &&
                           current_region %in% region_list) current_region else "All"
    
    updateSelectInput(session, "region_select",
                      choices = c("All", region_list), selected = selected_region)
  })
  
  
  # Populate the District Dropdown with a cascading logic
  observe({
    req(full_data())
    
    # Identify current selections
    selected_reg <- input$region_select
    
    # Logic to determine district choices
    # Provide choices for every district in the simulation data
    dist_choices <- if (is.null(selected_reg) || selected_reg == "All") {
      sort(unique(full_data()$admin_2))
      
    } else {
      # Provide choices only for districts within selected region
      full_data() %>% filter(admin_1 == selected_reg) %>% 
        pull(admin_2) %>% unique() %>% sort()
    }
    
    # If the region is "All", we force the district to "All" (Initial load + Reset).
    # If a region is chosen, we default to "All" so the plots show the Regional summary
    updateSelectInput(session, "dist_select",
                      choices = c("All", dist_choices),
                      selected = "All")
  })
  
  
  # All visual outputs will depend on this filtered dataset
  filtered_data <- reactive({
    req(full_data(), input$plan_select, input$year_range)
    df <- full_data() %>%
      filter(plan == input$plan_select,
             year >= input$year_range[1],
             year <= input$year_range[2])
    if (input$region_select != "All") df <- df %>% filter(admin_1 == input$region_select)
    if (input$dist_select   != "All") df <- df %>% filter(admin_2 == input$dist_select)
    df
  })
  
  ## SHARED SUMMARY CALCULATIONS ##
  # Central reactive so all tabs draw from the same numbers
  
  summary_stats <- reactive({
    req(filtered_data(), input$year_range)
    
    df  <- filtered_data()
    
    yrs <- input$year_range
    
    # District counts per stratum at end year
    end_year_data <- df %>% filter(year == yrs[2])
    
    total_districts <- n_distinct(end_year_data$admin_2)
    
    high_count      <- end_year_data %>% filter(risk_stratum == "High")%>%
      n_distinct("admin_2") %>% suppressWarnings()
    
    high_count      <- sum(end_year_data$risk_stratum == "High",     na.rm = TRUE)
    
    mod_count       <- sum(end_year_data$risk_stratum == "Moderate", na.rm = TRUE)
    
    low_count       <- sum(end_year_data$risk_stratum %in% c("Low", "Very Low"), na.rm = TRUE)
    
    # Persistence
    wide_df <- prep_sankey_data(df, yrs[1]:yrs[2])
    
    first_col <- paste0("yr_", yrs[1])
    
    last_col  <- paste0("yr_", yrs[2])
    
    # Initialize Values
    persist_n    <- 0
    persist_perc <- 0
    improved_n   <- 0
    declined_to_high <- 0
    declined_to_mod  <- 0
    
    if (!is.null(wide_df) && nrow(wide_df) > 0 &&
        first_col %in% names(wide_df) && last_col %in% names(wide_df)) {
      
      total_w      <- nrow(wide_df)
      
      persist_n    <- wide_df %>%
        filter(!!sym(first_col) %in% c("High", "Moderate"),
               !!sym(last_col)  %in% c("High", "Moderate")) %>% nrow()
      
      improved_n   <- wide_df %>%
        filter(!!sym(first_col) %in% c("High", "Moderate"),
               !!sym(last_col)  %in% c("Low", "Very Low"))  %>% nrow()
      
      persist_perc <- if (total_w > 0) round((persist_n / total_w) * 100, 1) else 0
      
      # Districts that worsened INTO High 
      declined_to_high <- wide_df %>%
        filter(!!sym(first_col) != "High",
               !!sym(last_col)  == "High") %>% nrow()
      
      # Districts that worsened INTO Moderate 
      declined_to_mod <- wide_df %>%
        filter(!!sym(first_col) %in% c("Low", "Very Low"),
               !!sym(last_col)  == "Moderate") %>% nrow()
    }
    
    # Mean prevalence change
    start_prev <- df %>% filter(year == yrs[1]) %>% pull(prevalenceRate)
    
    end_prev   <- df %>% filter(year == yrs[2]) %>% pull(prevalenceRate)
    
    med_change <- round((mean(end_prev, na.rm = TRUE) - mean(start_prev, na.rm = TRUE)) * 100, 2)
    
    list(
      total_districts  = total_districts,
      high_count       = high_count,
      mod_count        = mod_count,
      low_count        = low_count,
      persist_n        = persist_n,
      persist_perc     = persist_perc,
      improved_n       = improved_n,
      declined_to_high = declined_to_high,
      declined_to_mod  = declined_to_mod,
      med_change       = med_change,
      yr_start         = yrs[1],
      yr_end           = yrs[2]
    )
  })
  
  ## HELPER: value box HTML ##
  
  make_vbox <- function(value, label, type = "") {
    cls <- paste("summary-valuebox", if (nchar(type) > 0) paste0("vb-", type) else "")
    
    tags$div(class = cls,
             tags$div(class = "vb-value", value),
             tags$div(class = "vb-label", label))
  }
  
  make_alert <- function(text, type = "info") {
    bg <- switch(type,
                 danger  = "#fdecea",
                 warning = "#fff8e1",
                 success = "#e8f5e9",
                 "#e3f2fd")
    border <- switch(type,
                     danger  = "#d32f2f",
                     warning = "#ffb300",
                     success = "#26a69a",
                     "#1976d2")
    tags$div(class = "summary-alert",
             style = paste0("background:", bg, "; border-left: 4px solid ", border, ";"),
             HTML(text))
  }
  
  ## TAB 1: RISK MAP SUMMARY 
  
  output$summary_maps <- renderUI({
    req(summary_stats())
    s <- summary_stats()
    tagList(
      fluidRow(
        column(3, make_vbox(s$total_districts, "Total Districts")),
        column(3, make_vbox(s$high_count, paste0("High Risk (", s$yr_end, ")"), "danger")),
        column(3, make_vbox(s$mod_count,  paste0("Moderate Risk (", s$yr_end, ")"), "warning")),
        column(3, make_vbox(s$low_count,  paste0("Low / Very Low (", s$yr_end, ")"), "success"))
      ),
      fluidRow(
        column(12,
               make_alert(paste0(
                 "<b>Snapshot (", s$yr_end, "):</b> Of <b>", s$total_districts,
                 "</b> districts, <b>", s$high_count + s$mod_count,
                 "</b> remain in High or Moderate risk strata. ",
                 "Mean prevalence change since <b>", s$yr_start, "</b>: ",
                 ifelse(s$med_change >= 0, "+", ""), s$med_change, " pp."
               ), type = if (s$med_change < 0) "success" else "warning")
        )
      )
    )
  })
  
  ## TAB 2: SANKEY SUMMARY
  
  output$summary_sankey <- renderUI({
    req(summary_stats())
    s <- summary_stats()
    tagList(
      fluidRow(
        column(3, make_vbox(s$declined_to_high, "Declined to High Risk",     "danger")),
        column(3, make_vbox(s$declined_to_mod,  "Declined to Moderate Risk", "warning")),
        column(3, make_vbox(s$improved_n,       "Improved to Low/Very Low",  "success")),
        column(3, make_vbox(s$persist_n,        "Stayed High/Moderate",      "danger"))
      ),
      fluidRow(
        column(12,
               make_alert(paste0(
                 "<b>Flow Summary (", s$yr_start, " → ", s$yr_end, "):</b> ",
                 "<b>", s$declined_to_high, "</b> district(s) worsened into High risk and ",
                 "<b>", s$declined_to_mod, "</b> declined into Moderate. ",
                 "<b>", s$improved_n, "</b> improved out of elevated strata."
               ), type = if ((s$declined_to_high + s$declined_to_mod) > s$improved_n) "danger" else "warning")
        )
      )
    )
  })
  
  
  
  ## TAB 3: HEATMAP SUMMARY ##
  
  output$summary_heatmap <- renderUI({
    req(summary_stats(), filtered_data(), input$year_range)
    s    <- summary_stats()
    yrs  <- input$year_range[1]:input$year_range[2]
    n_yr <- length(yrs)
    
    # Districts persistently High/Moderate for ALL years
    hm_data <- prep_heatmap_data(filtered_data(), yrs)
    always_high <- 0
    if (!is.null(hm_data)) {
      always_high <- hm_data %>%
        group_by(admin_2) %>%
        summarise(all_high = all(is_high_mod), .groups = "drop") %>%
        filter(all_high) %>% nrow()
    }
    
    tagList(
      fluidRow(
        column(3, make_vbox(s$persist_n,                "Moderate/High Districts",          "danger")),
        column(3, make_vbox(always_high,                paste0("Elevated All ", n_yr, " Years"), "danger")),
        column(3, make_vbox(paste0(s$persist_perc, "%"), "Persistence Rate",                "warning")),
        column(3, make_vbox(n_yr,                       "Years in Period"))
      ),
      fluidRow(
        column(12,
               make_alert(paste0(
                 "<b>Heatmap guide:</b> Districts are ordered top-to-bottom by the number of years ",
                 "spent in High or Moderate strata. <b>", always_high,
                 "</b> district(s) remained elevated for the entire <b>",
                 s$yr_start, "–", s$yr_end, "</b> period."
               ), type = "info")
        )
      )
    )
  })
  
  
  ## TAB 4: PERSISTENCE TABLE SUMMARY ##
  
  output$summary_persistence <- renderUI({
    req(summary_stats())
    s <- summary_stats()
    tagList(
      fluidRow(
        column(4, make_vbox(s$persist_n,   "Persistently Elevated Districts", "danger")),
        column(4, make_vbox(paste0(s$persist_perc, "%"), "% of All Districts", "warning")),
        column(4, make_vbox(s$improved_n,  "Districts That Improved", "success"))
      ),
      fluidRow(
        column(12,
               make_alert(paste0(
                 "<b>Persistence (", s$yr_start, "–", s$yr_end, "):</b> ",
                 "Districts listed below started <em>and</em> ended in High or Moderate strata. ",
                 "These <b>", s$persist_n, "</b> districts should be prioritised for intensified intervention."
               ), type = "danger")
        )
      )
    )
  })
  
  ## SANKEY PLOT ##
  
  # Function to generate the Sankey data structure and plot
  render_sankey <- function(data_reactive, yrs) {
    
    # Switch between start→end only, or full year-by-year steps
    selected_years <- if (!is.null(input$sankey_mode) && input$sankey_mode == "stepwise") {
      yrs[1]:yrs[2]           # all intermediate years
    } else {
      c(yrs[1], yrs[2])       # just endpoints (default)
    }
    
    s_list     <- generate_sankey_data(data_reactive(), selected_years, weight_type = input$sankey_weight)
    unit_label <- ifelse(input$sankey_weight == "nHost", "People", "Districts")
    
    plot_ly(
      type = "sankey", orientation = "h",
      node = list(
        label      = s_list$nodes$label,
        color      = s_list$nodes$color,
        pad = 15, thickness = 20,
        customdata = round(s_list$nodes$total_val),
        hovertemplate = paste0("%{label}: %{customdata} ", unit_label, "<extra></extra>")
      ),
      link = list(
        source        = s_list$links$source,
        target        = s_list$links$target,
        value         = s_list$links$value,
        color         = s_list$links$color,
        hovertemplate = paste0("Flow: %{value} ", unit_label, "<extra></extra>")
      )
    ) %>%
      layout(font   = list(size = 12),
             margin = list(l = 50, r = 50, b = 20, t = 40))
  }
  
  output$sankey_main <- renderPlotly({
    req(filtered_data())
    render_sankey(filtered_data, input$year_range)
  })
  
  ## MAP OUTPUT ##
  # Helper function to avoid code duplication for the initial render
  render_base_map <- function(data_reactive, shape_object, yrs) {
    req(data_reactive(), shape_object)
    
    yr_start <- yrs[1]; yr_end <- yrs[2]
    
    # Data for hover tooltip
    hover_data <- data_reactive() %>%
      filter(year %in% c(yr_start, yr_end)) %>%
      select(admin_2, year, risk_stratum, prevalenceRate) %>%
      pivot_wider(names_from = year, values_from = c(risk_stratum, prevalenceRate))
    
    # Join simulation data to shapefile using 'admin_2'
    geo_data <- shape_object %>% left_join(hover_data, by = "admin_2")
    
    labels   <- create_map_labels(geo_data, yr_start, yr_end)
    
    pal      <- colorFactor(palette = unname(get_risk_colors()),
                            levels  = names(get_risk_colors()), na.color = "#D3D3D3")
    
    leaflet(geo_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(get(paste0("risk_stratum_", yr_end))),
        weight = 1, opacity = 1, color = "white", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label        = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend(pal = pal, values = names(get_risk_colors()),
                position = "bottomright",
                title    = paste0("Risk Stratum (", yr_end, ")"),
                layerId  = "map_legend")
  }
  
  output$map_main <- renderLeaflet({
    req(filtered_data(), input$year_range)
    render_base_map(filtered_data, tza_shape, input$year_range)
  })
  
  observeEvent(list(input$region_select, input$dist_select), {
    target_geom <- if (input$dist_select != "All") {
      tza_shape %>% filter(admin_2 == input$dist_select)
    } else if (input$region_select != "All") {
      tza_shape %>% filter(Region_Nam == input$region_select)
    } else {
      tza_shape
    }
    
    bbox <- st_bbox(target_geom)
    leafletProxy("map_main") %>%
      flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  }, ignoreInit = TRUE)
  
  
  # Logic to update proxies (runs whenever data/years change)
  # Update Colors, Labels, and Legends - triggers on ANY filter change
  observe({
    
    req(filtered_data(), input$year_range)
    
    # PROTECTIVE CHECK: If filters leave no data, don't update the map 
    if (nrow(filtered_data()) == 0) return(NULL)
    
    yr_start  <- input$year_range[1]; yr_end <- input$year_range[2]
    
    risk_cols <- get_risk_colors()
    
    pal       <- colorFactor(unname(risk_cols), levels = names(risk_cols), na.color = "#D3D3D3")
    
    # Process data
    hover_data <- filtered_data() %>%
      filter(year %in% c(yr_start, yr_end)) %>%
      select(admin_2, year, risk_stratum, prevalenceRate) %>%
      pivot_wider(names_from = year, values_from = c(risk_stratum, prevalenceRate))
    
    geo_data <- tza_shape %>% left_join(hover_data, by = "admin_2")
    
    # Create labels (Check if geo_data has rows)
    req(nrow(geo_data) > 0)
    labels   <- create_map_labels(geo_data, yr_start, yr_end)
    
    # Update Map Proxy
    leafletProxy("map_main", data = geo_data) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        fillColor = ~pal(get(paste0("risk_stratum_", yr_end))),
        weight = 1, color = "white", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = labels
      ) %>%
      
      # Add initial legend
      addLegend(pal = pal, values = names(risk_cols),
                position = "bottomright",
                title    = paste0("Risk (", yr_end, ")"))
  })
  
  
  ## PERSISTENCE HEATMAP 
  output$heatmap_main <- renderPlotly({
    
    req(filtered_data(), input$year_range, input$heatmap_top_n)
    
    yrs     <- input$year_range[1]:input$year_range[2]
    
    hm_data <- prep_heatmap_data(filtered_data(), yrs)
    
    validate(need(!is.null(hm_data) && nrow(hm_data) > 0,
                  "No data available for heatmap."))
    
    # Apply Top-N filter (slice the most-persistent districts) 
    top_n_val <- as.integer(input$heatmap_top_n)
    
    # Factor levels are already in bottom→top order; reverse to get top→bottom ranking
    all_districts <- levels(hm_data$admin_2)   # bottom → top (plotly default)
    
    ordered_top   <- rev(all_districts)         # top → bottom (High first)
    
    if (top_n_val < length(ordered_top)) {
      keep_districts <- ordered_top[1:top_n_val]
      hm_data <- hm_data %>% filter(admin_2 %in% keep_districts) %>%
        mutate(admin_2 = factor(admin_2, levels = rev(keep_districts)))
    }
    
    # Colour mapping for strata
    strata_num <- c("Very Low" = 1, "Low" = 2, "Moderate" = 3, "High" = 4)
    
    risk_cols  <- get_risk_colors()
    
    hm_data <- hm_data %>%
      mutate(strata_num = unname(strata_num[risk_stratum]))
    
    colorscale <- list(
      list(0,      "#26a69a"),
      list(0.2499, "#26a69a"),
      list(0.25,   "#d4e157"),
      list(0.4999, "#d4e157"),
      list(0.50,   "#ffb300"),
      list(0.7499, "#ffb300"),
      list(0.75,   "#d32f2f"),
      list(1,      "#d32f2f")
    )
    
    # Tooltip: stratum + persistence score + end prev 
    end_yr_val <- max(input$year_range)
    
    hm_data <- hm_data %>%
      mutate(hover_text = paste0(
        "<b>", admin_2, "</b><br>",
        "Year: ", year, "<br>",
        "Stratum: <b>", risk_stratum, "</b><br>",
        "Persistence score: ", persist_years, "/", total_years, " yrs in High/Moderate<br>",
        "End-year prevalence (", end_yr_val, "): ",
        round(end_prevalence * 100, 1), "%"
      ))
    
    n_districts <- n_distinct(hm_data$admin_2)
    
    row_height  <- max(18, min(35, 600 / max(n_districts, 1)))
    
    # Build the heatmap 
    p <- plot_ly(
      data          = hm_data,
      x             = ~year,
      y             = ~admin_2,
      z             = ~strata_num,
      type          = "heatmap",
      colorscale    = colorscale,
      zmin = 1, zmax = 4,
      text          = ~hover_text,
      hovertemplate = "%{text}<extra></extra>",
      showscale     = FALSE,
      xgap          = 2,   # grid gap between columns (years)
      ygap          = 2    # grid gap between rows (districts)  
    ) %>%
      layout(
        xaxis = list(
          title    = "Year",
          dtick    = 1,
          tickmode = "linear",
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title    = "",
          tickfont = list(size = min(11, max(8, row_height * 0.55)), color = "black"),
          showgrid = FALSE,
          zeroline = FALSE
        ),
        margin = list(l = 160, r = 120, b = 60, t = 20),
        plot_bgcolor  = "#f0f0f0",   # background behind cells makes grid visible
        paper_bgcolor = "white",
        
        # Manual strata legend 
        annotations = list(
          list(x = 1.02, y = 1.00, xref = "paper", yref = "paper",
               text = "<b>Risk Strata</b>", showarrow = FALSE,
               xanchor = "left", font = list(size = 12)),
          
          list(x = 1.02, y = 0.92, xref = "paper", yref = "paper",
               text = paste0("<span style='color:#d32f2f; font-size:18px;'>&#9632;</span>",
                             " <span style='font-size:11px;'>High</span>"),
               showarrow = FALSE, xanchor = "left"),
          
          list(x = 1.02, y = 0.84, xref = "paper", yref = "paper",
               text = paste0("<span style='color:#ffb300; font-size:18px;'>&#9632;</span>",
                             " <span style='font-size:11px;'>Moderate</span>"),
               showarrow = FALSE, xanchor = "left"),
          
          list(x = 1.02, y = 0.76, xref = "paper", yref = "paper",
               text = paste0("<span style='color:#d4e157; font-size:18px;'>&#9632;</span>",
                             " <span style='font-size:11px;'>Low</span>"),
               showarrow = FALSE, xanchor = "left"),
          
          list(x = 1.02, y = 0.68, xref = "paper", yref = "paper",
               text = paste0("<span style='color:#26a69a; font-size:18px;'>&#9632;</span>",
                             " <span style='font-size:11px;'>Very Low</span>"),
               
               showarrow = FALSE, xanchor = "left"),
          
          # Click-to-highlight instruction hint
          list(x = 1.02, y = 0.55, xref = "paper", yref = "paper",
               text = "<i style='font-size:10px; color:#888;'>Click a district<br>label to highlight</i>",
               showarrow = FALSE, xanchor = "left")
        )
      ) %>%
      
      
      # We use onRender to attach a JS listener that dims non-selected rows
      htmlwidgets::onRender("
        function(el, x) {
          var myPlot = el;
          var highlighted = null;

          myPlot.on('plotly_click', function(data) {
            var clickedY = data.points[0].y;

            if (highlighted === clickedY) {
              // Second click on same district → reset opacity
              Plotly.restyle(myPlot, {'opacity': 1});
              highlighted = null;
            } else {
              // Highlight clicked row, dim others
              // Get all unique y values from the trace
              var yData = myPlot.data[0].y;
              var opacityArr = yData.map(function(val) {
                return val === clickedY ? 1 : 0.25;
              });
              Plotly.restyle(myPlot, {'opacity': [opacityArr]});
              highlighted = clickedY;
            }
          });
        }
      ")
    
    p
  })
  
  ## PERSISTENCE TABLE ##
  
  output$tab_main <- renderDT({
    req(filtered_data(), input$year_range)
    
    data_table <- prep_persistence_data(filtered_data(), input$year_range)
    
    validate(need(!is.null(data_table) && nrow(data_table) > 0,
                  "No persistent districts found (or missing data)."))
    datatable(
      data_table,
      
      # Rename column names
      colnames = c("Region", "District", "Start Risk", "End Risk",
                   "Start PfPR", "End PfPR", "Abs. Change"),
      options  = list(pageLength = 10, scrollX = TRUE, dom = 'tp')
    ) %>%
      formatPercentage(grep("prevalenceRate|PfPR_Change", names(data_table)), 1)
  })
  
}