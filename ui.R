# UI for Malaria Risk Strata Dashboard 
# This script contains the UI housing the dashboard controls and layout.
# Updated: 4 sub-tabs, summary boxes, persistence heatmap, removed side-by-side card

library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Custom CSS for value boxes and summary styling
  tags$head(tags$style(HTML("
    .summary-valuebox {
      background: #f8f9fa;
      border-left: 4px solid #2c7bb6;
      border-radius: 6px;
      padding: 12px 16px;
      margin-bottom: 8px;
      text-align: center;
    }
    .summary-valuebox .vb-value {
      font-size: 2rem;
      font-weight: 700;
      color: #2c3e50;
      line-height: 1.1;
    }
    .summary-valuebox .vb-label {
      font-size: 0.78rem;
      color: #6c757d;
      text-transform: uppercase;
      letter-spacing: 0.04em;
    }
    .summary-valuebox.vb-danger  { border-left-color: #d32f2f; }
    .summary-valuebox.vb-warning { border-left-color: #ffb300; }
    .summary-valuebox.vb-success { border-left-color: #26a69a; }
    .summary-valuebox.vb-info   { border-left-color: #1976d2; }
    .summary-alert {
      padding: 10px 14px;
      border-radius: 6px;
      font-size: 0.88rem;
      margin-bottom: 4px;
    }
  "))),
  
  titlePanel("Tanzania Malaria Risk Strata Movement"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Dynamic Selection of plan 
      selectInput("plan_select", "Select Plan:", choices = NULL),
      
      # Dynamic Selection of age
      selectInput("age_select", "Select Age Group:", choices = NULL),
      
      # Selection of sankey flow type
      radioButtons("sankey_weight", "Sankey Flow Width:",
                   choices = list("District Count" = "count", "Population Weight" = "nHost"),
                   selected = "count"),
      helpText("Only updates Sankey diagrams"),
      hr(),
      
      # Dynamic Year Slider
      uiOutput("year_slider_ui"),
      hr(),
      
      # Geographic filters
      selectInput("region_select", "Filter by Region:", choices = "All"),
      selectInput("dist_select",   "Filter by District:", choices = "All"),
      
      # Filters are updated in server function
      hr(),
      
      # Strata Definitions
      card(
        card_header("Risk Strata Definitions"),
        card_body(
          style = "font-size: 0.85rem;",
          p(span(style="color:#26a69a; font-weight:bold;", "Very Low:"), "≤ 1.9% PfPR"),
          p(span(style="color:#d4e157; font-weight:bold;", "Low:"), "1.9% – 3.3% PfPR"),
          p(span(style="color:#ffb300; font-weight:bold;", "Moderate:"), "3.3% – 11% PfPR"),
          p(span(style="color:#d32f2f; font-weight:bold;", "High:"), "> 11% PfPR")
        )
      )
    
    ),
    
    mainPanel(
      width = 9,
      navset_card_underline(
        
        # Tab 1: Risk Map
        nav_panel(
          "Strata Map",
          # Summary row
          uiOutput("summary_maps"),
          hr(),
          card(
            full_screen = TRUE,
            card_header("Spatial Distribution of Risk Strata"),
            leafletOutput("map_main", height = "500px")
          )
        ),
        
        # Tab 2: Strata Movement 
        nav_panel(
          "Strata Movement",
          uiOutput("summary_sankey"),
          hr(),
          card(
            full_screen = TRUE,
            card_header("Risk Strata Movement (Start → End)"),
            plotlyOutput("sankey_main", height = "500px")
          )
        ),
        
        # Tab 3: Persistence Heatmap 
        nav_panel(
          "Persistence Heatmap",
          uiOutput("summary_heatmap"),
          hr(),
          card(
            full_screen = TRUE,
            card_header(
              div(
                style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
                span("District Risk Strata Over Time (ordered by persistence)"),
                div(
                  style = "display:flex; align-items:center; gap:8px;",
                  tags$label("Show districts:",
                             style = "font-size:0.85rem; margin-bottom:0; white-space:nowrap; color:#495057;"),
                  selectInput(
                    "heatmap_top_n",
                    label    = NULL,
                    choices  = c("Top 5" = 5, "Top 10" = 10, "Top 20" = 20, "Top 50" = 50, "All" = 9999),
                    selected = 9999,
                    width    = "110px"
                  )
                )
              )
            ),
            plotlyOutput("heatmap_main", height = "600px")
          )
        ),
        
        # Tab 4: Persistence Table 
        nav_panel(
          "Persistence Table",
          uiOutput("summary_persistence"),
          hr(),
          card(
            full_screen = TRUE,
            card_header("Districts Remaining in High / Moderate Strata"),
            DTOutput("tab_main")
          )
        )
        
      )
    )
  )
)