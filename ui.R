# UI for Malaria Risk Strata Dashboard 
# This script contains the UI housing the dashboard controls and layout.
# Updated: BAU vs NSP Side-by-Side Comparison Layout

library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Tanzania Malaria Risk Strata Movement"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Selection for NSP variants (BAU is handled as a fixed counterfactual)
      selectInput("plan_select", "Select Strategic Plan (NSP):", 
                  choices = c("NSP")),
      
      # Selection of age groups
      selectInput( "age_select", "Select Age Group:", choices = NULL),
      
      # Dynamic Year Slider
      uiOutput("year_slider_ui"),
      
      hr(),
      # Geographic Filters
      selectInput("region_select", "Filter by Region:", choices = "All"),
      selectInput("dist_select", "Filter by District:", choices = "All"),
      
      # Filters are updated in server function
      hr(),
      helpText("Comparison Logic:"),
      tags$ul(
        tags$li("Left: BAU (Business as Usual)"),
        tags$li("Right: Selected Strategic Plan")
      )
    ),
    
    mainPanel(
      width = 9,
      navset_card_underline(
        nav_panel(
          "Risk Strata Movement",
          navset_card_pill(
            # SANKEY TAB
            nav_panel(
              "District Risk Movements",
              layout_column_wrap(
                width = 1/2, # For side-by-side
                card(
                  full_screen = TRUE, 
                  card_header("Counterfactual: BAU"), 
                  plotlyOutput("sankey_bau", height = "500px")
                ),
                card(
                  full_screen = TRUE, 
                  card_header(textOutput("nsp_sankey_title")), 
                  plotlyOutput("sankey_nsp", height = "500px")
                )
              )
            ),
            # MAP TAB 
            nav_panel(
              "Risk Maps",
              layout_column_wrap(
                width = 1/2,
                card(
                  full_screen = TRUE, 
                  card_header("Spatial Distribution: BAU"), 
                  leafletOutput("map_bau", height = "500px")
                ),
                card(
                  full_screen = TRUE, 
                  card_header("Spatial Distribution: NSP"), 
                  leafletOutput("map_nsp", height = "500px")
                )
              )
            ),
            # PERSISTENCE TAB 
            nav_panel(
              "Persistence Tables",
              layout_column_wrap(
                width = 1/2,
                card(
                  full_screen = TRUE, 
                  card_header("Persistence: BAU"),
                  htmlOutput("sum_bau"), 
                  hr(),
                  DTOutput("tab_bau")
                ),
                card(
                  full_screen = TRUE, 
                  card_header("Persistence: NSP"),
                  htmlOutput("sum_nsp"), 
                  hr(),
                  DTOutput("tab_nsp")
                )
              )
            )
          )
        )
      )
    )
  )
)