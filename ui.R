# UI for Malaria Risk Strata Dashboard 
# This script contains the UI housing the dashboard controls and layout.


library(shiny)
library(leaflet)
library(plotly)
library(DT)

ui <- fluidPage(
  titlePanel("Tanzania Malaria Risk Strata Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Plan Selection
      selectInput("plan_select", "Intervention Plan:", choices = c("BAU", "NSP")),
      
      # Dynamic Year Slider
      uiOutput("year_slider_ui"),
      
      hr(),
      # Geographic Filters
      selectInput("region_select", "Filter by Region:", choices = "All"),
      selectInput("dist_select", "Filter by District:", choices = "All"),
      # Filters are updated in server function
      hr(),
      helpText("Note: Flows represent individual district movements.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Strata Movement",
                 fluidRow(
                   column(12, h4("District Risk Transitions"),
                          plotlyOutput("sankey_plot", height = "500px"))
                 ),
                 hr(),
                 fluidRow(
                   column(12, h4("Risk MAP"),
                          leafletOutput("risk_map", height = "450px"))
                 ),
                 hr(),
                 fluidRow(
                   column(12, h4("Persistent: High/Moderate Districts"),
                          htmlOutput("persistence_summary"),
                          DTOutput("persistence_table"))
                 )
        )
      )
    )
  )
)