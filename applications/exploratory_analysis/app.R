#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(tidyverse)
library(lubridate)
library(kableExtra)
library(plotly)
library(dbplyr)
library(RMySQL)
library(RSQLite)

library(shiny)
library(shinythemes)

options(scipen=999)

source("helper.R")

# 
ui <- fluidPage(theme = shinytheme("cosmo"),
  # Application title
  titlePanel("Exploratory Analysis Sandbox for All US Airports"),
  
  #
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "local",
        label = h3(
          "Type 3-letter airport code here. Processing speed can vary based on connection speed, quality, and server load. Thank you for your patience."
        ),
        value = "PDX"
      ),
      submitButton(text = "Generate Map and Graphs"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Carrier",
          plotOutput("p_carriers_2"),
          fluidRow(splitLayout(
            cellWidths = c("50%", "50%"),
            plotOutput("p_carriers_1"),
            plotlyOutput("p_carriers_3")
          ))
        ),
        
        tabPanel("Year",
                 plotlyOutput("p_years_1"),
                 plotlyOutput("p_years_2")),
        tabPanel("Month",
                 plotlyOutput("p_months_1"),
                 plotlyOutput("p_months_2")
        ),
        tabPanel("Weekday",
                 plotlyOutput("p_wdays_1"),
                 plotlyOutput("p_wdays_2")
        ),
        tabPanel("Hour",
                 plotlyOutput("p_hours_1"),
                 plotlyOutput("p_hours_2")),
        tabPanel("Destinations",
                 plotlyOutput("p_dist"),
                 plotlyOutput("p_geo"))
      )
    )
  )
)

# 
server <- function(input, output) {
  
  con_air <- reactive({
    openSQLconnection()
  })
  
  importedSQL <- reactive({
    flights_import(con_air(), input$local)
  })
  
  output$p_carriers_1 <- renderPlot({
    makeplot_carriers_1(importedSQL(), input$local)
  })
  
  output$p_carriers_2 <- renderPlot({
    makeplot_carriers_2(importedSQL(), input$local)
  })
  
  output$p_carriers_3 <- renderPlotly({
    makeplot_carriers_3(importedSQL(), input$local)
  })
  
  output$p_years_1 <- renderPlotly({
    makeplot_years_1(importedSQL(), input$local)
  })
  
  output$p_years_2 <- renderPlotly({
    makeplot_years_2(importedSQL(), input$local)
  })
  
  output$p_months_1 <- renderPlotly({
    makeplot_months_1(importedSQL(), input$local)
  })
  
  output$p_months_2 <- renderPlotly({
    makeplot_months_2(importedSQL(), input$local)
  })
  
  output$p_wdays_1 <- renderPlotly({
    makeplot_wdays_1(importedSQL(), input$local)
  })
  
  output$p_wdays_2 <- renderPlotly({
    makeplot_wdays_2(importedSQL(), input$local)
  })
  
  output$p_hours_1 <- renderPlotly({
    makeplot_hours_1(importedSQL(), input$local)
  })
  
  output$p_hours_2 <- renderPlotly({
    makeplot_hours_2(importedSQL(), input$local)
  })
  
  output$p_geo <- renderPlotly({
    makeplot_geo(importedSQL(), input$local)
  })
  
  output$p_dist <- renderPlotly({
    makeplot_dist(importedSQL(), input$local)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
