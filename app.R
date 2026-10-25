# Bring in libraries needed for this app
library(shiny)
library(tidyverse)
library(dplyr)

# Bring in data needed for this app
CDC <- readRDS("data/Cleaned CDC data.rds")

# Create list for state options
states <- CDC |>
  distinct(LocationDesc) |>
  arrange(LocationDesc)

states <- c("All States", states$LocationDesc)

# Create list for city options
cities <- CDC |>
  distinct(LocationDesc, City) |>
  arrange(LocationDesc, City)

# Define UI
ui <- fluidPage(
  
  titlePanel(title = "Fertility Clinics in the United States",
             windowTitle = "Fertility Clinics in the United States"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filter Criteria:"),
      
      selectizeInput(inputId = "state",
                     label = "State",
                     choices = states,
                     selected = "All States"),
      
      selectizeInput(inputId = "city",
                     label = "City",
                     choices = "All Cities",
                     selected = "All Cities")
      
      #Annual Cycles
      #Annual Deliveries
    ),
    
    mainPanel()
  )
)

# Define Server
server <- function(input, output, session) {
  
  # If the user selects a state, update city options
  observeEvent(input$state, {
    if (input$state == "All States"){
      updateSelectizeInput(session,
                           inputId = "city",
                           label = "City",
                           choices = "All Cities",
                           selected = "All Cities")
    }
    else{
      city_choices <- cities |> filter(LocationDesc == input$state)
      city_choices <- c("All Cities", city_choices$City)
      updateSelectizeInput(session,
                           inputId = "city",
                           label = "City",
                           choices = city_choices,
                           selected = "All Cities")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)