# Bring in libraries needed for this app
library(shiny)
library(tidyverse)
library(dplyr)

# Bring in data needed for this app
CDC <- readRDS("data/Cleaned CDC data.rds")

# State options
states <- CDC |>
  distinct(LocationDesc) |>
  arrange(LocationDesc)

states <- c("All States", states$LocationDesc)

# City options
cities <- CDC |>
  distinct(LocationDesc, City) |>
  arrange(LocationDesc, City)

# Numeric variable options
numvar_options <- c('Total cycles',
                    'Fertility preservation cycles',
                    'Pregnancies',
                    'Deliveries',
                    'Total infants born')

numvar_range = list()
for (i in 1:5){
  numvar_range[[i]] <- quantile(CDC[numvar_options[i]],
                              probs = c(0, 1),
                              na.rm = TRUE)
  
}

# Define UI
ui <- fluidPage(
  
  titlePanel(title = "Fertility Clinics in the United States",
             windowTitle = "Fertility Clinics in the United States"),
  
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        column(6, h4("Filter Criteria:")),
        column(6, align = "right", actionButton(inputId = "apply_filters",
                                                label = "Apply Filters"))
        ),
      
      selectizeInput(inputId = "state",
                     label = "State",
                     choices = states,
                     selected = "All States"),
      
      selectizeInput(inputId = "city",
                     label = "City",
                     choices = "All Cities",
                     selected = "All Cities"),
      
      checkboxGroupInput(inputId = "numeric_vars",
                         label = "Select Numeric Variables to Filter By:",
                         choices = numvar_options,
                         selected = NULL),
      
      # Sliders that conditionally appear based on checkboxes
      lapply(1:5, function(i){
        conditionalPanel(
          condition = paste0("input.numeric_vars.includes('", numvar_options[i], "')"),
          sliderInput(inputId = numvar_options[i],
                        label = numvar_options[i],
                        min = numvar_range[[i]][[1]], #defaults to min of unfiltered data
                        max = numvar_range[[i]][[2]], #defaults to max of unfiltered data
                        value = c(numvar_range[[i]][[1]],
                                  numvar_range[[i]][[2]]))
        )
      })
      
    ),
    
    mainPanel(
      tableOutput("test")
    )
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
  
  # If the user presses the action button, filter the data
  filterdata <- eventReactive(input$apply_filters, {
    df <- CDC |>
      filter(LocationDesc == input$state | input$state == "All States") |>
      filter(City == input$city | input$city == "All Cities")
    
    for (i in 1:5){
      df <- df |>
        filter((get(numvar_options[i]) >= input[[numvar_options[i]]][1] &
                  get(numvar_options[i]) <= input[[numvar_options[i]]][2])|
                 !(numvar_options[i] %in% input$numeric_vars))
    }
    
    return(df)
      
  }, ignoreNULL = FALSE)
  
  output$test <- renderTable({
    table(filterdata()$City)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)