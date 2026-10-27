# Bring in libraries needed for this app
library(shiny)
library(DT)
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

# Numeric variable options for filtering
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

# Character variable options for tables and plots
charvar_options <- c('Year',
                     'LocationDesc',
                     'FacilityName',
                     'MedicalDirector',
                     'City',
                     'Clinic Status',
                     'Donor egg services',
                     'Donated embryo services',
                     'Embryo cryopreservation services',
                     'Egg cryopreservation services',
                     'Gestational carrier services',
                     'SART member',
                     'Verified lab accreditation')

# Define UI
ui <- fluidPage(
  
  titlePanel(title = "Fertility Clinics in the United States 2020 - 2022",
             windowTitle = "Fertility Clinics in the United States 2020 - 2022"),
  
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        column(6, h4("Filter Criteria:")),
        column(6, align = "right", actionButton(inputId = "apply_filters",
                                                label = "Apply Filters"))
        ),
      
      selectizeInput(inputId = "year",
                     label = "Year",
                     choices = c("All Years", "2020", "2021", "2022"),
                     selected = "All Years"),
      
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
      tabsetPanel(
        tabPanel("About"),
        tabPanel("Data Download",
                 br(),
                 downloadButton(outputId = "download",
                                label = "Download CSV"),
                 br(), br(),
                 DT::dataTableOutput(outputId = "filtered_data")
                 ),
        tabPanel("Data Exploration",
                 tabsetPanel(
                   
                   tabPanel("One and Two-Way Contingency Tables",
                            br(),
                            selectizeInput(inputId = "contingency1",
                                  label = "Select First Categorical Variable:",
                                  choices = charvar_options[-2],
                                  selected = charvar_options[1]),
                            selectizeInput(inputId = "contingency2",
                                  label = "Select Second Categorical Variable:",
                                  choices = charvar_options[-1],
                                  selected = charvar_options[2]),
                            tableOutput("contingencytable1"),
                            tableOutput("contingencytable2"),
                            tableOutput("contingencytable1and2")
                            ),
                   
                   tabPanel("Numeric Data Summaries"),
                   
                   tabPanel("Plots")
                 ))
      )
      
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
      filter(Year == input$year | input$year == "All Years") |>
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
  
  # Data display on Download tab
  output$filtered_data <- DT::renderDataTable(
    filterdata()
  )
  
  # Download button
  output$download <- downloadHandler(
    filename = function() {
      'CDC Fertility Clinic Data.csv'
    },
    content = function(con){
      write.csv(filterdata(), con)
    }
  )
  
  # First one-way contingency table
  output$contingencytable1 <- renderTable({
    df <- filterdata() |>
      distinct(get(input$contingency1), ClinicId) |>
      group_by(`get(input$contingency1)`) |>
      summarize(`Number of Unique Clinics` = n())
    
    colnames(df)[1] <- input$contingency1
    
    return(df)
  })
  
  # Second one-way contingency table
  output$contingencytable2 <- renderTable({
    df <- filterdata() |>
      distinct(get(input$contingency2), ClinicId) |>
      group_by(`get(input$contingency2)`) |>
      summarize(`Number of Unique Clinics` = n())
    
    colnames(df)[1] <- input$contingency2
    
    return(df)
  })
  
  # Two-way contingency table
  output$contingencytable1and2 <- renderTable({
    df <- filterdata() |>
      distinct(get(input$contingency1), get(input$contingency2), ClinicId) |>
      group_by(`get(input$contingency1)`, `get(input$contingency2)`) |>
      summarize(count = n()) |>
      pivot_wider(names_from = `get(input$contingency2)`, values_from = count)
    
    colnames(df)[1] <- input$contingency1
    
    return(df)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)