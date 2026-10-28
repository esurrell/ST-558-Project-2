# Bring in libraries needed for this app
library(shiny)
library(DT)
library(tidyverse)
library(dplyr)

# Bring in data needed for this app
CDC <- readRDS("data/Cleaned CDC data.rds")

# State options
states <- CDC |>
  distinct(State) |>
  arrange(State)

states <- c("All States", states$State)

# City options
cities <- CDC |>
  distinct(State, City) |>
  arrange(State, City)

# Numeric variable options for filtering with sliders
slider_options <- c('Total cycles',
                    'Fertility preservation cycles',
                    'Pregnancies',
                    'Deliveries',
                    'Total infants born')

slider_range = list()
for (i in 1:5){
  slider_range[[i]] <- quantile(CDC[slider_options[i]],
                              probs = c(0, 1),
                              na.rm = TRUE)
  
}

# Numeric variable options for numeric summaries and plots
numvar_options <- c('Number of Records (One Record Per Clinic Per Year)',
                    'Total cycles',
                    'Fertility preservation cycles',
                    'Pregnancies',
                    'Deliveries',
                    'Total infants born',
                    'Ratio of Pregnancies to Total cycles',
                    'Ratio of Deliveries to Pregnancies',
                    'Ratio of Total infants born to Deliveries')

# Character variable options for tables and plots
charvar_options <- c('Year',
                     'State',
                     'Facility Name',
                     'Medical Director',
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
      
      checkboxGroupInput(inputId = "slider_vars",
                         label = "Select Numeric Variables to Filter Records By (One Record Per Clinic Per Year):",
                         choices = slider_options,
                         selected = NULL),
      
      # Sliders that conditionally appear based on check boxes
      lapply(1:5, function(i){
        conditionalPanel(
          condition = paste0("input.slider_vars.includes('", slider_options[i], "')"),
          sliderInput(inputId = slider_options[i],
                        label = slider_options[i],
                        min = slider_range[[i]][[1]], #defaults to min of unfiltered data
                        max = slider_range[[i]][[2]], #defaults to max of unfiltered data
                        value = c(slider_range[[i]][[1]],
                                  slider_range[[i]][[2]]))
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
                            fluidRow(
                            column(4, radioButtons(
                                inputId = "contingency_display", 
                                label = "Display counts of:", 
                                choices = c("Unique Clinics","Total Records (One Record Per Clinic Per Year)"),
                                selected = "Unique Clinics")),
                            column(4, selectizeInput(inputId = "contingency1",
                                  label = "Select First Categorical Variable:",
                                  choices = charvar_options,
                                  selected = charvar_options[1])),
                            column(4, selectizeInput(inputId = "contingency2",
                                  label = "Select Second Categorical Variable:",
                                  choices = charvar_options,
                                  selected = charvar_options[2]))
                            ),
                            h4("One-Way Contingency Tables"),
                            fluidRow(
                            column(6, tableOutput("contingencytable1")),
                            column(6, tableOutput("contingencytable2"))
                            ),
                            h4("Two-Way Contingency Table"),
                            tableOutput("contingencytable1and2")
                            ),
                   
                   tabPanel("Numeric Data Summaries",
                            br(),
                            fluidRow(
                              column(6, selectizeInput(inputId = "numericsummary1",
                                    label = "Select Categorical Variable:",
                                    choices = charvar_options,
                                    selected = charvar_options[1])),
                              column(6, selectizeInput(inputId = "numericsummary2",
                                    label = "Select Numeric Variable:",
                                    choices = numvar_options,
                                    selected = numvar_options[1])),
                            ),
                            h4("Summary"),
                            tableOutput("numeric_summary")
                            ),
                   
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
      city_choices <- cities |> filter(State == input$state)
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
      filter(State == input$state | input$state == "All States") |>
      filter(City == input$city | input$city == "All Cities")
    
    for (i in 1:5){
      df <- df |>
        filter((get(slider_options[i]) >= input[[slider_options[i]]][1] &
                  get(slider_options[i]) <= input[[slider_options[i]]][2])|
                 !(slider_options[i] %in% input$slider_vars))
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
    
    if (input$contingency_display == "Unique Clinics"){
      df <- filterdata() |>
        distinct(get(input$contingency1), ClinicId) |>
        group_by(`get(input$contingency1)`) |>
        summarize(`Number of Unique Clinics` = n())
    } else {
      df <- filterdata() |>
        distinct(get(input$contingency1), ClinicId, Year) |>
        group_by(`get(input$contingency1)`) |>
        summarize(`Number of Records (One Record Per Clinic Per Year)` = n())
    }
    
    colnames(df)[1] <- input$contingency1
    
    return(df)
  })
  
  # Second one-way contingency table
  output$contingencytable2 <- renderTable({
    
    if (input$contingency_display == "Unique Clinics"){
      df <- filterdata() |>
        distinct(get(input$contingency2), ClinicId) |>
        group_by(`get(input$contingency2)`) |>
        summarize(`Number of Unique Clinics` = n())
    } else {
      df <- filterdata() |>
        distinct(get(input$contingency2), ClinicId, Year) |>
        group_by(`get(input$contingency2)`) |>
        summarize(`Number of Records (One Record Per Clinic Per Year)` = n())
    }
    
    colnames(df)[1] <- input$contingency2
    
    return(df)
  })
  
  # Two-way contingency table
  output$contingencytable1and2 <- renderTable({
    
    if (input$contingency_display == "Unique Clinics"){
      df <- filterdata() |>
        distinct(get(input$contingency1), get(input$contingency2), ClinicId) |>
        group_by(`get(input$contingency1)`, `get(input$contingency2)`) |>
        summarize(count = n()) |>
        pivot_wider(names_from = `get(input$contingency2)`, values_from = count)
    } else {
      df <- filterdata() |>
        distinct(get(input$contingency1), get(input$contingency2), ClinicId, Year) |>
        group_by(`get(input$contingency1)`, `get(input$contingency2)`) |>
        summarize(count = n()) |>
        pivot_wider(names_from = `get(input$contingency2)`, values_from = count)
    }
    
    colnames(df)[1] <- input$contingency1
    
    return(df)
  })
  
  # Numeric summary
  output$numeric_summary <- renderTable({
    df <- filterdata() |>
      mutate(`Number of Records (One Record Per Clinic Per Year)` = 1,
             `Ratio of Pregnancies to Total cycles` = if_else((`Total cycles` > 0), Pregnancies / `Total cycles`, NA),
             `Ratio of Deliveries to Pregnancies` = if_else((Pregnancies > 0), Deliveries / Pregnancies, NA),
             `Ratio of Total infants born to Deliveries` = if_else((Deliveries > 0), `Total infants born` / Deliveries, NA)) |>
      group_by(get(input$numericsummary1)) |>
      summarize(Sum = sum(get(input$numericsummary2), na.rm = TRUE),
                `Mean (Per Clinic Per Year)` = mean(get(input$numericsummary2), na.rm = TRUE),
                `Median (Per Clinic Per Year)` = median(get(input$numericsummary2), na.rm = TRUE),
                `Standard Deviation (Per Clinic Per Year)` = sd(get(input$numericsummary2), na.rm = TRUE),
                `Inter-Quartile Range (Per Clinic Per Year)` = IQR(get(input$numericsummary2), na.rm = TRUE))
    
    if (input$numericsummary2 %in% c('Ratio of Pregnancies to Total cycles',
                                     'Ratio of Deliveries to Pregnancies',
                                     'Ratio of Total infants born to Deliveries')){
      df <- df |>
        select(-c(Sum))
    }
    
    colnames(df)[1] <- input$numericsummary1
    
    return(df)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)