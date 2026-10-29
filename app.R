# Bring in libraries needed for this app
library(shiny)
library(DT)
library(tidyverse)
library(dplyr)
library(bslib)
library(ggplot2)
library(usmap)
library(shinycssloaders)

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

# Chart Theme options
set_theme(theme_light())
update_theme(plot.title = element_text(hjust = 0.5))

# Define UI
ui <- fluidPage(
  
  titlePanel(title = "Fertility Clinics in the United States 2020 - 2022",
             windowTitle = "Fertility Clinics in the United States 2020 - 2022"),
  
  sidebarLayout(
    sidebarPanel(
      
      width = 3,
      
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
      
      width = 9,
      
      tabsetPanel(
        tabPanel("About",
                 br(),
                 "Use this app to analyze and download cleaned CDC data about Assisted Reproductive Technology (ART). This data provides information about fertility clinics across the United States in the years 2020-2022. Links to each year of the raw data are provided below:",
                 br(), br(),
                 tags$ul(
                     tags$li(tags$a(href = "https://data.cdc.gov/Assisted-Reproductive-Technology-ART-/2020-Final-Assisted-Reproductive-Technology-ART-Se/92ri-yjps/about_data", target = "_blank", "2020 Data")),
                     tags$li(tags$a(href = "https://data.cdc.gov/Assisted-Reproductive-Technology-ART-/2021-Final-Assisted-Reproductive-Technology-ART-Se/ui6g-vumy/about_data", target = "_blank", "2021 Data")),
                     tags$li(tags$a(href = "https://data.cdc.gov/Assisted-Reproductive-Technology-ART-/2022-Final-Assisted-Reproductive-Technology-ART-Se/ix4g-rt8v/about_data", target = "_blank", "2022 Data"))
                  ),
                 
                 "This data has been cleaned and does not look exactly like its raw form. Some data cleaning that has been performed includes combining the three years of data into a single data set, removing summary rows, variable formatting, and calculating ratios. The final data set contains one row of data per fertility clinic per year.",
                br(), br(),
                'To filter the data, use the panel to the left to make your selections and then click the "Apply Filters" button. The data will not filter until you click this button.',
                 br(), br(),
                 'To download data, set your filters and then proceed to the "Data Download" tab.',
                 br(), br(),
                 'Summary tables and plots are available on the "Data Exploration" tab. This includes:',
                br(), br(),
                tags$ul(
                  tags$li("One and two-way contingency tables, which provide counts of clinics and data records by categories of your choosing;"),
                  tags$li("Numeric Data Summaries, which provide statistical measures of a numerical variable of your choosing by a categorical variable of your choosing; and"),
                  tags$li("Plots. There are 6 types of plots available, and each of them allows you to select your variable(s) of interest.")
                  ),
                br(),
                imageOutput("image")
                ),
        tabPanel("Data Download",
                 br(),
                 downloadButton(outputId = "download",
                                label = "Download CSV"),
                 br(), br(),
                 withSpinner(DT::dataTableOutput(outputId = "filtered_data"))
                 ),
        tabPanel("Data Exploration",
                 tabsetPanel(
                   
                   tabPanel("One and Two-Way Contingency Tables",
                            br(),
                            fluidRow(
                            column(3, radioButtons(
                                inputId = "contingency_display", 
                                label = "Display counts of:", 
                                choices = c("Unique Clinics","Total Records (One Record Per Clinic Per Year)"),
                                selected = "Unique Clinics")),
                            column(3, selectizeInput(inputId = "contingency1",
                                  label = "Select First Categorical Variable:",
                                  choices = charvar_options,
                                  selected = "Year")),
                            column(3, selectizeInput(inputId = "contingency2",
                                  label = "Select Second Categorical Variable:",
                                  choices = charvar_options,
                                  selected = "Clinic Status"))
                            ),
                            h4("One-Way Contingency Tables"),
                            fluidRow(
                            column(6, withSpinner(tableOutput("contingencytable1"))),
                            column(6, withSpinner(tableOutput("contingencytable2")))
                            ),
                            h4("Two-Way Contingency Table"),
                            withSpinner(tableOutput("contingencytable1and2"))
                            ),
                   
                   tabPanel("Numeric Data Summaries",
                            br(),
                            fluidRow(
                              column(3, selectizeInput(inputId = "numericsummary1",
                                    label = "Select Categorical Variable:",
                                    choices = charvar_options,
                                    selected = charvar_options[1])),
                              column(3, selectizeInput(inputId = "numericsummary2",
                                    label = "Select Numeric Variable:",
                                    choices = numvar_options,
                                    selected = numvar_options[1])),
                            ),
                            h4("Summary"),
                            withSpinner(tableOutput("numeric_summary"))
                            ),
                   
                   tabPanel("Plots",
                            br(),
                            navset_pill_list(
                              widths = c(2,10),
                              nav_panel("Bar Chart",
                                               fluidRow(
                                                 column(3, 
                                                        selectizeInput(inputId = "plot1_x",
                                                                       label = "X Axis",
                                                                       choices = charvar_options,
                                                                       selected = "Year"),
                                                        selectizeInput(inputId = "plot1_y",
                                                                       label = "Y Axis",
                                                                       choices = numvar_options,
                                                                       selected = "Pregnancies"),
                                                        selectizeInput(inputId = "plot1_fill",
                                                                       label = "Color By",
                                                                       choices = charvar_options,
                                                                       selected = "Clinic Status")
                                                 ),
                                                 conditionalPanel(
                                                   condition = paste0("input.plot1_x == input.plot1_fill"),
                                                   column(9, "Please select two different variables for the X Axis and Color By options.")
                                                 ),
                                                 conditionalPanel(
                                                   condition = paste0("input.plot1_x != input.plot1_fill"),
                                                   column(9, withSpinner(plotOutput("plot1")))
                                                 )
                                               )
                              ),
                              nav_panel("Smoothed Histogram",
                                        fluidRow(
                                          column(3,
                                                 selectizeInput(inputId = "plot2_x",
                                                                label = "X Axis",
                                                                choices = numvar_options[-1],
                                                                selected = "Pregnancies"),
                                                 selectizeInput(inputId = "plot2_color",
                                                                label = "Color By",
                                                                choices = charvar_options,
                                                                selected = "Gestatioinal carrier services")
                                                 ),
                                          column(9, withSpinner(plotOutput("plot2")))
                                        )
                              ),
                              nav_panel("Box Plot",
                                        fluidRow(
                                          column(3,
                                                 selectizeInput(inputId = "plot3_x",
                                                                label = "X Axis",
                                                                choices = charvar_options,
                                                                selected = "Year"),
                                                 selectizeInput(inputId = "plot3_y",
                                                                label = "Y Axis",
                                                                choices = numvar_options[-1],
                                                                selected = "Pregnancies")
                                          ),
                                          column(9, withSpinner(plotOutput("plot3")))
                                         )
                              ),
                              nav_panel("Scatter Plot",
                                        fluidRow(
                                          column(3,
                                                 selectizeInput(inputId = "plot4_x",
                                                                label = "X Axis",
                                                                choices = numvar_options[-1],
                                                                selected = "Total cycles"),
                                                 selectizeInput(inputId = "plot4_y",
                                                                label = "Y Axis",
                                                                choices = numvar_options[-1],
                                                                selected = "Deliveries"),
                                                 selectizeInput(inputId = "plot4_color",
                                                                label = "Color By",
                                                                choices = charvar_options,
                                                                selected = "Gestational carrier services"),
                                                 checkboxInput(inputId = "plot4_lm",
                                                               label = "Include Linear Model?",
                                                               value = TRUE)
                                                 ),
                                          column(9, withSpinner(plotOutput("plot4")))
                                        )
                              ),
                              nav_panel("Faceted Bar Chart",
                                        fluidRow(
                                          column(3, 
                                                 selectizeInput(inputId = "plot5_x",
                                                                label = "X Axis",
                                                                choices = charvar_options,
                                                                selected = "Year"),
                                                 selectizeInput(inputId = "plot5_y",
                                                                label = "Y Axis",
                                                                choices = numvar_options,
                                                                selected = "Pregnancies"),
                                                 selectizeInput(inputId = "plot5_fill",
                                                                label = "Color By",
                                                                choices = charvar_options,
                                                                selected = "Clinic Status"),
                                                 selectizeInput(inputId = "plot5_facet",
                                                                label = "Facet By",
                                                                choices = charvar_options,
                                                                selected = "Verified lab accreditation")
                                          ),
                                          conditionalPanel(
                                            condition = paste0("input.plot5_x == input.plot5_fill"),
                                            column(9, "Please select two different variables for the X Axis and Color By options.")
                                          ),
                                          conditionalPanel(
                                            condition = paste0("input.plot5_x == input.plot5_facet"),
                                            column(9, "Please select two different variables for the X Axis and Facet By options.")
                                          ),
                                          conditionalPanel(
                                            condition = paste0("input.plot5_fill == input.plot5_facet"),
                                            column(9, "Please select two different variables for the Color By and Facet By options.")
                                          ),
                                          conditionalPanel(
                                            condition = paste0("input.plot5_x != input.plot5_fill & input.plot5_x != input.plot5_facet & input.plot5_fill != input.plot5_facet"),
                                            column(9, withSpinner(plotOutput("plot5")))
                                          )
                                        )
                              ),
                              nav_panel("Map",
                                        fluidRow(
                                          column(3,
                                                 selectizeInput(inputId = "plot6_variable",
                                                                label = "Variable",
                                                                choices = numvar_options,
                                                                selected = "Pregnancies"),
                                                 ),
                                          column(9, withSpinner(plotOutput("plot6")))
                                        )
                              )
                            )
                            
                             
                           ))
                 ))
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
      mutate(`Number of Records (One Record Per Clinic Per Year)` = as.integer(1)) |>
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
  
  # Plot 1: Bar Chart
  output$plot1 <- renderPlot({
    
    if(input$plot1_y == "Number of Records (One Record Per Clinic Per Year)"){
      df <- filterdata() |>
        group_by(get(input$plot1_x), get(input$plot1_fill)) |>
        summarize(`Number of Records (One Record Per Clinic Per Year)` = n())
    } 
    
    else if (input$plot1_y == "Ratio of Pregnancies to Total cycles"){
      df <- filterdata() |>
        group_by(get(input$plot1_x), get(input$plot1_fill)) |>
        summarize(numerator = sum(Pregnancies, na.rm = TRUE), denominator = sum(`Total cycles`, na.rm = TRUE)) |>
        mutate(ratio = if_else((denominator > 0), numerator / denominator, NA)) |>
        select(-c(numerator, denominator))
    } 
    
    else if (input$plot1_y == "Ratio of Deliveries to Pregnancies"){
      df <- filterdata() |>
        group_by(get(input$plot1_x), get(input$plot1_fill)) |>
        summarize(numerator = sum(Deliveries, na.rm = TRUE), denominator = sum(Pregnancies, na.rm = TRUE)) |>
        mutate(ratio = if_else((denominator > 0), numerator / denominator, NA)) |>
        select(-c(numerator, denominator))
    } 
    
    else if (input$plot1_y == "Ratio of Total infants born to Deliveries"){
      df <- filterdata() |>
        group_by(get(input$plot1_x), get(input$plot1_fill)) |>
        summarize(numerator = sum(`Total infants born`, na.rm = TRUE), denominator = sum(Deliveries, na.rm = TRUE)) |>
        mutate(ratio = if_else((denominator > 0), numerator / denominator, NA)) |>
        select(-c(numerator, denominator))
    }
    
    else {
      df <- filterdata() |>
        group_by(get(input$plot1_x), get(input$plot1_fill)) |>
        summarize(sum = sum(get(input$plot1_y), na.rm = TRUE))
    }
    
    colnames(df) <- c(input$plot1_x, input$plot1_fill, input$plot1_y)
    
    g <- ggplot(data = df,
           aes(x = get(input$plot1_x), y = get(input$plot1_y), fill = get(input$plot1_fill))) +
      geom_col() +
      labs(x = input$plot1_x, y = input$plot1_y, fill = input$plot1_fill) +
      labs(title = str_wrap(paste0(input$plot1_y," by ",input$plot1_x," and ",input$plot1_fill), width = 50))
    
    return(g)
  })
  
  # Plot 2: Smoothed Histogram
  output$plot2 <- renderPlot({
    
      df <- filterdata() |> filter(!(is.na(get(input$plot2_x))))
    
      g <- ggplot(df) +
        geom_density(aes(x = get(input$plot2_x), color = get(input$plot2_color))) +
        labs(x = input$plot2_x, color = input$plot2_color) +
        labs(title = str_wrap(paste0("Density of ", input$plot2_x ," by ", input$plot2_color), width = 50))
  
      return(g)    
    })
  
  # Plot 3: Box Plot
  output$plot3 <- renderPlot({
    
    df <- filterdata() |> filter(!(is.na(get(input$plot3_y))))
    
    g <- ggplot(df,
                aes(x = get(input$plot3_x),
                    y = get(input$plot3_y),
                    fill = get(input$plot3_x))) +
      geom_boxplot() +
      labs(x = input$plot3_x, y = input$plot3_y, fill = input$plot3_x) +
      labs(title = str_wrap(paste0(input$plot3_y," by ", input$plot3_x), width = 50))
    
    return(g)
  })
  
  # Plot 4: Scatter Plot
  output$plot4 <- renderPlot({
    
    df <- filterdata() |>
      filter(!(is.na(get(input$plot4_x)))) |> 
      filter(!(is.na(get(input$plot4_y))))
    
    g <- ggplot(df,
                aes(x = get(input$plot4_x),
                    y = get(input$plot4_y),
                    color = get(input$plot4_color))) +
      geom_point() +
      labs(x = input$plot4_x, y = input$plot4_y, color = input$plot4_color) +
      labs(title = str_wrap(paste0(input$plot4_y,
                                   " and ", input$plot4_x,
                                   " by ", input$plot4_color), width = 50))
    
    if(input$plot4_lm == TRUE){
      g <- g + geom_smooth(method = lm)
    }
    
    return(g)
  })
  
  # Plot 5: Faceted Bar Chart
  output$plot5 <- renderPlot({
    
    if(input$plot5_y == "Number of Records (One Record Per Clinic Per Year)"){
      df <- filterdata() |>
        group_by(get(input$plot5_x), get(input$plot5_fill), get(input$plot5_facet)) |>
        summarize(`Number of Records (One Record Per Clinic Per Year)` = n())
    } 
    
    else if (input$plot5_y == "Ratio of Pregnancies to Total cycles"){
      df <- filterdata() |>
        group_by(get(input$plot5_x), get(input$plot5_fill), get(input$plot5_facet)) |>
        summarize(numerator = sum(Pregnancies, na.rm = TRUE), denominator = sum(`Total cycles`, na.rm = TRUE)) |>
        mutate(ratio = if_else((denominator > 0), numerator / denominator, NA)) |>
        select(-c(numerator, denominator))
    } 
    
    else if (input$plot5_y == "Ratio of Deliveries to Pregnancies"){
      df <- filterdata() |>
        group_by(get(input$plot5_x), get(input$plot5_fill), get(input$plot5_facet)) |>
        summarize(numerator = sum(Deliveries, na.rm = TRUE), denominator = sum(Pregnancies, na.rm = TRUE)) |>
        mutate(ratio = if_else((denominator > 0), numerator / denominator, NA)) |>
        select(-c(numerator, denominator))
    } 
    
    else if (input$plot5_y == "Ratio of Total infants born to Deliveries"){
      df <- filterdata() |>
        group_by(get(input$plot5_x), get(input$plot5_fill), get(input$plot5_facet)) |>
        summarize(numerator = sum(`Total infants born`, na.rm = TRUE), denominator = sum(Deliveries, na.rm = TRUE)) |>
        mutate(ratio = if_else((denominator > 0), numerator / denominator, NA)) |>
        select(-c(numerator, denominator))
    }
    
    else {
      df <- filterdata() |>
        group_by(get(input$plot5_x), get(input$plot5_fill), get(input$plot5_facet)) |>
        summarize(sum = sum(get(input$plot5_y), na.rm = TRUE))
    }
    
    colnames(df) <- c(input$plot5_x, input$plot5_fill, input$plot5_facet, input$plot5_y)
    
    g <- ggplot(data = df,
                aes(x = get(input$plot5_x), y = get(input$plot5_y), fill = get(input$plot5_fill))) +
      geom_col() +
      facet_wrap(~ get(input$plot5_facet)) +
      labs(x = input$plot5_x, y = input$plot5_y, fill = input$plot5_fill) +
      labs(title = str_wrap(paste0(input$plot5_y," by ",input$plot5_x," and ",input$plot5_fill), width = 50))
    
    return(g)
  })
  
  output$plot6 <- renderPlot({
    
    df <- filterdata() |>
      mutate(`Number of Records (One Record Per Clinic Per Year)` = as.integer(1)) |>
      rename(state = State)
    
    if (input$plot6_variable == "Ratio of Pregnancies to Total cycles"){
      df <- df |>
        group_by(state) |>
        summarize(numerator = sum(Pregnancies, na.rm = TRUE), denominator = sum(`Total cycles`, na.rm = TRUE)) |>
        mutate(mapvalue = if_else((denominator > 0), numerator / denominator, NA)) |>
        select(-c(numerator, denominator))
    } 
    
    else if (input$plot6_variable == "Ratio of Deliveries to Pregnancies"){
      df <- df |>
        group_by(state) |>
        summarize(numerator = sum(Deliveries, na.rm = TRUE), denominator = sum(Pregnancies, na.rm = TRUE)) |>
        mutate(mapvalue = if_else((denominator > 0), numerator / denominator, NA)) |>
        select(-c(numerator, denominator))
    } 
    
    else if (input$plot6_variable == "Ratio of Total infants born to Deliveries"){
      df <- df |>
        group_by(state) |>
        summarize(numerator = sum(`Total infants born`, na.rm = TRUE), denominator = sum(Deliveries, na.rm = TRUE)) |>
        mutate(mapvalue = if_else((denominator > 0), numerator / denominator, NA)) |>
        select(-c(numerator, denominator))
    }
    
    else {
      df <- df |>
        filter(!is.na(get(input$plot6_variable))) |>
        group_by(state) |>
        summarize(mapvalue = sum(get(input$plot6_variable), na.rm = TRUE))
    }
      
    g <- plot_usmap(data = df,
               values = "mapvalue",
               regions = "states") +
      scale_fill_continuous(low = "white", high = "red", name = input$plot6_variable) +
      labs(title = str_wrap(paste0(input$plot6_variable, " by State"), width = 50))
    
    return(g)
  })
  
  # image for About tab
  output$image <- renderImage({
    list(src = "data/Logo.jpg", width = 400, alt = "Image of Logo of Data.CDC.gov website")
  }, deleteFile=TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)