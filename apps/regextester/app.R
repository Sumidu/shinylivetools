library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

# Read the regex help data
# make sure that an empty dataframe is returned if file is not found
regex_help <- tibble()
tryCatch({
  regex_help <- read.csv("regexhelp.csv")
}, error = function(e) {
  regex_help <- data.frame()
})

ui <- dashboardPage(
  dashboardHeader(title = "Regular Expression Tester"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Regex Tester", tabName = "regex", icon = icon("code"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Data Upload Tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            width = 12,
            title = "Upload Data",
            status = "primary",
            solidHeader = TRUE,
            # File upload
            fileInput("file", "Upload CSV file", 
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            # Preview of uploaded data
            DTOutput("data_preview")
          )
        )
      ),
      
      # Regex Tester Tab
      tabItem(
        tabName = "regex",
        fluidRow(
          # Left column with inputs
          column(
            width = 4,
            box(
              width = NULL,
              title = "Input Controls",
              status = "primary",
              solidHeader = TRUE,
              # Column selection
              uiOutput("column_select"),
              
              # Regex input
              textInput("regex", "Enter regular expression:", value = "\\w+"),
              
              # Regex options
              checkboxInput("ignore_case", "Ignore case", value = TRUE)
            ),
            
            # Regex help box
            box(
              width = NULL,
              title = "Regex Quick Reference",
              status = "info",
              solidHeader = TRUE,
              tableOutput("regex_help")
            )
          ),
          
          # Right column with results
          column(
            width = 8,
            box(
              width = NULL,
              title = "Matching Results",
              status = "primary",
              solidHeader = TRUE,
              DTOutput("matches")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Display regex help
  output$regex_help <- renderTable({
    regex_help
  }, sanitize.text.function = function(x) x)  # This allows HTML tags to render
  
  # Reactive value for the uploaded data
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Preview of uploaded data
  output$data_preview <- renderDT({
    req(data())
    datatable(head(data(), 5),
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                dom = 'ft'  # Only show filter and table
              ))
  })
  
  # Dynamic column selector
  output$column_select <- renderUI({
    req(data())
    selectInput("column", "Select column to analyze:", 
                choices = names(data()),
                selected = names(data())[1])
  })
  
  # Process regex matches
  matches <- reactive({
    req(data(), input$column, input$regex)
    
    # Get the selected column data
    text_vector <- data()[[input$column]]
    
    # Create a data frame with original text and matches
    result <- tibble(
      original_text = text_vector,
      matches = map(text_vector, function(text) {
        matches <- str_extract_all(
          text,
          regex(input$regex, ignore_case = input$ignore_case)
        )[[1]]
        if (length(matches) == 0) return(NA)
        paste(matches, collapse = ", ")
      })
    )
    
    # Remove rows with no matches
    result |> filter(!is.na(matches))
  })
  
  # Display matches
  output$matches <- renderDT({
    req(matches())
    datatable(matches(),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'ftip'  # Show filter, table, info, and pagination
              ))
  })
}

shinyApp(ui, server)
