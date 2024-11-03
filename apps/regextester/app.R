library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Regular Expression Tester"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Regex Tester", tabName = "regex", icon = icon("code"))
    )
  ),
  
  dashboardBody(
    tabItems(
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
              # File upload
              fileInput("file", "Upload CSV file", 
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
              
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
              tags$table(
                class = "table table-sm table-striped",
                tags$thead(
                  tags$tr(
                    tags$th("Pattern"),
                    tags$th("Description")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(tags$code(".")),
                    tags$td("Any single character")
                  ),
                  tags$tr(
                    tags$td(tags$code("\\w")),
                    tags$td("Word character [A-Za-z0-9_]")
                  ),
                  tags$tr(
                    tags$td(tags$code("\\d")),
                    tags$td("Digit [0-9]")
                  ),
                  tags$tr(
                    tags$td(tags$code("\\s")),
                    tags$td("Whitespace character")
                  ),
                  tags$tr(
                    tags$td(tags$code("[abc]")),
                    tags$td("Any of a, b, or c")
                  ),
                  tags$tr(
                    tags$td(tags$code("[^abc]")),
                    tags$td("Any character except a, b, or c")
                  ),
                  tags$tr(
                    tags$td(tags$code("a|b")),
                    tags$td("Match a or b")
                  ),
                  tags$tr(
                    tags$td(tags$code("^")),
                    tags$td("Start of string")
                  ),
                  tags$tr(
                    tags$td(tags$code("$")),
                    tags$td("End of string")
                  ),
                  tags$tr(
                    tags$td(tags$code("\\b")),
                    tags$td("Word boundary")
                  ),
                  tags$tr(
                    tags$td(tags$code("*")),
                    tags$td("0 or more")
                  ),
                  tags$tr(
                    tags$td(tags$code("+")),
                    tags$td("1 or more")
                  ),
                  tags$tr(
                    tags$td(tags$code("?")),
                    tags$td("0 or 1")
                  ),
                  tags$tr(
                    tags$td(tags$code("{n}")),
                    tags$td("Exactly n times")
                  ),
                  tags$tr(
                    tags$td(tags$code("{n,}")),
                    tags$td("n or more times")
                  ),
                  tags$tr(
                    tags$td(tags$code("(...)")),
                    tags$td("Capturing group")
                  )
                )
              )
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
              tableOutput("matches")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value for the uploaded data
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
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
  output$matches <- renderTable({
    req(matches())
    matches()
  })
}

shinyApp(ui, server)
