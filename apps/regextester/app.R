library(shiny)
library(bslib)
library(tibble)
library(purrr)
library(stringr)
library(tidyr)
library(dplyr)

ui <- page_sidebar(
  title = "Regular Expression Tester",
  sidebar = sidebar(
    # File upload
    fileInput("file", "Upload CSV file", 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    
    # Column selection (shown only after file is uploaded)
    uiOutput("column_select"),
    
    # Regex input
    textInput("regex", "Enter regular expression:", value = "\\w+"),
    
    # Regex options
    checkboxInput("ignore_case", "Ignore case", value = TRUE),
    
    # Regex help card
    card(
      card_header("Regex Quick Reference"),
      card_body(
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
    )
  ),
  
  # Main panel with results
  card(
    card_header("Matching Results"),
    card_body(
      tableOutput("matches")
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
