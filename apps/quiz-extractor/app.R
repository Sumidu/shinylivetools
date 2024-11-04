library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(rclipboard)
library(shinyjs)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Quiz Extractor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("View Quizzes", tabName = "quizzes", icon = icon("question")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),  # Initialize shinyjs
    rclipboardSetup(),
    
    tabItems(
      # Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Upload CSV File",
                  width = 12,
                  fileInput("file", "Choose CSV File",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                  ),
                  helpText("Upload a CSV file containing quiz data.")
                )
              )
      ),
      
      # Quizzes Tab
      tabItem(tabName = "quizzes",
              fluidRow(
                box(
                  title = "Quiz Content",
                  width = 12,
                  
                  # Navigation Controls
                  div(style = "margin-bottom: 20px; text-align: center;",
                      actionButton("prev_btn", "Previous", icon = icon("arrow-left")),
                      span(
                        style = "margin: 0 15px;",
                        textOutput("current_position", inline = TRUE)
                      ),
                      actionButton("next_btn", "Next", icon = icon("arrow-right"))
                  ),
                  
                  # Group Display
                  div(style = "margin-bottom: 20px",
                      h4("Group:"),
                      verbatimTextOutput("group")
                  ),
                  
                  # Question Display
                  div(style = "margin-bottom: 20px",
                      h4("Question:"),
                      verbatimTextOutput("question"),
                      rclipButton("copy_question", "Copy Question", verbatimTextOutput("question"))
                  ),
                  
                  # Answer 1 Display
                  div(style = "margin-bottom: 20px",
                      h4("Answer 1:"),
                      verbatimTextOutput("answer1"),
                      rclipButton("copy_answer1", "Copy Answer 1", verbatimTextOutput("answer1"))
                  ),
                  
                  # Answer 2 Display
                  div(style = "margin-bottom: 20px",
                      h4("Answer 2:"),
                      verbatimTextOutput("answer2"),
                      rclipButton("copy_answer2", "Copy Answer 2", verbatimTextOutput("answer2"))
                  ),
                  
                  # Answer 3 Display
                  div(style = "margin-bottom: 20px",
                      h4("Answer 3:"),
                      verbatimTextOutput("answer3"),
                      rclipButton("copy_answer3", "Copy Answer 3", verbatimTextOutput("answer3"))
                  ),
                  
                  # Answer 4 Display
                  div(style = "margin-bottom: 20px",
                      h4("Answer 4:"),
                      verbatimTextOutput("answer4"),
                      rclipButton("copy_answer4", "Copy Answer 4", verbatimTextOutput("answer4"))
                  ),
                  
                  # Grading Checkbox
                  div(style = "margin-top: 20px",
                      checkboxInput("grade_checkbox", "Mark as correct (1 point)", value = FALSE)
                  )
                )
              )
      ),
      
      # Data Table Tab
      tabItem(tabName = "table",
              fluidRow(
                box(
                  title = "Quiz Data Table",
                  width = 12,
                  DTOutput("quiz_table"),
                  downloadButton("download_csv", "Download CSV")
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive values to store processed data and current position
  processed_data <- reactiveVal(NULL)
  current_index <- reactiveVal(1)
  
  # Function to process quiz text
  process_quiz <- function(text) {
    if(is.na(text) || text == "") {
      return(list(
        question = NA,
        answer1 = NA,
        answer2 = NA,
        answer3 = NA,
        answer4 = NA
      ))
    }
    
    # Split the text by numbers (1., 2., 3., 4.)
    parts <- strsplit(text, "\\s*\\d\\.\\s*")[[1]]
    
    # Extract question (everything before first number)
    question <- parts[1]
    
    # Remove any leading/trailing whitespace and numbers
    question <- gsub("^\\s+|\\s+$", "", question)
    
    # Extract answers (if they exist)
    answers <- if(length(parts) > 1) parts[2:5] else rep(NA, 4)
    
    # Clean answers
    answers <- gsub("^\\s+|\\s+$", "", answers)
    
    list(
      question = question,
      answer1 = if(length(answers) >= 1) answers[1] else NA,
      answer2 = if(length(answers) >= 2) answers[2] else NA,
      answer3 = if(length(answers) >= 3) answers[3] else NA,
      answer4 = if(length(answers) >= 4) answers[4] else NA
    )
  }
  
  # File Upload Handler
  observeEvent(input$file, {
    req(input$file)
    
    # Read the CSV file
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    
    # Process the data
    processed <- df %>%
      filter(!is.na(Texteingabe.online), 
             Texteingabe.online != "", 
             Texteingabe.online != "NA") %>%
      distinct(Gruppe, Texteingabe.online) %>%
      mutate(
        quiz_content = lapply(Texteingabe.online, process_quiz)
      ) %>%
      mutate(
        Question = sapply(quiz_content, `[[`, "question"),
        Answer1 = sapply(quiz_content, `[[`, "answer1"),
        Answer2 = sapply(quiz_content, `[[`, "answer2"),
        Answer3 = sapply(quiz_content, `[[`, "answer3"),
        Answer4 = sapply(quiz_content, `[[`, "answer4"),
        Bewertung = 0  # Initialize all grades to 0
      ) %>%
      select(Gruppe, Question, Answer1, Answer2, Answer3, Answer4, Bewertung)
    
    processed_data(processed)
    current_index(1)
  })
  
  # Update checkbox when navigating
  observe({
    req(processed_data())
    current <- current_index()
    updateCheckboxInput(session, "grade_checkbox", 
                        value = processed_data()$Bewertung[current] == 1)
  })
  
  # Handle grade changes
  observeEvent(input$grade_checkbox, {
    req(processed_data())
    current <- current_index()
    
    # Update the processed data with new grade
    new_data <- processed_data()
    new_data$Bewertung[current] <- if(input$grade_checkbox) 1 else 0
    processed_data(new_data)
  })
  
  # Navigation handlers
  observeEvent(input$prev_btn, {
    req(processed_data())
    current <- current_index()
    if (current > 1) {
      current_index(current - 1)
    }
  })
  
  observeEvent(input$next_btn, {
    req(processed_data())
    current <- current_index()
    if (current < nrow(processed_data())) {
      current_index(current + 1)
    }
  })
  
  # Current position display
  output$current_position <- renderText({
    req(processed_data())
    sprintf("%d / %d", current_index(), nrow(processed_data()))
  })
  
  # Group display
  output$group <- renderText({
    req(processed_data())
    processed_data()$Gruppe[current_index()]
  })
  
  # Question and answers outputs
  output$question <- renderText({
    req(processed_data())
    processed_data()$Question[current_index()]
  })
  
  output$answer1 <- renderText({
    req(processed_data())
    processed_data()$Answer1[current_index()]
  })
  
  output$answer2 <- renderText({
    req(processed_data())
    processed_data()$Answer2[current_index()]
  })
  
  output$answer3 <- renderText({
    req(processed_data())
    processed_data()$Answer3[current_index()]
  })
  
  output$answer4 <- renderText({
    req(processed_data())
    processed_data()$Answer4[current_index()]
  })
  
  # Data Table Output
  output$quiz_table <- renderDT({
    req(processed_data())
    datatable(processed_data(), 
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # Download Handler
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("quiz_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
)
  
  # Disable navigation buttons when appropriate
  observe({
    req(processed_data())
    if (current_index() <= 1) {
      shinyjs::disable("prev_btn")
    } else {
      shinyjs::enable("prev_btn")
    }
    
    if (current_index() >= nrow(processed_data())) {
      shinyjs::disable("next_btn")
    } else {
      shinyjs::enable("next_btn")
    }
  })
  }

# Run the application
shinyApp(ui = ui, server = server)