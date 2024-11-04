library(shiny)

ui <- fluidPage(
  titlePanel("Quiz Fragen Checker"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text_input",
                    "Kopieren Sie ihre Frage und anworten hier hin:",
                    rows = 10,
                    width = "100%",
                    placeholder = "Beispiel:
Was ist die Hauptstadt von Frankreich?
1. London
2. Paris
3. Berlin
4. Madrid"),
      actionButton("validate_btn", "Format prüfen")
    ),
    
    mainPanel(
      h4("Ergebnis:"),
      div(id = "validation_message",
          textOutput("validation_result")),
      
      hr(),
      
      h4("Extrahierte Elemente:"),
      textAreaInput("extracted_question", "Frage:", "", rows = 3),
      textAreaInput("extracted_answer1", "Antwort 1:", "", rows = 2),
      textAreaInput("extracted_answer2", "Antwort 2:", "", rows = 2),
      textAreaInput("extracted_answer3", "Antwort 3:", "", rows = 2),
      textAreaInput("extracted_answer4", "Antwort 4:", "", rows = 2),
      
      hr(),
      
      h4("Format Anforderungen:"),
      tags$ul(
        tags$li("Text muss mit eine Frage beginnen, die mit einem Fragezeichen endet"),
        tags$li("Es müssen genau 4 Antworten existieren"),
        tags$li("Die Antworten müssen mit 1. 2. 3. 4. nummeriert sein"),
        tags$li("Formatierter Text führt zu Fehlern")
      )
    )
  )
)

server <- function(input, output, session) {
  
  validate_and_extract <- function(text) {
    # Split text into lines and remove empty lines
    lines <- trimws(unlist(strsplit(text, "\n")))
    lines <- lines[lines != ""]
    
    if (length(lines) != 5) {
      return(list(
        valid = FALSE,
        message = "Eingabe muss genau 5 nicht-leere Zeilen enthalten (1 Frage + 4 Antworten)",
        components = NULL
      ))
    }
    
    # Check if first line ends with question mark
    if (!grepl("\\?\\s*$", lines[1])) {
      return(list(
        valid = FALSE,
        message = "Erste Zeile muss mit einem Fragezeichen enden",
        components = NULL
      ))
    }
    
    # Check answer format and extract answers
    answer_pattern <- "^([1-4])\\.\\s*(.+)$"
    expected_numbers <- 1:4
    extracted_answers <- character(4)
    
    for (i in 1:4) {
      current_line <- lines[i + 1]
      if (!grepl(answer_pattern, current_line)) {
        return(list(
          valid = FALSE,
          message = sprintf("Antwort %d muss mit '%d.' starten und dann den Fragentext enthalten", i, i),
          components = NULL
        ))
      }
      
      # Extract answer number and text
      matches <- regexec(answer_pattern, current_line)
      answer_parts <- regmatches(current_line, matches)[[1]]
      
      actual_number <- as.numeric(answer_parts[2])
      answer_text <- answer_parts[3]
      
      if (actual_number != expected_numbers[i]) {
        return(list(
          valid = FALSE,
          message = sprintf("Antworten müssen sequenziell beantwortet werden. Erwartet: %d, Vorgefunden: %d", 
                            expected_numbers[i], actual_number),
          components = NULL
        ))
      }
      
      extracted_answers[i] <- answer_text
    }
    
    return(list(
      valid = TRUE,
      message = "Format ist gültig!",
      components = list(
        question = lines[1],
        answers = extracted_answers
      )
    ))
  }
  
  observeEvent(input$validate_btn, {
    result <- validate_and_extract(input$text_input)
    
    output$validation_result <- renderText({
      result$message
    })
    
    if (result$valid) {
      updateTextAreaInput(session, "extracted_question", value = result$components$question)
      updateTextAreaInput(session, "extracted_answer1", value = result$components$answers[1])
      updateTextAreaInput(session, "extracted_answer2", value = result$components$answers[2])
      updateTextAreaInput(session, "extracted_answer3", value = result$components$answers[3])
      updateTextAreaInput(session, "extracted_answer4", value = result$components$answers[4])
    } else {
      # Clear the fields if validation fails
      updateTextAreaInput(session, "extracted_question", value = "")
      updateTextAreaInput(session, "extracted_answer1", value = "")
      updateTextAreaInput(session, "extracted_answer2", value = "")
      updateTextAreaInput(session, "extracted_answer3", value = "")
      updateTextAreaInput(session, "extracted_answer4", value = "")
    }
  })
}

shinyApp(ui = ui, server = server)