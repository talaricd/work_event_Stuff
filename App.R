library(shiny)
library(lubridate)
library(shinyjs)

#############################
# Pay Period Functions
#############################

generate_pay_periods <- function(start_date, num_periods) {
  pay_periods <- data.frame(
    Start_Date = as.Date(character()),
    End_Date = as.Date(character()),
    Period = character(),
    stringsAsFactors = FALSE
  )
  
  current_start <- start_date
  for (i in 1:num_periods) {
    current_end <- current_start + days(13)
    period_text <- paste(current_start, "to", current_end)
    pay_periods <- rbind(
      pay_periods,
      data.frame(
        Start_Date = current_start,
        End_Date = current_end,
        Period = period_text,
        stringsAsFactors = FALSE
      )
    )
    current_start <- current_start + days(14)
  }
  return(pay_periods)
}

find_pay_period <- function(date, pay_periods) {
  for (i in 1:nrow(pay_periods)) {
    if (date >= pay_periods$Start_Date[i] && date <= pay_periods$End_Date[i]) {
      return(pay_periods$Period[i])
    }
  }
  return(NA)
}

start_date <- as.Date("2025-02-16")  
num_periods <- ceiling(24 * 2)
pp_data <- generate_pay_periods(start_date, num_periods)

#############################
# CSV Data Storage Functions
#############################

load_data_from_csv <- function(file_path) {
  if (file.exists(file_path)) {
    df <- read.csv(file_path, stringsAsFactors = FALSE)
    return(df)
  } else {
    return(data.frame(
      Event_Name = character(),
      Event_Date = character(),
      Event_Time = character(),
      Event_Duration = numeric(),
      Pay_Period = character(),
      Form_Submission_Timestamp = character(),
      stringsAsFactors = FALSE
    ))
  }
}

save_data_to_csv <- function(data, file_path) {
  write.csv(data, file = file_path, row.names = FALSE)
}

file_path <- "data/events.csv"

#############################
# Shiny App: UI
#############################

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Event Input Form"),
  sidebarLayout(
    sidebarPanel(
      textInput("event_name", "Event Name"),
      dateInput("event_date", "Event Date"),
      textInput("event_time", "Event Time (HHMM)"),
      numericInput("event_duration", "Event Duration (minutes)", value = NA, min = 0),
      actionButton("submit", "Submit"),
      verbatimTextOutput("error_message")
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)

#############################
# Shiny App: Server
#############################

server <- function(input, output, session) {
  shared_data <- reactiveVal(load_data_from_csv(file_path))
  
  observeEvent(input$submit, {
    if (input$event_name == "" || input$event_time == "" || is.na(input$event_duration)) {
      output$error_message <- renderText("Error: All fields must be filled out.")
    } else if (!grepl("^([01]\\d|2[0-3])[0-5]\\d$", input$event_time)) {
      output$error_message <- renderText("Error: Event Time must be in HHMM format (military time) without a colon.")
    } else {
      output$error_message <- renderText("")
      
      pp <- find_pay_period(as.Date(input$event_date), pp_data)
      
      new_entry <- data.frame(
        Event_Name = input$event_name,
        Event_Date = as.character(input$event_date),
        Event_Time = input$event_time,
        Event_Duration = input$event_duration,
        Pay_Period = pp,
        Form_Submission_Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      )
      
      updated_data <- rbind(shared_data(), new_entry)
      shared_data(updated_data)
      save_data_to_csv(updated_data, file_path)
      
      updateTextInput(session, "event_name", value = "")
      updateDateInput(session, "event_date", value = Sys.Date())
      updateTextInput(session, "event_time", value = "")
      updateNumericInput(session, "event_duration", value = NA)
    }
  })
  
  output$data <- renderTable({
    shared_data()
  })
}

shinyApp(ui = ui, server = server)