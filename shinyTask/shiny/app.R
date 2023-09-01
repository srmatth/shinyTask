# Code from shiny::runExample("01_hello')

library(shiny)
library(dplyr)

WDAYS <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")


ui <- fluidPage(
  theme = shinythemes::shinytheme("slate"),
  fluidRow(
    column(
      width = 6, h1("Task Manager")
    ),
    column(width = 2),
    column(
      width = 3,
      fluidRow(br()),
      fluidRow(
        actionButton(
          inputId = "add",
          label = "Add Task",
          icon = icon("add"),
          width = "100%"
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 4,
      uiOutput(outputId = "task_yesterday")
    ),
    column(
      width = 4,
      uiOutput(outputId = "task_today")
    ),
    column(
      width = 4,
      uiOutput(outputId = "task_tomorrow")
    )
  ),
  shinyjs::useShinyjs()
)


server <- function(input, output) {
  
  rv <- reactiveValues()
  
  observeEvent(TRUE, {
    # print(getwd())
    if (fs::file_exists("www/one_time_events.csv")) {
      rv$one_time_events <- readr::read_csv("www/one_time_events.csv")
    }
    if (fs::file_exists("www/recurring_events.csv")) {
      rv$recurring_events <- readr::read_csv("www/recurring_events.csv")
      # View(rv$recurring_events)
    }
    if (fs::file_exists("www/completed_working.csv")) {
      rv$all_completed <- readr::read_csv("www/completed_working.csv")
      rv$completed_working <- rv$all_completed %>%
        filter(date >= Sys.Date() - lubridate::days(1))
      rv$finished_tasks <- rv$completed_working %>%
        filter(date == Sys.Date()) %>%
        pull(task_name)
      rv$finished_tasks_yesterday <- rv$completed_working %>%
        filter(date == Sys.Date() - lubridate::days(1)) %>%
        pull(task_name)
    }
  })
  
  observeEvent(input$add, {
    showModal(
      modalDialog(
        title = "New To-Do",
        footer = tagList(
          actionButton(inputId = "save", label = "Save Task"),
          modalButton("Cancel")
        ),
        size = "m",
        easyClose = TRUE,
        fluidRow(
          textInput(
            inputId = "task_name",
            label = "Input Task Title",
            value = "",
            placeholder = "e.g., Complete Homework 5",
            width = "100%"
          )
        ),
        fluidRow(
          checkboxInput(
            inputId = "recur",
            label = "This is a Recurring Task",
            value = FALSE, width = "100%"
          )
        ),
        fluidRow(
          shinyjs::hidden(
            div(
              id = "recur_time",
              radioButtons(
                inputId = "recur_freq",
                label = "Repeat Every",
                choices = c("Day", "Week"),
                selected = "Day"
              )
            ),
            shinyjs::hidden(
              checkboxGroupInput(
                inputId = "days",
                label = "Days to Repeat On",
                choices = WDAYS,
                selected = WDAYS[lubridate::wday(Sys.Date())]
              )
            ),
            shinyjs::hidden(
              dateInput(
                inputId = "date_start",
                label = "Start Date",
                value = Sys.Date(), min = Sys.Date()
              )
            ),
            shinyjs::hidden(
              checkboxInput(
                inputId = "never_end",
                label = "Recurring Event Never Ends",
                value = TRUE
              )
            ),
            shinyjs::hidden(
              dateInput(
                inputId = "date_end",
                label = "End Date",
                value = Sys.Date(), min = Sys.Date() + lubridate::days(1)
              )
            ),
            dateInput(
              inputId = "date_sel",
              label = "Date",
              value = Sys.Date(), min = Sys.Date()
            )
          )
        )
      )
    )
    
    shinyjs::show("date_sel")
  })
  
  observeEvent(input$recur, {
    if (input$recur) {
      shinyjs::show("recur_time")
      shinyjs::show("date_start")
      shinyjs::show("never_end")
      shinyjs::hide("date_sel")
      if (input$recur_freq == "Week") shinyjs::show("days")
      if (!input$never_end) shinyjs::show("date_end")
    } else {
      shinyjs::hide("recur_time")
      shinyjs::hide("date_start")
      shinyjs::hide("never_end")
      shinyjs::hide("date_end")
      shinyjs::hide("days")
      shinyjs::show("date_sel")
    }
  })
  
  observeEvent(input$recur_freq, {
    if (input$recur_freq == "Day") {
      shinyjs::hide("days")
    } else {
      shinyjs::show("days")
    }
  })
  
  observeEvent(input$never_end, {
    if (input$never_end) {
      shinyjs::hide("date_end")
    } else {
      shinyjs::show("date_end")
    }
  })
  
  observeEvent(input$save, {
    if (input$task_name != "") {
      ## save the data
      # print(input$task_name)
      
      if (input$recur) {
      dat <- data.frame(
        time_created = Sys.time(),
        name = isolate(input$task_name),
        recur_freq = isolate(input$recur_freq),
        recur_days = ifelse(
          isolate(input$recur_freq == "Week"), 
          stringr::str_c(isolate(input$days), collapse = ","),
          stringr::str_c(WDAYS, collapse = ",")
        ),
        start_date = isolate(input$date_start),
        end_date = ifelse(
          isolate(input$never_end),
          "Never",
          as.character(isolate(input$date_end))
        )
      )
      
      if (fs::file_exists("www/recurring_events.csv")) {
        full_dat <- readr::read_csv("www/recurring_events.csv")
        rbind(full_dat, dat) %>%
          readr::write_csv("www/recurring_events.csv")
      } else {
        readr::write_csv(dat, "www/recurring_events.csv")
      }
      
      rv$recurring_events <- readr::read_csv("www/recurring_events.csv")
      
      } else {
        dat <- data.frame(
          time_created = Sys.time(),
          name = isolate(input$task_name),
          date = isolate(input$date_sel)
        )
        
        if (fs::file_exists("www/one_time_events.csv")) {
          full_dat <- readr::read_csv("www/one_time_events.csv")
          rbind(full_dat, dat) %>%
            readr::write_csv("www/one_time_events.csv")
        } else {
          readr::write_csv(dat, "www/one_time_events.csv")
        }
        
        rv$one_time_events <- readr::read_csv("www/one_time_events.csv")
      }
      
      # View(dat)
      
      removeModal()
    } else {
      showNotification(
        p("Error: You must give your task a name."),
        type = "error"
      )
    }
  })
  
  observe({
    ## Yesterday Events
    if (!is.null(rv$one_time_events)) {
      non_recur_y <- rv$one_time_events %>%
        dplyr::filter(date == Sys.Date() - lubridate::days(1))
    } else non_recur_y <- NULL
    if (!is.null(rv$recurring_events)) {
      # print(rv$recurring_events$recur_days)
      # print(WDAYS[lubridate::wday(Sys.Date())])
      recur_y <- rv$recurring_events %>%
        dplyr::filter(
          start_date <= Sys.Date() - lubridate::days(1),
          (end_date == "Never" | lubridate::ymd(end_date) >= Sys.Date() - lubridate::days(1)),
          stringr::str_detect(recur_days, WDAYS[lubridate::wday(Sys.Date() - lubridate::days(1))])
        )
      # print(recur$recur_days)
      # print(WDAYS[lubridate::wday(Sys.Date())])
    } else recur_y <- NULL
    rv$yesterday_events <- c(non_recur_y$name, recur_y$name)
    # print(rv$today_events)
    
    ## Today Events
    if (!is.null(rv$one_time_events)) {
    non_recur <- rv$one_time_events %>%
      dplyr::filter(date == Sys.Date())
    } else non_recur <- NULL
    if (!is.null(rv$recurring_events)) {
      # print(rv$recurring_events$recur_days)
      # print(WDAYS[lubridate::wday(Sys.Date())])
    recur <- rv$recurring_events %>%
      dplyr::filter(
        start_date <= Sys.Date(),
        (end_date == "Never" | lubridate::ymd(end_date) >= Sys.Date()),
        stringr::str_detect(recur_days, WDAYS[lubridate::wday(Sys.Date())])
      )
    # print(recur$recur_days)
    # print(WDAYS[lubridate::wday(Sys.Date())])
    } else recur <- NULL
    rv$today_events <- c(non_recur$name, recur$name)
    # print(rv$today_events)
    
    ## Tomorrow Events
    if (!is.null(rv$one_time_events)) {
      non_recur_t <- rv$one_time_events %>%
        dplyr::filter(date == Sys.Date() + lubridate::days(1))
    } else non_recur_t <- NULL
    if (!is.null(rv$recurring_events)) {
      # print(rv$recurring_events$recur_days)
      # print(WDAYS[lubridate::wday(Sys.Date())])
      recur_t <- rv$recurring_events %>%
        dplyr::filter(
          start_date <= Sys.Date() + lubridate::days(1),
          (end_date == "Never" | lubridate::ymd(end_date) >= Sys.Date() + lubridate::days(1)),
          stringr::str_detect(recur_days, WDAYS[lubridate::wday(Sys.Date() + lubridate::days(1))])
        )
      # print(recur$recur_days)
      # print(WDAYS[lubridate::wday(Sys.Date())])
    } else recur_t <- NULL
    rv$tomorrow_events <- c(non_recur_t$name, recur_t$name)
    # print(rv$today_events)
  })
  
  output$task_today <- renderUI({
    # req(rv$today_events)
    checkboxGroupInput(
      inputId = "tasks",
      label = "Today's To-Do List",
      choices = rv$today_events,
      selected = isolate(rv$finished_tasks)
    )
  })
  
  observeEvent(input$tasks, ignoreInit = TRUE, ignoreNULL = FALSE, {
    # print(rv$finished_tasks)
    if (is.null(rv$finished_tasks) | length(input$tasks) > length(rv$finished_tasks)) {
    completed <- setdiff(input$tasks, rv$finished_tasks)
    # print(completed)
    req(completed)
    print("Made it past req")
    tmp_dat <- data.frame(
      date = Sys.Date(),
      time = Sys.time(),
      task_name = completed,
      status = "completed"
    )
    # rv$all_completed <- readr::read_csv("www/completed_working.csv")
    # rv$completed_working <- rv$all_completed %>%
    #   filter(date >= Sys.Date() - lubridate::days(1))
    # rv$finished_tasks <- rv$completed_working %>%
    #   filter(date == Sys.Date()) %>%
    #   pull(task_name)
    # rv$finished_tasks_yesterday <- rv$completed_working %>%
    #   filter(date == Sys.Date() - lubridate::days(1)) %>%
    #   pull(task_name)
    rv$all_completed <- rbind(rv$all_completed, tmp_dat)
    readr::write_csv(rv$all_completed, "www/completed_working.csv")
    rv$finished_tasks <- rv$all_completed %>%
      filter(date == Sys.Date()) %>%
      pull(task_name)
    } else if (length(input$tasks) == 0 & length(rv$finished_tasks) == 0) {
      print("this is the case")
    } else if (length(input$tasks) < length(rv$finished_tasks)) {
      print(rv$finished_tasks)
      print(input$tasks)
      # take away the one that was done from the file
      incompleted <- setdiff(rv$finished_tasks, input$tasks)
      rv$all_completed <- rv$all_completed %>%
        filter(task_name != incompleted | date != Sys.Date())
      readr::write_csv(rv$all_completed, "www/completed_working.csv")
      rv$finished_tasks <- rv$all_completed %>%
        filter(date == Sys.Date()) %>%
        pull(task_name)
    }
  })
  
  output$task_yesterday <- renderUI({
    shinyjs::disabled(
      checkboxGroupInput(
        inputId = "tasks_yesterday",
        label = "Yesterday's To-Do List",
        choices = rv$yesterday_events,
        selected = isolate(rv$finished_tasks_yesterday)
      )
    )
  })
  
  output$task_tomorrow <- renderUI({
    shinyjs::disabled(
      checkboxGroupInput(
        inputId = "tasks_tomorrow",
        label = "Tomorrow's To-Do List",
        choices = rv$tomorrow_events,
        selected = NULL
      )
    )
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
