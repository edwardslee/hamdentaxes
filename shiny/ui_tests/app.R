library(shiny)
library(scales)

ui <- fluidPage(
  tags$head(
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('toggleFY2026', function(message) {
        if(message.enabled) {
          $('#fy2026').removeAttr('readonly');
        } else {
          $('#fy2026').attr('readonly', 'readonly');
        }
      });
      $(document).on('shiny:connected', function() {
        $('#fy2026').attr('readonly', 'readonly');
      });
      "
    ))
  ),
  titlePanel("Budget Projection"),
  sidebarLayout(
    sidebarPanel(
      h4("Choose a budget for FY2026"),
      radioButtons(
        inputId = "fy2026_budget_choice",
        label = NULL,
        choices = c(
          "Council's Budget" = "council",
          "Mayor Garrett's Budget" = "mayor"
        ),
        selected = "council"
      ),
      h4("Phase-in?"),
      radioButtons(
        inputId = "phase_in",
        label = NULL,
        choices = c("Yes" = "yes", "No" = "no"),
        selected = "no"
      ),
      radioButtons(
        inputId = "budget_option",
        label = "Budget Projection",
        choices = c(
          "Budget grows 4% yearly" = "grow",
          "Budget remains flat yearly" = "flat",
          "Budget decreases 2% yearly" = "decrease",
          "Custom" = "custom"
        ),
        selected = "grow"
      ),
      textInput("fy2026", "FY2026", value = comma(310653000)),
      textInput("fy2027", "FY2027", value = comma(324285756)),
      textInput("fy2028", "FY2028", value = comma(340605668)),
      textInput("fy2029", "FY2029", value = comma(356648195))
    ),
    mainPanel(
      h4("Entered Budgets"),
      verbatimTextOutput("budgets")
    )
  )
)

server <- function(input, output, session) {
  parse_input <- function(x) as.numeric(gsub(",", "", x))
  council_vals <- list(
    grow     = c(310653000, 324285756, 340605668, 356648195),
    flat     = rep(310653000, 4),
    decrease = c(310653000, 304439940, 298351141, 292384118)
  )
  mayor_vals <- list(
    grow     = c(313653000, 326199120, 339247085, 352817968),
    flat     = rep(313653000, 4),
    decrease = c(313653000, 307379940, 301232341, 295207694)
  )
  
  # Always keep FY2026 non-editable
  observe({
    session$sendCustomMessage("toggleFY2026", list(enabled = FALSE))
  })
  
  # Handle Council/Mayor button presses: always set FY2026, even if projection is Custom
  observeEvent(input$fy2026_budget_choice, {
    if (input$fy2026_budget_choice == "council") {
      updateTextInput(session, "fy2026", value = comma(310653000))
    } else if (input$fy2026_budget_choice == "mayor") {
      updateTextInput(session, "fy2026", value = comma(313653000))
    }
  }, ignoreInit = TRUE)
  
  # Normal projection updates (for non-custom projections)
  observeEvent({
    input$fy2026_budget_choice
    input$budget_option
  }, {
    if (input$budget_option != "custom") {
      if (input$fy2026_budget_choice == "council") {
        vals <- council_vals[[input$budget_option]]
        updateTextInput(session, "fy2026", value = comma(vals[1]))
        updateTextInput(session, "fy2027", value = comma(vals[2]))
        updateTextInput(session, "fy2028", value = comma(vals[3]))
        updateTextInput(session, "fy2029", value = comma(vals[4]))
      } else if (input$fy2026_budget_choice == "mayor") {
        vals <- mayor_vals[[input$budget_option]]
        updateTextInput(session, "fy2026", value = comma(vals[1]))
        updateTextInput(session, "fy2027", value = comma(vals[2]))
        updateTextInput(session, "fy2028", value = comma(vals[3]))
        updateTextInput(session, "fy2029", value = comma(vals[4]))
      }
    }
  }, ignoreInit = TRUE)
  
  # If user edits their own numbers, projection radio auto-selects "Custom"
  observe({
    if (input$budget_option != "custom") {
      vals <- c(
        parse_input(input$fy2026),
        parse_input(input$fy2027),
        parse_input(input$fy2028),
        parse_input(input$fy2029)
      )
      all_presets <- c(
        council_vals,
        mayor_vals
      )
      match_any <- FALSE
      for (preset in all_presets) {
        if (all(round(vals) == round(preset))) {
          match_any <- TRUE
          break
        }
      }
      if (!match_any) {
        updateRadioButtons(session, "budget_option", selected = "custom")
      }
    }
  })
  
  # Format FY2027-29 with commas on edit
  lapply(c("fy2027", "fy2028", "fy2029"), function(id) {
    observeEvent(input[[id]], {
      val <- suppressWarnings(parse_input(input[[id]]))
      if (!is.na(val) && input[[id]] != comma(val)) {
        updateTextInput(session, id, value = comma(val))
      }
    }, ignoreInit = TRUE)
  })
  
  # Format FY2026 with commas on edit, though it's always readonly now
  observeEvent(input$fy2026, {
    val <- suppressWarnings(parse_input(input$fy2026))
    if (!is.na(val) && input$fy2026 != comma(val)) {
      updateTextInput(session, "fy2026", value = comma(val))
    }
  }, ignoreInit = TRUE)
  
  output$budgets <- renderPrint({
    vals <- sapply(
      c(input$fy2026, input$fy2027, input$fy2028, input$fy2029),
      parse_input
    )
    names(vals) <- c("FY2026", "FY2027", "FY2028", "FY2029")
    print(vals)
  })
}

shinyApp(ui, server)