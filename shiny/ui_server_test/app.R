library(shiny)
library(scales)
library(tidyverse)

df <- read_rds("all_streets_data_4_19_25_cleaned.rds")

phase_in_term <- 4

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
      # Custom CSS for padding
      tags$style(HTML("
        .top-section {
          padding-bottom: 20px;
        }
        .output-padding {
          padding-bottom: 20px;
        }
        .year-panel {
          margin-bottom: 15px;
          padding: 10px;
          border: 1px solid #ddd;
          border-radius: 5px;
          background-color: #f9f9f9;
        }
        .year-title {
          font-weight: bold;
          margin-bottom: 8px;
        }
      ")),
      h4("Find Your Address"),
      textInput("address_search", "Search for your address", ""),
      selectInput("address", "Choose your address", choices = NULL),
      br(),
      h4("Choose the following options for the budget for 2026-2029"),
      radioButtons(
        inputId = "fy2026_budget_choice",
        label = "Choose a proposed budget for FY2026",
        choices = c(
          "Council's Budget" = "council",
          "Mayor Garrett's Budget" = "mayor"
        ),
        selected = "council"
      ),
      radioButtons(
        inputId = "phase_in",
        label = "Phase-in?",
        choices = c("Yes" = "yes", "No" = "no"),
        selected = "yes"
      ),
      radioButtons(
        inputId = "budget_option",
        label = "Budget Projection",
        choices = c(
          "Budget grows 4.71% yearly" = "grow",
          "Budget remains flat yearly" = "flat",
          "Budget decreases 2% yearly" = "decrease",
          "Custom" = "custom"
        ),
        selected = "grow"
      ),
      h4("Set the budget totals for the next four years"),
      textInput("fy2026", "FY2026", value = comma(310653000)),
      textInput("fy2027", "FY2027", value = comma(325284756)),
      textInput("fy2028", "FY2028", value = comma(340605668)),
      textInput("fy2029", "FY2029", value = comma(356648195))
    ),
    mainPanel(
      h4("Estimated Mill Rates For the Next Four Years"),
      tableOutput("mill_rate_table"),
      h4("Property Tax Estimates by Year"),
      div(class = "output-padding",
          div(class = "year-panel",
              div(class = "year-title", "2025-2026 (FY2026)"),
              htmlOutput("string_year1")
          ),
          div(class = "year-panel",
              div(class = "year-title", "2026-2027 (FY2027)"),
              htmlOutput("string_year2")
          ),
          div(class = "year-panel",
              div(class = "year-title", "2027-2028 (FY2028)"),
              htmlOutput("string_year3")
          ),
          div(class = "year-panel",
              div(class = "year-title", "2028-2029 (FY2029)"),
              htmlOutput("string_year4")
          )
      ),
      fluidRow(
        column(12,
               h4("Tax Comparison Across Years (With Phase-In)"),
               plotOutput("tax_comparison")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  parse_input <- function(x) as.numeric(gsub(",", "", x))
  council_vals <- list(
    grow     = c(310653000, 325284756, 340605668, 356648195),
    flat     = rep(310653000, 4),
    decrease = c(310653000, 304439940, 298351141, 292384118)
  )
  mayor_vals <- list(
    grow     = c(313653000,  328426056, 343894924, 360092374),
    flat     = rep(313653000, 4),
    decrease = c(313653000, 307379940, 301232341, 295207694)
  )
  
  # Make mill_rate available everywhere by using a reactive
  mill_rate <- reactive({
    grand_list <- if (input$phase_in == "no") {
      rep(5726470270, 4)
    } else {
      c(4480700206, 4895956894, 5311213582, 5726470270)
    }
    
    fyvals <- sapply(
      c(input$fy2026, input$fy2027, input$fy2028, input$fy2029),
      parse_input
    )
    
    if (input$fy2026_budget_choice == "council") {
      revenue_ask <- numeric(4)
      revenue_ask[1] <- 232000000
      revenue_ask[2] <- 0.8 * fyvals[2]
      revenue_ask[3] <- 0.8 * fyvals[3]
      revenue_ask[4] <- 0.8 * fyvals[4]
    } else { # mayor
      revenue_ask <- numeric(4)
      revenue_ask[1] <- 248471545
      revenue_ask[2] <- 0.8 * fyvals[2]
      revenue_ask[3] <- 0.8 * fyvals[3]
      revenue_ask[4] <- 0.8 * fyvals[4]
    }
    
    mill_rate_vec <- revenue_ask / grand_list * 1000
    
    if (input$fy2026_budget_choice == "council") {
      if (input$phase_in == "no") {
        mill_rate_vec[1] <- 39.99 
      } else if (input$phase_in == "yes") {
        mill_rate_vec[1] <-  52.16
      }
    }
    
    mill_rate_vec
  })
  
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
  
  # Show entered budgets
  output$budgets <- renderPrint({
    vals <- sapply(
      c(input$fy2026, input$fy2027, input$fy2028, input$fy2029),
      parse_input
    )
    names(vals) <- c("FY2026", "FY2027", "FY2028", "FY2029")
    print(vals)
  })
  
  # Calculate mill_rate and show as table
  output$mill_rate_table <- renderTable({
    df <- tibble(
      Year = c("FY2026", "FY2027", "FY2028", "FY2029"),
      `Mill Rate` = round(mill_rate(), 2)
    )
    df
  }, striped = TRUE, spacing = "m", align = "c")
  
  filtered_addresses <- reactive({
    search_term <- input$address_search
    if (is.null(search_term) || search_term == "") {
      return(character(0))
    }
    matches <- df %>%
      filter(str_detect(tolower(address), tolower(search_term))) %>%
      pull(address)
    if (length(matches) > 100) {
      matches <- matches[1:100]
    }
    return(matches)
  })
  
  observe({
    choices <- filtered_addresses()
    updateSelectInput(session, "address", choices = choices)
  })
  
  home_chooser <- reactive({df |>
      filter(address == input$address)
  })
  
  calculate_taxes_for_all_years <- reactive({
    req(input$address)
    home_data <- home_chooser()
    if (nrow(home_data) == 0) return(NULL)
    old_tax <- as.numeric(home_data$property_tax_old)
    appraisal_old <- as.numeric(home_data$appraisal_old)
    appraisal_new <- as.numeric(home_data$appraisal_new)
    assessment_old <- as.numeric(home_data$assessment_old)
    assessment_new <- as.numeric(home_data$assessment_new)
    if(is.na(old_tax)) old_tax <- 0
    if(is.na(assessment_old)) assessment_old <- 0
    if(is.na(assessment_new)) assessment_new <- 0
    if(is.na(appraisal_old)) appraisal_old <- 0
    if(is.na(appraisal_new)) appraisal_new <- 0
    result <- list()
    
    
    result <- list()
    bool_phase_in <- input$phase_in

    
    if (bool_phase_in == "no") {
      for (year in 1:phase_in_term) {
        current_assessment <- assessment_new
        current_tax <- current_assessment * mill_rate()[year] / 1000
        
        result[[paste0("year", year)]] <- list(
          tax = current_tax,
          mill_rate = mill_rate()[year],
          assessment = current_assessment,
          diff_from_prev = if (year == 1) current_tax - old_tax else current_tax - result[[paste0("year", year-1)]]$tax,
          perc_inc_from_prev = if(year == 1) (current_tax - old_tax) / max(old_tax, 1) * 100
          else (current_tax - result[[paste0("year", year-1)]]$tax) / max(result[[paste0("year", year-1)]]$tax, 1) * 100,
          monthly_inc_from_prev = if(year == 1) (current_tax - old_tax) / 12
          else (current_tax - result[[paste0("year", year-1)]]$tax) / 12
        )
      }
      
      result$baseline <- list(
        tax = old_tax,
        assessment = assessment_old,
        appraisal = appraisal_old
      )
      
      return(result)
    } else {
      for (year in 1:phase_in_term) {
        current_assessment <- assessment_old + ((assessment_new - assessment_old) * (year/phase_in_term))
        current_tax <- current_assessment * mill_rate()[year] / 1000
        
        result[[paste0("year", year)]] <- list(
          tax = current_tax,
          mill_rate = mill_rate()[year],
          assessment = current_assessment,
          diff_from_prev = if (year == 1) current_tax - old_tax else current_tax - result[[paste0("year", year-1)]]$tax,
          perc_inc_from_prev = if(year == 1) (current_tax - old_tax) / max(old_tax, 1) * 100
          else (current_tax - result[[paste0("year", year-1)]]$tax) / max(result[[paste0("year", year-1)]]$tax, 1) * 100,
          monthly_inc_from_prev = if(year == 1) (current_tax - old_tax) / 12
          else (current_tax - result[[paste0("year", year-1)]]$tax) / 12
        )
      }
      
      result$baseline <- list(
        tax = old_tax,
        assessment = assessment_old,
        appraisal = appraisal_old
      )
      
      return(result)
    }
    
  })
  
  generate_tax_string <- function(year_data, year_num, prev_year_name) {
    req(year_data)
    tryCatch({
      tax_value <- ifelse(is.null(year_data[[paste0("year", year_num)]]$tax),
                          0,
                          as.numeric(year_data[[paste0("year", year_num)]]$tax))
      tax <- round(tax_value) |> formatC(format="d", big.mark=",")
      if (year_num == 1) {
        prev_tax_value <- ifelse(is.null(year_data$baseline$tax), 0, as.numeric(year_data$baseline$tax))
        prev_tax <- round(prev_tax_value) |> formatC(format="d", big.mark=",")
        prev_year_text <- "2024"
      } else {
        prev_tax_value <- ifelse(is.null(year_data[[prev_year_name]]$tax),
                                 0,
                                 as.numeric(year_data[[prev_year_name]]$tax))
        prev_tax <- round(prev_tax_value) |> formatC(format="d", big.mark=",")
        prev_year_text <- paste0("Year ", year_num - 1)
      }
      diff_value <- year_data[[paste0("year", year_num)]]$diff_from_prev
      if(is.null(diff_value)) diff_value <- 0
      diff_value <- as.numeric(diff_value)
      diff <- round(diff_value) |> formatC(format="d", big.mark=",")
      perc_inc_value <- year_data[[paste0("year", year_num)]]$perc_inc_from_prev
      if(is.null(perc_inc_value)) perc_inc_value <- 0
      perc_inc_value <- as.numeric(perc_inc_value)
      perc_inc <- formatC(perc_inc_value, digits=1, format="f")
      monthly_value <- year_data[[paste0("year", year_num)]]$monthly_inc_from_prev
      if(is.null(monthly_value)) monthly_value <- 0
      monthly_value <- as.numeric(monthly_value)
      monthly <- formatC(round(monthly_value), format="d", big.mark=",")
      mill_rate_value <- year_data[[paste0("year", year_num)]]$mill_rate
      if(is.null(mill_rate_value)) mill_rate_value <- 0
      mill_rate_value <- as.numeric(mill_rate_value)
      mill_rate <- formatC(mill_rate_value, digits=2, format="f")
      str_c("With a mill rate of ", mill_rate, ", your property taxes will be <b>$", tax, "</b>.<br/>",
            "Compared to ", prev_year_text, " ($", prev_tax, "), this is ",
            ifelse(diff_value >= 0,
                   paste0("an increase of $", diff, " (", perc_inc, "%)"),
                   paste0("a decrease of $", gsub("-", "", diff), " (", gsub("-", "", perc_inc), "%)")),
            ".<br/>",
            "This is ",
            ifelse(monthly_value >= 0,
                   paste0("an additional $", monthly),
                   paste0("a reduction of $", gsub("-", "", monthly))),
            " per month.")
    }, error = function(e) {
      return(paste("Error calculating tax information:", e$message))
    })
  }
  
  output$string_year1 <- renderText({
    req(input$address)
    tax_data <- calculate_taxes_for_all_years()
    if (is.null(tax_data)) return("Please select a valid address")
    generate_tax_string(tax_data, 1, "baseline")
  })
  
  output$string_year2 <- renderText({
    req(input$address)
    tax_data <- calculate_taxes_for_all_years()
    if (is.null(tax_data)) return("Please select a valid address")
    generate_tax_string(tax_data, 2, "year1")
  })
  
  output$string_year3 <- renderText({
    req(input$address)
    tax_data <- calculate_taxes_for_all_years()
    if (is.null(tax_data)) return("Please select a valid address")
    generate_tax_string(tax_data, 3, "year2")
  })
  
  output$string_year4 <- renderText({
    req(input$address)
    tax_data <- calculate_taxes_for_all_years()
    if (is.null(tax_data)) return("Please select a valid address")
    generate_tax_string(tax_data, 4, "year3")
  })
  
  output$tax_comparison <- renderPlot({
    # browser()
    req(input$address)
    tax_data <- calculate_taxes_for_all_years()
    if (is.null(tax_data)) return(NULL)
    tryCatch({
      plot_data <- tibble(
        Year = c("2024", "Year 1", "Year 2", "Year 3", "Year 4"),
        Tax = c(
          as.numeric(tax_data$baseline$tax),
          as.numeric(tax_data$year1$tax),
          as.numeric(tax_data$year2$tax),
          as.numeric(tax_data$year3$tax),
          as.numeric(tax_data$year4$tax)
        ),
        Mill_Rate = c(
          NA,
          as.numeric(tax_data$year1$mill_rate),
          as.numeric(tax_data$year2$mill_rate),
          as.numeric(tax_data$year3$mill_rate),
          as.numeric(tax_data$year4$mill_rate)
        )
      )
      plot_data$Tax[is.na(plot_data$Tax)] <- 0
      plot_data$Year <- factor(plot_data$Year, levels = c("2024", "Year 1", "Year 2", "Year 3", "Year 4"))
      ggplot(plot_data, aes(x = Year, y = Tax, group = 1)) +
        geom_col(aes(fill = Year), width = 0.7) +
        geom_text(aes(label = paste0("$", formatC(round(Tax), format = "d", big.mark = ","))),
                  vjust = -0.5, size = 3.5) +
        geom_text(aes(label = ifelse(is.na(Mill_Rate), "", paste0("Mill Rate: ", formatC(Mill_Rate, digits=2, format="f")))),
                  vjust = 1.5, size = 3) +
        labs(title = "Property Tax Comparison Across Years (With Phase-In)",
             y = "Annual Property Tax ($)",
             x = "") +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text = element_text(size = 10),
          panel.grid.major.x = element_blank()
        ) +
        scale_fill_brewer(palette = "YlOrRd")
    }, error = function(e) {
      plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_void()
      return(plot)
    })
  })
  
  # If you want a no-phase-in plot, you can add a similar reactive for mill_rate_no_phase_in, then use it here.
}

shinyApp(ui, server)