library(shiny)
library(tidyverse)

df <- read_rds("all_streets_data_4_19_25_cleaned.rds")

# Hard-coded mill rates for phase-in and no phase-in scenarios
mill_rates <- c(51.78, 53.15, 51.30, 49.82)
no_phase_in_mill_rates <- c(39, 45.44, 47.58, 49.82)
phase_in_term <- 4

# Define UI
ui <- fluidPage(
  # ====== Customizable text above the tabs ======
  tags$div(
    id = "above-tabs-text",
    h2("Hamden Tax Calculator (updated on 6/10/2025)"),
    # ---- old two tab text
    # p("This app helps you estimate your property taxes for 2025-2026 (FY2026).
    # The calculator will estimate taxes based on different scenarios regarding the budget and the revaluation phase-in approved by the Legislative Council.
    #   There are two tabs that will display different information:"),
    # tags$li("The first tab ", strong("2025 Property Tax Calculator"), " estimates property taxes with and without a phase-in for next year (FY2026) and how taxes are affected by applying the surplus to the budget."),
    # tags$li("The second tab ", strong("2025-2028 Tax Estimator"), " attempts to estimate and project how property taxes will change for the next 4 years (FY2026-2029) (please read the explanation within this tab carefully)."),
    # br(),
    # p(strong("Please note"), " that the budget and tax situation is still not finalized, and these numbers are still subject to change.")
    
    # ---- one tab text
    p("This app helps you estimate your property taxes for 2025-2026 (FY2026).
    The calculator will estimate and compare taxes based on different scenarios with the budget and the revaluation phase-in approved by the Legislative Council (LC).
    The budget approved by the council (", strong("The Council Budget"), ") includes a reduction of $8.8 million in expenses and an additional $9.1 million
      in non-tax revenue compared to the budget proposed by Mayor Lauren Garrett (", strong("Garrett's Budget"), ").
    The LC also approved a 4-year", strong("phase-in"),  "that defers the changes in property valuations over the next 4 years."),
    p(strong("Please note"), " that the budget and tax situation is still not finalized, and these numbers are still subject to change.")
  ),
  # ====== Tabs ======
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      title = "2025 Property Tax Calculator",
      value = "prop2025_tab",
      # Add your custom UI elements here for the blank tab
      fluidRow(
        column(12, class = "top-section",
               h3("2025-2026 (FY26) Property Tax Calculator"),
               p("Estimate your property taxes for the upcoming year (FY2026) based on three scenarios:"),
               tags$li(strong("Council budget with phase-in:"), "Property taxes with the council's budget that includes cuts with", strong("a 4-year phase-in")),
               tags$li(strong("Council budget without phase-in:"), "Property taxes with the council's budget that includes cuts with", strong("no"), "phase-in."),
               tags$li(strong("Garrett's budget:"), "Property taxes with ", strong("Mayor Lauren Garrett's original budget"),  " with no phase-in."), 
               br(),
               p("To use the calculator:"), 
               tags$li("On the left, search for an address and then choose one from the dropdown menu"),
               tags$li("The default mill rates are estimated by the LC, but you are able to adjust the mill rates for all three situations."),
               br()
        )
      ),
      
      # Sidebar layout
      sidebarLayout(
        # Left sidebar for choosing options
        sidebarPanel(
          width = 4,  # 1/3 of the page
          h4("Options"),
          # Using a textInput for search and a selectInput for selection
          textInput("address_search_tab1", "Search for your address", ""),
          selectInput("address_tab1", "Choose your address", choices = NULL),
          # Add mill rate input
          numericInput("mill_rate_tab1", "Enter mill rate for the Council Budget with phase-in:", 
                       value = 52.16, min = 15, max = 100, step = 0.01),
          helpText("The estimated mill rate for the Council Budget with phase-in is 52.16 for FY26"),
          numericInput("mill_rate_nophase", "Enter mill rate for the Council Budget with no phase-in:", 
                       value = 39.99, min = 15, max = 100, step = 0.01),
          helpText("The estimated mill rate for the Council Budget with no phase-in is 39.99 for FY26"),
          numericInput("mill_rate_original", "Enter mill rate for Garrett's Budget:", 
                       value = 43.39, min = 15, max = 100, step = 0.01),
          helpText("The estimated mill rate for Garrett's Budget with no phase-in is 43.39")#,
        ),
        
        # Right sidebar for strings and graphs
        mainPanel(
          width = 8,  # 2/3 of the page
          h4("Estimated Taxes"),
          htmlOutput("string_tab1"),
          div(class = "output-padding", htmlOutput("string_tab1")),
          
          fluidRow(
            column(12,
                   # h4("Monthly Property Tax"),
                   plotOutput("tax_compare_tab1")
            )
          ),
        )
      )
    ),
    tabPanel(
      title = "2025-2028 Tax Estimator",
      value = "tax_calculator_tab",
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
      # Application title
      # titlePanel("Estimated property taxes with phased-in mill rate reduction"),
      # Top bar for introduction text
      fluidRow(
        column(12, class = "top-section",
               h3("Estimated property taxes with phased-in mill rate reduction"),
               p(paste0("This app lets you estimate your property taxes for all 4 years of the phase-in period (FY2026-FY2029) based on forecasting the budget for the next four years using some broad assumptions. ",
                        "The mill rates are estimated for each year by assuming an annual growth of 4.7% in the budget and with the one-time $17.7 million surplus applied to the 2025-2026 (FY2026) budget.
                        In the right, the calculator displays how property taxes will change with each year of the 4-year phase-in.",
                        "The resulting mill rates range from ",
                        mill_rates[1], " in Year 1 to ",
                        mill_rates[4], " in Year 4.")),
               tags$li("On the left, search for an address and then choose one from the dropdown menu"),
               tags$li("The app will display tax estimates for all 4 years of the phase-in period"),
               tags$li("The app will also show a graph of the taxes with the phase-in (in blue) and without the phase-in (in orange)."),
               br(),
               p(strong("Please note"), " that these calculations are projections using relatively naive assumptions, and the actual property taxes for Years 2 to 4 may vary significantly
                 depending on how the yearly budgets change. These numbers are mainly meant to be display how the taxes might possibly change if the budget continues to grow
                 and how the phase-in affects property taxes.")
        )
      ),
      # Sidebar layout
      sidebarLayout(
        # Left sidebar for choosing options
        sidebarPanel(
          width = 4,
          h4("Options"),
          textInput("address_search", "Search for your address", ""),
          selectInput("address", "Choose your address", choices = NULL),
          h4("Mill Rates by Year"),
          tags$div(
            tags$p(paste0("Year 1: ", mill_rates[1])),
            tags$p(paste0("Year 2: ", mill_rates[2])),
            tags$p(paste0("Year 3: ", mill_rates[3])),
            tags$p(paste0("Year 4: ", mill_rates[4]))
          )
        ),
        # Right sidebar for strings and graphs
        mainPanel(
          width = 8,
          h4("Property Tax Estimates by Year (with a 4-year phase-in and with the surplus)"),
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
          ),
          fluidRow(
            column(12,
                   h4("Tax Comparison Across Years (No Phase-In)"),
                   plotOutput("tax_comparison_no_phase_in")
            )
          )
        )
      )
    )
  )
)

# Server logic unchanged
server <- function(input, output, session) {
  
  #-----
  # Tab 1
  # Create a reactive expression for filtered addresses
  filtered_addresses_tab1 <- reactive({
    # Get the search term
    search_term <- input$address_search_tab1
    
    # If search is empty, return empty vector
    if (is.null(search_term) || search_term == "") {
      return(character(0))
    }
    
    # Filter addresses based on search input
    matches <- df %>%
      filter(str_detect(tolower(address), tolower(search_term))) %>%
      pull(address)
    
    # Limit results to prevent overwhelming the UI
    if (length(matches) > 100) {
      matches <- matches[1:100]
    }
    
    return(matches)
  })
  
  # Update the select input when the search changes
  observe({
    choices_tab1 <- filtered_addresses_tab1()
    updateSelectInput(session, "address_tab1", choices = choices_tab1)
  })
  
  home_chooser_tab1 <- reactive({df |>
      filter(address == input$address_tab1)
  })
  
  # Calculate property tax based on chosen mill rate
  calculated_tax_tab1 <- reactive({
    req(input$address_tab1)
    home_data <- home_chooser_tab1()
    
    # Calculate new tax based on input mill rate
    
    # phase in + surplus
    new_tax_phasein <- (0.25 * home_data$assessment_new + 0.75 * home_data$assessment_old) * input$mill_rate_tab1 / 1000
    
    # no phase in + surplus
    mill_rate_est <- input$mill_rate_nophase
    new_tax_nophase <- home_data$assessment_new * mill_rate_est / 1000
    
    # original
    new_tax_original <- home_data$assessment_new * input$mill_rate_original / 1000
    
    # Original 2024 tax stays the same
    old_tax <- home_data$property_tax_old
    
    list(
      new_tax_phasein = new_tax_phasein,
      new_tax_nophase = new_tax_nophase,
      new_tax_original = new_tax_original,
      old_tax = old_tax,
      diff_phasein = new_tax_phasein - old_tax,
      diff_nophase = new_tax_nophase - old_tax,
      diff_original = new_tax_original - old_tax,
      perc_inc_phasein = (new_tax_phasein - old_tax) / old_tax * 100,
      monthly_phasein = (new_tax_phasein - old_tax) / 12,
      perc_inc_nophase = (new_tax_nophase - old_tax) / old_tax * 100,
      monthly_nophase = (new_tax_nophase - old_tax) / 12,
      perc_inc_original = (new_tax_original - old_tax) / old_tax * 100,
      monthly_original = (new_tax_original - old_tax) / 12
    )
  })
  
  output$string_tab1 <- renderText({
    req(input$address_tab1)  # Only proceed if an address is selected
    
    df_home <- home_chooser_tab1()
    
    # Check if we have valid data
    if (nrow(df_home) == 0) return("Please select a valid address")
    
    tax_data <- calculated_tax_tab1()
    
    prop_tax_new_phasein <- round(tax_data$new_tax_phasein)
    prop_tax_new_nophase <- round(tax_data$new_tax_nophase)
    prop_tax_new_original <- round(tax_data$new_tax_original)
    prop_tax_old <- round(df_home[["property_tax_old"]])
    appraisal_new <- round(df_home[["appraisal_new"]])
    appraisal_old <- round(df_home[["appraisal_old"]])
    prop_tax_diff_phasein <- round(tax_data$diff_phasein)
    prop_tax_diff_nophase <- round(tax_data$diff_nophase)
    prop_tax_diff_original <- round(tax_data$diff_original)
    prop_perc_inc_phasein <- formatC(tax_data$perc_inc_phasein, digits = 1, format = "f")
    prop_perc_inc_nophase <- formatC(tax_data$perc_inc_nophase, digits = 1, format = "f")
    prop_perc_inc_original <- formatC(tax_data$perc_inc_original, digits = 1, format = "f")
    monthly_payment_phasein <- round(tax_data$monthly_phasein)
    monthly_payment_nophase <- round(tax_data$monthly_nophase)
    monthly_payment_original <- round(tax_data$monthly_original)
    
    # adding commas within the numbers for easier reading
    prop_tax_new_phasein <- prop_tax_new_phasein |> formatC(format="d", big.mark=",")
    prop_tax_old <- prop_tax_old |> formatC(format="d", big.mark=",")
    prop_tax_new_nophase <- prop_tax_new_nophase |> formatC(format="d", big.mark=",")
    prop_tax_new_original <- prop_tax_new_original |> formatC(format="d", big.mark=",")
    appraisal_new <- appraisal_new |> formatC(format="d", big.mark=",")
    appraisal_old <- appraisal_old |> formatC(format="d", big.mark=",")
    monthly_payment_phasein <- formatC(monthly_payment_phasein, digits = 2, format = "f")
    monthly_payment_nophase <- formatC(monthly_payment_nophase, digits = 2, format = "f")
    monthly_payment_original <- formatC(monthly_payment_original, digits = 2, format = "f")
    
    if (prop_tax_diff_phasein > 0) {
      string <- str_c("In 2024-2025 (FY2025), your property taxes were $",
                      prop_tax_old,
                      ", and your home had a valuation of $",
                      appraisal_old, ".<br/><br/>",
                      
                      # surplus + 4 year phase in
                      "<b>Council budget with phase-in:</b>", " your property taxes in 2025-2026 (FY2026) are estimated to be <b>$",
                      prop_tax_new_phasein, "</b>. Your property taxes would increase by $", prop_tax_diff_phasein,
                      ", which is a ", prop_perc_inc_phasein, "% increase, and you would pay an extra <b>$",
                      monthly_payment_phasein,"</b> per month in taxes. <br/><br/>",
                      
                      # surplus + no phase in 
                      "<b>Council budget with no phase-in:</b>", " your property taxes in 2025-2026 (FY2026) are estimated to be <b>$",
                      prop_tax_new_nophase, "</b>. Your property taxes would increase by $", prop_tax_diff_nophase,
                      ", which is a ", prop_perc_inc_nophase, "% increase, and you would pay an extra <b>$",
                      monthly_payment_nophase, "</b> per month in taxes. <br/><br/>",
                      
                      # original
                      "<b>Mayor Lauren Garrett's Budget:</b>", " your property taxes in 2025-2026 (FY2026) are estimated to be <b>$",
                      prop_tax_new_original, "</b>. Your property taxes would increase by $", prop_tax_diff_original,
                      ", which is a ", prop_perc_inc_original, "% increase, and you would pay an extra <b>$",
                      monthly_payment_original, "</b> per month in taxes. <br/><br/>"
      )
    } else {
      monthly_payment_phasein <- -1 * monthly_payment_phasein
      monthly_payment_nophase <- -1 * monthly_payment_nophase
      prop_perc_inc_phasein   <- -1 * prop_perc_inc_phasein
      prop_perc_inc_nophase   <- -1 * prop_perc_inc_nophase
      
      string <- str_c("In 2024-2025 (FY2025), your property taxes were $",
                      prop_tax_old,
                      ", and your home had a valuation of $",
                      appraisal_old, ".<br/><br/>",
                      
                      # surplus + 4 year phase in
                      "<b>Council budget with phase-in:</b>", " your property taxes in 2025-2026 (FY2026) are estimated to be <b>$",
                      prop_tax_new_phasein, "</b>. Your property taxes would decrease by $", prop_tax_diff_phasein,
                      ", which is a ", prop_perc_inc_phasein, "% decrease, and you would pay $",
                      monthly_payment_phasein," less per month in taxes. <br/><br/>",
                      
                      # surplus + no phase in 
                      "<b>Council budget with no phase-in:</b>", " your property taxes in 2025-2026 (FY2026) are estimated to be <b>$",
                      prop_tax_new_nophase, "</b>. Your property taxes would decrease by $", prop_tax_diff_nophase,
                      ", which is a ", prop_perc_inc_nophase, "% decrease, and you would pay $",
                      monthly_payment_nophase, " less per month in taxes. <br/><br/>",
                      
                      # original
                      "<b>Mayor Lauren Garrett's Budget:</b>", " your property taxes in 2025-2026 (FY2026) are estimated to be <b>$",
                      prop_tax_new_original, "</b>. Your property taxes would decrease by $", prop_tax_diff_original,
                      ", which is a ", prop_perc_inc_original, "% decrease, and you would pay $",
                      monthly_payment_original, " less per month in taxes. <br/><br/>"
      )
    }
    string
  })
  
  
  output$tax_compare_tab1 <- renderPlot({
    req(input$address_tab1)  # Only proceed if an address is selected
    
    df_home <- home_chooser_tab1()
    
    # Check if we have valid data
    if (nrow(df_home) == 0) return("Please select a valid address")
    
    tax_data <- calculated_tax_tab1()
    
    prop_tax_new_phasein <- round(tax_data$new_tax_phasein)
    prop_tax_new_nophase <- round(tax_data$new_tax_nophase)
    prop_tax_new_original <- round(tax_data$new_tax_original)

    monthly_payment_phasein <- round(tax_data$monthly_phasein)
    monthly_payment_nophase <- round(tax_data$monthly_nophase)
    monthly_payment_original <- round(tax_data$monthly_original)
    
    
    tryCatch({
      plot_data <- tibble(
        Budget = c("Council + Phase-In", "Council, No Phase-In", "Garrett"), 
        annual = c(prop_tax_new_phasein, prop_tax_new_nophase, prop_tax_new_original),
        monthly = c(prop_tax_new_phasein, prop_tax_new_nophase, prop_tax_new_original) / 12,
      )
      ggplot(plot_data, aes(x = Budget, y = monthly)) +
        geom_col(aes(fill = Budget), width = 0.7) +
        geom_text(aes(label = paste0("$", formatC(monthly, format = "d", big.mark = ","))), 
                  vjust = -0.5, size = 3.5) +
        labs(title = "Monthly Property Tax For Different Budget Scenarios",
             y = "Monthly Property Tax ($)",
             x = "") +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text = element_text(size = 10),
          panel.grid.major.x = element_blank()
        ) +
        scale_fill_manual(values = c("#99cc67", "#aab75d", "#d35c3f"))
    }, error = function(e) {
      plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_void()
      return(plot)
    })
  })
  
  
  
  #----
  # Tab 2
  # Create a reactive expression for filtered addresses
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
    tryCatch({
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
      for (year in 1:phase_in_term) {
        current_assessment <- assessment_old + ((assessment_new - assessment_old) * (year/phase_in_term))
        current_tax <- current_assessment * mill_rates[year] / 1000
        result[[paste0("year", year)]] <- list(
          tax = current_tax,
          mill_rate = mill_rates[year],
          assessment = current_assessment,
          diff_from_prev = if(year == 1) current_tax - old_tax else current_tax - result[[paste0("year", year-1)]]$tax,
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
      result$appraisal_new <- appraisal_new
      return(result)
    }, error = function(e) {
      print(paste("Error in calculate_taxes_for_all_years:", e$message))
      return(NULL)
    })
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
    req(input$address)
    tax_data <- calculate_taxes_for_all_years()
    if (is.null(tax_data)) return(NULL)
    tryCatch({
      plot_data <- data.frame(
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
        scale_fill_brewer(palette = "Blues")
    }, error = function(e) {
      plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_void()
      return(plot)
    })
  })

  output$tax_comparison_no_phase_in <- renderPlot({
    req(input$address)
    home_data <- home_chooser()
    if (nrow(home_data) == 0) return(NULL)
    tryCatch({
      full_assessment <- as.numeric(home_data$assessment_new)
      if(is.na(full_assessment)) {
        return(NULL)
      }
      no_phase_in_taxes <- vector("numeric", 4)
      for (i in 1:4) {
        no_phase_in_taxes[i] <- full_assessment * no_phase_in_mill_rates[i] / 1000
      }
      baseline_tax <- as.numeric(home_data$property_tax_old)
      if(is.na(baseline_tax)) {
        baseline_tax <- 0
      }
      plot_data <- data.frame(
        Year = c("2024", "Year 1", "Year 2", "Year 3", "Year 4"),
        Tax = c(baseline_tax, no_phase_in_taxes),
        Mill_Rate = c(NA, no_phase_in_mill_rates)
      )
      plot_data$Year <- factor(plot_data$Year, levels = c("2024", "Year 1", "Year 2", "Year 3", "Year 4"))
      ggplot(plot_data, aes(x = Year, y = Tax, group = 1)) +
        geom_col(aes(fill = Year), width = 0.7) +
        geom_text(aes(label = paste0("$", formatC(round(Tax), format = "d", big.mark = ","))),
                  vjust = -0.5, size = 3.5) +
        geom_text(aes(label = ifelse(is.na(Mill_Rate), "", paste0("Mill Rate: ", formatC(Mill_Rate, digits=2, format="f")))),
                  vjust = 1.5, size = 3) +
        labs(title = "Property Tax Comparison (No Phase-In)",
             y = "Annual Property Tax ($)",
             x = "") +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text = element_text(size = 10),
          panel.grid.major.x = element_blank()
        ) +
        scale_fill_brewer(palette = "Oranges")
    }, error = function(e) {
      plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_void()
      return(plot)
    })
  })
}

shinyApp(ui = ui, server = server)