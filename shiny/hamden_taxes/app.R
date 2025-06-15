library(shiny)
library(scales)
library(tidyverse)

df <- read_rds("all_streets_data_4_19_25_cleaned.rds")

phase_in_term <- 4

# Define UI
ui <- fluidPage(
  # ====== Customizable text above the tabs ======
  tags$div(
    id = "above-tabs-text",
    h2("Hamden Tax Calculator (updated on 6/15/2025)"),
    # ---- old two tab text
    # p("This app helps you estimate your property taxes for 2025-2026 (FY2026).
    # The calculator will estimate taxes based on different scenarios regarding the budget and the revaluation phase-in approved by the Legislative Council.
    #   There are two tabs that will display different information:"),
    # tags$li("The first tab ", strong("2025 Property Tax Calculator"), " estimates property taxes with and without a phase-in for next year (FY2026) and how taxes are affected by applying the surplus to the budget."),
    # tags$li("The second tab ", strong("2025-2028 Tax Estimator"), " attempts to estimate and project how property taxes will change for the next 4 years (FY2026-2029) (please read the explanation within this tab carefully)."),
    # br(),
    # p(strong("Please note"), " that the budget and tax situation is still not finalized, and these numbers are still subject to change.")
    
    # ---- one tab text
    p("This app helps you estimate your property taxes for 2025-2026 (FY2026) and provides projections for the next four years (FY2026-FY2029).
    The calculator will estimate and compare taxes based on different scenarios with the budget (e.g., Mayor's vs Council's Budget), the revaluation phase-in, and other options."),
    tags$li("The first tab ", strong("2025 Property Tax Calculator"), " estimates property taxes for next year (FY2026) with and without a phase-in under the Council's Budget and taxes under the Mayor's Budget."),
    tags$li("The second tab ", strong("2025–2028 Tax Estimator and Projections"), " attempts to estimate and project how property taxes will change for the next 4 years (FY2026-2029) under various potential budget outcomes (please read the explanations within this tab carefully)."),
    br(),
    p(strong("Please note"), " that the budget and tax situation is still not finalized, and these numbers are still subject to change."),
    br()
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
               h3("2025–2026 (FY26) Property Tax Calculator"),
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
      title = "2025–2028 Propery Tax Estimator and Projections",
      value = "tax_calculator_tab",
      # Custom CSS for padding
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
      # Application title
      # titlePanel("Estimated property taxes with phased-in mill rate reduction"),
      # Top bar for introduction text
      fluidRow(
        column(12, class = "top-section",
               h3("2025–2028 Propery Tax Estimator and Projections"),
               p("This calculator attempts to estimate your property taxes for the next four fiscal years under different budget scenarios that you can choose.
           It starts with using either the Council's Budget or Mayor Garrett's Budget for FY2026 (i.e., the upcoming year). Then, you can choose if a 4-year phase-in of the revaluation is in place.
           Lastly, you can decide what the budget totals are for the subsequent 3 years. We provide 3 different scenarios of how the budget can change, but you are also free to manually change the budget totals for each year with your own custom numbers."),
               br(),
               p("To estimate your taxes for the next four years:"),
               tags$li("Search and choose the address of your property"),
               tags$li("Choose a proposed budget for FY2026 (either the Council's Budget or Mayor Garrett's Budget"),
               tags$li("Choose if there is a 4-year phase-in of the new valuations"),
               tags$li("Choose one of the convenient budget projection options", strong("or"), "choose your own total budget numbers for each of the three following years."),
               br(),
               p("Please note that these estimates are not official estimates from the town nor are they guaranteed to be accurate.
           The methodology used for these calculations uses a set of broad assumptions (more details explained at the bottom). 
           This tool is meant to illustrate the possible changes to your property taxes under different scenarios for the town budget over the next four years.")
        )
      ),
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
          h4("Choose the following options for the budget for 2026–2029"),
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
          textInput("fy2026", "FY2026", value = comma(303790424)),
          textInput("fy2027", "FY2027", value = comma(319146053)),
          textInput("fy2028", "FY2028", value = comma(334177832)),
          textInput("fy2029", "FY2029", value = comma(349917608))
        ),
        mainPanel(
          h4("Estimated Mill Rates For the Next Four Years"),
          tableOutput("mill_rate_table"),
          h4("Property Tax Estimates by Year"),
          div(class = "output-padding",
              div(class = "year-panel",
                  div(class = "year-title", "2025–2026 (FY2026)"),
                  htmlOutput("string_year1")
              ),
              div(class = "year-panel",
                  div(class = "year-title", "2026–2027 (FY2027)"),
                  htmlOutput("string_year2")
              ),
              div(class = "year-panel",
                  div(class = "year-title", "2027–2028 (FY2028)"),
                  htmlOutput("string_year3")
              ),
              div(class = "year-panel",
                  div(class = "year-title", "2028–2029 (FY2029)"),
                  htmlOutput("string_year4")
              )
          ),
          fluidRow(
            column(12,
                   h4("Monthly Property Taxes Across The Next 4 Years"),
                   plotOutput("tax_comparison_monthly"),
                   h4("Annual Property Taxes Across The Next 4 Years"),
                   plotOutput("tax_comparison")
            )
          )
        )
      ),
      fluidRow(
        column(12, class = "top-section",
               h4("Methodology"),
               p("This section explains some of the numbers, formulas, methods, and assumptions used within the code of this calculator. The estimates and projections are naturally affected by these assumptions and choices, so they must be 
                 evaluated in light of the methodology described below. No guarantee or warranty is provided with the use of this calculator. If there are any potential errors or mistakes, please reach out with details."),
               tags$li("Grand List (i.e., the net taxable property): the pre-revaluation grand list is  $4,065,443,518, and the post-revaluation grand list is $5,726,470,270. The latter is assumed to be the value of the grand list for all four years if there is no phase-in."),
               tags$li("Grand List for 4-year phase-in: the grand list is incrementally increased by 25% of the new valuation total with each year [Grand List of Year X = Pre Grand list + X/4 * (Post Grand List - Pre Grand List)]; 
                   namely, FY2026 grand list would be $4,480,700,206, FY2027 grand list would be  $4,895,956,894, FY2028 grand list would be  $5,311,213,582, and FY2029 grand list would be  $5,726,470,270."),
               tags$li("Property valuation: valuations for properties are obtained from Hamden's online database: https://gis.vgsi.com/Hamdenct/"),
               tags$li("Property valuation with or without phase-in: if no phase in is selected, the property valuations are the post-revaluation numbers for all four years, and if phase-in is selected, the property value is determined
                   similarly to how the phase-in grand list is calculated [Value of Year X = PreValue + X/4 * (PostValue - PreValue)]."),
               tags$li("Annual Property Tax: calculated with the formula Tax = Valuation * 0.7 * Mill Rate / 1000"),
               tags$li("Mayor's Budget: Mayor Garrett's budget is proposed to have a total of $313,653,000 with a mill rate of 43.39 and a total property tax revenue ask of $248,471,545."),
               tags$li("Council's Budget: The Council's budget is proposed to have a total of $304,790,424.29 with a mill rate of 52.16 (with the 4-year phase in), which imputes a total property tax revenue ask of $233,713,322.74."),
               tags$li("Total property tax revenue ask: the total property tax revenue ask is assumed to be 80% of the total budget. This assumption is based on historical data; FY2021 was 80.5%, FY2022 was 82.1%, FY2023 was 80.5%, FY2024 was 79.9%, and FY2025 was 75.2%."),
               tags$li("Certain mill rates are set manually according to reported values: Council's Budget with no phase-in was estimated to have a mill rate of 39.99, Council's Budget with phase-in was estimated to have a mill rate of 52.16 (both provided by the LC), and the Mayor's Budget with no phase-in was reported by the Mayor's office to be 43.39.
                   Mill rates with no reported or published numbers are calculated by dividing the total property tax revenue ask by the grand list of that year and multiplying by 1000: mill rate = property tax revenue ask / grand list * 1000."),
               tags$li("The default option of 'Budget grows 4.71% yearly' has an increase rate of 4.71% because this is the average yearly growth of budget from FY2021 to FY2026 (using the Mayor's proposed budget for FY2026)"),
               tags$li("The option of 'Budget decreases 2% yearly' was chosen to have a decrease rate of 2% because it is a nice round number that seems to keep property taxes relatively flat."),
               tags$li("The budget values for FY2026 are deliberately not editable since there are currently two budget proposals in contention. However, it should be acknowledged that the 'Budget grows 4.71% yearly' option with the Council's Budget will naturally 
                       have lower property taxes across all four years because the starting point for the budget at FY2026 is lower (which is a consequence of compounding interest). If you are more interested in comparing both budget with identical absolute yearly increases, do so by manually changing the budget numbers for FY2027–FY2029.")
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
  parse_input <- function(x) as.numeric(gsub(",", "", x))
  council_vals <- list(
    grow     = c(303790424, 319146053, 334177832, 349917608),
    flat     = rep(303790424, 4),
    decrease = c(303790424, 298694616, 292720723, 286866309)
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
        labs(y = "Annual Property Tax ($)",
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
  
  output$tax_comparison_monthly <- renderPlot({
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
      plot_data <- plot_data |>
        mutate(Tax = round(Tax/12))
      plot_data$Tax[is.na(plot_data$Tax)] <- 0
      plot_data$Year <- factor(plot_data$Year, levels = c("2024", "Year 1", "Year 2", "Year 3", "Year 4"))
      ggplot(plot_data, aes(x = Year, y = Tax, group = 1)) +
        geom_col(aes(fill = Year), width = 0.7) +
        geom_text(aes(label = paste0("$", formatC(round(Tax), format = "d", big.mark = ","))),
                  vjust = -0.5, size = 3.5) +
        geom_text(aes(label = ifelse(is.na(Mill_Rate), "", paste0("Mill Rate: ", formatC(Mill_Rate, digits=2, format="f")))),
                  vjust = 1.5, size = 3) +
        labs(y = "Annual Property Tax ($)",
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
  
}

shinyApp(ui = ui, server = server)