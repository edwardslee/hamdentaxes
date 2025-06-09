library(shiny)
library(tidyverse)

df <- read_rds("all_streets_data_4_19_25_cleaned.rds")

# Initial mill rate and target mill rate for phase-in
initial_mill_rate <- 52
target_mill_rate <- 41
phase_in_term <- 4
annual_growth_rate <- 0.04

# Calculate mill rates for each year of phase-in
# First calculate the base decrease, then apply 4% annual growth
mill_rates <- vector("numeric", phase_in_term)
for (i in 1:phase_in_term) {
  # Base decrease from initial to target over phase_in_term years
  base_decrease_per_year <- (initial_mill_rate - target_mill_rate) / phase_in_term
  
  # Baseline mill rate without growth
  base_mill_rate <- initial_mill_rate - base_decrease_per_year * (i-1)
  
  # Apply compound growth
  mill_rates[i] <- base_mill_rate * (1 + annual_growth_rate)^(i-1)
}

# Define UI
ui <- fluidPage(
  # Custom CSS for padding
  tags$style(HTML("
    .top-section {
      padding-bottom: 20px; /* Adjust the value as needed */
    }
    .output-padding {
      padding-bottom: 20px; /* Adjust the value as needed */
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
  titlePanel("Estimated property taxes with phased-in mill rate reduction"),
  
  # Top bar for introduction text
  fluidRow(
    column(12, class = "top-section",
           h3("Hamden Tax Calculator (updated on 6/6/2026)"),
           p(paste0("This app lets you estimate your property taxes for all 4 years of the phase-in period. ",
                    "The mill rate starts with a base decrease from ", initial_mill_rate, " to ", target_mill_rate, 
                    " over 4 years, plus a 4% annual growth rate to account for town budget increases. ",
                    "The resulting mill rates range from ", 
                    round(mill_rates[1], 2), " in Year 1 to ", 
                    round(mill_rates[4], 2), " in Year 4.")),
           tags$li("On the left, search for an address and then choose one from the dropdown menu"),
           tags$li("The app will display tax estimates for all 4 years of the phase-in period"),
           tags$li("Mill rates incorporate both the phase-in reduction and 4% annual budget growth")
    )
  ),
  
  # Sidebar layout
  sidebarLayout(
    # Left sidebar for choosing options
    sidebarPanel(
      width = 4,  # 1/3 of the page
      h4("Options"),
      # Using a textInput for search and a selectInput for selection
      textInput("address_search", "Search for your address", ""),
      selectInput("address", "Choose your address", choices = NULL),
      # Show the mill rates for each year
      h4("Mill Rates by Year"),
      tags$div(
        tags$p(paste0("Year 1: ", round(mill_rates[1], 2))),
        tags$p(paste0("Year 2: ", round(mill_rates[2], 2))),
        tags$p(paste0("Year 3: ", round(mill_rates[3], 2))),
        tags$p(paste0("Year 4: ", round(mill_rates[4], 2)))
      )
    ),
    
    # Right sidebar for strings and graphs
    mainPanel(
      width = 8,  # 2/3 of the page
      h4("Property Tax Estimates by Year"),
      div(class = "output-padding", 
          div(class = "year-panel", 
              div(class = "year-title", "Year 1 (2025)"),
              htmlOutput("string_year1")
          ),
          div(class = "year-panel", 
              div(class = "year-title", "Year 2 (2026)"),
              htmlOutput("string_year2")
          ),
          div(class = "year-panel", 
              div(class = "year-title", "Year 3 (2027)"),
              htmlOutput("string_year3")
          ),
          div(class = "year-panel", 
              div(class = "year-title", "Year 4 (2028)"),
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
               h4("Tax Comparison Across Years (No Phase-In, Starting Mill Rate: 41)"),
               plotOutput("tax_comparison_no_phase_in")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Create a reactive expression for filtered addresses
  filtered_addresses <- reactive({
    # Get the search term
    search_term <- input$address_search
    
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
    choices <- filtered_addresses()
    updateSelectInput(session, "address", choices = choices)
  })
  
  home_chooser <- reactive({df |>
      filter(address == input$address)
  })
  
  # Calculate property taxes for each year of phase-in
  calculate_taxes_for_all_years <- reactive({
    req(input$address)
    home_data <- home_chooser()
    
    if (nrow(home_data) == 0) return(NULL)
    
    # Make sure we have numeric values
    tryCatch({
      # Old tax and appraisal - ensure numeric
      old_tax <- as.numeric(home_data$property_tax_old)
      appraisal_old <- as.numeric(home_data$appraisal_old)
      appraisal_new <- as.numeric(home_data$appraisal_new)
      assessment_old <- as.numeric(home_data$assessment_old)
      assessment_new <- as.numeric(home_data$assessment_new)
      
      # Handle any NA values
      if(is.na(old_tax)) old_tax <- 0
      if(is.na(assessment_old)) assessment_old <- 0
      if(is.na(assessment_new)) assessment_new <- 0
      if(is.na(appraisal_old)) appraisal_old <- 0
      if(is.na(appraisal_new)) appraisal_new <- 0
      
      # Calculate taxes for each year
      result <- list()
      for (year in 1:phase_in_term) {
        # Calculate assessment for the current phase-in year
        current_assessment <- assessment_old + ((assessment_new - assessment_old) * (year/phase_in_term))
        
        # Calculate tax using the mill rate for this year
        current_tax <- current_assessment * mill_rates[year] / 1000
        
        # Store the results
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
      
      # Add the baseline (2024) data for reference
      result$baseline <- list(
        tax = old_tax,
        assessment = assessment_old,
        appraisal = appraisal_old
      )
      
      result$appraisal_new <- appraisal_new
      
      return(result)
    }, error = function(e) {
      # If there's an error, return NULL
      print(paste("Error in calculate_taxes_for_all_years:", e$message))
      return(NULL)
    })
  })
  
  # Generate tax info string for a specific year
  generate_tax_string <- function(year_data, year_num, prev_year_name) {
    req(year_data)
    
    # Error handling
    tryCatch({
      # Format numbers for display, ensuring they are numeric
      tax_value <- ifelse(is.null(year_data[[paste0("year", year_num)]]$tax), 
                         0, 
                         as.numeric(year_data[[paste0("year", year_num)]]$tax))
      tax <- round(tax_value) |> formatC(format="d", big.mark=",")
      
      if (year_num == 1) {
        # For Year 1, compare with 2024
        prev_tax_value <- ifelse(is.null(year_data$baseline$tax), 0, as.numeric(year_data$baseline$tax))
        prev_tax <- round(prev_tax_value) |> formatC(format="d", big.mark=",")
        prev_year_text <- "2024"
      } else {
        # For later years, compare with previous year
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
      
      # Create the string
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
  
  # Render the tax information for each year
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
  
  # Create a comparison plot for all years
  output$tax_comparison <- renderPlot({
    req(input$address)
    tax_data <- calculate_taxes_for_all_years()
    if (is.null(tax_data)) return(NULL)
    
    # Use tryCatch to handle any errors safely
    tryCatch({
      # Create a data frame for plotting, ensuring all values are numeric
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
          NA,  # No mill rate shown for baseline year
          as.numeric(tax_data$year1$mill_rate),
          as.numeric(tax_data$year2$mill_rate),
          as.numeric(tax_data$year3$mill_rate),
          as.numeric(tax_data$year4$mill_rate)
        )
      )
      
      # Handle any NA values that might have been created
      plot_data$Tax[is.na(plot_data$Tax)] <- 0
      
      # Convert Year to factor to maintain order
      plot_data$Year <- factor(plot_data$Year, levels = c("2024", "Year 1", "Year 2", "Year 3", "Year 4"))
      
      # Create the plot
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
      # If there's an error, return an empty plot with an error message
      plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_void()
      return(plot)
    })
  })
  
  # Create a comparison plot for all years without phase-in
  output$tax_comparison_no_phase_in <- renderPlot({
    req(input$address)
    home_data <- home_chooser()
    if (nrow(home_data) == 0) return(NULL)
    
    # Make sure we have numeric values
    tryCatch({
      # Get the assessment without phase-in and ensure it's numeric
      full_assessment <- as.numeric(home_data$assessment_new)
      if(is.na(full_assessment)) {
        return(NULL)
      }
      
      # Calculate taxes with a starting mill rate of 41 that grows by 4% each year
      no_phase_in_mill_rates <- vector("numeric", 4)
      no_phase_in_mill_rates[1] <- 41
      for (i in 2:4) {
        no_phase_in_mill_rates[i] <- no_phase_in_mill_rates[1] * (1 + annual_growth_rate)^(i-1)
      }
      
      # Calculate taxes for each year
      no_phase_in_taxes <- vector("numeric", 4)
      for (i in 1:4) {
        no_phase_in_taxes[i] <- full_assessment * no_phase_in_mill_rates[i] / 1000
      }
      
      # Add baseline (2024) tax for comparison
      baseline_tax <- as.numeric(home_data$property_tax_old)
      if(is.na(baseline_tax)) {
        baseline_tax <- 0
      }
      
      # Create a data frame for plotting
      plot_data <- data.frame(
        Year = c("2024", "Year 1", "Year 2", "Year 3", "Year 4"),
        Tax = c(baseline_tax, no_phase_in_taxes),
        Mill_Rate = c(NA, no_phase_in_mill_rates)  # No mill rate for baseline year
      )
      
      # Convert Year to factor to maintain order
      plot_data$Year <- factor(plot_data$Year, levels = c("2024", "Year 1", "Year 2", "Year 3", "Year 4"))
      
      # Create the plot
      ggplot(plot_data, aes(x = Year, y = Tax, group = 1)) +
        geom_col(aes(fill = Year), width = 0.7) +
        geom_text(aes(label = paste0("$", formatC(round(Tax), format = "d", big.mark = ","))), 
                  vjust = -0.5, size = 3.5) +
        geom_text(aes(label = ifelse(is.na(Mill_Rate), "", paste0("Mill Rate: ", formatC(Mill_Rate, digits=2, format="f")))),
                  vjust = 1.5, size = 3) +
        labs(title = "Property Tax Comparison (No Phase-In, Starting Mill Rate: 41)",
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
      # If there's an error, return an empty plot with an error message
      plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
        theme_void()
      return(plot)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
