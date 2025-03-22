library(shiny)
library(tidyverse)

df <- read_rds("all_streets_data_4_19_25_cleaned.rds")

# Define UI
ui <- fluidPage(
  # Custom CSS for padding
  # Custom CSS for padding
  tags$style(HTML("
    .top-section {
      padding-bottom: 20px; /* Adjust the value as needed */
    }
    .output-padding {
      padding-bottom: 20px; /* Adjust the value as needed */
    }
  ")),
  
  # Application title
  titlePanel("Estimated property taxes with proposed mill rate"),
  
  # Top bar for introduction text
  fluidRow(
    column(12, class = "top-section",
           h3("Hamden Tax Calculator"),
           p("This app lets you estimate your property taxes for the year 2025 based on the proposed mill rate of 46.61.
             It also shows you how much your taxes would have been in 2024 and the increase in taxes for 2025."),
           tags$li("On the left, search for an address and then choose one from the dropdown menu"),
           tags$li("'Enter mill rate' uses the current proposed mill rate. You can enter your own mill rate to see property taxes are affected.")#,
           # tags$li("Click the 'Generate Email' for a prewritten email template that includes numbers for the increase taxes for the property.")
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
      # Add mill rate input
      numericInput("mill_rate", "Enter mill rate:", 
                   value = 46.61, min = 20, max = 100, step = 0.01),
      helpText("Default proposed mill rate is 46.61")#,
      # Add action button
      # actionButton("generate_email", "Generate Email")
    ),
    
    # Right sidebar for strings and graphs
    mainPanel(
      width = 8,  # 2/3 of the page
      h4("Output"),
      htmlOutput("string"),
      div(class = "output-padding", htmlOutput("string")),
      
      fluidRow(
        column(6, plotOutput("plot1")),
        column(6, plotOutput("plot2"))
      ),
      
      # # Bottom panel
      # fluidRow(
      #   h4("Email template with property tax numbers for the property"),
      #   column(12, class  = "bottom-panel",
      #          htmlOutput("bottom_string")
      #   )
      # )
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

  # Calculate property tax based on chosen mill rate
  calculated_tax <- reactive({
    req(input$address)
    home_data <- home_chooser()
    
    # Calculate new tax based on input mill rate
    new_tax <- (home_data$assessment_new / 1000) * input$mill_rate
    
    # Original 2024 tax stays the same
    old_tax <- home_data$property_tax_old
    
    list(
      new_tax = new_tax,
      old_tax = old_tax,
      diff = new_tax - old_tax,
      perc_inc = (new_tax - old_tax) / old_tax * 100,
      monthly = (new_tax - old_tax) / 12
    )
  })

  output$string <- renderText({
    req(input$address)  # Only proceed if an address is selected
    
    df_home <- home_chooser()
    
    # Check if we have valid data
    if (nrow(df_home) == 0) return("Please select a valid address")
    
    tax_data <- calculated_tax()
    
    prop_tax_new <- round(tax_data$new_tax)
    prop_tax_old <- round(df_home[["property_tax_old"]])
    appraisal_new <- round(df_home[["appraisal_new"]])
    appraisal_old <- round(df_home[["appraisal_old"]])
    prop_tax_diff <- round(tax_data$diff)
    prop_perc_inc <- formatC(tax_data$perc_inc, digits = 1, format = "f")
    monthly_payment <- round(tax_data$monthly)
    
    # adding commas within the numbers for easier reading
    prop_tax_new <- prop_tax_new |> formatC(format="d", big.mark=",")
    prop_tax_old <- prop_tax_old |> formatC(format="d", big.mark=",")
    appraisal_new <- appraisal_new |> formatC(format="d", big.mark=",")
    appraisal_old <- appraisal_old |> formatC(format="d", big.mark=",")
    monthly_payment <- formatC(monthly_payment, digits = 2, format = "f")
    
    string <- str_c("With a mill rate of ", input$mill_rate, ", your property taxes will be <b>$",
                   prop_tax_new,
                   "</b> based on the current value of your home of $",
                   appraisal_new, ".<br/><br/>",
                   "In 2024, your property taxes were $",
                   prop_tax_old,
                   ", and your home was valued at $",
                   appraisal_old, ".<br/><br/>",
                   "Your property taxes will increase by $",
                   prop_tax_diff,
                   ", which is a ",
                   prop_perc_inc, 
                   "% increase, and you will pay an extra $",
                   monthly_payment,
                   " per month in taxes.")
    string
  })
  
  
  
  output$plot1 <- renderPlot({
    req(input$address)  # Only proceed if an address is selected
    df_home <- home_chooser()
    tax_data <- calculated_tax()
    
    # Check if we have data
    if(nrow(df_home) == 0) return(NULL)
    
    # Create a modified df_home with the calculated tax
    df_home <- df_home |>
      mutate(`Proposed New Property Tax` = tax_data$new_tax) |>
      rename(`2024 Property Tax` = property_tax_old,
             `Appraisal - 2025` = appraisal_new,
             `Appraisal - 2024` = appraisal_old,
             `Tax Assessment - 2025` = assessment_new,
             `Tax Assessment - 2024` = assessment_old)
    
    # finding nice y max limits for each number
    ymax_tax <- df_home$`Proposed New Property Tax`
    ymax_tax <- ceiling(ymax_tax / 1000) * 1000 # round to nearest thousand that's larger
    ymax_tax <- ifelse(ymax_tax %% 2 != 0, ymax_tax + 2000, ymax_tax + 1000)
    
    # First create the long data
    plot_data <- df_home |>
      pivot_longer(cols = -address) |>
      filter(grepl("Property", name))
    
    # Only add the group if we have the right number of rows
    if(nrow(plot_data) == 2) {
      plot_data <- plot_data |> mutate(group = as_factor(1:2))
    } else {
      plot_data <- plot_data |> mutate(group = as_factor(1))
    }
    
    p1 <- plot_data |>
      ggplot() +
      geom_col(aes(name, value, fill = group), width = 0.65) +
      xlab("") +
      ylab("") +
      ggtitle("Property Taxes") + 
      scale_fill_manual(values = c("#b1eb61", "#fc4b11")) +
      scale_y_continuous(breaks = seq(0, ymax_tax, by = 2000)) +
      theme_classic() +
      theme(legend.position="none") +
      theme(axis.text = element_text(color = "black",size = 10)) +
      scale_x_discrete(guide = guide_axis(angle = 45))

    p1
  })
  
  
  output$plot2 <- renderPlot({
    req(input$address)  # Only proceed if an address is selected
    df_home <- home_chooser()
    
    # Check if we have data
    if(nrow(df_home) == 0) return(NULL)
    
    df_home <- df_home |>
      rename(`Proposed New Property Tax` = property_tax_new,
             `2024 Property Tax` = property_tax_old,
             `Appraisal - 2025` = appraisal_new,
             `Appraisal - 2024` = appraisal_old,
             `Tax Assessment - 2025` = assessment_new,
             `Tax Assessment - 2024` = assessment_old)
    
    # finding nice y max limits for each number
    ymax_appraisal <- df_home$`Appraisal - 2025`
    ymax_appraisal <- ceiling(ymax_appraisal / 25000) * 25000 + 10000
    
    # First create the long data
    plot_data <- df_home |>
      pivot_longer(cols = -address) |>
      filter(grepl("Appraisal", name))
    
    # Only add the group if we have the right number of rows
    if(nrow(plot_data) == 2) {
      plot_data <- plot_data |> mutate(group = as_factor(1:2))
    } else {
      plot_data <- plot_data |> mutate(group = as_factor(1))
    }
    
    p2 <- plot_data |>
      ggplot() +
      geom_col(aes(name, value, fill = group), width = 0.65) +
      xlab("") +
      ylab("") +
      ggtitle("Appraisal Value") + 
      scale_fill_manual(values = c("#73b0d9", "#a9e1e6")) +
      scale_y_continuous(breaks = seq(0, ymax_appraisal, by = 50000)) +
      theme_classic() +
      theme(legend.position="none") +
      theme(
        axis.text = element_text(color = "black",size = 10)
      )  +
      scale_x_discrete(guide = guide_axis(angle = 45))
    
    p2
  })
  
  output$bottom_string <- renderText({
    req(input$address)  # Only proceed if an address is selected
    
    df_home <- home_chooser()
    
    # Check if we have valid data
    if (nrow(df_home) == 0) return("Please select a valid address")
    
    # Additional information text
    bottom_string <- "Click Generate Email to create a custom email template"
    bottom_string
  })
  
  # observeEvent(input$generate_email, {
  #   df_home <- home_chooser()
  #   
  #   # Check if we have valid data
  #   if (nrow(df_home) == 0) {
  #     output$bottom_string <- renderText("Please select a valid address")
  #     return()
  #   }
  #   
  #   tax_data <- calculated_tax()
  #   
  #   prop_tax_new <- round(tax_data$new_tax)
  #   prop_tax_old <- round(df_home[["property_tax_old"]])
  #   appraisal_new <- round(df_home[["appraisal_new"]])
  #   appraisal_old <- round(df_home[["appraisal_old"]])
  #   prop_tax_diff <- round(tax_data$diff)
  #   prop_perc_inc <- formatC(tax_data$perc_inc, digits = 1, format = "f")
  #   
  #   # adding commas within the numbers for easier reading
  #   prop_tax_new <- prop_tax_new |> formatC(format="d", big.mark=",")
  #   
  #   email_string <- 
  #     str_c(
  #       "To Mayor Lauren Garret and the Hamden Legislative Council Members, <br><br>",
  #       "I am writing as a homeowner in Hamden, CT, to urge you to reconsider the proposed mill rate of 46.61 ",
  #       "and instead adopt a significantly lower rate—ideally in the mid-to-low 30s. This drastic increase in ",
  #       "property taxes would place an overwhelming financial burden on residents, many of whom are already struggling",
  #       "with rising costs in nearly every aspect of daily life, from exorbitant grocery prices to some of the highest utility rates in the nation. ",
  #       "For my property, the proposed mill rate would increase my property taxes by $", prop_tax_diff, ", which is a ", prop_perc_inc, "% increase compared to my 2024 property taxes.<br><br>",
  #       "Connecticut is already one of the most expensive states to live in, and Hamden ranks among its costliest towns. With the new property appraisals set to take effect in May 2025, many homeowners have seen their home values increase by 40% to 60%. Under the proposed mill rate, the average homeowner’s annual property tax bill would rise over 20%, adding hundreds of dollars to monthly expenses. This is not only an unsustainable financial strain on individual residents but also a significant threat to the town’s long-term economic health.<br><br>",
  #       "A town thrives when it attracts and retains taxpaying homeowners who invest in and maintain their properties. Excessive property taxes will discourage new residents and businesses from moving to Hamden while forcing out long-time homeowners—especially retirees who may have paid off their mortgages but can no longer afford the tax burden. If this trend continues, Hamden risks declining property values, reduced homeownership, and an eroded sense of community.<br><br>",
  #       "Homeowners cannot and should not bear the full weight of the town’s financial burdens. This level of taxation is untenable, unfair, and unsustainable. I urge you to consider the broader implications of this decision, not just for individual residents but for the future stability and prosperity of Hamden itself.<br><br>",
  #       "Please know that we, the homeowners, are paying close attention. The outcome of this vote will not be forgotten when we cast our ballots in the primaries and general elections.<br>",
  #       "I'm not alone in how I feel—check out this petition to see others who support this change.<br><br>",
  #       "Sincerely,<br>",
  #       "[Your Name]<br>",
  #       "[Your Address]"
  #     )
  #   
  #   output$bottom_string <- renderText(email_string)
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
