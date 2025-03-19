library(shiny)
library(tidyverse)

df <- read_rds("all_streets_data_4_18_25_cleaned.rds")

list_addresses <- c("", df$address)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Estimated property taxes with proposed mill rate"),
  
  # Top bar for introduction text
  fluidRow(
    column(12,
           h3("Introduction"),
           p("This is the introduction text for the Shiny app.")
    )
  ),
  
  # Sidebar layout
  sidebarLayout(
    # Left sidebar for choosing options
    sidebarPanel(
      width = 4,  # 1/3 of the page
      h4("Options"),
      selectizeInput(inputId = "address",
                     label = "Search and choose your address",
                     list_addresses,
                     selected = FALSE,
                     multiple = FALSE,
                     options = list(create = TRUE)),
    ),
    
    # Right sidebar for strings and graphs
    mainPanel(
      width = 8,  # 2/3 of the page
      h4("Output"),
      htmlOutput("string"),
      
      fluidRow(
        column(6, plotOutput("plot1")),
        column(6, plotOutput("plot2"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  home_chooser <- reactive({df |>
      filter(address == input$address)
  })

  output$string <- renderText({
    df_home <- home_chooser()
    prop_tax_new <- df_home[["property_tax_new"]] |> round()
    prop_tax_old <- df_home[["property_tax_old"]] |> round()
    appraisal_new <- df_home[["appraisal_new"]] |> round()  
    appraisal_old <- df_home[["appraisal_old"]] |> round()  
    prop_tax_diff <- prop_tax_new - prop_tax_old 
    prop_perc_inc <- formatC(prop_tax_diff / prop_tax_old * 100, digits = 1, format = "f")
    monthly_payment <- prop_tax_diff / 12 |> round()
    
    # adding commas within the numbers for easier reading
    prop_tax_new <- prop_tax_new |> formatC(format="d", big.mark=",")
    prop_tax_old <- prop_tax_old |> formatC(format="d", big.mark=",")
    appraisal_new <- appraisal_new |> formatC(format="d", big.mark=",")
    appraisal_old <- appraisal_old |> formatC(format="d", big.mark=",")
    monthly_payment <- formatC(monthly_payment, digits = 2, format = "f")
    
    string <- str_c("With the proposed mill rate of 55.61, your property taxes will be $",
                    prop_tax_new,
                    " based on the current value of your home of $",
                    appraisal_new, ".<br/><br/>",
                    "In 2024, your property taxes was $",
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
    df_home <- home_chooser()
    
    df_home <- df_home |>
      rename(`Proposed New Property Tax` = property_tax_new,
             `2024 Property Tax` = property_tax_old,
             `Appraisal - 2025` = appraisal_new,
             `Appraisal - 2024` = appraisal_old,
             `Tax Assessment - 2025` = assessment_new,
             `Tax Assessment - 2024` = assessment_old)
    
    # finding nice y max limits for each number
    ymax_tax <- df_home$`Proposed New Property Tax`
    ymax_tax <- ceiling(ymax_tax / 1000) * 1000 # round to nearest thousand that's larger
    ymax_tax <- ifelse(ymax_tax %% 2 != 0, ymax_tax + 2000, ymax_tax + 1000)
    
   
    p1 <- df_home |>
      pivot_longer(cols = -address) |>
      filter(grepl("Property", name)) |>
      mutate(group = as_factor(1:2)) |>
      ggplot() +
      geom_col(aes(name, value, fill = group), width = 0.65) +
      xlab("") +
      ylab("Property Tax") +
      scale_fill_manual(values = c("#fc4b11", "#b1eb61")) +
      scale_y_continuous(breaks = seq(0, ymax_tax, by = 2000)) +
      theme_classic() +
      theme(legend.position="none")

    p1
  })
  
  
  output$plot2 <- renderPlot({
    df_home <- home_chooser()
    
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
    
    p2 <- df_home |>
      pivot_longer(cols = -address) |>
      filter(grepl("Appraisal", name)) |>
      mutate(group = as_factor(1:2)) |>
      ggplot() +
      geom_col(aes(name, value, fill = group), width = 0.65) +
      xlab("") +
      ylab("Appraisal Tax") +
      scale_fill_manual(values = c("#73b0d9", "#a9e1e6")) +
      scale_y_continuous(breaks = seq(0, ymax_appraisal, by = 50000)) +
      theme_classic() +
      theme(legend.position="none")
    
    p2
  })



  # p1 <- df_home |>
  #   pivot_longer(cols = -address) |>
  #   filter(grepl("Property", name)) |>
  #   mutate(group = as_factor(1:2)) |>
  #   ggplot() +
  #   geom_col(aes(name, value, fill = group), width = 0.65) +
  #   xlab("") +
  #   ylab("Property Tax") +
  #   scale_fill_manual(values = c("#fc4b11", "#b1eb61")) +
  #   scale_y_continuous(breaks = seq(0, ymax_tax, by = 2000)) + 
  #   theme_classic() +
  #   theme(legend.position="none")
  # 
  # # plot for 2024 and new appraisal  
  p2 <- df_home |>
    pivot_longer(cols = -address) |>
    filter(grepl("Appraisal", name)) |>
    mutate(group = as_factor(1:2)) |>
    ggplot() +
    geom_col(aes(name, value, fill = group), width = 0.65) +
    xlab("") +
    ylab("Appraisal Value") +
    scale_fill_manual(values = c("#73b0d9", "#a9e1e6")) +
    scale_y_continuous(breaks = seq(0, ymax_appraisal, by = 50000)) +
    theme_classic() +
    theme(legend.position="none")
  # 
  # output$plot1 <- renderPlot({
  #   p1
  # })
  # 
  # output$plot2 <- renderPlot({
  #   p2
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)