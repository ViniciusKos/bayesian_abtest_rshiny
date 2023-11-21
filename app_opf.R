# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard"),
  dashboardSidebar(
    checkboxInput("cumulative", "Show Accumulated Sales", value = FALSE),
    checkboxGroupInput("groups", "Filter by Group:",
                       choices = unique(sales_data$Group),
                       selected = unique(sales_data$Group)),
    checkboxInput("total", "Show Total Sales", value = FALSE)
  ),
  dashboardBody(
    fluidRow(
      box(
        plotOutput("lineplot", height = 400),
        width = 12
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    data <- sales_data %>%
      filter(Group %in% input$groups)
    
    if (input$cumulative) {
      data <- data %>%
        group_by(Group) %>%
        mutate(Sales = cumsum(Sales))
    }
    
    if (input$total) {
      total_data <- data %>%
        group_by(Date) %>%
        summarise(Sales = sum(Sales)) |> 
        mutate(Group = "Total")
      
      if (input$cumulative) {
        total_data <- total_data %>%
          mutate(Sales = cumsum(Sales))
      }
      
      data <- total_data
    }
    
    data
  })
  
  output$lineplot <- renderPlot({
    p <- ggplot(filtered_data(), aes(x = Date, y = Sales, color = Group)) +
      geom_line() +
      labs(title = "Resultado Mensal (em R$ milhares de VPL)",
           x = "Date",
           y = if (input$cumulative) "Cumulative Sales" else "Monthly Sales") +
      theme_minimal() + theme(plot.title = element_text(size=22))
    
    # Add value labels with "K" suffix
    if (!is.null(filtered_data())) {
      p <- p + geom_text(aes(label = paste0(format(Sales / 1000, digits = 1), "mil")),
                         hjust = 0, vjust = -0.5, size = 5, show.legend = FALSE)
    }
    
    p
  })
}

# Run the application
shinyApp(ui, server)
