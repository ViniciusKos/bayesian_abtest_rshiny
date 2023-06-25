#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages("renv")


renv::activate()


# Sys.setenv(RETICULATE_PYTHON = r"(P:\Python\GitHub\bayesian_abtest\app_shiny\bayesian_env\Scripts\python.exe)")
library(shiny)
library(ggplot2)
library(readr)
library(magrittr)


df <- read_csv(r"(data_experiment_probas.csv)")


# Define the UI
ui <- fluidPage(
  titlePanel("Bayesian Test"),
  plotOutput("comparisonPlot")
  )


# Define the server logic
server <- function(input, output, session) {
  selected <- reactive(df)
  
  output$comparisonPlot <- renderPlot({
    selected() %>%
      ggplot( aes(x1, proba_b_better_a)) + geom_line(colour="blue") + labs( y="Prob Test Better Control")}
    ,res=96)
}


# Run the app
shinyApp(ui = ui, server = server)