library(shiny)
library(plotly)

library(shiny)

# This function, borrowed from http://www.r-bloggers.com/safe-loading-of-rdata-files/, load the Rdata into a new environment to avoid side effects
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

ui <- shinyUI(fluidPage(
  
  titlePanel("Example"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("generate", "Click to generate an Rdata file")
    ),
    
    mainPanel(
      tableOutput("table")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
  # Click the button to generate a new random data frame and write to file
  observeEvent(input$generate, {
    sample_dataframe <- data.frame(a=runif(10), b=rnorm(10))
    save(sample_dataframe, file="test.Rdata")
    rm(sample_dataframe)
  })
  
  output$table <- renderTable({
    # Use a reactiveFileReader to read the file on change, and load the content into a new environment
    env <- reactiveFileReader(1000, session, "test.Rdata", LoadToEnvironment)
    # Access the first item in the new environment, assuming that the Rdata contains only 1 item which is a data frame
    env()[[names(env())[1]]]
  })
  
})

shinyApp(ui = ui, server = server)