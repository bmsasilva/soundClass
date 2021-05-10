library(shiny)
library(keras)
library(kerasR)

ui <- fluidPage(
  shinyjs::useShinyjs(),  # Include shinyjs
  actionButton("button", "Click me"),
  textOutput('metrics')
)

server <- function(input, output) {
  
  observeEvent(input$button, {
    
    model <- keras_model_sequential() %>%
      layer_dense(1)
    
    model %>%
      compile(loss = "mse" , optimizer = "adam", metrics = "mse")
    
    csv_logger <- CSVLogger('training.log')
    
     cb <- callback_lambda(on_epoch_end = function(csv_logger) {
      dd<- read.csv('training.log')
       shinyjs::html("metrics",csv_logger)
     })
    
    #history(
    model %>% 
      fit(
        x = matrix(runif(100), ncol = 1), 
        y = matrix(runif(100), ncol = 1),
        callbacks = list(csv_logger),
        verbose = 1
      )
    #)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)