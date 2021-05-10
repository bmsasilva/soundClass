library(shiny)
library(keras)

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
    
    cb <- callback_lambda(on_epoch_end = function(epoch, logs) {
      shinyjs::html("metrics", paste("mse: ", logs$mse))
    })
    
    model %>% 
      fit(
        x = matrix(runif(100), ncol = 1), 
        y = matrix(runif(100), ncol = 1),
        callbacks = list(cb),
        verbose = 0
      )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)