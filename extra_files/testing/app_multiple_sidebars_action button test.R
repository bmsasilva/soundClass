library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("tab1",
             actionButton("btn1","Button 1"),
             textOutput("text")
    ),
    tabPanel("tab2",
             actionButton("btn2","Button 2"),
             textOutput("text2")
    )
  )
  
)

server <- function(input, output) {
  data=eventReactive(input$btn1,{
     print("Button 1 is pressed!")
    # print(input$btn1)
    # print(input$btn2)
    iris
  })
  data2=eventReactive(input$btn2,{
    print("Button 2 is pressed!")
    print(input$btn1)
    print(input$btn2)
    iris
  })
  
  output$text=renderText({
    req(data())
    print(head(data()))
    "Really? Button 1 is pressed!"
  })
  
  output$text2=renderText({
    req(data2())
    print(head(data2()))
    "Really? Button 2 is pressed!"
  })
}

# Run the application 
shinyApp(ui = ui, server = server)