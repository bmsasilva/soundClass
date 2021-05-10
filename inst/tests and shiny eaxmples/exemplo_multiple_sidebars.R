library(shiny)
library(plotly)

shinyApp(
  ui = fluidPage(
    titlePanel("Fit model"),
    tabsetPanel(
      tabPanel("Map", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(selectInput("Country", "Select Country", choices = "", selected = "")),
                 mainPanel(
                   htmlOutput("Attacks")
                 ) #mainPanel
               )#sidebarLayout
      ),#tabPanel Map
      tabPanel("plot", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(fluidRow(
                   column(7,  plotlyOutput("")),
                   column(5, plotlyOutput(""))   
                 )#fluidRow
                 )#mainPanel
               )#sidebarLayout
      )#tabPanel plot
    ) # tabsetPanel
  ), # fluidPage
  server = function(input, output) {
    
  }
) #shinyApp