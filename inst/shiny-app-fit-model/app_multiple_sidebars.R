library(shiny)
library(plotly)

shinyApp(
  
  # UI ----------------------------------------------------------------------
  ui = fluidPage(
    titlePanel("Modeling"),
    tabsetPanel(
      # Panel create train data -------------------------------------------------
      tabPanel("Create train data", 
               sidebarLayout(fluid = FALSE,
                             # Define the sidebar panel ------------------------------------------------
                             sidebarPanel(width = 2,
                                          # Button 1 - Choose folder
                                          shinyDirButton(id = 'folder',
                                                         label = 'Choose folder',
                                                         title = 'Choose recordings folder', 
                                                         style = 'width:100%'),
                                          br(), 
                                          br(), 
                                          # Button 2 - Choose database
                                          shinyFilesButton(id = 'selected_db', 
                                                           label = 'Choose database',
                                                           title = 'Choose database file',
                                                           multiple = FALSE, 
                                                           style='width:100%'),
                                          br(), 
                                          br(), 
                                          # Button 3 - Choose model
                                          shinyFilesButton(id = 'selected_model', 
                                                           label = 'Choose model',
                                                           title = 'Choose model file', 
                                                           multiple = FALSE,
                                                           style = 'width:100%'),
                                          hr(), # Introduzir linha divisoria
                                          h4("Spectrogram parameters", align = "left"),
                                          hr(), # Introduzir linha divisoria
                                          # Input 1
                                          numericInput(inputId = "spec_size",
                                                       label = "Size (ms)",
                                                       value = '20'),
                                          # Input 2
                                          numericInput(inputId ="window_length",  
                                                       label = "Moving window (ms)",
                                                       value = '1'),
                                          # Input 3
                                          selectInput(inputId = "time_step_size",
                                                      label = "Overlap",
                                                      choices = c('25%' = '0.75',
                                                                  '50%' = '0.50',
                                                                  '75%' = '0.25'),
                                                      selected = '0.25'),
                                          # Input 4
                                          selectInput(inputId  = "frequency_resolution", 
                                                      label = "Resolution",
                                                      choices = c('Low' = '1',
                                                                  'Medium' = '2',
                                                                  'High' = '3'),
                                                      selected = '1'),
                                          # Input 5
                                          selectInput(inputId = "dynamic_range",
                                                      label = "Threshold",
                                                      choices = c('40 dB' = '40',
                                                                  '50 dB' = '50',
                                                                  '60 dB' = '60',
                                                                  '70 dB' = '70',
                                                                  '80 dB' = '80',
                                                                  '90 dB' = '90',
                                                                  '100 dB' = '100'),
                                                      selected = '90'),
                                          # Input 6
                                          fluidRow(
                                            column(6, align = "center",
                                                   textInput("low",
                                                             "Lower (kHz)",
                                                             value = '10')
                                                                                 ),
                                            column(6, align = "center",
                                                   textInput("high",
                                                             "Higher (kHz)",
                                                             value = '120')
                                            )
                                                                                      )
                                          
                                          
                             ),
                             # Define the main panel ---------------------------------------------------
                             mainPanel(
                               htmlOutput("Attacks")
                             ) #mainPanel
               )#sidebarLayout
      ),
      
      
      
      
      # Panel Fit model ---------------------------------------------------------
      tabPanel("Fit model", fluid = FALSE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(fluidRow(
                   column(7,  plotlyOutput("")),
                   column(5, plotlyOutput(""))   
                 )#fluidRow
                 )#mainPanel
               )#sidebarLayout
      ),#tabPanel plot
      
      
      # Panel re-fit model  -----------------------------------------------------
      tabPanel("Re-fit model",
               sidebarLayout(fluid = FALSE,
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
  
  
  # Server ------------------------------------------------------------------
  server = function(input, output) {
    
  }
) #shinyApp