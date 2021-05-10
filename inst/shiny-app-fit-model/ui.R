library(shiny)

library(zoo) # funcao rollmean()
#library(Peaks) # Package para explorar com opcoes para escolha dos pulsos
#library.dynam('Peaks', 'Peaks', lib.loc=NULL) # Patch para um bug do Peaks nesta versao de R, mas parece que tb nao funciona
library(shinyFiles) # Da para escolher ficheiros e pastas mas ainda nao explorei
require(tcltk)
require(neuralnet)
library(rhandsontable)
library(shinythemes)
library(shinyjs)

shinyUI(
  fluidPage(
    # theme = shinytheme("slate"),
    #  theme = "bootstrap.css",
    
    tags$head( #mudar a posicao da notification. posso tb usar width e height
      tags$style(
        HTML(
          ".shiny-notification {
             width: 300px;
             position:fixed;
             top: calc(1%);
             left: calc(50%);
             }"
        )
      )
    ),
    
    # Application title
    titlePanel("Fit model"),
    
    sidebarLayout(fluid = FALSE,
                  
                  # Define the sidebar panel
                  sidebarPanel(width = 2,
                               
                               # Button 2
                               shinyDirButton('folder',
                                              'Choose folder',
                                              'Choose recordings folder', style='width:100%'),
                               
                               br(), # introduzir espacamento
                               br(), # introduzir espacamento
                               
                               
                               
                               # Button 1
                               # ver solucao aqui: https://stackoverflow.com/questions/42945833/getting-file-path-from-shiny-ui-not-just-directory-using-browse-button-without
                               shinyFilesButton('selected_db', 
                                                'Choose database',
                                                'Choose database file', FALSE, style='width:100%'),
                               
                               br(), # introduzir espacamento
                               br(), # introduzir espacamento
                               shinyFilesButton('selected_model', 
                                                'Choose model',
                                                'Choose model file', FALSE, style='width:100%')
                               
                               
                               
                               
                               
                               
                  ),
                  
                  # Create a spot for the barplot
                  
                  mainPanel(
                    tabsetPanel(id = "inTabset",
                                tabPanel("Create train data", 
                                         value = "panel_plot",
                                         br(),
#####
                                         

                                         
                                         fluidRow(
                                           column(4,
                                                  numericInput("spec_size",
                                                              "Spectrogram size (ms)",
                                                              '20')
                                           ),
                                           column(4,
                                                  numericInput("window_length",  
                                                               "Moving window size (ms)",
                                                               '1')
                                           ),
                                           column(4,
                                                  
                                                  selectInput("time_step_size",
                                                              "Moving window overlap",
                                                              choices = c('25%' = '0.75',
                                                                          '50%' = '0.50',
                                                                          '75%' = '0.25'
                                                                          ),
                                                              '0.25')
                                           )
                                         ),
                                         
                                         fluidRow(
                                           column(4,
                                                  selectInput("frequency_resolution", 
                                                              "Frequency resolution",
                                                              choices = c('low resolution' = '1',
                                                                          'normal resolution' = '2',
                                                                          'high resolution' = '3'),
                                                              '1')
                                           ),
                                           column(4,
                                                  selectInput("dynamic_range",
                                                              "Threshold",
                                                              choices = c('40 dB' = '40',
                                                                          '50 dB' = '50',
                                                                          '60 dB' = '60',
                                                                          '70 dB' = '70',
                                                                          '80 dB' = '80',
                                                                          '90 dB' = '90',
                                                                          '100 dB' = '100'),
                                                              '90')
                                           ),
                                         
                                          
                                             column(2,
                                                    
                                                    textInput("low",
                                                              "Min frequency (kHz)",
                                                              value = '10')
                                                    
                                                    
                                                    
                                             ),
                                             column(2,
                                                    
                                                    textInput("high",
                                                              "Max frequency (kHz)",
                                                              value = '80')
                                             )
                                             
                                           
                                         
                                         ),
                                          fluidRow(
                                            column(6, align="center", offset = 3,
                                                  
                                                  
                                                  
                                                  # Button "create specs"
                                                  # Should be disabled while spec_calls is running. See how in:
                                                  # https://stackoverflow.com/questions/40621393/disabling-buttons-in-shiny
                                                   actionButton("create_specs", "Create spectrograms from labels", style='width:100%'),
                                                   tags$style(type='text/css', "#create_specs { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                                           )
                                         ),
                                         
                                       
                                         tableOutput("spec"),
                                       
                                         
                                         
                                         fluidRow(column(4,
                                                         textOutput("db_path")
                                         )
                                         ),
                                         fluidRow(column(4,
                                                         textOutput("folder_path")
                                         )
                                         ),
                                         fluidRow(column(4,
                                                         textOutput("model_path")
                                         )
                                         )
                                ),
                                
                                
                                tabPanel(title = "Fit model", 
                                         value = "panel_options", 
                                         fluidRow(
                                           column(4,
                                                  
                                                  selectInput("dynamicRange",
                                                              "Threshold",
                                                              choices = c('40 dB' = '40',
                                                                          '50 dB' = '50',
                                                                          '60 dB' = '60',
                                                                          '70 dB' = '70',
                                                                          '80 dB' = '80',
                                                                          '90 dB' = '90',
                                                                          '100 dB' = '100'),
                                                              '70')
                                           ),
                                           
                                           column(4,
                                                  
                                                  selectInput("windowLength",
                                                              "Window length",
                                                              choices = c('1 ms' = '1',
                                                                          '2 ms' = '2',
                                                                          '3 ms' = '3',
                                                                          '4 ms' = '4',
                                                                          '5 ms' = '5'),
                                                              '5')
                                           )
                                         ),
                                         fluidRow(
                                           column(4,
                                                  
                                                  selectInput("timeStep",
                                                              "Moving window overlap",
                                                              choices = c('25%' = '0.75',
                                                                          '50%' = '0.50',
                                                                          '75%' = '0.25'),
                                                              '0.25')
                                                              ),
                                                              
                                                                           column(4,
                                                  
                                                  selectInput("freqResolution", "Resolution",
                                                              choices = c('low resolution' = '1',
                                                                          'normal resolution' = '2',
                                                                          'high resolution' = '3'),
                                                              '1')
                                           )
                                           
                                         ),
                                         actionButton("save", "Save and update spectrogram"))
                                
                                
                    )# final tabSetPanel
                    
                  )#Final main panel
                  
    )# final side bar layout
    
    
  )# final fluid pane
)# final shiny ui

