library(shiny)

library(zoo) # funcao rollmean()
#library(Peaks) # Package para explorar com opcoes para escolha dos pulsos
#library.dynam('Peaks', 'Peaks', lib.loc=NULL) # Patch para um bug do Peaks nesta versao de R, mas parece que tb nao funciona
library(shinyFiles) # Da para escolher ficheiros e pastas mas ainda nao explorei
require(tcltk)
require(neuralnet)
library(rhandsontable)
library(shinythemes)

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
    titlePanel("Labeler"),
    
    sidebarLayout(fluid = FALSE,
                  
                  # Define the sidebar panel
                  sidebarPanel(width = 2,
                               
                               # Button 1
                               # ver solucao aqui: https://stackoverflow.com/questions/42945833/getting-file-path-from-shiny-ui-not-just-directory-using-browse-button-without
                               shinyFilesButton('selected_db', 
                                                'Choose database',
                                                'Choose database file', FALSE, style='width:100%'),
                               br(), # introduzir espacamento
                               br(), # introduzir espacamento
                               
                               # Button 2
                               shinyDirButton('folder',
                                              'Choose folder',
                                              'Choose recordings folder', FALSE, style='width:100%'),
                               
                               br(), # introduzir espacamento
                               br(), # introduzir espacamento
                               hr(), # Introduzir linha divisoria
                               
                               # Box
                               selectInput(inputId = "files", label = NULL, choices = NULL, width="100%"),#, ),
                               
                               # Button 3
                               actionButton('Next', "Next recording", width="100%"),
                               
                               hr(), # Introduzir linha divisoria
                               h5("Butterworth filter (kHz)", align = "center"),
                               
                               

                               
                               fluidRow(
                                 column(6, align = "center",
                                        
                                        textInput("low",
                                                  "Low",
                                                  value = '10')
                                        
                                        
                                        
                                 ),
                                 column(6, align = "center",
                                        
                                        textInput("high",
                                                  "High",
                                                  value = '120')
                                 )
                                 
                               ),
                               
                               
                               # br(),
                               # br(),
                               hr(),
                               
                               
                               actionButton("analisar", "Set labels", width="100%")
                               
                               
                               
                               
                               
                  ),
                  
                  # Create a spot for the barplot
                  
                  mainPanel(
                    tabsetPanel(id = "inTabset",
                      tabPanel("Plot", 
                               value = "panel_plot",
                               plotOutput("spec",
                                                  click="specClick",
                                                  dblclick = "plot1_dblclick", #esta linha e as finais controlam o zoom
                                                  brush = brushOpts(
                                                    id = "plot1_brush",
                                                    resetOnNew = TRUE
                                                  )),
                               
                               
                               fluidRow(
                                 column(6,
                                        textInput("Lb", "Label", value='')
                                        
                                 ),
                                 column(6,
                                        textInput("Obs", "Observations", value='')
                                 )
                               ),
                               
                               fluidRow(column(6,
                                               textOutput("db_path")
                               )
                               ),
                               fluidRow(column(6,
                                               textOutput("folder_path")
                               )
                               ),
                               

                               
                               
                               
                               fluidRow(
                                 verbatimTextOutput("clickInfo") # So para ter no server uma funcao reactive que me grave os pulsos em ficheiro
                               )
                      ),
                      
                      
                      tabPanel(title = "Options", 
                               value = "panel_options", 
                               fluidRow(
                                 column(4,

                                        selectInput("dynamicRange",
                                                    "Thresh",
                                                    choices = c('4' = '40',
                                                                '5' = '50',
                                                                '6' = '60',
                                                                '7' = '70',
                                                                '8' = '80',
                                                                '9' = '90',
                                                                '10' = '100',
                                                                '11' = '110',
                                                                '12' = '120'),
                                                    '70')
                                 ),

                                 column(4,

                                        selectInput("windowLength",
                                                    "Window",
                                                    choices = c('1' = '1',
                                                                '2' = '2',
                                                                '3' = '3',
                                                                '4' = '4',
                                                                '5' = '5'),
                                                    '5')
                                 )
                               ),
                               fluidRow(
                                 column(4,

                                        selectInput("timeStep",
                                                    "Overlap",
                                                    choices = c('1' = '5',
                                                                '2' = '4',
                                                                '3' = '3',
                                                                '4' = '2',
                                                                '5' = '1'),
                                                    '2')


                                 ),
                                 column(4,

                                        selectInput("freqResolution", "Definicao",
                                                    choices = c('1' = '5',
                                                                '2' = '4',
                                                                '3' = '3',
                                                                '4' = '2',
                                                                '5' = '1'),
                                                    '3')
                                 )

                               ),
                               actionButton("save", "Update spectrogram"))
                      
                      
                    )# final tabSetPanel
                    
                  )#Final main panel
                  
    )# final side bar layout
    
    
  )# final fluid pane
)# final shiny ui

