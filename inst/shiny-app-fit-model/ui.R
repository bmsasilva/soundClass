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

# fluidPage ---------------------------------------------------------------

  
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
    

# sidebar_layout ---------------------------------------------------------
    sidebarLayout(fluid = FALSE,
# sidebarPanel ------------------------------------------------------------

                  sidebarPanel(width = 2,
                               
                               # Button 2
                               shinyDirButton('folder',
                                              'Choose folder',
                                              'Choose recordings folder', FALSE, style='width:100%'),
                               
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
                                                'Choose model file', FALSE, style='width:100%'),
                               
                               
                               

   ),

# end sidebarPanel ----------------------------------------------------------



# main_panel --------------------------------------------------------------
mainPanel(

# tab_set_panel -----------------------------------------------------------

  
  tabsetPanel(id = "inTabset",

# tab_panel_training_data -----------------------------------             
              tabPanel("Training data", 
                       value = "panel_plot",
                       
                       plotOutput("spec",
                                  height = "auto", #controlar a altura do plot. no sevrer define o tamanho
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
                       fluidRow(column(6,
                                       textOutput("model_path")
                       )
                       )
                       
              ),
# end_tab_panel_training_data ---------------------------------------------

# tab_panel_fit_model -----------------------------------------------------

  
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
                                            "Overlap",
                                            choices = c('60%' = '0.4',
                                                        '70%' = '0.3',
                                                        '80%' = '0.2',
                                                        '90%' = '0.1'),
                                            '0.2')
                                
                                
                         ),
                         column(4,
                                
                                selectInput("freqResolution", "Resolution",
                                            choices = c('low resolution' = '1',
                                                        'medium resolution' = '4',
                                                        'high resolution' = '8'),
                                            '4')
                         )
                         
                       ),
                       actionButton("save", "Save and update spectrogram")
                       )
# end_tab_panel_fit_model ------------------------------------------

  )

# end_tab_set_panel -------------------------------------------------------


)
# end_main_panel ----------------------------------------------------------









)

# end_sidebar_layout ----------------------------------------------------------


)

# end_fluidPage ----------------------------------------------------------------


)
