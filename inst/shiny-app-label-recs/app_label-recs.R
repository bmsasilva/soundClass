## clean environment including hidden objects
#rm(list = ls(all.names = TRUE))
#rm(list = ls())  deixa packages e hidden objects

library(shiny)
library(plotly)
library(shinyBS)
library(zoo) # funcao rollmean()
#library(Peaks) # Package para explorar com opcoes para escolha dos pulsos
#library.dynam('Peaks', 'Peaks', lib.loc=NULL) # Patch para um bug do Peaks nesta versao de R, mas parece que tb nao funciona
library(shinyFiles) # Da para escolher ficheiros e pastas mas ainda nao explorei
require(tcltk)
require(neuralnet)
library(rhandsontable)
library(shinythemes)
library(shinyjs)
library(DBI)
library(dbplyr)
library(dplyr)
library(RSQLite)
library(tuneR)
library(keras)

##### Function to read RDATA #####
# This function, borrowed from http://www.r-bloggers.com/safe-loading-of-rdata-files/, 
# load the Rdata into a new environment to avoid side effects
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

##### Load all #####
system <- Sys.info()[['sysname']]
if (system == "Windows") files <- list.files("C://Users//silva//Projects//R_packages//soundClass//R", pattern = ".R", full.names = TRUE)
if (system == "Linux") files <- list.files("/mnt/5F9DC8AD3B9B9A40/Bruno/r_packages/soundClass/R/", pattern = ".R", full.names = TRUE)

for(file in files) source(file)

#eliminate previous fit_log.csv file
if (file.exists("./fit_log.csv")) 
  file.remove("./fit_log.csv") #limpar a tabela de resultados se correr fit moedl 2 vezes


#shinyApp(

# UI ----------------------------------------------------------------------
ui =   fluidPage(
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
                             
                             # Button 2
                             shinyDirButton('folder',
                                            'Choose folder',
                                            'Choose recordings folder', style='width:100%'),
                             
                             br(), # introduzir espacamento
                             br(), # introduzir espacamento
                             
                             # Button "create db" and respective modal box
                             actionButton("create_db", "Create database", style='width:100%'),
                             
                             shinyBS::bsModal(id = "modal", Title = "Database name", trigger = "create_db", size = "small",
                                              HTML("What is the database name?"),
                                              textInput("name", "", ""),
                                              actionButton("conf", "Confirm")),
                             
                             
                             br(), # introduzir espacamento
                             br(), # introduzir espacamento
                             
                             # Button 1
                             # ver solucao aqui: https://stackoverflow.com/questions/42945833/getting-file-path-from-shiny-ui-not-just-directory-using-browse-button-without
                             shinyFilesButton('selected_db', 
                                              'Choose database',
                                              'Choose database file', FALSE, style='width:100%'),
                             
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
                                       )
                              ),
                              
                              
                              tabPanel(title = "Spectrogram options", 
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
                                       actionButton("save", "Save and update spectrogram"))
                              
                              
                  )# final tabSetPanel
                  
                )#Final main panel
                
  )# final side bar layout
  
  
)# final fluid pane

# Server ------------------------------------------------------------------
server = function(input, output, session) {
  
  
  
  # File and folder chooser paths -------------------------------
  system <- Sys.info()[['sysname']]
  if (system == "Windows") roots <- c(home = 'C://')
  if (system == "Linux") roots <- getVolumes() # c(home = getVolumes()) #funciona no pc de casa mas nao no Portatil   
  
  # Ask user for db name -------------------------------
  observeEvent(input$conf, {
    shinyBS::toggleModal(session, "modal", toggle = "open")
    create_db(".//", input$name)
  })
  
  # Button  1 -----------------------------------------
  observe({ 
    shinyFileChoose(input, 'selected_db', roots = roots)
    if(!is.null(input$selected_db)){
      file_selected <- parseFilePaths(roots, input$selected_db)
      db_path <- paste("Database file =", as.character(file_selected$datapath))
      output$db_path <- renderText(as.character(db_path)) #output para mostrar o path da bd escolhida
    }
  })
  
  db_path <- reactive({ # db_path para usar noutras funcoes
    file_selected <- parseFilePaths(roots, input$selected_db)
    db_path <- as.character(file_selected$datapath)
    db_path
  })
  
  # Button  2 -----------------------------------------
  observe({
    shinyDirChoose(input, 'folder', roots = roots)
    if(!is.null(input$folder)){
      folder_selected <- parseDirPath(roots, input$folder)
      folder_path <- paste("Recordings folder =", as.character(folder_selected))
      output$folder_path <- renderText(as.character(folder_path)) #output para mostrar o path da pasta escolhida
    }
  })
  
  observeEvent(input$folder, {
    if(length(parseDirPath(roots, input$folder))>0){ 
      setwd(parseDirPath(roots, input$folder))
    }
  })
  
  file_names <- reactivePoll(1000, session,
                             checkFunc = function() 
                             {list.files(".", recursive = FALSE, pattern="wav|WAV")},
                             valueFunc = function() 
                             {list.files(".", recursive = FALSE, pattern="wav|WAV")}
  )
  
  observeEvent(file_names(), {
    if(length(parseDirPath(roots, input$folder)) > 0){ 
      updateSelectInput(session, "files", choices = file_names())
    }
  })
  
  
  
  # Button "save" -------------------------------------
  observeEvent(input$save, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel_plot")
    
  })
  
  
  # Importar gravacao ----------------------------------------------------
  sound <- reactive({
    # https://shiny.rstudio.com/articles/validation.html
    # Mensagem para substituir o erro enquanto nao abre o primeiro ficheiro.
    validate(
      need(input$files != "",
           "Analysis steps:
1) Select folder with recordings
2) If needed, create new database to store recording labels
3) Select pre-existing database to store recording labels
4) Select events by clicking in the spectrogram before and after the event of interest (bat call, bird song, etc) 
   Important: Be carefull not to drag mouse while clicking as it will erase previously selected positions
5) Press 'Set labels' button to add labels to database
6) Repeat step 4 and 5 if more than one indivudual is in the recording
7) Press 'Next' button to advance to next recording or pick another recording from the dropdown list

Spectrogram visualization:
- Zoom spectrogram by click and drag to select area and then double click on it
- Unzoom by double clicking on spectrogram without area selected
- Adjust spectrogram settings with:
    THRESHOLD - minimum energy values displayed, higher values best suited for low quality recordings
    WINDOW - window size in ms, smaller windows best suited for short calls
    OVERLAP - overlap between consecutive windows, higher values give best visualization but lower performance")
    )
    
    sound <- import_audio(path = input$files, butt = TRUE,
                          low = as.numeric(input$low),
                          high = as.numeric(input$high))
    
    sound
  })
  # Controles do zoom ------------------------------------------------------------
  
  # Variavel para controlar o zoom
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Observador para o evento duplo click. Preparear isto para os eliminar maxpos.x qd se mete zoom
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      file.remove("temp_file.csv")
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
      file.remove("temp_file.csv")
    }
  })
  
  # # eliminar ficheiro de pulsos qd se cria uma caixa para zoom
  # observeEvent(input$plot1_brush, {
  #        file.remove("temp_file.csv")
  # })
  
  
  
  # Eliminar os valores do zoom e de maxpos$x quando se muda de ficheiro
  observeEvent(input$files, {
    ranges$x <- NULL
    ranges$y <- NULL
    maxpos$x <- NULL
    file.remove("temp_file.csv")
  })
  
  # Lidar com os clicks do rato ----------------------------------------------
  
  # Guardar as posicoes do rato quando clickado, em ficheiro externo
  observeEvent(input$specClick$x, {
    
    # print(getwd())
    
    
    
    write.table(input$specClick$x,
                file = "temp_file.csv",
                append = TRUE,
                col.names = FALSE,
                row.names= FALSE)
  })
  
  
  # Plotar espectrograma ----------------------------------------------------------
  
  # Plotar o espectrograma
  output$spec <- renderPlot({
    
    Spectrogram(as.numeric(sound()$sound_samples),                   
                SamplingFrequency=sound()$fs,  
                WindowLength = as.numeric(input$windowLength),        
                FrequencyResolution = as.numeric(input$freqResolution), 
                TimeStepSize = as.numeric(input$timeStep) * as.numeric(input$windowLength),     
                nTimeSteps = NULL,       
                Preemphasis = TRUE,      
                DynamicRange = as.numeric(input$dynamicRange),       
                Omit0Frequency = FALSE,  
                WindowType = "hanning",   
                WindowParameter = NULL,  
                plot = TRUE,             
                PlotFast = TRUE,         
                add = FALSE,             
                col = batsound,              
                xlim = ranges$x,             
                ylim = ranges$y,             
                main = sound()$file_name,               
                xlab = "Time (ms)",      
                ylab = "Frequency (kHz)")
    
    # Obriga a recriar o spectrograma antes de adicionar as linhas
    abline(v= ms2samples(maxpos$x, tx = sound()$tx, fs = sound()$fs,
                         inv=T))
    
   }, height = function() {
     0.6 * (session$clientData$output_spec_width) #controlar a altura do plot (0.6 * o comprimento)
  })
  
  # # Cria a tabela reactive
  #   DF <- reactiveFileReader(1000, session=session, filePath = "output_semiauto.csv", readFunc = read.csv_bs)
  #   
  #   # Mostra a tabela
  #   output$hot <- renderRHandsontable({
  #     if (!is.null(DF()))
  #       rhandsontable(DF(),  stretchH = "all") %>%
  #       hot_cols(renderer = "
  #            function (instance, td, row, col, prop, value, cellProperties) {
  #              Handsontable.renderers.TextRenderer.apply(this, arguments);
  # td.style.background = 'lightgray';
  # td.style.color = 'black';
  # 
  #            }")
  #   })
  
  # Botoes (server side actions) ----------------------------------------------------------
  
  ## Botao proxima gravacao
  observeEvent(input$Next, {
    
    # Obter a posicao do ficheiro carregado da lista de ficheiros
    fileInUse <- which(file_names() %in% c(input$files))
    
    # so corre se nao for a ultima gravacao
    if (fileInUse < length(file_names())){
      # Update do menu "files" na UI com o novo ficheiro escolhido
      updateSelectInput(session,"files", choices = file_names(), selected = file_names()[fileInUse + 1])
    }
  })
  
  
  
  
  
  ## Botao ANALISAR
  maxpos <- reactiveValues(x=NULL) # reactive para depois dar para plotar no spec
  
  observeEvent(input$analisar, {
    
    # Validar o numero de clicks para escolha dos pulsos
    if(file.exists('temp_file.csv')){
      labs <- read.csv(file = "temp_file.csv", header = FALSE)[,1]
      labs <- ms2samples(labs, fs = isolate(sound()$fs), tx = isolate(sound()$tx))
      np <- length(labs)/2
      file.remove("temp_file.csv")
      # if(!is_even(length(labs))) {
      #   showNotification("Please choose calls again", type = "error", closeButton = T, duration = 3)
      #   remove(labs)
      #   file.remove("temp_file.csv")
       # } else{
        maxpos$x<- labs
        
        ## Obtém o local de fmaxe de cada pulso seleccionado anteriormente
        # j <- 1
        #  print( <- NULL #para eliminar outros pulsos ja escolhidos nesta rec
        # for (i in 1:np) {
        #   maxpos$x[i]<-(which.max(abs(isolate(sound()$sound_samples[labs[j]:labs[j+1]]))) + labs[j]) 
        #   j <- j+2
        # }
        

        output <- data.frame("recording" = isolate(sound()$file_name),
                             "label_position" = maxpos$x,
                             "label_class" = isolate(input$Lb),
                             "observations" = isolate(input$Obs))
        
         add_record(path = isolate(db_path()), df = output)
        
        
        
      # } # final do else
    }
  })


  
}#server


# Run the application 
shinyApp(ui = ui, server = server)