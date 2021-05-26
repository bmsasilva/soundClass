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

#shinyApp(
  
  # UI ----------------------------------------------------------------------
  ui = fluidPage(
    titlePanel("Modeling"),
    tabsetPanel(
      # 1) Panel create train data -------------------------------------------------
      tabPanel("Create train data", 
               sidebarLayout(fluid = FALSE,
                             # 1.1) Sidebar panel ------------------------------------------------
                             sidebarPanel(width = 2,
                                          # Button 1 - Choose folder
                                          shinyDirButton(id = 'folder',
                                                         label = 'Choose folder',
                                                         title = 'Choose recordings folder', 
                                                         style = 'width:100%'),
                                          br(),
                                          # Button 2 - Choose database
                                          shinyFilesButton(id = 'selected_db', 
                                                           label = 'Choose database',
                                                           title = 'Choose database file',
                                                           multiple = FALSE, 
                                                           style='width:100%'),
                                          br(),
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
                             # 1.2) Main panel ---------------------------------------------------
                             mainPanel(
                               br(),
                               fluidRow(
                                 column(6,
                                        textOutput("folder_path")
                                 )
                               ),
                               br(),
                               fluidRow(
                                 column(6,
                                        textOutput("db_path")
                                 )
                               ),
                               hr(),
                               fluidRow(
                                 column(6, align="center", offset = 3,
                                        # Button 3 - Create train data
                                        # Should be disabled while spec_calls is running. See how in:
                                        # https://stackoverflow.com/questions/40621393/disabling-buttons-in-shiny
                                        actionButton("create_specs", "Create training data from labels", style='width:100%'),
                                        tags$style(type='text/css', "#create_specs { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                                 )
                               ),
                               
                               
                               
                               br(),
                               tableOutput("spec")
                             ) #mainPanel
                             
                             
                             
                             
                             
                             
               )#sidebarLayout
      ),
      
      
      
      
      # 2) Panel fit model ---------------------------------------------------------
      tabPanel("Fit model", 
               sidebarLayout(fluid = FALSE,
# 2.1) Sidebar panel ------------------------------------------------------
                 sidebarPanel(width = 2,
                   # Button 1 - Choose RDATA with spectrogram matrices
                   shinyFilesButton(id = 'rdata_path', 
                                    label = 'Choose train data',
                                    title = 'Choose train data file',
                                    multiple = FALSE, 
                                    style='width:100%'),
                   # Button 2 - Choose model
                   shinyFilesButton(id = 'model_path', 
                                    label = 'Choose model',
                                    title = 'Choose model',
                                    multiple = FALSE, 
                                    style='width:100%'),
                   hr(), # Introduzir linha divisoria
                   h4("Model parameters", align = "left"),
                   hr(), # Introduzir linha divisoria
                   # Input 1
                   numericInput(inputId ="train_per",  
                                label = "Train %",
                                value = '0.7'),
                   # Input 2
                   numericInput(inputId ="batch",  
                                label = "Batch size",
                                value = '2'),
                   # Input 3
                   numericInput(inputId ="lr",  
                                label = "Learning rate",
                                value = '0.01'),
                   # Input 4
                   numericInput(inputId ="stop",  
                                label = "Early stop",
                                value = '2'),
                   # Input 5
                   numericInput(inputId ="epochs",  
                                label = "Max epochs",
                                value = '2')
                   
                 ), #sidebarPanel

# 2.2) Main panel ---------------------------------------------------------
                 mainPanel(
                   br(),
                   fluidRow(
                     column(6,
                            textOutput("rdata_path")
                     )
                   ),
                  br(),
                   fluidRow(
                     column(6,
                            textOutput("model_path")
                     )
                   ),
                   hr(),
                   fluidRow(
                      column(6, align="center", offset = 3,
                            # Action button - Fit model (nao esta a funcionar este butao)
                            actionButton("fit_model", "Fit model", style='width:100%'),
                             tags$style(type='text/css', "#fit_model { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                      )
                   )
                   
                   
                   
                   
                   
                   
                   
                 )#mainPanel
               )#sidebarLayout
      ),#tabPanel plot
      
      # 3) Panel re-fit model ---------------------------------------------------------
      tabPanel("Re-fit model", fluid = FALSE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(
                   
                   actionButton("teste", "Fit teste")
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                 )#mainPanel
               )#sidebarLayout
      ),#tabPanel plot
      
      
      # 4) Panel run model  -----------------------------------------------------
      tabPanel("Run model",
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
  ) # fluidPage
  
  
  # Server ------------------------------------------------------------------
  server = function(input, output) {
    
    # File and folder chooser paths -------------------------------
    system <- Sys.info()[['sysname']]
    if (system == "Windows") roots <- c(home = 'C://')
    if (system == "Linux") roots <- c(Computer = "/") #getVolumes()#funciona no HP mas nao no myotis # c(home = getVolumes()) #funciona no pc de casa mas nao no Portatil   
    
    
    
    # 1) Panel create train data -------------------------------------------------
    # Button 1 - Choose folder
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
    files_path <- reactive({ # files_path para usar noutras funcoes
      folder_selected <- parseDirPath(roots, input$folder)
      files_path <- as.character(paste0(folder_selected, "//")) #dupla barra para funcionar em linux e windows
      files_path
    })
    
    # Button 2 - Choose database
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
    
    # Button 3 - Create train data
    spectro_calls <- eventReactive(input$create_specs, {
      sp_data <- spectro_calls_shiny(files_path(), db_path(),
                                     spec_size = as.numeric(input$spec_size),
                                     window_length = as.numeric(input$window_length),
                                     frequency_resolution = as.numeric(input$frequency_resolution),
                                     time_step_size = as.numeric(input$time_step_size) * as.numeric(input$window_length),
                                     dynamic_range = as.numeric(input$dynamic_range),
                                     freq_range = c(as.numeric(input$low), as.numeric(input$high))
      )
      save(sp_data, file = "sp_data.RDATA")
      return(sp_data)
    })
    
    output$spec <- renderTable({
      table(spectro_calls()[[2]])
    })
    

# 2) Panel fit model ------------------------------------------------------
    # Button 1 - Choose train rdata
    observe({ 
      shinyFileChoose(input, 'rdata_path', roots = roots)
      if(!is.null(input$rdata_path)){
        file_selected <- parseFilePaths(roots, input$rdata_path)
        rdata_path <- paste("Train data file =", as.character(file_selected$datapath))
        output$rdata_path <- renderText(as.character(rdata_path)) #output para mostrar o path da bd escolhida
      }
    })
    
    rdata_list <- reactive({ # reactive com a lista importada no rdata
      if(length(input$rdata_path) > 1){ 
      file_selected <- parseFilePaths(roots, input$rdata_path)
      env <- LoadToEnvironment(as.character(file_selected$datapath))
      rdata_list <- env[[names(env)[1]]]
      return(rdata_list)
      }
    })
    

    # Button 2 - Choose model
    observe({ 
      shinyFileChoose(input, 'model_path', roots = roots)
      if(!is.null(input$model_path)){
        file_selected <- parseFilePaths(roots, input$model_path)
        model_path <- paste("Model file =", as.character(file_selected$datapath))
        output$model_path <- renderText(as.character(model_path)) #output para mostrar o path da bd escolhida
      }
    })
    
    
    model_path <- reactive({ # model_path para usar noutras funcoes
      file_selected <- parseFilePaths(roots, input$selected_db)
      model_path <- as.character(file_selected$datapath)
      model_path
    })
    
    ### O action button nao funciona se nao tiver 
    ### um output para o eventReactive. A alternativa é usar observeEvent

    # output$model_fit <- renderText({
    #   model_fit()
    # })

    # Action Button - Fit model (se nao usar model_fit() tem de se usar
    # observeEvent em vez de eventReative)
    

    
    #model_fit <- eventReactive(input$fit_model, {
    observeEvent(input$fit_model, {
      print("action button fit model working")
      ######funcao model fit########
#
#       # set seed
#       seed = 1002
#
#       # Parametros para o treino
#       total <- dim(rdata_list()[[1]])[1]
#       img_rows = rdata_list()[[3]]$img_rows
#       img_cols = rdata_list()[[3]]$img_cols
#       input_shape=c(img_rows, img_cols, 1)
#       num_classes <- length(unique(rdata_list()[[2]]))
#
#
#       rdata_list()[[2]] <- as.factor(rdata_list()[[2]])#converter para factor para facilitar os numeros e aos nomes de classe repectivos
#       labels_code <- as.integer(rdata_list()[[2]]) - 1
#       labels_name <- as.character(rdata_list()[[2]])
#       labels_df <- data.frame(name = levels(rdata_list()[[2]]), code = (1:length(levels(rdata_list()[[2]])))-1)
#
#
#
#       #  Randomizar a ordem dos casos
#       set.seed(seed)
#       data_x <- rdata_list()[[1]]
#       data_y <- labels_code
#       randomize <- sample(length(data_y))
#       data_x <- data_x[randomize,]
#       data_y <- data_y[randomize]
#
#       # preparar data para tensorflow
#       data_y <- to_categorical(as.numeric(data_y), num_classes = num_classes)
#       data_x <- array(data_x, dim = c(total, img_rows, img_cols, 1))
#
#       # load net structure
#       source(model_path())
#       summary(model)
#
#       # fit
#       model %>%
#         compile(
#           optimizer = optimizer_sgd(lr=0.01, momentum=0.9, nesterov=T), # mesmo leraning rate do base model
#           loss = 'categorical_crossentropy',
#           metrics = c('accuracy')
#         )
#
#       history<-model %>% fit(data_x, data_y,
#                              batch_size = 64,
#                              epochs = 20,
#                              callbacks = list(callback_early_stopping(patience = 1, monitor = 'val_accuracy'),
#                                               callback_model_checkpoint("./epoch{epoch:02d}-val_accuracy-{val_accuracy:.4f}.hdf5",
#                                                                         monitor = "val_accuracy")),
#                              shuffle = TRUE,
#                              validation_split = 0.3,
#                              verbose = 1)
#
#       save(history, file="./history_model.RDATA")
#       
#       # ###### so para confirmar que as imagens estão a entrar correctamebnte
#       # gen_images <- image_data_generator()
#       # 
#       # #Fit image data generator internal statistics to some sample data
#       # gen_images %>% fit_image_data_generator(data_x)
#       # 
#       # #Generates batches of augmented/normalized data from image data and #labels to visually see the input images to the Model
#       # model %>% fit_generator( #fit_generator em versoes antigas
#       #   flow_images_from_data(data_x, data_y,
#       #                         gen_images,
#       #                         batch_size=1,
#       #                         save_to_dir=".//extra_files//"),
#       #   steps_per_epoch=1,
#       #   epochs = 1 )
#       #####fim fucao model fit######
#       
     })
    
    #so para testar a importacao do rdata_list
    output$selected_rdata <- renderText({
     as.character(rdata_list()[[3]][1])
    })
    
  }#server
#) #shinyApp

# Run the application 
shinyApp(ui = ui, server = server)
