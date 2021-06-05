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
# if (file.exists("./fit_log.csv")) 
#   file.remove("./fit_log.csv") #limpar a tabela de resultados se correr fit moedl 2 vezes


#shinyApp(
  
  # UI ----------------------------------------------------------------------
  ui = fluidPage(
    shinyjs::useShinyjs(),  # Include shinyjs
    titlePanel("Deploy model"),
    tabsetPanel(
      # 1) Panel Deploy model -------------------------------------------------
      tabPanel("Run model", 
               sidebarLayout(fluid = FALSE,
                             # 1.1) Sidebar panel ------------------------------------------------
                             sidebarPanel(width = 2,
                                          # Button 1 - Choose recordings folder
                                          shinyDirButton(id = 'selected_folder',
                                                         label = 'Choose folder',
                                                         title = 'Choose folder', 
                                                         style = 'width:100%'),
  
                                          br(),
                                          # Button 2 - Choose model
                                          shinyFilesButton(id = 'selected_model', 
                                                           label = 'Choose model',
                                                           title = 'Choose model file',
                                                           multiple = FALSE, 
                                                           style='width:100%'),
                                          br(),
                                          # Button 3 - Choose metadata
                                          shinyFilesButton(id = 'selected_metadata', 
                                                           label = 'Choose metadata',
                                                           title = 'Choose database file',
                                                           multiple = FALSE, 
                                                           style='width:100%'),
                                          br(),
                                          br(),
                                          # Input 1
                                          textInput(inputId = "out_file",
                                                    label = "Name of output file",
                                                    value = 'id_results'), 
                                          br(),
                                          # Input 2
                                          radioButtons("rem_noise", "Non-relevant class?",
                                                       c("Yes" = TRUE,
                                                         "No" = FALSE)),
                                          br(),
                                          # Input 3
                                          radioButtons("lab_plots", "Export labeled plots",
                                                       c("Yes" = TRUE,
                                                         "No" = FALSE))


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
                                        textOutput("model_path")
                                 )
                               ),
                               br(),
                               fluidRow(
                                 column(6,
                                        textOutput("metadata_path")
                                 )
                               ),
                               hr(),
                               fluidRow(
                                 column(6, align="center", offset = 3,
                                        # Button 4 - Run analysis
                                        # Should be disabled while analysis is running. See how in:
                                        # https://stackoverflow.com/questions/40621393/disabling-buttons-in-shiny
                                        actionButton("id_recs", "Classify recordings", style='width:100%'),
                                        tags$style(type='text/css', "#id_recs { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                                 )
                               ),
                               br(),
                               tableOutput("spec")
                             ) #mainPanel
                             
                             
                             
                             
                             
                             
               )#sidebarLayout
      ),
      
      
      
      
      # 2) Panel review results ---------------------------------------------------------
      tabPanel("Review results", 
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
                            #textOutput("rdata_path")
                     )
                   ),
                  br(),
                   fluidRow(
                     column(6,
                          #  textOutput("model_path")
                     )
                   ),
                   hr(),
                   fluidRow(
                      column(6, align="center", offset = 3,
                            # Action button - Fit model (nao esta a funcionar este butao)
                            actionButton("fit_model", "Fit model", style='width:100%'),
                             tags$style(type='text/css', "#fit_model { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                      )
                   ), #tirar a virgula qd apagar a linha de baixo
                  br(),
                  br(),
                  tableOutput('fit_log'),
                  br(),
                  br(),
                  tableOutput('end_fit')
                   
                   
                   
                   
                   
                 )#mainPanel
               )#sidebarLayout
      )#tabPanel plot
      
      # 3) Panel re-fit model ---------------------------------------------------------
      # tabPanel("Re-fit model", fluid = FALSE,
      #          sidebarLayout(
      #            sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 # mainPanel(
                 #   
                 #   actionButton("teste", "Fit teste")
                 #   
                 #   
                 #   
                 #   
                 #   
                 #   
                 #   
                 #   
                 #   
                 #   
                 # )#mainPanel
      #          )#sidebarLayout
      # ),#tabPanel plot
      
      
      # 4) Panel run model  -----------------------------------------------------
      # tabPanel("Run model",
               # sidebarLayout(fluid = FALSE,
               #               sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
               #               mainPanel(fluidRow(
               #                 column(7,  plotlyOutput("")),
               #                 column(5, plotlyOutput(""))
               #               )#fluidRow
               #               )#mainPanel
               # )#sidebarLayout
      # )#tabPanel plot
    ) # tabsetPanel
  ) # fluidPage
  
  
  # Server ------------------------------------------------------------------
  server = function(input, output, session) {
    
    # File and folder chooser paths -------------------------------
    system <- Sys.info()[['sysname']]
    if (system == "Windows") roots <- c(home = 'C://')
    if (system == "Linux") roots <- c(Computer = "/") #getVolumes()#funciona no HP mas nao no myotis # c(home = getVolumes()) #funciona no pc de casa mas nao no Portatil   
    
    
    
    # 1) Panel deploy model -------------------------------------------------
    # Button 1 - Choose folder
    observe({
      shinyDirChoose(input, 'selected_folder', roots = roots)
      if(!is.null(input$selected_folder)){
        folder_selected <- parseDirPath(roots, input$selected_folder)
        folder_path <- paste("Recordings folder =", as.character(folder_selected))
        output$folder_path <- renderText(as.character(folder_path)) #output para mostrar o path da pasta escolhida
      }
    })
    observeEvent(input$selected_folder, {
      if(length(parseDirPath(roots, input$selected_folder))>0){
        setwd(parseDirPath(roots, input$selected_folder))
      }
    })
    files_path <- reactive({ # files_path para usar noutras funcoes
      folder_selected <- parseDirPath(roots, input$selected_folder)
      files_path <- as.character(paste0(folder_selected, "//")) #dupla barra para funcionar em linux e windows
      files_path
    })
    
    # Button 2 - Choose model
    observe({
      shinyFileChoose(input, 'selected_model', roots = roots)
      if(!is.null(input$selected_model)){
        file_selected <- parseFilePaths(roots, input$selected_model)
        model_path <- paste("Model =", as.character(file_selected$datapath))
        output$model_path <- renderText(as.character(model_path)) #output para mostrar o path da bd escolhida
      }
    })
    model_path <- reactive({ # db_path para usar noutras funcoes
      file_selected <- parseFilePaths(roots, input$selected_model)
      model_path <- as.character(file_selected$datapath)
      model_path
    })
    

    # Button 3 - Choose metadata
    observe({ 
      shinyFileChoose(input, 'selected_metadata', roots = roots)
      if(!is.null(input$selected_metadata)){
        file_selected <- parseFilePaths(roots, input$selected_metadata)
        metadata_path <- paste("Metadata file =", as.character(file_selected$datapath))
        output$metadata_path <- renderText(as.character(metadata_path)) #output para mostrar o path da bd escolhida
      }
    })
    metadata <- reactive({ # reactive com a lista importada no rdata
      if(length(input$selected_metadata) > 1){ 
        file_selected <- parseFilePaths(roots, input$selected_metadata)
        env <- LoadToEnvironment(as.character(file_selected$datapath))
        metadata <- env[[names(env)[1]]]
        rm(env)
        return(metadata)
      }
    })
    
    # Button 4 - Run analysis
    observeEvent(input$id_recs, {

      # Criar pasta de output se nao existir ainda
      if(!dir.exists(paste0(files_path(), "/", "output/"))) dir.create(paste0(files_path(), "/", "output/"))

      # Criar csv com nomes nas colunas se nao existir ainda...em desenvolvimento
      if(!file.exists(paste0(files_path(), "/", "output/",paste0(input$out_file,".csv")))) 
        file.create(paste0(files_path(), "/", "output/",paste0(input$out_file,".csv")))
      
       #####    
      # Create a Progress object
      total <- length( list.files(files_path(), recursive = F, pattern="wav|WAV"))
      progress <- shiny::Progress$new(max=total)
      progress$set(message = "Processing recordings", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      # Create a callback function to update progress.
      # Each time this is called:
      # - If `value` is NULL, it will move the progress bar 1/max of the remaining
      #   distance. If non-NULL, it will set the progress to that value.
      # - It also accepts optional detail text.
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue() + 1
        }
        progress$set(value = value, detail = detail)
      }
      #####      
      
      
      
      
      auto_id_shiny(model_path(),updateProgress,
                    metadata(),
                    files_path(),
                    csv_file = paste0(input$out_file,".csv"),
                    out_dir = paste0(files_path(), "/", "output/"),
                    save_spec = F, 
                    save_png = as.logical(input$lab_plots),
 #                   class_labels = as.character(metadata()$classes$name),
                    win_size = metadata()$parameters$spec_size * 2,  #minimum distnace between calls and chunck size
                    remove_noise = as.logical(input$rem_noise),          # if model has non-relevant class, eliminate from output
                    plot2console = FALSE,
                    recursive = FALSE)
      
    })
    

     # output$spec <- renderTable({ # so funciona depois da auto_id parar de funcionar
     #   read.csv(paste0(files_path(), "/", "output/", input$out_file,".csv"))
     # })
    

# 2) Panel fit model ------------------------------------------------------
    # Button 1 - Choose train rdata
    # observe({ 
    #   shinyFileChoose(input, 'rdata_path', roots = roots)
    #   if(!is.null(input$rdata_path)){
    #     file_selected <- parseFilePaths(roots, input$rdata_path)
    #     rdata_path <- paste("Train data file =", as.character(file_selected$datapath))
    #     output$rdata_path <- renderText(as.character(rdata_path)) #output para mostrar o path da bd escolhida
    #   }
    # })
    # 
    # rdata_list <- reactive({ # reactive com a lista importada no rdata
    #   if(length(input$rdata_path) > 1){ 
    #   file_selected <- parseFilePaths(roots, input$rdata_path)
    #   env <- LoadToEnvironment(as.character(file_selected$datapath))
    #   rdata_list <- env[[names(env)[1]]]
    #   rm(env)
    #   return(rdata_list)
    #   }
    # })
    
    # # Button 2 - Choose model
    # observe({ 
    #   shinyFileChoose(input, 'model_path', roots = roots)
    #   if(!is.null(input$model_path)){
    #     file_selected <- parseFilePaths(roots, input$model_path)
    #     model_path <- paste("Model file =", as.character(file_selected$datapath))
    #     output$model_path <- renderText(as.character(model_path)) #output para mostrar o path da bd escolhida
    #   }
    # })
    
    
    # model_path <- reactive({ # model_path para usar noutras funcoes
    #   file_selected <- parseFilePaths(roots, input$model_path)
    #   model_path <- as.character(file_selected$datapath)
    #   model_path
    # })
    
    # text output do treio do mofelo
    # end_fit <- reactiveFileReader(intervalMillis = 1000, NULL, filePath =
    #                                "fit_log.csv", readFunc = read.csv) #duranteofit nao funciona.so daoresultado no final

    # output$end_fit <- renderTable({
    #   end_fit()
    # })
    # 
    ### O action button nao funciona se nao tiver 
    ### um output para o eventReactive. A alternativa é usar observeEvent

    # output$model_fit <- renderText({
    #   model_fit()
    # })

    # Action Button - Fit model (se nao usar model_fit() tem de se usar
    # observeEvent em vez de eventReative)
    
    
    #Apagar a tabela dos resultados no caso de 2 volta de fit 
    observeEvent(input$fit_model, {
    output$end_fit <- renderTable({
      
    })
      })
    
    
    #model_fit <- eventReactive(input$fit_model, {
   
     observeEvent(input$fit_model, {
       #eliminate previous fit_log.csv file
       # if (file.exists("./fit_log.csv")) 
       #   file.remove("./fit_log.csv") #limpar a tabela de resultados se correr fit moedl 2 vezes
       
     
       
      rdata_list <- rdata_list() 
       
      # set seed
      seed <- 1002

      # Parametros para o treino
      total <- dim(rdata_list[[1]])[1]
      img_rows <- rdata_list[[3]]$img_rows
      img_cols <- rdata_list[[3]]$img_cols
      input_shape <- c(img_rows, img_cols, 1)
      num_classes <- length(unique(rdata_list[[2]]))
      

      rdata_list[[2]] <- factor(rdata_list[[2]])#converter para factor para facilitar os numeros e aos nomes de classe repectivos
      labels_code <- as.integer(rdata_list[[2]]) - 1
      labels_name <- as.character(rdata_list[[2]])
      labels_df <- data.frame(name = levels(rdata_list[[2]]), code = (1:length(levels(rdata_list[[2]])))-1)

      #  Randomizar a ordem dos casos
      set.seed(seed)
      data_x <- rdata_list[[1]]
      data_y <- labels_code
      randomize <- sample(length(data_y))
      data_x <- data_x[randomize,]
      data_y <- data_y[randomize]
      
      # preparar data para tensorflow
      data_y <- keras::to_categorical(as.numeric(data_y), num_classes = num_classes)
      data_x <- array(data_x, dim = c(total, img_rows, img_cols, 1))

      # load net structure
      source(model_path(), local=TRUE)

      # fit
      model %>%
        compile(
          optimizer = optimizer_sgd(lr=0.01, momentum=0.9, nesterov=T), 
          loss = 'categorical_crossentropy',
          metrics = c('accuracy')
        )

      history<-model %>% fit(data_x, data_y,
                             batch_size = 64,
                             epochs = 20,
                             callbacks = list(callback_early_stopping(patience = 10, monitor = 'val_accuracy'),
                                              callback_model_checkpoint("./fitted_model.hdf5",
                                                                        monitor = "val_accuracy", save_best_only = T),
                                      #        callback_model_checkpoint("./epoch{epoch:02d}-val_accuracy-{val_accuracy:.4f}.hdf5",
                                       #                                 monitor = "val_accuracy"),
                                      callback_lambda(on_train_begin = function(logs) {
                                        shinyjs::html("fit_log", paste("Initiating epoch 1"))}),
                                      
                                      callback_lambda(on_epoch_end = function(epoch, logs) {
                                        shinyjs::html("fit_log", paste("Validation accuracy %: ", logs$val_accuracy))}),
                                      
                                     
                                      callback_lambda(on_train_end = function(logs) {
                                        shinyjs::html("fit_log", paste("Model fitted"))}),
                                      
                                              callback_csv_logger("./fit_log.csv")), 
                             
                             shuffle = TRUE,
                             validation_split = 0.3,
                             verbose = 1)

      save(history, file="./history_model.RDATA")

      
      output$end_fit <- renderTable({
        read.csv("fit_log.csv")

        
      })
      
     # file.remove("./fit_log.csv")

     })
    
    #so para testar a importacao do rdata_list
    output$selected_rdata <- renderText({
     as.character(rdata_list()[[3]][1])
    })
    
  }#server
#) #shinyApp

# Run the application 
shinyApp(ui = ui, server = server)
