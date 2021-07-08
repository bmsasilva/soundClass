#' @title Start app fit model
#' @description Starts the app to fit the model
#' @export
#' @import htmltools shinyBS

app_fit_model <- function() {
  
  # eliminate previous fitted_model_log.csv file # Ver onde colocar isto dentyro do server
  if (file.exists("./fitted_model_log.csv")) {
    file.remove("./fitted_model_log.csv")
  } # limpar a tabela de resultados se correr fit moedl 2 vezes
  
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Modeling"),
    shiny::tabsetPanel(
      shiny::tabPanel(
        title = "Create train data",
        shiny::sidebarLayout(
          fluid = FALSE,
          shiny::sidebarPanel(
            width = 2,
            shinyFiles::shinyDirButton(
              id = "folder",
              label = "Choose folder",
              title = "Choose recordings folder",
              style = "width:100%"
            ),
            htmltools::br(),
            shinyFiles::shinyFilesButton(
              id = "selected_db",
              label = "Choose database",
              title = "Choose database file",
              multiple = FALSE,
              style = "width:100%"
            ),
            htmltools::br(),
            htmltools::hr(),
            htmltools::h4("Spectrogram parameters", align = "left"),
            htmltools::hr(),
            shiny::numericInput(
              inputId = "spec_size",
              label = "Size (ms)",
              value = "20"
            ),
            shiny::numericInput(
              inputId = "window_length",
              label = "Moving window (ms)",
              value = "1"
            ),
            shiny::selectInput(
              inputId = "overlap",
              label = "Overlap",
              choices = c(
                "50%" = "0.50",
                "75%" = "0.75"
              ),
              selected = "0.75"
            ),
            shiny::selectInput(
              inputId = "frequency_resolution",
              label = "Resolution",
              choices = c(
                "Low" = "1",
                "Medium" = "2",
                "High" = "3"
              ),
              selected = "1"
            ),
            shiny::selectInput(
              inputId = "dynamic_range",
              label = "Threshold",
              choices = c(
                "40 dB" = "40",
                "50 dB" = "50",
                "60 dB" = "60",
                "70 dB" = "70",
                "80 dB" = "80",
                "90 dB" = "90",
                "100 dB" = "100"
              ),
              selected = "90"
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                align = "center",
                shiny::textInput(
                  inputId = "low",
                  label = "Lower (kHz)",
                  value = "10"
                )
              ),
              shiny::column(
                width = 6,
                align = "center",
                shiny::textInput(
                  inputId = "high",
                  label = "Higher (kHz)",
                  value = "120"
                )
              )
            )
          ),
          shiny::mainPanel(
            htmltools::br(),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textOutput("folder_path")
              )
            ),
            htmltools::br(),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textOutput("db_path")
              )
            ),
            htmltools::hr(),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                align = "center",
                offset = 3,
                shiny::actionButton(
                  inputId = "create_specs",
                  label = "Create training data from labels",
                  style = "width:100%"
                ),
                htmltools::tags$style(
                  type = "text/css",
                  "#create_specs { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"
                )
              )
            ),
            htmltools::br(),
            shiny::tableOutput("spec")
          )
        )
      ),
      shiny::tabPanel(
        title = "Fit model",
        shiny::sidebarLayout(
          fluid = FALSE,
          shiny::sidebarPanel(
            width = 2,
            shinyFiles::shinyFilesButton(
              id = "rdata_path",
              label = "Choose train data",
              title = "Choose train data file",
              multiple = FALSE,
              style = "width:100%"
            ),
            shinyFiles::shinyFilesButton(
              id = "model_path",
              label = "Choose model",
              title = "Choose model",
              multiple = FALSE,
              style = "width:100%"
            ),
            htmltools::hr(),
            htmltools::h4(
              "Model parameters",
              align = "left"
            ),
            htmltools::hr(),
            shiny::numericInput(
              inputId = "train_per",
              label = "Train %",
              value = "0.7"
            ),
            shiny::numericInput(
              inputId = "batch",
              label = "Batch size",
              value = "2"
            ),
            shiny::numericInput(
              inputId = "lr",
              label = "Learning rate",
              value = "0.01"
            ),
            shiny::numericInput(
              inputId = "stop",
              label = "Early stop",
              value = "2"
            ),
            shiny::numericInput(
              inputId = "epochs",
              label = "Max epochs",
              value = "2"
            )
          ),
          shiny::mainPanel(
            htmltools::br(),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textOutput("rdata_path")
              )
            ),
            htmltools::br(),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textOutput("model_path")
              )
            ),
            htmltools::hr(),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                align = "center",
                offset = 3,
                shiny::actionButton(
                  inputId = "fit_model",
                  label = "Fit model",
                  style = "width:100%"
                ),
                htmltools::tags$style(
                  type = "text/css",
                  "#fit_model { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"
                )
              )
            ),
            htmltools::br(),
            htmltools::br(),
            shiny::tableOutput("fitted_model_log"),
            htmltools::br(),
            htmltools::br(),
            shiny::tableOutput("end_fit")
          )
        )
      )
      
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
  server <- function(input, output) {
    
    # File and folder chooser paths -------------------------------
    system <- Sys.info()[["sysname"]]
    if (system == "Windows") roots <- c(home = "C://")
    if (system == "Linux") roots <- c(Computer = "/") # getVolumes()#funciona no HP mas nao no myotis # c(home = getVolumes()) #funciona no pc de casa mas nao no Portatil
    
    shiny::observe({
      shinyFiles::shinyDirChoose(
        input = input,
        id = "folder",
        roots = roots
      )
      if (!is.null(input$folder)) {
        folder_selected <- shinyFiles::parseDirPath(
          roots = roots,
          selection = input$folder
        )
        folder_path <- paste(
          "Recordings folder =",
          as.character(folder_selected)
        )
        output$folder_path <- shiny::renderText(as.character(folder_path))
      }
    })
    shiny::observeEvent(input$folder, {
      if (length(shinyFiles::parseDirPath(roots, input$folder)) > 0) {
        setwd(shinyFiles::parseDirPath(
          roots = roots,
          selection = input$folder
        ))
      }
    })
    files_path <- shiny::reactive({
      folder_selected <- shinyFiles::parseDirPath(
        roots = roots,
        selection = input$folder
      )
      files_path <- as.character(paste0(
        folder_selected,
        "//"
      )
      )
      return(files_path)
    })
    
    shiny::observe({
      shinyFiles::shinyFileChoose(
        input = input,
        id = "selected_db", 
        roots = roots
      )
      if (!is.null(input$selected_db)) {
        file_selected <- shinyFiles::parseFilePaths(
          roots = roots,
          selection = input$selected_db
        )
        db_path <- paste(
          "Database file =",
          as.character(file_selected$datapath)
        )
        output$db_path <- shiny::renderText(as.character(db_path)) 
      }
    })
    db_path <- shiny::reactive({
      file_selected <- shinyFiles::parseFilePaths(
        roots = roots,
        selection = input$selected_db
      )
      db_path <- as.character(file_selected$datapath)
      return(db_path)
    })
    
    spectro_calls <- shiny::eventReactive(input$create_specs, {
      
      total <- length(list.files(
        path = files_path(),
        recursive = F,
        pattern = "wav|WAV")
      )
      progress <- shiny::Progress$new(max = total)
      progress$set(
        message = "Processing recordings", 
        value = 0
      )
      on.exit(progress$close())
      
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue() + 1
        }
        progress$set(
          value = value, 
          detail = detail
        )
      }
      
      sp_data <- spectro_calls(
        files_path = files_path(), 
        updateProgress = updateProgress, 
        db_path = db_path(),
        spec_size = as.numeric(input$spec_size),
        window_length = as.numeric(input$window_length),
        frequency_resolution = as.numeric(input$frequency_resolution),
        time_step_size = (1 - as.numeric(input$overlap)) * as.numeric(input$window_length),#mudar isto para dentro da funcao
        dynamic_range = as.numeric(input$dynamic_range),
        freq_range = c(as.numeric(input$low), as.numeric(input$high))
      )
      save(
        sp_data,
        file = "train_data.RDATA"
      )
      return(sp_data)
    })
    
    output$spec <- shiny::renderTable({
      table(spectro_calls()[[2]])
    })
    
    shiny::observe({
      shinyFiles::shinyFileChoose(
        input = input,
        id = "rdata_path",
        roots = roots
      )
      if (!is.null(input$rdata_path)) {
        file_selected <- shinyFiles::parseFilePaths(
          roots = roots, 
          selection = input$rdata_path
        )
        rdata_path <- paste(
          "Train data file =",
          as.character(file_selected$datapath)
        )
        output$rdata_path <- shiny::renderText(as.character(rdata_path)) 
      }
    })
    
    rdata_list <- shiny::reactive({
      if (length(input$rdata_path) > 1) {
        file_selected <- shinyFiles::parseFilePaths(
          roots = roots, 
          selection = input$rdata_path
        )
        env <- load2env(as.character(file_selected$datapath))
        rdata_list <- env[[names(env)[1]]]
        rm(env)
        return(rdata_list)
      }
    })
    
    shiny::observe({
      shinyFiles::shinyFileChoose(
        input = input,
        id = "model_path",
        roots = roots
      )
      if (!is.null(input$model_path)) {
        file_selected <- shinyFiles::parseFilePaths(
          roots = roots, 
          selection = input$model_path
        )
        model_path <- paste(
          "Model file =",
          as.character(file_selected$datapath)
        )
        output$model_path <- shiny::renderText(as.character(model_path))
      }
    })
    
    model_path <- shiny::reactive({
      file_selected <- shinyFiles::parseFilePaths(
        roots = roots,
        selection = input$model_path
      )
      model_path <- as.character(file_selected$datapath)
      return(model_path)
    })
    
    
    # Apagar a tabela dos resultados no caso de 2 volta de fit
    shiny::observeEvent(input$fit_model, {
      output$end_fit <- shiny::renderTable({
        
      })
    })
    
    shiny::observeEvent(input$fit_model, {
      
      rdata_list <- rdata_list() 
      
      ######### Colocar isto dentro de uma funcao #######
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
      
      ##########
      
      # fit
      model %>%
        generics::compile(
          optimizer = keras::optimizer_sgd(lr=input$lr, momentum=0.9, nesterov=T), 
          loss = 'categorical_crossentropy',
          metrics = c('accuracy')
        )
      
      model %>% generics::fit(data_x, data_y,
                              batch_size = input$batch,
                              epochs = input$epochs,
                              callbacks = list(
                                keras::callback_early_stopping(patience = input$stop, monitor = 'val_accuracy'),
                                keras::callback_model_checkpoint("./fitted_model.hdf5",
                                                                 monitor = "val_accuracy", save_best_only = T),
                                #        callback_model_checkpoint("./epoch{epoch:02d}-val_accuracy-{val_accuracy:.4f}.hdf5",
                                #                                 monitor = "val_accuracy"),
                                keras::callback_lambda(on_train_begin = function(logs) {
                                  shinyjs::html("fitted_model_log", paste("Initiating epoch 1"))}),
                                
                                keras::callback_lambda(on_epoch_end = function(epoch, logs) {
                                  shinyjs::html("fitted_model_log", 
                                                paste("Epoch = ", epoch + 1, " | Validation accuracy = ", round(logs$val_accuracy,3)*100, "%"))}),
                                
                                
                                keras::callback_lambda(on_train_end = function(logs) {
                                  shinyjs::html("fitted_model_log", paste("Model fitted"))}),
                                
                                keras::callback_csv_logger("./fitted_model_log.csv")), 
                              
                              shuffle = TRUE,
                              validation_split = 1 - input$train_per,
                              verbose = 1)
      
      # save(history, file="./fitted_model_history.RDATA")
      metadata <- list(parameters =  rdata_list[[3]], classes = labels_df)
      save(metadata,
           file="fitted_model_metadata.RDATA")
      
      
      output$end_fit <- shiny::renderTable({
        utils::read.csv("fitted_model_log.csv")
      })
    })
  } # server
  
  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
