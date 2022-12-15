#' @title Shiny app to fit a model or run a fitted model
#' @description Shiny app to fit a model from training recordings or to run a 
#' fitted model to classify new recordings. This app consists of three GUIs, 
#' i.e. three main panels, accessible by the tabs at the top:
#'  \enumerate{
#'   \item Create train data -- create train data from recordings and their
#'   respective annotations database  
#'   \item Fit model -- fit a model from training data
#'   \item Run model -- run a fitted model to classify new recordings
#'   }
#' 
#' ## 1. Create train data
#' This panel is used to create train data from recordings and their
#' respective annotations database. The sidebar panel has the following 
#' buttons/boxes to input required user data: 
#' \itemize{
#'   \item Choose folder -- choose the folder containing the training recordings
#'   \item Choose database -- choose the database with the annotations for the
#'   training recordings
#'   \item Time expanded -- choose the correct time expansion factor, normally 
#'   only used in recorders specifically intended for bat recordings. Can take 
#'   the values "auto", 1 or 10. If the recording is in real time the value
#'   must be 1. If it's time expanded, the value 10 or "auto" can be selected. 
#'   If "auto" is selected it is assumed that sampling rates < 50kHz 
#'   corresponds to a value of 10 and sampling rates > 50kHz to corresponds to a
#'   value of 1
#'   \item Spectrogram parameters -- different typologies of sound events 
#'   require different parameters for computing the spectrograms.
#'   The more relevant are: size (in ms), which should be large 
#'   enough to encompass the duration of the largest sound event in 
#'   analysis (not only in the training data but also in novel recordings 
#'   where the classifiers are to be applied) and moving window (in ms), 
#'   that should be smaller for shorter sound events (to capture the quick 
#'   changes in time) and larger for longer sound events (to avoid redundant 
#'   information). The other parameters are more generalist and the same 
#'   values can be used for different sound events, as they only change 
#'   the definition of the images created. Please refer to 
#'   \code{\link{spectro_calls}} documentation for further details
#'   }
#'  
#' After entering the required information press the button "Create training 
#' data from labels" to generate the training data that will be used for 
#' fitting a model. This object is saved in the folder containing the
#' training recordings with the name "train_data.RDATA".
#'     
#' ## 2. Fit model 
#' This panel is used to fit a model from training data. The sidebar panel has 
#' the following buttons/boxes to input required user data: 
#' \itemize{
#' \item Choose train data -- the file "train_data.RDATA" created in
#' the previous panel
#' \item Choose model -- a blank model to be fitted. A custom model is provided
#' but must be copied to an external folder if it is to be used. The model path
#' can be obtained by running the following line at the R console:
#' system.file("model_architectures", "model_vgg_sequential.R", package="soundClass")
#' and should be manually copied to a an external folder
#' \item Model parameters -- the train percentage indicates the percentage of 
#' data that is used to fit the model while the remaining are used for 
#' validation, batch size indicates the number
#' of samples per gradient update, the learning rate indicates the degree of the
#' gradient update, early stop indicates the maximum number of epochs without
#' improvement allowed before training stops and epochs indicate the maximum 
#' number of epochs to train. Further information can be found in keras
#' documentation \url{https://keras.io/api/} 
#' }
#' 
#' The model is evaluated during fitting using the validation data. After 
#' completion, by reaching the maximum epochs or the early stopping parameters,
#' the fitted model, the fitting log and the model metadata are saved to the 
#' folder containing the train data with file names: "fitted_model.hdf5", 
#' "fitted_model_log.csv" and "fitted_model_metadata.RDATA" respectively.
#' 
#' ## 3. Run model 
#' This panel is used to run a fitted model to classify new recordings. The 
#' sidebar panel has the following buttons/boxes to input required user data:
#' \itemize{
#' \item Choose folder -- choose the folder containing the recordings to be
#' classified
#' \item Choose model -- a fitted model to be used for classification
#' \item Choose metadata -- the file containing the fitted model metadata
#' \item Time expanded -- choose the correct time expansion factor, normally 
#'   only used in recorders specifically intended for bat recordings. Can take 
#'   the values "auto", 1 or 10. If the recording is not time expanded the value
#'   must be 1. If it's time expanded, the value 10 or "auto" can be selected. 
#'   If "auto" is selected it is assumed that sampling rates < 50kHz 
#'   corresponds to a value of 10 and sampling rates > 50kHz to corresponds to a
#'   value of 1
#'   \item Output file -- the name of the files to store the results of the
#'   classification 
#'   \item Irrelevant -- does the fitted model includes an irrelevant class? 
#'   \item Export plots -- should a spectrogram of the classified recordings be
#'   saved to disk?
#' }
#' 
#' The classification results are stored in a folder called "output", 
#' created inside the folder containing the recordings. They are stored in a 
#' database in sqlite3 format with
#' all the relevant events detected and the respective probability of belonging
#' to a given class. Additionally a file in the csv format is saved to disk, 
#' containing summary statistics per recording, i.e. the class with most events 
#' detected in each particular recording and the average frequency of maximum 
#' energy of the events detected. 
#' 
#' @usage app_model()
#' @return Starts the shiny app, no return value.
#' @author Bruno Silva
#' @export
#' @import htmltools shinyBS

app_model <- function() {
  
  if (file.exists("./fitted_model_log.csv")) {
    file.remove("./fitted_model_log.csv")
  }
  
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
            htmltools::br(),
            shiny::fluidRow(
              shiny::column(
                width = 12,
                align = "left",
                shiny::selectInput(
                  inputId = "tx",
                  label = "Time expanded",
                  choices = c(
                    "1" = "1",
                    "10" = "10",
                    "auto" = "auto"
                  ),
                  selected = "1"
                ),
              )
            ),
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
                "55%" = "0.55",
                "60%" = "0.60",
                "65%" = "0.75",
                "70%" = "0.70",
                "75%" = "0.75"
              ),
              selected = "0.50"
            ),
            shiny::selectInput(
              inputId = "frequency_resolution",
              label = "Resolution",
              choices = c(
                "Low" = "1"
              ),
              selected = "1"
            ),
            shiny::selectInput(
              inputId = "dynamic_range",
              label = "Threshold",
              choices = c(
                "60 dB" = "60",
                "70 dB" = "70",
                "80 dB" = "80",
                "90 dB" = "90",
                "100 dB" = "100",
                "110 dB" = "110",
                "120 dB" = "120"
              ),
              selected = "100"
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
                  "#create_specs {vertical-align- middle;
                  height- 50px; width- 100%; font-size- 30px;}"
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
              value = "128"
            ),
            shiny::numericInput(
              inputId = "lr",
              label = "Learning rate",
              value = "0.01"
            ),
            shiny::numericInput(
              inputId = "stop",
              label = "Early stop",
              value = "4"
            ),
            shiny::numericInput(
              inputId = "epochs",
              label = "Max epochs",
              value = "20"
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
                  "#fit_model {vertical-align- middle; height- 50px;
                  width- 100%; font-size- 30px;}"
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
      ),
      
      shiny::tabPanel("Run model",
                      shiny::sidebarLayout(fluid = FALSE,
                                           shiny::sidebarPanel(
                                             width = 2,
                                             shinyFiles::shinyDirButton(id = "fitted_selected_folder",
                                                                        label = "Choose folder",
                                                                        title = "Choose folder",
                                                                        style = "width:100%"),
                                             
                                             shinyFiles::shinyFilesButton(
                                               id = "fitted_selected_model",
                                               label = "Choose model",
                                               title = "Choose model",
                                               multiple = FALSE,
                                               style = "width:100%"),
                                             
                                             htmltools::br(),
                                             shinyFiles::shinyFilesButton(id = "fitted_selected_metadata",
                                                                          label = "Choose metadata",
                                                                          title = "Choose metadata file",
                                                                          multiple = FALSE,
                                                                          style = "width:100%"),
                                             htmltools::br(),
                                             htmltools::br(),
                                             shiny::fluidRow(
                                               shiny::column(
                                                 width = 12,
                                                 align = "left",
                                                 shiny::selectInput(
                                                   inputId = "tx2",
                                                   label = "Time expanded",
                                                   choices = c(
                                                     "1" = "1",
                                                     "10" = "10",
                                                     "auto" = "auto"
                                                   ),
                                                   selected = "1"
                                                 ),
                                               )
                                             ),
                                         #    htmltools::br(),
                                             shiny::numericInput(
                                               inputId = "win_chunck",
                                               label = "Window size (ms)",
                                               value = "50"),
                                           
                                            #   htmltools::br(),
                                             
                                             shiny::textInput(inputId = "out_file",
                                                              label = "Name of output file",
                                                              value = "id_results"),
                                            # htmltools::br(),
                                             
                                             shiny::radioButtons("bt_filt",
                                                                 "Butterworth",
                                                                 c("Yes" = TRUE,
                                                                   "No" = FALSE)),
                                             
                                             shiny::radioButtons("rem_noise",
                                                                 "Irrelevant class",
                                                                 c("Yes" = TRUE,
                                                                   "No" = FALSE)),
                                            # htmltools::br(),
                                             
                                             shiny::radioButtons("lab_plots",
                                                                 "Export labeled plots",
                                                                 c("Yes" = TRUE,
                                                                   "No" = FALSE))
                                           ),
                                           shiny::mainPanel(
                                             htmltools::br(),
                                             shiny::fluidRow(
                                               shiny::column(6,
                                                             shiny::textOutput("fitted_folder_path")
                                               )
                                             ),
                                             htmltools::br(),
                                             shiny::fluidRow(
                                               shiny::column(6,
                                                             shiny::textOutput("fitted_model_path")
                                               )
                                             ),
                                             htmltools::br(),
                                             shiny::fluidRow(
                                               shiny::column(6,
                                                             shiny::textOutput("fitted_metadata_path")
                                               )
                                             ),
                                             htmltools::hr(),
                                             shiny::fluidRow(
                                               shiny::column(6,
                                                             align = "center",
                                                             offset = 3,
                                                             shiny::actionButton("id_recs", "Classify recordings", style = "width:100%"),
                                                             htmltools::tags$style(type = "text/css", "#id_recs { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                                               )
                                             ),
                                             shiny::column(5, shiny::plotOutput(""))
                                           )
                      )
      )
    )
  )
  
  server <- function(input, output) {
    
    system <- Sys.info()[["sysname"]]
    if (system == "Windows") roots <- c(home = "C://")
    if (system == "Linux") roots <- c(Computer = "/")
    
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
    
    spec_calls <- shiny::eventReactive(input$create_specs, {
      
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
      
      update_progress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue() + 1
        }
        progress$set(
          value = value,
          detail = detail
        )
      }
      
      train_data <- spectro_calls(
        files_path = files_path(),
        update_progress = update_progress,
        db_path = db_path(),
        spec_size = as.numeric(input$spec_size),
        window_length = as.numeric(input$window_length),
        frequency_resolution = as.numeric(input$frequency_resolution),
        overlap = as.numeric(input$overlap),
        dynamic_range = as.numeric(input$dynamic_range),
        freq_range = c(as.numeric(input$low), as.numeric(input$high)),
        tx = ifelse(nchar(input$tx) > 2, input$tx, as.numeric(input$tx))
      )
      save(
        train_data,
        file = "train_data.RDATA"
      )
      return(train_data)
    })
    
    output$spec <- shiny::renderTable({
      table(spec_calls()[[2]])
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
        if(length(dirname(as.character(file_selected$datapath))) > 0)
          setwd(dirname(as.character(file_selected$datapath)))
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
    
    shiny::observeEvent(input$fit_model, {
      output$end_fit <- shiny::renderTable({
      })
    })
    
    shiny::observeEvent(input$fit_model, {
      
      rdata_list <- rdata_list()
      
      model <- c()
      input_shape <- c(rdata_list$parameters$img_rows, 
                       rdata_list$parameters$img_cols,
                       1)
      num_classes <- rdata_list$parameters$num_classes
      source(model_path(), local = TRUE)
      
      model %>%
        generics::compile(
          optimizer = keras::optimizer_sgd(learning_rate = input$lr,
                                           momentum = 0.9, nesterov = T),
          loss = "categorical_crossentropy",
          metrics = c("accuracy")
        )
      
      model %>% generics::fit(rdata_list$data_x, rdata_list$data_y,
                              batch_size = input$batch,
                              epochs = input$epochs,
                              callbacks = list(
                                keras::callback_early_stopping(patience = input$stop, monitor = "val_accuracy"),
                                keras::callback_model_checkpoint("./fitted_model.hdf5",
                                                                 monitor = "val_accuracy", save_best_only = T),
                                keras::callback_lambda(on_train_begin = function(logs) {
                                  shinyjs::html("fitted_model_log", paste("Initiating epoch 1"))}),
                                keras::callback_lambda(on_epoch_end = function(epoch, logs) {
                                  shinyjs::html("fitted_model_log",
                                                paste("Epoch = ", epoch + 1, " | Validation accuracy = ", round(logs$val_accuracy, 3) * 100, "%"))}),
                                keras::callback_lambda(on_train_end = function(logs) {
                                  shinyjs::html("fitted_model_log", paste("Model fitted"))}),
                                keras::callback_csv_logger("./fitted_model_log.csv")),
                              shuffle = TRUE,
                              validation_split = 1 - input$train_per,
                              verbose = 1)
      
      metadata <- train_metadata(rdata_list)
      save(metadata,
           file = "fitted_model_metadata.RDATA")
      
      output$end_fit <- shiny::renderTable({
        utils::read.csv("fitted_model_log.csv")
      })
    })
    
    shiny::observe({
      shinyFiles::shinyDirChoose(input, "fitted_selected_folder", roots = roots)
      if (!is.null(input$fitted_selected_folder)) {
        folder_selected <- shinyFiles::parseDirPath(roots, input$fitted_selected_folder)
        folder_path <- paste("Recordings folder =",
                             as.character(folder_selected))
        output$fitted_folder_path <- shiny::renderText(as.character(folder_path))
      }
    })
    shiny::observeEvent(input$fitted_selected_folder, {
      if (length(shinyFiles::parseDirPath(roots,
                                          input$fitted_selected_folder)) > 0) {
        setwd(shinyFiles::parseDirPath(roots, input$fitted_selected_folder))
      }
    })
    fitted_files_path <- shiny::reactive({
      folder_selected <- shinyFiles::parseDirPath(roots,
                                                  input$fitted_selected_folder)
      files_path <- as.character(paste0(folder_selected, "//"))
      files_path
    })
    
    shiny::observe({
      shinyFiles::shinyFileChoose(
        input = input,
        id = "fitted_selected_model",
        roots = roots
      )
      if (!is.null(input$fitted_selected_model)) {
        file_selected <- shinyFiles::parseFilePaths(
          roots = roots,
          selection = input$fitted_selected_model
        )
        fitted_model_path <- paste(
          "Model file =",
          as.character(file_selected$datapath)
        )
        output$fitted_model_path <- shiny::renderText(as.character(fitted_model_path))
      }
    })
    fitted_model_path <- shiny::reactive({
      file_selected <- shinyFiles::parseFilePaths(
        roots = roots,
        selection = input$fitted_selected_model
      )
      fitted_model_path <- as.character(file_selected$datapath)
      return(fitted_model_path)
    })
    
    shiny::observe({
      shinyFiles::shinyFileChoose(input, "fitted_selected_metadata",
                                  roots = roots)
      if (!is.null(input$fitted_selected_metadata)) {
        file_selected <- shinyFiles::parseFilePaths(roots,
                                                    input$fitted_selected_metadata)
        fitted_metadata_path <- paste("Metadata file =",
                                      as.character(file_selected$datapath))
        output$fitted_metadata_path <- shiny::renderText(as.character(fitted_metadata_path))
      }
    })
    
    fitted_metadata <- shiny::reactive({
      if (length(input$fitted_selected_metadata) > 1) {
        file_selected <- shinyFiles::parseFilePaths(roots,
                                                    input$fitted_selected_metadata)
        env <- load2env(as.character(file_selected$datapath))
        metadata <- env[[names(env)[1]]]
        rm(env)
        return(metadata)
      }
    })
    
    shiny::observeEvent(input$id_recs, {
      if (!dir.exists(paste0(fitted_files_path(), "/", "output/")))
        dir.create(paste0(fitted_files_path(), "/", "output/"))
      
      total <- length(list.files(fitted_files_path(),
                                 recursive = F, pattern = "wav|WAV"))
      progress <- shiny::Progress$new(max = total)
      progress$set(message = "Processing recordings", value = 0)
      on.exit(progress$close())
      
      update_progress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue() + 1
        }
        progress$set(value = value, detail = detail)
      }
      auto_id(fitted_model_path(), 
              update_progress,
              fitted_metadata(),
              fitted_files_path(),
              out_file = input$out_file,
              out_dir = paste0(fitted_files_path(), "/", "output/"),
              save_png = as.logical(input$lab_plots),
              win_size = as.numeric(input$win_chunck),
              remove_noise = as.logical(input$rem_noise),
              plot2console = FALSE,
              recursive = FALSE,
              tx = ifelse(nchar(input$tx2) > 2, input$tx2, as.numeric(input$tx2)),
              butt_filter = as.logical(input$bt_filt))
    })
  }
  shiny::shinyApp(ui = ui, server = server)
}
