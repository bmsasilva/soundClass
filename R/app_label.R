#' @title Shiny app to label recordings
#' @description Shiny app to label recordings. Use this app to visualize your
#' training recordings, create annotations and store them in a sqlite database.
#' The app has a sidebar panel with the following buttons/boxes to input
#' required user data: 
#' \enumerate{
#'  \item Create database -- if no database exists to store the annotations,
#'  use this button to create one
#'  \item Choose database -- choose the database to store the annotations
#'  \item Butterworth filter -- check box to apply filter and 
#'  indicate low and high frequencies in kHz to filter the recordings
#'  \item Time expanded -- only used in recorders specifically intended for
#'  bat recordings. Can take any numeric value. If the recording is not time
#'  expanded the value must be set to 1. If it is time expanded the numeric
#'  value corresponding to the time expansion should be indicated
#'  \item Choose folder -- choose the folder containing the training recordings
#' }
#' 
#' After the spectrogram is ploted:
#' \enumerate{
#'  \item Select events by clicking in the spectrogram on the middle of the 
#'  event of interest (bat call, bird song, etc)
#'  \item Insert the correct label in the "Label" box and add any additional 
#'  notes in the "Observations" box
#'  \item Press 'Set labels' button to add labels to database
#'  \item Repeat above steps if more than one set of events is present
#'  in the recording
#'  \item Press 'Next' button to advance to next recording or pick another 
#'  recording from the dropdown list
#'   }
#' 
#' The spectrogram can be zoomed by pressing mouse button and dragging to select
#' an area and then double click on it. To unzoom simply double clicking on the 
#' spectrogram without an area selected. To adjust visualization settings,
#' in the top right, the tab "Spectrogram options" can be used to:
#' \itemize{
#'  \item Threshold -- minimum intensity values to show
#'  in the spectrogram. A value of 100 will typically be adequate for the
#'  majority of the recorders
#'  \item Window length -- moving window length in ms. Smaller windows best 
#'  suited for short calls
#'  \item Overlap -- overlap between consecutive windows, higher values give best 
#'  visualization but lower performance
#'  \item Resolution -- frequency resolution of the spectrogram
#' }
#' @usage app_label()
#' @return Starts the shiny app, no return value.
#' @author Bruno Silva
#' @export
#' @import htmltools shinyBS

app_label <- function() {

    ui <- shiny::fluidPage(
    shiny::titlePanel("Labeler"),
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
        htmltools::br(),
        shiny::actionButton(
          inputId = "create_db",
          label = "Create database",
          style = "width:100%"
        ),
        shinyBS::bsModal(
          id = "modal",
          Title = "Database name",
          trigger = "create_db",
          size = "small",
          htmltools::HTML("What is the database name?"),
          shiny::textInput("name", "", ""),
          shiny::actionButton("conf", "Confirm")
        ),
        htmltools::br(),
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
        shiny::selectInput(
          inputId = "files",
          label = NULL,
          choices = NULL,
          width = "100%"
        ),
        shiny::actionButton(
          inputId = "Next",
          label = "Next recording",
          width = "100%"
        ),
        htmltools::hr(),
         shiny::checkboxInput("but_filt", "Butterworth filter (kHz)",
                              value = FALSE, width = NULL),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            align = "center",
            shiny::textInput(
              inputId = "low",
              label = "Low",
              value = "0"
            )
          ),
          shiny::column(
            width = 6,
            align = "center",
            shiny::textInput(
              inputId = "high",
              label = "High",
              value = "120"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
          width = 12,
          align = "center",
          shiny::selectInput(
            inputId = "tx",
            label = "Time expanded",
            choices = c(
              "1" = "1",
              "10" = "10",
              "auto" = "auto"
            ),
            selected = "1"
          )
          )
        ),

        htmltools::hr(),
        shiny::actionButton(
          inputId = "analysis",
          label = "Set labels",
          width = "100%"
        ),
        htmltools::hr(),
        shinyFiles::shinyDirButton(
          id = "noise_folder",
          label = "Add irrelevant",
          title = "Add irrelevant",
          style = "width:100%"
        )
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          id = "inTabset",
          shiny::tabPanel(
            title = "Plot",
            value = "panel_plot",
            shiny::plotOutput(
              outputId = "spec",
              height = "auto",
              click = "specClick",
              dblclick = "plot1_dblclick",
              brush = shiny::brushOpts(
                id = "plot1_brush",
                resetOnNew = TRUE
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textInput(
                  inputId = "Lb",
                  label = "Label",
                  value = ""
                )
              ),
              shiny::column(
                width = 6,
                shiny::textInput(
                  inputId = "Obs",
                  label = "Observations",
                  value = ""
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textOutput("db_path")
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textOutput("folder_path")
              )
            )
          ),
          shiny::tabPanel(
            title = "Spectrogram options",
            value = "panel_options",
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = "dynamicRange",
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
                  selected = "70"
                )
              ),
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = "windowLength",
                  label = "Window length",
                  choices = c(
                    "1 ms" = "1",
                    "2 ms" = "2",
                    "4 ms" = "4",
                    "10 ms" = "10",
                    "20 ms" = "20",
                    "40 ms" = "40",
                    "100 ms" = "100",
                    "200 ms" = "200",
                    "400 ms" = "400",
                    "1000 ms" = "1000",
                    "2000 ms" = "2000",
                    "4000 ms" = "4000"
                  ),
                  selected = "4"
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = "timeStep",
                  label = "Overlap",
                  choices = c(
                    "50%" = "0.5",
                    "55%" = "0.45",
                    "60%" = "0.4",
                    "65%" = "0.35",
                    "70%" = "0.3",
                    "75%" = "0.25"
                  ),
                  selected = "0.5"
                )
              ),
              shiny::column(
                width = 4,
                shiny::selectInput(
                  inputId = "freqResolution",
                  label = "Resolution",
                  choices = c(
                    "low resolution" = "1"
                  ),
                  selected = "1"
                )
              )
            ),
            shiny::actionButton(
              inputId = "save",
              label = "Save and update spectrogram"
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    system <- Sys.info()[["sysname"]]
    if (system == "Windows") {
      roots <- c(home = "C://")
    }
    if (system == "Linux") {
      roots <- c(Computer = "/")
    }

    shiny::observeEvent(input$conf, {
      shinyBS::toggleModal(
        session = session,
        modalId = "modal",
        toggle = "open"
      )
      create_db(".//", input$name)
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
        setwd(shinyFiles::parseDirPath(roots, input$folder))
      }
    })

    file_names <- shiny::reactivePoll(
      1000,
      session,
      checkFunc = function() {
        list.files(
          path = ".",
          recursive = FALSE,
          pattern = "wav|WAV"
        )
      },
      valueFunc = function() {
        list.files(
          path = ".",
          recursive = FALSE,
          pattern = "wav|WAV"
        )
      }
    )

    shiny::observeEvent(file_names(), {
      if (length(shinyFiles::parseDirPath(roots, input$folder)) > 0) {
        shiny::updateSelectInput(
          session = session,
          inputId = "files",
          choices = file_names()
        )
      }
    })

    shiny::observeEvent(input$save, {
      shiny::updateTabsetPanel(
        session = session,
        inputId = "inTabset",
        selected = "panel_plot"
      )
    })

    sound <- shiny::reactive({
      shiny::validate(
        shiny::need(
         input$files != "",
          "Analysis steps:
1) Select buterworth filter limits if desired
2) Input the time expanded factor of the recordings. Choose '1' for no time expanded, or 'auto' for bat recordings
3) Select folder with recordings
4) If needed, create new database to store recording labels
5) Select database to store recording labels
6) Select events by clicking in the spectrogram on the middle of the event of interest (bat call, bird song, etc)
7) Press 'Set labels' button to add labels to database
8) Repeat steps 6 and 7 if more than one set of events is present in the recording
7) Press 'Next' button to advance to next recording or pick another recording from the dropdown list

Spectrogram visualization:
- Zoom spectrogram by click and drag to select area and then double click on it
- Unzoom by double clicking on spectrogram without area selected
- Adjust spectrogram settings with:
    THRESHOLD - minimum energy values displayed, higher values best suited for low quality recordings
    WINDOW - window size in ms, smaller windows best suited for short calls
    OVERLAP - overlap between consecutive windows, higher values give best visualization but lower performance
    RESOLUTION - frequency resolution of the spectrogram
    "
             )
      )

      tx <- ifelse(input$tx == "auto", "auto", as.numeric(input$tx))
      sound <- import_audio(
        path = input$files,
        butt = input$but_filt,
        low = as.numeric(input$low),
        high = as.numeric(input$high),
        tx = tx
      )
      return(sound)
    })

    ranges <- shiny::reactiveValues(x = NULL, y = NULL)

    shiny::observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        if (file.exists("temp_file.csv")) file.remove("temp_file.csv")
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
        if (file.exists("temp_file.csv")) file.remove("temp_file.csv")
      }
    })
    shiny::observeEvent(input$files, {
      ranges$x <- NULL
      ranges$y <- NULL
      maxpos$x <- NULL
      if (file.exists("temp_file.csv")) file.remove("temp_file.csv")
    })

    shiny::observeEvent(input$specClick$x, {
      utils::write.table(
        input$specClick$x,
        file = "temp_file.csv",
        append = TRUE,
        col.names = FALSE,
        row.names = FALSE
      )
    })

    output$spec <- shiny::renderPlot({
        Spectrogram(
          as.numeric(sound()$sound_samples),
          SamplingFrequency = sound()$fs * sound()$tx,
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
          ylab = "Frequency (kHz)"
        )

        graphics::abline(
          v = ms2samples(
            maxpos$x,
            tx = sound()$tx,
            fs = sound()$fs,
            inv = T
          )
        )
      },
      height = function() {
        0.6 * (session$clientData$output_spec_width)
      }
    )

    shiny::observeEvent(input$Next, {
      file_in_use <- which(file_names() %in% c(input$files))

      if (file_in_use < length(file_names())) {
        shiny::updateSelectInput(
          session = session,
          inputId = "files",
          choices = file_names(),
          selected = file_names()[file_in_use + 1]
        )
      }
    })

    maxpos <- shiny::reactiveValues(x = NULL)

    shiny::observeEvent(input$analysis, {
      if (file.exists("temp_file.csv")) {
        labs <- utils::read.csv(
          file = "temp_file.csv",
          header = FALSE
        )[, 1]
        labs <- ms2samples(
          value = labs,
          fs = shiny::isolate(sound()$fs),
          tx = shiny::isolate(sound()$tx)
        )
        np <- length(labs)
        if (file.exists("temp_file.csv")) file.remove("temp_file.csv")

        maxpos$x <- labs

        output <- data.frame(
          "recording" = shiny::isolate(sound()$file_name),
          "label_position" = maxpos$x,
          "label_class" = shiny::isolate(input$Lb),
          "observations" = shiny::isolate(input$Obs)
        )

        add_record(
          path = shiny::isolate(db_path()),
          df = output
        )
      }
    })

    shiny::observe({
      shinyFiles::shinyDirChoose(
        input = input,
        id = "noise_folder",
        roots = roots
      )
      if (!is.null(input$noise_folder)) {
        folder_selected <- shinyFiles::parseDirPath(
          roots = roots,
          selection = input$noise_folder
        )
        folder_path <- as.character(folder_selected)

        noise_files <- list.files(
          path = folder_path,
          pattern = "wav|WAV"
        )
        total <- length(noise_files)
        progress <- shiny::Progress$new(max = total)
        progress$set(message = "Processing recordings", value = 0)
        on.exit(progress$close())

        update_progress <- function(value = NULL,
                                    detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue() + 1
          }
          progress$set(
            value = value,
            detail = detail
          )
        }

        for (i in seq(noise_files)) {
          sound <-
            import_audio(
              path = paste0(folder_path, "/", noise_files[i]),
              butt = input$but_filt,
              low = as.numeric(input$low),
              high = as.numeric(input$high)
            )

          peak <- find_noise(
            recording = sound,
            nmax = 1,
            plot = F
          )

          output <- data.frame(
            "recording" = sound$file_name,
            "label_position" = peak,
            "label_class" = "0",
            "observations" = NA
          )

          add_record(
            path = db_path(),
            df = output
          )

          if (is.function(update_progress)) {
            text <- paste0(
              i,
              " of ",
              total
            )
            update_progress(detail = text)
          }
        }
      }
    })
  }
  shiny::shinyApp(ui = ui, server = server)
}
