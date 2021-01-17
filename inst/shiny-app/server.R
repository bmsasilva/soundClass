read.csv_bs <- function(x) read.csv(x, stringsAsFactors = F)
counterCalls <<- 0

##### Load all #####
files <- list.files("C://Users//silva//Projects//R_packages//soundClass//R", pattern = ".R", full.names = TRUE)
for(file in files) source(file)

shinyServer(function(input, output, session) {
  

  # File and folder chooser paths -------------------------------
  system <- Sys.info()[['sysname']]
  if (system == "Windows") roots <- c(home = 'C://')
  if (system == "Linux") roots <- getVolumes() # c(home = getVolumes()) #funciona no pc de casa mas nao no Portatil   
  
  # Ask user for db name -------------------------------
  observeEvent(input$conf, {
    toggleModal(session, "modal", toggle = "open")
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
4) Select calls by clicking in the spectrogram before and after the call
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
  
  # Observador para o evento duplo click
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      file.remove("pulsos.csv")
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
      file.remove("pulsos.csv")
    }
  })
  
  # eliminar ficheiro de pulsos qd se cria uma caixa para zoom
  observeEvent(input$plot1_brush, {
         file.remove("pulsos.csv")
  })
  
  
  
  # Eliminar os valores do zoom quando se muda de ficheiro
  observeEvent(input$files, {
    ranges$x <- NULL
    ranges$y <- NULL
    file.remove("pulsos.csv")
  })
  
  # Lidar com os clicks do rato ----------------------------------------------
  
  # Guardar as posicoes do rato quando clickado, em ficheiro externo
  observeEvent(input$specClick$x, {
    
    write.table(input$specClick$x,
                file = "pulsos.csv",
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
                  col = NULL,              
                  xlim = ranges$x,             
                  ylim = ranges$y,             
                  main = "",               
                  xlab = "Time (ms)",      
                  ylab = "Frequency (kHz)")
    
    
  }, height = function() {
    0.6 * (session$clientData$output_spec_width) #crontolar a altura do plot (0.6 * o comprimento)
  })
  
  # # Cria a tabela reactive
  DF <- reactiveFileReader(1000, session=session, filePath = "output_semiauto.csv", readFunc = read.csv_bs)
  
  # Mostra a tabela
  output$hot <- renderRHandsontable({
    if (!is.null(DF()))
      rhandsontable(DF(),  stretchH = "all") %>%
      hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
td.style.background = 'lightgray';
td.style.color = 'black';

           }")
  })
  
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
  observeEvent(input$analisar, {
    
    # Validar o numero de clicks para escolha dos pulsos
    if(file.exists('pulsos.csv')){
      aux <- read.csv(file = "pulsos.csv", header = FALSE)[,1]
      aux <- ms2samples(aux, fs = isolate(sound()$fs), tx = isolate(sound()$tx))
      file.remove("pulsos.csv")
      if(!is_even(length(aux))) {
        showNotification("Please choose calls again", type = "error", closeButton = T, duration = 3)
        remove(pulsos)
        file.remove("pulsos.csv")
      } else{
        pulsos <- matrix(aux, byrow=TRUE, ncol = 2)
        print(pulsos)
        np <- nrow(pulsos)
        label <- 
        obs <- 
        ## Obtém o local de fmaxe de cada pulso seleccionado anteriormente
        j <- 1
        maxpos<-NULL
        for (i in 1:np) {
          maxpos[i]<-(which.max(abs(isolate(sound()$sound_samples[pulsos[j]:pulsos[j+1]]))) + pulsos[j]) 
          j <- j+2
        }
        
        output <- data.frame("recording" = isolate(sound()$file_name),
                             "label_position" = maxpos,
                             "label_class" = isolate(input$Lb),
                             "observations" = isolate(input$Obs))

        
         add_record(path = isolate(db_path()), df = output)
        
        
        
        
      } # final do else
    }
    
  
    
    # 

  
  })
  


  

  
})




