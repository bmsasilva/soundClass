library(shinyBS)


##### Load all #####
system <- Sys.info()[['sysname']]
if (system == "Windows") files <- list.files("C://Users//silva//Projects//R_packages//soundClass//R", pattern = ".R", full.names = TRUE)
if (system == "Linux") files <- list.files("~/Projectos/phd/0.workfolder/meus_papers/ms02_package_cnn/soundClass/R/", pattern = ".R", full.names = TRUE)

for(file in files) source(file)

shinyServer(function(input, output, session) {
  

  # File and folder chooser paths -------------------------------
  system <- Sys.info()[['sysname']]
  if (system == "Windows") roots <- c(home = 'C://')
  if (system == "Linux") roots <- getVolumes() # c(home = getVolumes()) #funciona no pc de casa mas nao no Portatil   
  
  # run function activated by action button -------------------------------
  observeEvent(input$conf, {
    shinyBS::toggleModal(session, "modal", toggle = "open")
    create_specs(".//", input$name)
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
  
  # Button  3 -----------------------------------------
  observe({ 
    shinyFileChoose(input, 'selected_model', roots = roots)
    if(!is.null(input$selected_model)){
      model_selected <- parseFilePaths(roots, input$selected_model)
      model_path <- paste("Model file =", as.character(model_selected$datapath))
      output$model_path <- renderText(as.character(model_path)) #output para mostrar o path do modelo escolhido
    }
  })
  
  model_path <- reactive({ # model_path para usar noutras funcoes
    model_selected <- parseFilePaths(roots, input$selected_model)
    model_path <- as.character(model_selected$datapath)
    model_path
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
           
           
           "\n \n Create spectrograms from label database:
1) Select folder with recordings
2) Select pre-existing database with recording labels
3) Press 'Create spectrograms from labels' button
6) Input spectrogram parameters in modal box
7) Press 'Confirm' button \n \n" )
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
                  col = NULL,              
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
      if(!is_even(length(labs))) {
        showNotification("Please choose calls again", type = "error", closeButton = T, duration = 3)
        remove(labs)
        file.remove("temp_file.csv")
      } else{
        #pulsos <- matrix(aux, byrow=TRUE, ncol = 2)
        
        ## Obtém o local de fmaxe de cada pulso seleccionado anteriormente
        j <- 1
        maxpos$x <- NULL #para eliminar outros pulsos ja escolhidos nesta rec
        for (i in 1:np) {
          maxpos$x[i]<-(which.max(abs(isolate(sound()$sound_samples[labs[j]:labs[j+1]]))) + labs[j]) 
          j <- j+2
        }
        output <- data.frame("recording" = isolate(sound()$file_name),
                             "label_position" = maxpos$x,
                             "label_class" = isolate(input$Lb),
                             "observations" = isolate(input$Obs))
        
         add_record(path = isolate(db_path()), df = output)
       


        } # final do else
    }
  })
})