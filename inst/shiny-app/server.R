## FALTA MUDAR ALGUNS PATHS RELATIVOS


# source('bin/Global.R')
# source('bin/BatRecording.R')
# source('bin/vars.R')
# source('bin/spectrogram_com normalizacao a 140 db.R')
# source('bin/read_ANN_data.R')
# source('bin/computeResults.R')
# source('bin/spec.bs.colors.R')
# source('bin/dir_create.R')
# source('bin/output_create.R')

read.csv_bs <- function(x) read.csv(x, stringsAsFactors = F)
counterCalls <<- 0

shinyServer(function(input, output, session) {
  
  
  # File and folder chooser paths -------------------------------
  system <- Sys.info()[['sysname']]
  if (system == "Windows") roots <- c(home = 'C://')
  if (system == "Linux") roots <- getVolumes() # c(home = getVolumes()) #funciona no pc de casa mas nao no Portatil   
  
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
  
  # output$db_path <- renderText({ # so para testar se db_path() funciona
  #   db_path()
  # })
  
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
  
  
  
  
  
  
  
  
  
  # # Obtain file names from folder
  #  fileNames <- reactivePoll(1000, session,
  #    checkFunc = function() {list.files(".", recursive = FALSE, pattern="wav|WAV")},
  #    valueFunc = function() {list.files(".", recursive = FALSE, pattern="wav|WAV")}
  #      )
  #Update filenames in UI
  # observeEvent(folder_path(), {
  #   # Update do menu "files" na UI
  #   updateSelectInput(session,"files", choices = folder_path())
  # })
  ####################################################################################333
  #Criar as pastas Sem morcegos e analisada
  # observeEvent(db_path(), {
  #   #setwd(parseDirPath(volumes, input$folder))
  #   print(db_path())
  #  # dir_create(c("Analisadas", "Sem_morcegos", "temp"), "./")
  #  #  output_create("output_semiauto.csv", ".")
  # })
  
  
  
  #
  # pngPlots <- reactivePoll(1000, session,
  #                           checkFunc = function() {list.files("./temp")},
  #                           valueFunc = function() {list.files("./temp")}
  # )
  #
  # # Observador para obter a listagem dos ficheiros da pasta escolhida
  # # e actualizar a listagem de especies na UI para escolher
  # observeEvent(fileNames(), {
  #   # Update do menu "files" na UI
  #   updateSelectInput(session,"files", choices=fileNames())
  # })
  
  observeEvent(input$files, {
    #reset ao numero de calls qd se muda de ficheiro
    counterCalls <<- 0
  })
  
  
  # Importar gravacao ----------------------------------------------------
  sound <- reactive({
    # https://shiny.rstudio.com/articles/validation.html
    # Mensagem para substituir o erro enquanto nao abre o primeiro ficheiro.
    validate(
      need(input$files != "",
           "Analysis steps:
1)Select folder with recordings
2) Select bat calls by clicking in the spectrogram before and after the call
3) Press CLASSIFY button to perform bat species classification and store the result is .csv format
4) Repeat step 2 and 3 if more than one bat is in the recording
5) Press NEXT button to advance to next recording

Spectrogram visualization:
- Zoom spectrogram by click and drag to select area and then double click on it
- Unzoom by double clicking on spectrogram without area selected
- Adjust spectrogram settings with:
    THRESHOLD - minimum energy values displayed, higher values best suited for low quality recordings
    WINDOW - window size in ms, smaller windows best suited for short calls
    OVERLAP - overlap between consecutive windows, higher values give best visualization but lower performance")
    )
    
    
    sound <- import_audio(path = input$files, butt = TRUE,
                          low = input$low,
                          high = input$high)
    
    
    
    sound
  })
  
  
  # Preparar opcoes para o spectrogram --------------------------------------
  
  
  opts <- reactive({
    opts <- list(windowLength = as.numeric(input$windowLength),
                 freqResolution = as.numeric(input$freqResolution),
                 timeStep = as.numeric(input$timeStep),
                 dynamicRange = as.numeric(input$dynamicRange))
    opts
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
  
  # Eliminar os valores do zoom quando se muda de ficheiro
  observeEvent(input$files, {
    ranges$x <- NULL
    ranges$y <- NULL
    file.remove("pulsos.csv")
  })
  
  # Lidar com os clicks do rato ----------------------------------------------
  
  # Guardar as posicoes do rato quando clickado, numa variavel
  posicao <- eventReactive(input$specClick$x, {
    input$specClick$x
  })
  
  # Guardar os clicks do rato em ficheiro externo
  output$clickInfo <- renderText({
    
    # Criar a tabela externa com a posicao x dos clicks do rato
    write.table(posicao(),
                file = "pulsos.csv",
                append = TRUE,
                col.names = FALSE,
                row.names= FALSE)
    
    # Mostrar o valor de x do click na caixa de texto reactiva
    print(posicao())
  })
  
  # Plotar espectrograma ----------------------------------------------------------
  
  # Plotar o espectrograma
  output$spec <- renderPlot({
    
    powSpec(as.numeric(sound), xlim = ranges$x, ylim = ranges$y)
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
  observeEvent(input$proxRec, {
    
    # # Obter a posicao do ficheiro carregado da lista de ficheiros
    # fileInUse <- which(fileNames() %in% c(input$files))
    #
    # # so corre se nao for a ultima gravacao
    # if (fileInUse < length(fileNames())){
    #   # Update do menu "files" na UI com o novo ficheiro escolhido
    #   updateSelectInput(session,"files", choices=fileNames(), selected = fileNames()[fileInUse + 1])
    if(counterCalls == 0 ) {
      file.rename(input$files,paste("Sem_morcegos",input$files,sep="/"))
    } else {file.rename(input$files,paste("Analisadas",input$files,sep="/"))}
    
  })
  
  # observeEvent(input$antRec, {
  #
  #   # Obter a posicao do ficheiro carregado da lista de ficheiros
  #   fileInUse <- which(fileNames() %in% c(input$files))
  #
  #   if (fileInUse > 1){ # so corre se nao for a primeira gravacao
  #   # Update do menu "files" na UI com o novo ficheiro escolhido
  #   updateSelectInput(session,"files", choices=fileNames(), selected = fileNames()[fileInUse - 1])
  #   }
  # })
  
  # observeEvent(input$analisar, {
  #   showNotification("This is a notification.")
  # })
  
  ## Botao ANALISAR
  observeEvent(input$analisar, {
    # Validar o numero de clicks para escolha dos pulsos
    if(file.exists('pulsos.csv')){
      aux <- read.csv(file = "pulsos.csv", header = FALSE)[,1]
      file.remove("pulsos.csv")
      if(!isEven(length(aux))) {
        showNotification("Please choose calls again", type = "error", closeButton = T, duration = 3)
        remove(pulsos)
        file.remove("pulsos.csv")
      } else{
        pulsos <- matrix(aux, byrow=TRUE, ncol = 2)
      }
    }
    
    ##analisar
    try({
      # print(getwd())
      updateTextInput(session,"FB", value='')
      updateTextInput(session,"SC", value='')
      updateTextInput(session,"Comm", value='')
      counterCalls <<- counterCalls + 1
      print(counterCalls)
      
      analisar(som(), pulsos, input$files, input$FB, input$SC, input$Comm, counterCalls)
      
      plotCalls(som(),np, input$files, counterCalls)
      
      # eliminar o ficheiro auxiliar com os pulsos escolhidos no final da analise
      file.remove("pulsos.csv")
    })#final try
  })
  
  
  ## Save file
  # observe({
  #   # remove button and isolate to update file automatically
  #   # after each table change
  #   input$save
  #   hot = isolate(input$hot)
  #   if (!is.null(hot)) {
  #     write.csv(hot_to_r(input$hot), "teste.csv")
  #
  #   }
  # })
  
  observeEvent(input$save, {
    hot = isolate(input$hot)
    if (!is.null(hot)) {
      write.csv(hot_to_r(input$hot), "output_semiauto.csv")#mudar path
    }
  })
  
  output$plotCalls1 <- renderImage({
    
    folder <- pngPlots() # apenas para tornar este plot reactivo qd se acresecentam ficheiros png
    # Return a list containing the filename
    list(src = files_spectrograms[1],
         contentType = 'image/png',
         width = 150,
         height = 150,
         alt = "No bat call to show")
    
    
    
  }, deleteFile = FALSE)
  
  output$plotCalls2 <- renderImage({
    
    folder <- pngPlots()
    # Return a list containing the filename
    list(src = files_spectrograms[2],
         contentType = 'image/png',
         width = 150,
         height = 150,
         alt = "No bat call to show")
    
    
    
  }, deleteFile = FALSE)
  
  
  output$plotCalls3 <- renderImage({
    
    folder <- pngPlots()
    # Return a list containing the filename
    list(src = files_spectrograms[3],
         contentType = 'image/png',
         width = 150,
         height = 150,
         alt = "No bat call to show")
    
    
    
  }, deleteFile = FALSE)
  output$plotCalls4 <- renderImage({
    
    folder <- pngPlots()
    # Return a list containing the filename
    list(src = files_spectrograms[4],
         contentType = 'image/png',
         width = 150,
         height = 150,
         alt = "No bat call to show")
    
    
    
  }, deleteFile = FALSE)
  
})




