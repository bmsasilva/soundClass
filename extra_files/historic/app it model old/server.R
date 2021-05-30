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
  
  #run function activated by action button -------------------------------
  # # Estou a usar isto so para testar os paths para a spectro_calls
  # observeEvent(input$create_specs, {
  #   print(files_path())
  # })
  
  # https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent
  spectro_calls <- eventReactive(input$create_specs,{
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
  
  # Criar a tabela resumo depois de calculados os specs individuais----------
  output$spec <- renderTable({
    table(spectro_calls()[[2]])
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
  
  files_path <- reactive({ # files_path para usar noutras funcoes
    folder_selected <- parseDirPath(roots, input$folder)
    files_path <- as.character(paste0(folder_selected, "//")) #dupla barra para funcionar em linux e windows
    files_path
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
  
  

  

  

  

  

})