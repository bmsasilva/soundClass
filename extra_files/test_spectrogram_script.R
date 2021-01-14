db_path <- reactive({ # db_path para usar noutras funcoes
  file_selected <- parseFilePaths(roots, input$selected_db)
  db_path <- as.character(file_selected$datapath)
  db_path
})

sound <- import_audio(".//recs//M500-20190704_030003.wav", low = 10, high = 120)

Spectrogram(as.numeric(sound$sound_samples),                   
            SamplingFrequency=sound$fs,  
            WindowLength = 5,        
            FrequencyResolution = 3, 
            TimeStepSize = 4,     
            nTimeSteps = NULL,       
            Preemphasis = TRUE,      
            DynamicRange = 70,       
            Omit0Frequency = FALSE,  
            WindowType = "hanning",   
            WindowParameter = NULL,  
            plot = TRUE,             
            PlotFast = TRUE,         
            add = FALSE,             
            col = NULL,              
            xlim = NULL,             
            ylim = NULL,             
            main = "",               
            xlab = "Time (ms)",      
            ylab = "Frequency (kHz)")
