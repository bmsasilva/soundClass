### Falta introduzir a progress bar aqui dentro da funcao ou em alternativa desmontar a função no shiny e colocar como se fosse um script
#https://shiny.rstudio.com/articles/progress.html
auto_id_shiny <- function(model_path, updateProgress,
                          metadata,
                          file_path, 
                          out_file, 
                          out_dir, 
                          save_spec = F,
                          save_png = T, 
 #                         class_labels, 
                          win_size = 50, 
                          plot2console = F, 
                          remove_noise = T, # if model has non-relevant class, eliminate from output 
                          recursive = FALSE){
  
#####
  print("auto_id_arranca")
  print(metadata)
  
  #Parameteros dos metadados do modelo e dos spectrogramas de treino
  spec_size <- metadata$parameters$spec_size
  window_length <- metadata$parameters$window_length
  dynamic_range <- metadata$parameters$dynamic_range
  frequency_resolution <- metadata$parameters$frequency_resolution
  freq_range <- c(metadata$parameters$freq_range_low, metadata$parameters$freq_range_high)
  img_rows <- metadata$parameters$img_rows
  img_cols <- metadata$parameters$img_cols
  input_shape <- c(metadata$parameters$img_rows, metadata$parameters$img_cols, 1)
  num_classes <- length(unique(metadata$classes$name))
  time_step_size <- (1 - as.numeric(metadata$parameters$overlap)) * as.numeric(metadata$parameters$window_length)
  class_labels <- as.character(metadata$classes$name)
  
  ##### 
  print("parametros ok")
  
  
  fileName <- list.files(file_path, recursive = recursive, pattern="wav|WAV")
  if(recursive == TRUE) fileName <- paste0("/", fileName)
  
  #####
  print("rec files ready")
  
  size <- length(fileName)
  
  model <- load_model_hdf5(model_path)
  
  #####
  print("rec files and model ready")
  
  
  for(i in seq(fileName)){
    try({

      
      
      
      
      
      
      
      
      
      
      
      
      morc <- import_audio(path = paste0(file_path, fileName[i]),
                           low = freq_range[1], high = freq_range[2])
      
      if(recursive == TRUE) morc$file_name_full_path <- fileName[i]
      sound_peaks <- peaks(morc, win_size = win_size) #esta funcao com tempoexpandido nao da os chuncks todos
      
      
      ######
      calls <- peaks2spec_shiny(morc, sound_peaks, spec_size = spec_size, 
                                window_length = window_length, 
                                frequency_resolution = frequency_resolution, 
                                time_step_size = time_step_size,
                                dynamic_range = dynamic_range,
                                freq_range = freq_range)
      
      ######
      
      out <- classify_calls(calls, sound_peaks, model = model)
      out_tidy <- tidy_output(out, class_labels = class_labels, 
                              remove_noise = remove_noise, min_dist = win_size) #remove_noise tem de ser umparametro de input
      
      
      png_file <- ifelse(save_png == T,  paste0(out_dir, morc$file_name, ".png"),
                         NA)
      
      
      
      out_peaks <- save_output_shiny(output = out_tidy, bat_recording = morc,
                               out_file = paste0(out_dir, out_file),#### ajustar os nomes dos out_files para ter um .csv e outro .sqlite3
                               png_file = png_file,
                               plot2console = plot2console,
                               recursive = recursive)

      try({
        if(save_spec == T){
          out_spec <- calls$spec_calls[as.numeric(row.names(out_tidy)),]
          save(out_spec, out_peaks, file = paste0(out_dir, morc$file_name, ".RDATA")) 
        }
      })
      
      
    })
    #####
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0( i, " of ", length(fileName))
      updateProgress(detail = text)
    }
    #####
    if (i %% 10 == 0) print(paste0(i," of ", size))
  }
}