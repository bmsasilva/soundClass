
auto_id_shiny <- function(model_path, file_path, csv_file, out_dir = "./", 
                          save_spec = T,
                          save_png = T, 
                          class_labels, 
                          win_size = 50, spec_size = NA, window_length = NA, 
                          frequency_resolution = NA, time_step_size = NA, dynamic_range = NA,
                          freq_range = NA, plot2console = F, low = 10, high = 120, recursive = FALSE){
  
  fileName <- list.files(file_path, recursive = recursive, pattern="wav|WAV")
  if(recursive == TRUE) fileName <- paste0("/", fileName)
  size <- length(fileName)
  model <- load_model_hdf5(model_path)
  
  for(i in seq(fileName)){
    try({
      morc <- import_audio(path = paste0(file_path, fileName[i]),
                           low = low, high = high)
      if(recursive == TRUE) morc$file_name_full_path <- fileName[i]
      sound_peaks <- peaks(morc, win_size = win_size)
      
      
      ######
      calls <- peaks2spec_shiny(morc, sound_peaks, spec_size = NA, window_length = NA, 
                                   frequency_resolution = NA, time_step_size = NA, dynamic_range = NA,
                                   freq_range = NA)
      
      ######
      
      out <- classify_calls(calls, sound_peaks, model = model)
      out_tidy <- tidy_output(out, class_labels = class_labels, min_dist = 40)
      
     
      png_file <- ifelse(save_png == T,  paste0(out_dir, morc$file_name, ".png"),
                         NA)
      
      out_peaks <- save_output(output = out_tidy, bat_recording = morc,
                               csv_file = paste0(out_dir, csv_file),
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
    if (i %% 10 == 0) print(paste0(i," of ", size))
  }
}
