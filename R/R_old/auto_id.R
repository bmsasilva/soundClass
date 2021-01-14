
auto_id <- function(model_path, file_path, csv_file, out_dir = "./", save_spec = T,
                    save_png = T, class_labels, win_size = 50, version = "v1"){

  fileName <- list.files(file_path, recursive = FALSE, pattern="wav|WAV")
  size <- length(fileName)
  model <- load_model_hdf5(model_path)

  for(i in seq(fileName)){
    try({
      morc <- import_audio(paste0(file_path, fileName[i]), low = 10, high = 125)
      sound_peaks <- peaks(morc, win_size = 50)
      calls <- peaks2spec(morc, sound_peaks, frequency_bin = T,
                          time_bin = F, version = version)
      out <- classify_calls(calls, sound_peaks, model = model) # inserir save pulso aqui??
      out_tidy <- tidy_output(out, class_labels = class_labels, min_dist = 40)
      png_file <- ifelse(save_png == T,  paste0(out_dir, fileName[i], ".png"),
                         NA)
      out_peaks <- save_output(out_tidy, morc,
                  csv_file = paste0(out_dir, csv_file),
                  png_file = png_file,
                  plot = T)## Se retirar este plot pode ser que demore menos a correr
      try({
      if(save_spec == T){
        out_spec <- calls$spec_calls[as.numeric(row.names(out_tidy)),]
          save(out_spec, out_peaks, file = paste0(out_dir, fileName[i], ".RDATA")) 
      }
      })
      
    })
    if (i %% 10 == 0) print(paste0(i," of ", size))
  }
}
