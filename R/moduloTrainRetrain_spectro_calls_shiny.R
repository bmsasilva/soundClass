#' Generate spectrograms from labels
#' @title Generates spectrograms from recording labels. 
#' @description Generates spectrograms from recording's labels for
#' classification purposes. The spectrogram parameters are user defined
#' and should be selected depending on the type of sound event to classify. Three  
#' sets of parameters for the spectrograms are provided, with values prepared for
#' classification of vocalizations of european bat species (one set for all species
#' and another excluding Rhinolophus species) and birds. Aditional sets of parameters 
#' for other vocalizations or sound events may also be provided by the user.
#' @param files_path Path for the folder containing sound recordings
#' @param db_path Path for the database of recording labels created with the 
#' shinny app provided in the package
#' @param parameters Parameters for the spectrograms. A list with: 
#' spectrogram size in ms (spec_size - integer),
#' moving window length used to create the spectrogram in ms (window_length),
#' spectrogram frequency resolution in kHz (frequency_resolution - integer),
#' moving window step in ms (time_step_size - positive numeric),
#' threshold intensity value (dynamic_range - integer),
#' frequency range of the spectrogram (freq_range - integer vector).
#' The predefined values are indicated with: "v1", "v2", "v3".
#' @usage spectro_calls(files_path, db_path, parameters)
#' @return A list with the spectrogram and the respective label
#' @author Bruno Silva
#' @export

spectro_calls_shiny <- function(files_path, db_path, spec_size = NA, window_length = NA, # nota: files_path tem de acabar em /
                                frequency_resolution = NA, time_step_size = NA, dynamic_range = NA,
                                freq_range = NA) { 
  # parameters tem de ser uma lista para aceitar os varios valores
  
  
    # spec_size <- 20 # ms
    # window_length <- 1 # em milisegundos
    # frequency_resolution <- 1 # valor normal e 2
    # time_step_size <- 0.25
    # dynamic_range <- 90
    # freq_range <- c(10, 80) #khertz
     input_shape <- c(spec_size / time_step_size, 
                     (freq_range[2]-freq_range[1])*frequency_resolution)
 
    
  
  # get table from database
  conn <- dplyr::src_sqlite(db_path, create = FALSE) # open connection
  query <- dplyr::tbl(conn, "labels")
  db_table <- dplyr::collect(query)
  audio_files <- unique(db_table$recording)
  
  spec_image <- matrix(NA, ncol = input_shape[1]*input_shape[2])
  label <- c()
  for(i in seq(audio_files)){
    try({
      sound_peaks <- dplyr::pull(db_table[db_table$recording == audio_files[i], "label_position"], 1)
      
      ## Tenho de por isto porque alguns nomes na bd tem .wav e outros nao
      if(grepl(".wav", audio_files[i])){
        name <-paste0(files_path, audio_files[i])
      } else{
        name <-paste0(files_path, audio_files[i], ".wav")
      }
      
      morc <- import_audio(name, low = 10, high = 125)
      calls <- peaks2spec_shiny(morc, sound_peaks, spec_size, window_length, 
                                frequency_resolution, time_step_size, dynamic_range,
                                freq_range)# desta funcao tem de sair um vector com o label que esta na bd
      
      # Como sai uma matriz da peaks2spec vou usar rbind() mas sei que tenho trocar
      spec_image <- rbind(spec_image, calls$spec_calls)
      label <- c(label, pull(db_table[db_table$recording == audio_files[i], "label_class"], 1))
      print(i)
      print(audio_files[i])
    })#final try
  }
  spec_image <- spec_image[-1,] # para eliminar a linha de NA por causa do rbind
  
  # criar data.frame com os parametros dos espectrogramas
  parameters <- data.frame (spec_size = spec_size, 
     window_length = window_length,
     frequency_resolution = frequency_resolution,
     overlap = 1 - time_step_size,
     dynamic_range = dynamic_range,
     freq_range_low = freq_range[1],
    freq_range_high = freq_range[2],
    img_rows = input_shape[2],
    img_cols = input_shape[1])
  
  return(list(spec_image, label, parameters))
}