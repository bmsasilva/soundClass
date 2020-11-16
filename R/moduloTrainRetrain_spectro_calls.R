spectro_calls <- function(files_path, db_path, version){ # nota: file_path tem de acabar em /

  if(version == "v1"){
    # all species # smaller image
    size <- 20 # ms
    window_length <- 1 # em milisegundos
    frequency_resolution <- 1 # valor normal e 2
    time_step_size <- 0.25
    dynamic_range <- 90
    freq_range <- c(10, 125) #hertz
    input_shape <- c(size / time_step_size, 115)
  } else if(version == "v2"){
    # without rhinolophus # smaller image
    size <- 20 # ms
    window_length <- 1 # em milisegundos
    frequency_resolution <- 1 # valor normal e 2
    time_step_size <- 0.25
    dynamic_range <- 90
    freq_range <- c(10, 80) #khertz
    input_shape <- c(size / time_step_size, 70)
  } else if(version == "v3"){
    # all species  # better resolution
    size <- 20 # ms
    window_length <- 1 # em milisegundos
    frequency_resolution <- 2 # valor normal e 2
    time_step_size <- 0.1
    dynamic_range <- 90
    freq_range <- c(10, 125) #hertz
    input_shape <- c(size / time_step_size, 230)
  } else if(version == "v4"){
    # without rhinolophus # better resolution
    size <- 20 # ms
    window_length <- 1 # em milisegundos
    frequency_resolution <- 2 # valor normal e 2
    time_step_size <- 0.1
    dynamic_range <- 90
    freq_range <- c(10, 80) #hertz
    input_shape <- c(size / time_step_size, 140)
  } else {
    stop("No valid version selected", call. = FALSE)
  }

  # get table from database
  conn <- dplyr::src_sqlite(db_path, create = FALSE) # open connection
  query <- dplyr::tbl(conn, "rec_labels")
  db_table <- dplyr::collect(query)
  audio_files <- unique(db_table$recording)

  spec_image <- matrix(NA, ncol = input_shape[1]*input_shape[2])
  label <- c()
  for(i in seq(audio_files)){
try({
    sound_peaks <- pull(db_table[db_table$recording == audio_files[i], "calls_samples"], 1)

    ## Tenho de por isto porque alguns nomes na bd tem .wav e outros nao
    if(grepl(".wav", audio_files[i])){
      name <-paste0(files_path, audio_files[i])
    } else{
      name <-paste0(files_path, audio_files[i], ".wav")
    }

    morc <- import_audio(name, low = 10, high = 125)
    calls <- peaks2spec(morc, sound_peaks, frequency_bin=T,
                        time_bin=F, version = "v2")# desta funcao tem de sair um vector com o label que esta na bd

    # Como sai uma matriz da peaks2spec vou usar rbind() mas sei que tenho trocar
    spec_image <- rbind(spec_image, calls$spec_calls)
    label <- c(label, pull(db_table[db_table$recording == audio_files[i], "spe"], 1))
    print(i)
    print(audio_files[i])
})#final try
  }
  spec_image <- spec_image[-1,] # para eliminar a linha de NA por causa do rbind
  return(list(spec_image, label))
}



