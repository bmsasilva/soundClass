##### Function to read RDATA #####
# This function, borrowed from http://www.r-bloggers.com/safe-loading-of-rdata-files/, 
# load the Rdata into a new environment to avoid side effects
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

model_path <- "/home/bruno/packages_test_data/recordings/fitted_model.hdf5"
file_path <- "/home/bruno/packages_test_data/recordings/"
csv_file <- "output.csv"
model_metadata_path = "/home/bruno/packages_test_data/recordings/fitted_model_metadata.RDATA"

env <- LoadToEnvironment(model_metadata_path)
metadata <- env[[names(env)[1]]]
rm(env)

# # for shinny
# metadata <- reactive({ # reactive com a lista importada no rdata
#   if(length(input$metadata_path) > 1){ 
#     file_selected <- parseFilePaths(roots, input$metadata_path)
#     env <- LoadToEnvironment(as.character(file_selected$datapath))
#     metadata <- env[[names(env)[1]]]
#     rm(env)
#     return(metadata)
#   }
# })



auto_id_shiny(model_path, 
                          metadata,
                          file_path, 
                          csv_file, 
                          out_dir = "./", 
                          save_spec = T,
                          save_png = T, 
                          class_labels, 
                          win_size = 50, 
                          plot2console = F, 
                          recursive = FALSE)


auto_id_shiny <- function(model_path, 
                          metadata,
                          file_path, 
                          csv_file, 
                          out_dir = "./", 
                          save_spec = T,
                          save_png = T, 
                          class_labels, 
                          win_size = 50, 
                          plot2console = F, 
                          recursive = FALSE){
  
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
  
  
  
  fileName <- list.files(file_path, recursive = recursive, pattern="wav|WAV")
  if(recursive == TRUE) fileName <- paste0("/", fileName)

  size <- length(fileName)
  
  model <- load_model_hdf5(model_path)
  
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
                              remove_noise = F, min_dist = 40) #min_dist tem de ser umparametro de input
      
      
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
