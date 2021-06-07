#' Output analysis results
#' @title Save output
#' @description Writes and plots to csv and png files the results of the classification
#' @param output Object of class "tidy_output"
#' @param bat_recording Object of class "bat_recording"
#' @param out_file Character. Name of databse file to write analysis results. If
#' NA, no analysis results file is created
#' @param png_file Character. Name of file for plotting spectrogram with call
#' labels. If NA, no spectrogram file is created
#' @param plot2console If TRUE plots the spectrogram with the peaks identified
#' to the console. Please be advised that if TRUE the analysis takes
#' considerably more time.
#' @param recursive Logical. Is the analysis beeing processed in recursive mode?
#' @usage save_output(output, bat_recording)
#' @return Add row to csv file, plot spectrogram to console and save spectrogram
#' in file
#' @author Bruno Silva
#' @export

save_output_shiny <- function(output, bat_recording, out_file = NA, png_file = NA,
                        plot2console = F, recursive = FALSE){
  
  # Criar bd de output se nao existir ainda
  if(!file.exists(paste0(out_file,".sqlite3"))) 
    create_db(out_file,"", type = "id")
  
  # Criar csv de output se nao existir ainda
  if(!file.exists(paste0(out_file,".csv")))
    write.table(data.frame(recording = character(),
                           label_position = numeric(),
                           label_class = character(),
                           probability = numeric(),
                           fmaxe = numeric(),
                           n_calls = numeric()),
                col.names = T, 
                file=paste0(out_file,".csv"))

  fs <- bat_recording$fs
  sound_samples <- bat_recording$sound_samples
  tx <- bat_recording$tx
 if(recursive == TRUE){
   file_name <- bat_recording$file_name_full_path
 } else{
  file_name <- bat_recording$file_name
 }
  if (!is.na(png_file)){
    png(filename = png_file)
    spec2 <- Spectrogram(Audio = as.numeric(sound_samples),
                         norm = 150,
                         col = batsound, #gray.colors(255, start = 0.1, end = 0.8, gamma = 0.1),
                         SamplingFrequency = fs * tx,
                         WindowLength = 3, # em milisegundos
                         FrequencyResolution = 3, # valor normal e 2
                         TimeStepSize = 0.25  ,
                         DynamicRange = 85,
                         WindowType = "hanning",
                         plot = T,
                         PlotFast = T)
    if(length(output[,1]) > 0){
      abline(v= ms2samples(output$peaks, fs = fs, tx = tx,  inv = TRUE), col = alpha(rgb(0,0,0), 0.2))
      text(x=ms2samples(output$peaks, fs = fs, tx = tx,  inv = TRUE),
           y=5, labels = output$spe, col="red", srt = 45, cex = 0.8)
    }
    dev.off()
  }
  
  
  ####### guarda os pulsos em base de dados mas continua a exportar um csv com os resultados finais da id

  if (!is.na(out_file)){
    if(length(output[,1]) > 0) {
      aux_csv <- cbind(file_name, output$peaks, get_mode(output$spe), round(mean(output$prob), 2),
                   round(mean(output$fmaxe), 2), length(output$peaks))
      
      
      
      aux_db <- data.frame("recording" = file_name,
                           "label_position" = output$peaks,
                           "label_class" = get_mode(output$spe),
                           "probability" = round(output$prob, 2),
                           "fmaxe" = round(output$fmaxe, 2))
      
      add_record(path = paste0(out_file,".sqlite3"),
                 df = aux_db)
    } else {
      aux_csv <- cbind(file_name, NA, "Noise", NA, NA, NA)
      
    }
    
    
    write.table(aux_csv, file = paste0(out_file,".csv"),
                append = T, col.names = T, row.names = F, sep = ",", dec = ".")
   


  }

  if (plot2console == T){
    spec2 <- Spectrogram(Audio = as.numeric(sound_samples),
                         norm = 150,
                         col = gray.colors(255, start = 0.1, end = 0.8, gamma = 0.1),
                         SamplingFrequency = fs * tx,
                         WindowLength = 3, # em milisegundos
                         FrequencyResolution = 3, # valor normal e 2
                         TimeStepSize = 0.25  ,
                         DynamicRange = 85,
                         WindowType = "hanning",
                         plot = T,
                         PlotFast = T)
    if(length(output[,1]) > 0){
      abline(v= ms2samples(output$peaks, fs = fs, tx = tx,  inv = TRUE), col ='blue')
      text(x=ms2samples(output$peaks, fs = fs, tx = tx,  inv = TRUE),
           y=5, labels = output$spe, col="red", srt = 45, cex = 0.8)
    }

  }
  return(output$peaks)
}
