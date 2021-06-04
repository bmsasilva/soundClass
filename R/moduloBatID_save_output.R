#' Output analysis results
#' @title Save output
#' @description Writes and plots to csv and png files the results of the classification
#' @param output Object of class "tidy_output"
#' @param bat_recording Object of class "bat_recording"
#' @param csv_file Character. Name of file to write analysis results. If
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

save_output <- function(output, bat_recording, csv_file = NA, png_file = NA,
                        plot2console = F, recursive = FALSE){
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
    dev.off()
  }

  if (!is.na(csv_file)){
    if(length(output[,1]) > 0) {
      aux <- cbind(file_name, get_mode(output$spe), round(mean(output$prob), 2),
                   round(mean(output$fmaxe), 2), length(output$peaks))
    } else {
      aux <- cbind(file_name, "Noise", NA, NA, NA)
    }
    write.table(aux, file = csv_file, append = T, col.names = F, row.names = F, sep = ",", dec = ".")
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
