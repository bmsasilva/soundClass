#' @title Denoise and standartize spectrogram energy values by mean subtraction
#' @description Applies noise removal to a spectrogram matrix, by subtracting
#' the mean of the time or/and frequency bin to all pixels of that bin. Energy
#' values are afterwards converted to min = 0 and max = 1.
#' @param spec Spectrogram matrix 
#' @param frequency_bin Logical. If TRUE applies denoising by frequency bin, 
#' assuming columns represent frequency bins
#' @param time_bin Logical. If TRUE applies denoising by time bin, assuming 
#' rows represent frequency bins
#' @usage denoise_spec(spec, frequency_bin = T, time_bin = F)
#' @return A denoised matrix spectrogram, with standartized energy values 
#' between 0 and 1
#' @export
#' @author Bruno Silva

denoise_spec <- function(spec, frequency_bin = T, time_bin = F){
ncol <- length(colnames(spec)) # frequencia
 nrow <- length(rownames(spec)) # tempo

mean_frequency_bin <- apply(spec, 2, mean) # mean by frequency bin
mean_time_bin <- apply(spec, 1, mean) # mean by time bin

if (frequency_bin == T && time_bin == T){
  denoise_frequency_bin <- matrix(mean_frequency_bin, nrow, ncol, byrow = T) #matrix(mediana,nrow,ncol) ##Matriz com nrow e ncol com todas as linhas iguais
  denoise_time_bin <- matrix(mean_time_bin, nrow, ncol, byrow = F)
  spec <- spec - denoise_frequency_bin
  spec <- spec - denoise_time_bin
} else if (frequency_bin == T){
  denoise_frequency_bin <- matrix(mean_frequency_bin, nrow, ncol, byrow = T) #matrix(mediana,nrow,ncol) ##Matriz com nrow e ncol com todas as linhas iguais
  spec <- spec -  denoise_frequency_bin
} else {
  denoise_time_bin <- matrix(mean_time_bin, nrow, ncol, byrow = F)
  spec <- spec -  denoise_time_bin
}

spec <- spec / max(spec)
spec[spec < 0] <- 0
return(spec)
}

