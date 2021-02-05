#' @title Denoise and standardize a spectrogram matrix
#' @description Applies noise removal to a spectrogram matrix, by subtracting
#' the mean of each frequency column to all rows of that column. Please note that
#' columns must represent frequency and rows must represent time.Energy values are afterwards standartized
#' to min = 0 and max = 1 by dividing by maximum energy value of the spectrogram matrix.
#' @param spec Spectrogram matrix 
#' @usage denoise(spec)
#' @return A denoised spectrogram matrix, with standardized energy values 
#' between 0 and 1
#' @keywords internal
#' @author Bruno Silva

denoise <- function(spec){
  ncol <- dim(spec)[2] 
  nrow <- dim(spec)[1] 
  mean_frequency_bin <- apply(spec, 2, mean)
  denoise_frequency_bin <- matrix(mean_frequency_bin, nrow, ncol, byrow = T) 
  spec <- spec - denoise_frequency_bin
  spec <- spec / max(spec)
  spec[spec < 0] <- 0
  return(spec)
}

