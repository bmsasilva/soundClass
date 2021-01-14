#' Convert time (in ms) to samples and vice-versa in sound files
#' @title Convert time (in ms) to samples
#' @description  Convert time (in ms) to samples
#' @param value time in ms or number of samples
#' @param fs sampling frequency in samples per second
#' @param tx time expansion factor
#' @param inv if TRUE converts time (in ms) to samples.
#' If FALSE converts samples to time (in ms)
#' @usage ms2samples(value, fs = 300000, tx = 1, inv = F)
#' @return if TRUE returns number of samples, if FALSE
#' returns time in ms
#' @author Bruno Silva
#' @export
ms2samples <- function(value, fs = 300000, tx = 1, inv = F){

  if(inv){

    ms <- floor( value / (fs * tx) * 1000)
    return(ms)
  } else {

  samples <- floor(fs * tx * value / 1000)
  return(samples)
  }
}
