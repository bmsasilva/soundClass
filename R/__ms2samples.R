#' Convert between time and samples in sound files
#' @title Convert between time and samples in sound files
#' @description  Convert time to samples or vice-versa in sound files
#' @param value time in ms or number of samples
#' @param fs sampling frequency in samples per second
#' @param tx time expansion factor
#' @param inv if TRUE converts time to samples.
#' If FALSE converts samples to time
#' @usage ms2samples(value, fs = 300000, tx = 1, inv = F)
#' @return if TRUE returns number of samples, if FALSE
#' returns time in ms
#' @author Bruno Silva
#' @export

ms2samples <- function(value, fs = 300000, tx = 1, inv = F) {
  if (inv) {
    ms <- floor(value / (fs * tx) * 1000)
    return(ms)
  } else {
    samples <- floor(fs * tx * value / 1000)
    return(samples)
  }
}
