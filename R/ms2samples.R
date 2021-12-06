#' Convert between time and number of samples in sound files
#' @title Convert between time and number of samples in sound files
#' @description  Convert time to number of samples or vice-versa 
#' in sound files.
#' @param value Numeric representing time in ms or number of samples.
#' @param fs Integer. The sampling frequency in samples per second.
#' @param tx Integer. Time expansion factor.
#' @param inv Logical. If TRUE converts time to number of samples, if FALSE
#' number of samples to time.
#' @usage ms2samples(value, fs = 300000, tx = 1, inv = FALSE)
#' @examples ms2samples(150000, fs = 300000, tx = 1, inv = FALSE)
#' ms2samples(100, fs = 300000, tx = 1, inv = TRUE)
#' @return if inv = TRUE returns number of samples, if unv = FALSE
#' returns time in ms
#' @author Bruno Silva
#' @export

ms2samples <- function(value, fs = 300000, tx = 1, inv = FALSE) {
  if (inv) {
    ms <- floor(value / (fs * tx) * 1000)
    return(ms)
  } else {
    samples <- floor(fs * tx * value / 1000)
    return(samples)
  }
}
