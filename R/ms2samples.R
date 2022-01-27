#' @title Convert between time and number of samples in sound files
#' @description  Convert time to number of samples or vice versa
#' in sound files.
#' @param value Integer. Number of samples or time in ms.
#' @param fs Integer. The sampling frequency in samples per second.
#' @param tx Integer. Indicating the time expansion factor. If the
#' recording is not time expanded tx must be set to 1 (the default).
#' @param inv Logical. If TRUE converts time to number of samples, if FALSE
#' number of samples to time.
#' @usage ms2samples(value, fs = 300000, tx = 1, inv = FALSE)
#' @examples ms2samples(150000, fs = 300000, tx = 1, inv = FALSE)
#' ms2samples(100, fs = 300000, tx = 1, inv = TRUE)
#' @return Integer. If inv = TRUE returns number of samples, if inv = FALSE
#' returns time in ms.
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
