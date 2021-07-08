#' Buttterworth filter
#' @title Apply a buttterworth filter to sound samples
#' @description Apply a buttterworth filter, high pass or/and low pass,
#' to sound samples. Based on the function \link[signal]{butter}
#' @param sound_samples Vector. Sound samples to filter
#' @param low Minimum frequency in kHz for the butterworth filter
#' @param high Maximum frequency in kHz for the butterworth filter
#' @param fs Sampling frequency
#' @param tx Expanded time factor
#' @param order Filter order
#' @usage butter_filter(sound_samples, low = NA, high = NA, fs, tx, order = 12)
#' @examples sound <- runif(22000, min = -10000, max = 10000) # 1s sound sample
#' sound_filt <- butter_filter(sound, low = 4, high = 8,
#'                             fs = 22000, tx = 1, order = 10) # filter sound
#' Spectrogram(sound, 22000) # original spectrogram
#' Spectrogram(sound_filt, 22000) # filtered spectrogram
#' @return A vector with the filtered sound samples
#' @author Bruno Silva
#' @export

butter_filter <- function(sound_samples, low = NA, high = NA,
                          fs, tx, order = 12) {
  if (!is.na(low)) {
    if (!is.numeric(low)) stop("Parameter low must be numeric", call. = FALSE)
    limit_low <- low * 1000 / (fs * tx / 2)
    bt_low <- signal::butter(order, limit_low, type = "high")
    sound_samples <- as.integer(signal::filter(bt_low, sound_samples))
  }

  if (!is.na(high)) {
    if (!is.numeric(high)) stop("Parameter high must be numeric", call. = FALSE)
    limit_high <- high * 1000 / (fs * tx / 2)
    bt_high <- signal::butter(order, limit_high, type = "low")
    sound_samples <- as.integer(signal::filter(bt_high, sound_samples))
  }

  return(as.numeric(sound_samples))
}
