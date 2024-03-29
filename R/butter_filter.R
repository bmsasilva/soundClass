#' Butterworth filter
#' @title Apply a butterworth filter to sound samples
#' @description Apply a butterworth filter, high pass or/and low pass,
#' to sound samples. Based on the function \link[signal]{butter}
#' @param sound_samples Numeric vector with the sound samples to filter
#' @param low Numeric. Minimum frequency in kHz for the butterworth filter
#' @param high Numeric. Maximum frequency in kHz for the butterworth filter
#' @param fs Integer with the sampling frequency of the recording
#' @param tx Integer indicating the expanded time factor of the recording
#' @param order Integer indicating the filter order to apply to the recording
#' @usage butter_filter(sound_samples, low = NA, high = NA, fs, tx, order = 10)
#' @return A vector with the filtered sound samples
#' @author Bruno Silva
#' @keywords internal

butter_filter <- function(sound_samples, low = NA, high = NA,
                          fs, tx, order = 10) {

  if (!is.numeric(sound_samples)) stop("Sound samples must be numeric",
                                       call. = FALSE)
  if (!is.numeric(fs)) stop("Fs (sampling frequency) must be numeric",
                                       call. = FALSE)
  if (!is.numeric(tx)) stop("Tx (expanded time factor) must be numeric",
                            call. = FALSE)

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
