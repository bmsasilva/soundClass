#' Buttterworth filter
#' @title Apply a buttterworth filter to a recording
#' @description  Apply a buttterworth filter to a recording
#' using the function signal::butter()
#' @param sound_samples Vector. Sound samples
#' @param low Minimum frequency in kHz for the butterworth filter
#' @param high Maximum frequency in kHz for the butterworth filter
#' @param order Butterworth fiter order
#' @usage butter_filter(sound_samples, low, high, fs, tx)
#' @return the filtered sound samples
#' @author Bruno Silva
#' @export

butter_filter <- function(sound_samples, low = NA, high = NA, fs, tx, order = 12){

  if(!is.na(low)) {
    limit_low <- low*1000 / (fs * tx / 2)
    bt_low <- signal::butter(order, limit_low, type = "high")
    sound_samples <- as.integer(signal::filter(bt_low, sound_samples))
  }

  if(!is.na(high)){
    limit_high <- high*1000 / (fs * tx / 2)
    bt_high <- signal::butter(order, limit_high, type = "low")
    sound_samples <- as.integer(signal::filter(bt_high, sound_samples))
  }
  return(sound_samples)
}

