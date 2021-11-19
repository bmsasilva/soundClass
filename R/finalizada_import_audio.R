#' Import a recording
#' @title Import a recording
#' @description  Import a "wav" recording using \link[tuneR]{readWave}
#' and create a S3 object of class "rc". If the recording is stereo it is
#' #' converted to mono by keeping the channel with overall higher amplitude
#' @param path Full path to the recording
#' @param butt Logical. If TRUE filters the recording with a 12th order
#' filter. The filter is applied twice to better cleaning of the recording
#' @param low Minimum frequency in kHz for the butterworth filter
#' @param high Maximum frequency in kHz for the butterworth filter
#' @param tx Time expanded. Only used in recorders specifically intended for
#' bat recordings. Can take the values "auto" or any numeric value. If the
#' recording is not time expanded tx must be set to 1 (the default). If it's
#' time expanded the numeric value correponding to the time expansion should
#' be indicated or "auto" should be selected. If tx = "auto" the function
#' assumes that sampling rates < 50kHz correponds to
#' tx = 10 and > 50kHz to tx = 1.
#' @usage import_audio(path, butt = TRUE, low, high, tx = 1)
#' @examples
#' # Create a sample wav file in a temporary directory
#' recording <- tuneR::sine(440)
#' temp_dir <- tempdir()
#' rec_path <- file.path(temp_dir, "recording.wav")
#' tuneR::writeWave(recording, filename = rec_path)
#' # Import the sample wav file
#' new_rec <- import_audio(rec_path, low = 1, high = 50, tx = 1)
#' new_rec
#' file.remove(rec_path)
#' @return an object of class "rc". This object is a list
#' with the following components:
#' \itemize{
#'   \item sound_samples -- sound samples of the recording
#'   \item file_name -- name of the recording
#'   \item file_time -- time of modification of the file (indicated for
#' Pettersson Elektronic detectors, for other manufactures creation time should
#' be preferable but it's not implemented yet)
#'   \item fs -- sample frequency
#'   \item tx -- expanded time factor
#' }
#' @author Bruno Silva
#' @export

import_audio <- function(path, butt = TRUE, low, high, tx = 1) {
  if (!is.logical(butt)) stop("Parameter butt must be TRUE or FALSE",
                              call. = FALSE)
  if (!is.character(path)) stop("Parameter path must be character",
                                call. = FALSE)
  if (isTRUE(butt)){
  if (!is.numeric(low)) stop("Parameter low must be numeric",
                             call. = FALSE)
  if (!is.numeric(high)) stop("Parameter high must be numeric",
                              call. = FALSE)
  }

  data <- tuneR::readWave(path)

  file_name <- strsplit(path, "/|[\\]")[[1]]
  file_name <- file_name[length(file_name)]
  file_name <- strsplit(as.character(file_name), "\\.")[[1]][1]
  file_time <- file.info(path)$mtime
  fs <- data@samp.rate
  if (tx == "auto" & fs < 50000) {
    tx <- 10
  } else if (tx == "auto" & fs >= 50000) {
    tx <- 1
  } else {
    tx <- tx
  }

  if (data@stereo) {
    if (sum(abs(data@left)) >= sum(abs(data@right))) {
      sound_samples <- data@left
    } else {
      sound_samples <- data@right
    }
  } else {
    sound_samples <- data@left
  }

  if (isTRUE(butt)) {
    limit_low <- low * 1000 / (fs * tx / 2)
    limit_high <- high * 1000 / (fs * tx / 2)
    bt_low <- signal::butter(10, limit_low, type = "high")
    bt_high <- signal::butter(10, limit_high, type = "low")
    sound_samples <- as.integer(signal::filter(bt_low, sound_samples))
    sound_samples <- as.integer(signal::filter(bt_high, sound_samples))

    sound_samples <- as.integer(signal::filter(bt_low, sound_samples))
    sound_samples <- as.integer(signal::filter(bt_high, sound_samples))
  }
  structure(list(
    sound_samples = as.numeric(sound_samples),
    file_name = file_name,
    file_time = file_time,
    fs = fs, tx = tx
  ),
  class = "rc"
  )
}
