#' Detect peaks in recordings
#' @title Detect energy peaks in recordings
#' @description Splits the recording in segments using a pre-defined window size
#' and detects the temporal position of the peak (event with most energy) in
#' each window.
#' @param recording Object of class "rc"
#' @param win_size Window size in ms to search peaks. Sound is divided into
#' equal size chunks of win_size duration and one peak is identified
#' in each chunk.
#' @param plot If TRUE plots the sound_samples with the peaks identified
#' @usage peaks(recording, win_size = 40, plot = F)
#' @return Vector with the temporal position of the identified peaks in samples
#' @author Bruno Silva
#' @keywords internal
#' @noRd

peaks <- function(recording, win_size = 40, plot = F) {
  if (!is_rc(recording)) {
    stop("Recording object must be of class rc. Use
      import_audio() as constructor.", call. = FALSE)
  }

  fs <- recording$fs
  sound_samples <- recording$sound_samples
  tx <- recording$tx
  len_sound_ms <- length(sound_samples) / (fs * tx / 1000)
  band <- len_sound_ms / win_size

  oscilo_smooth <- sound_samples %>%
    abs() %>%
    zoo::rollmean(100) %>%
    stats::approx(n = length(sound_samples) / 200)

  pks <- oscilo_smooth %>%
    unlist() %>%
    matrix(ncol = 2) %>%
    seewave::localpeaks(plot = plot, band = band)

  pks <- floor(pks[, 1])
  pks <- pks[-1]
  pks <- pks[-length(pks)]

  return(pks)
}
