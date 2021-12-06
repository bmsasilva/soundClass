#' Detect energy peaks in non-relevant recordings
#' @title Detect energy peaks in non-relevant recordings
#' @description Detects the temporal position of the desired number of
#' energy peaks in a recording of non-relevant events.
#' @param recording Object of class "rc"
#' @param nmax Integer indicating the maximum number of peaks to detect in
#' the recording.
#' @param plot Logical. If TRUE a plot showing the peak(s) is returned.
#' @usage find_noise(recording, nmax = 1, plot = F)
#' @examples
#' # Create a sample wav file in a temporary directory
#' recording <- tuneR::noise(duration = 44100)
#' temp_dir <- tempdir()
#' rec_path <- file.path(temp_dir, "recording.wav")
#' tuneR::writeWave(recording, filename = rec_path)
#' # Import the sample wav file
#' new_rec <- import_audio(rec_path, butt = FALSE, tx = 1)
#' find_noise(new_rec, nmax = 1, plot = F)
#' file.remove(rec_path)
#' @return A vector with the temporal position of the
#' identified peak(s), in samples.
#' @author Bruno Silva
#' @export

find_noise <- function(recording, nmax = 1, plot = F) {

  if (!is_rc(recording)) {
    stop("Recording object must be of class 'rc'. Use
      import_audio() as constructor.", call. = FALSE)
  }

  fs <- recording$fs
  tx <- recording$tx
  sound_samples <- recording$sound_samples

  oscilo_smooth <- sound_samples %>%
    abs() %>%
    zoo::rollmean(100) %>%
    stats::approx(n = length(sound_samples) / 200)

  pks <- oscilo_smooth %>%
    unlist() %>%
    matrix(ncol = 2) %>%
    seewave::fpeaks(plot = plot, nmax = nmax, f = fs * tx)

  pks <- floor(pks[, 1])

  return(pks)
}
