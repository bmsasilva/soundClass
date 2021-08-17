#' Convert sound to spectrogram
#' @title Sound to spectrogram
#' @description Convert sound to spectrogram
#' @param recording Object of class "rc"
#' @param sound_peaks Peaks detected in recording samples
#' @param spec_size Spectrogram size in ms
#' @param window_length Moving window length to create the spectrogram in ms
#' @param frequency_resolution Spectrogram frequency resolution with higher
#' numbers meaning better resolution. Specifically, for any integer X provided,
#' 1/X the analysis bandwidth (as determined by the number of samples
#' in the analysis window) will be used. Note that this greatly impacts
#' processing time, so adjust with care!
#' @param time_step_size Moving window step in ms
#' @param dynamic_range Threshold of minimum intensity values to show
#' in the spectrogram
#' @param freq_range Frequency range of the spectrogram. Vector with two values,
#' refering to the minimum and maximum frequency to show in the spectrogram
#' @usage peaks2spec(recording, sound_peaks, spec_size = NA, window_length = NA,
#' frequency_resolution = NA, time_step_size = NA, dynamic_range = NA,
#' freq_range = NA)
#' @return an object of class "calls". This object is a list
#' with the following components:
#' \itemize{
#'  \item spec_calls -- matrix with one spectrogram per row
#'  \item img_cols -- number of columns in each spectrogram
#'  \item img_rows -- number of rows in each spectrogram
#' }
#' @author Bruno Silva
#' @keywords internal

peaks2spec <- function(recording, sound_peaks, spec_size = NA,
                       window_length = NA, frequency_resolution = NA,
                       time_step_size = NA, dynamic_range = NA,
                       freq_range = NA) {
  if (!is_rc(recording)) {
    stop("Input object must be of class recording. Use
      import_audio() as constructor.", call. = FALSE)
  }
  if (!is.vector(sound_peaks)) {
    stop("Parameter sound_peaks must be numeric", call. = FALSE)
  }

  fs <- recording$fs
  sound_samples <- recording$sound_samples
  tx <- recording$tx
  input_shape <- c(
    spec_size / time_step_size,
    (freq_range[2] - freq_range[1]) * frequency_resolution
  )

  windows <- matrix(NA,
    nrow = length(sound_peaks),
    ncol = ms2samples(spec_size, fs, tx)
  )
  half_window <- ms2samples(spec_size / 2, fs, tx)

  if (sound_peaks[1] < ms2samples(spec_size / 2, fs, tx)) {
    sound_peaks[1] <- ms2samples(spec_size / 2, fs, tx) + 100
  }

  if (sound_peaks[length(sound_peaks)] > length(sound_samples) - ms2samples(spec_size / 2, fs, tx)) {
    sound_peaks[length(sound_peaks)] <- length(sound_samples) - ms2samples(spec_size / 2, fs, tx) - 100
  }

  for (i in seq(length(sound_peaks))) {
    windows[i, ] <- sound_samples[(sound_peaks[i] - half_window):
    (sound_peaks[i] + half_window - 1)]
  }

  n_calls <- length(windows[, 1])
  spec_df <- matrix(NA, nrow = n_calls, ncol = input_shape[1] * input_shape[2])
  fmaxe_df <- c()
  for (k in seq_len(n_calls)) {
    sound_samples_xs <- windows[k, ]

    spec2 <- Spectrogram(
      Audio = as.numeric(sound_samples_xs),
      norm = 150,
      SamplingFrequency = fs * tx,
      WindowLength = window_length,
      FrequencyResolution = frequency_resolution,
      TimeStepSize = time_step_size,
      DynamicRange = dynamic_range,
      WindowType = "hanning",
      plot = F
    )

    filt <- c(
      which(as.numeric(colnames(spec2)) < freq_range[1] * 1000),
      which(as.numeric(colnames(spec2)) > freq_range[2] * 1000)
    )
    spec2_filt <- spec2[, -filt]
    spec2_filt <- spec2_filt + dynamic_range
    dim(spec2_filt)

    fmaxe <- dimnames(spec2)[[2]][which(spec2 == max(spec2), arr.ind = TRUE)[2]]

    while (dim(spec2_filt)[1] > input_shape[1]) spec2_filt <- spec2_filt[-1, ]
    while (dim(spec2_filt)[2] > input_shape[2]) spec2_filt <- spec2_filt[, -c(length(spec2_filt[1, ]))]

    spec <- denoise(spec2_filt)
    spec[is.na(spec)] <- 0
    spec_df[k, ] <- as.numeric(r(spec))
    fmaxe_df[k] <- as.numeric(fmaxe)
  }

  structure(list(
    spec_calls = spec_df, img_cols = input_shape[1],
    img_rows = input_shape[2], fmaxe = fmaxe_df
  ), class = "calls")
}
