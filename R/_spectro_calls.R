#' Generate spectrograms from labels
#' @title Generates spectrograms from recording labels.
#' @description Generates spectrograms from recording's labels for
#' classification purposes. The spectrogram parameters are user defined
#' and should be selected depending on the type of sound event to classify.
#' @param files_path Path for the folder containing sound recordings
#' @param updateProgress Progress bar only to be used inside shiny
#' @param db_path Path for the database of recording labels created with the
#' shinny app provided in the package
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
#' @usage spectro_calls(files_path, updateProgress,
#' db_path, spec_size = NA, window_length = NA,
#' frequency_resolution = NA, time_step_size = NA, dynamic_range = NA,
#' freq_range = NA)
#' @return A list with the spectrogram and the respective label
#' @author Bruno Silva
#' @export

spectro_calls <- function(files_path, updateProgress,
                          db_path, spec_size = NA, window_length = NA,
                          frequency_resolution = NA, time_step_size = NA,
                          dynamic_range = NA, freq_range = NA) {
  input_shape <- c(
    spec_size / time_step_size,
    (freq_range[2] - freq_range[1]) * frequency_resolution
  )

  conn <- dplyr::src_sqlite(db_path, create = FALSE)
  query <- dplyr::tbl(conn, "labels")
  db_table <- dplyr::collect(query)
  audio_files <- unique(db_table$recording)

  spec_image <- matrix(NA, ncol = input_shape[1] * input_shape[2])
  label <- c()
  for (i in seq(audio_files)) {
    try({
      sound_peaks <- dplyr::pull(db_table[
        db_table$recording == audio_files[i],
        "label_position"
      ], 1)

      if (grepl(".wav", audio_files[i])) {
        name <- paste0(files_path, audio_files[i])
      } else {
        name <- paste0(files_path, audio_files[i], ".wav")
      }

      morc <- import_audio(name, low = freq_range[1], high = freq_range[2])
      calls <- peaks2spec(
        morc, sound_peaks, spec_size, window_length,
        frequency_resolution, time_step_size,
        dynamic_range,
        freq_range
      )

      spec_image <- rbind(spec_image, calls$spec_calls)
      label <- c(label, pull(db_table[
        db_table$recording == audio_files[i],
        "label_class"
      ], 1))

      if (is.function(updateProgress)) {
        text <- paste0(i, " of ", length(audio_files))
        updateProgress(detail = text)
      }
    })
  }
  spec_image <- spec_image[-1, ]

  parameters <- data.frame(
    spec_size = spec_size,
    window_length = window_length,
    frequency_resolution = frequency_resolution,
    overlap = (1 - time_step_size) * window_length,
    dynamic_range = dynamic_range,
    freq_range_low = freq_range[1],
    freq_range_high = freq_range[2],
    img_rows = input_shape[2],
    img_cols = input_shape[1],
    num_classes = length(unique(label))
  )

  return(list(spec_image, label, parameters))
}
