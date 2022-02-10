#' @title Generate spectrograms from labels
#' @description Generate spectrograms from recording labels for
#' classification purposes. The spectrogram parameters are user defined
#' and should be selected depending on the type of sound event to classify.
#' @param files_path Character. Path for the folder containing sound recordings.
#' @param update_progress Progress bar only to be used inside shiny.
#' @param db_path Character. Path for the database of recording labels created
#' with the shinny app provided in the package.
#' @param spec_size Integer. Spectrogram size in ms.
#' @param window_length Numeric. Moving window length in ms.
#' @param frequency_resolution Integer. Spectrogram frequency resolution with
#' higher values meaning better resolution. Specifically, for any integer X
#' provided, 1/X the analysis bandwidth (as determined by the number of samples
#' in the analysis window) will be used. Note that this greatly impacts
#' processing time, so adjust with care!
#' @param overlap Percentage of overlap between moving windows. Accepts values
#' between 0.5 and 0.75.
#' @param dynamic_range Threshold of minimum intensity values to show
#' in the spectrogram. A value of 100 will typically be adequate for the
#' majority of the recorders. If this is set to NULL, no threshold is applied.
#' @param freq_range Frequency range of the spectrogram. Vector with two values,
#' referring to the minimum and maximum frequency to show in the spectrogram.
#' @param tx Time expanded. Only used in recorders specifically intended for
#' bat recordings. Can take the values "auto" or any numeric value. If the
#' recording is not time expanded tx must be set to 1 (the default). If it's
#' time expanded the numeric value corresponding to the time expansion should
#' be indicated or "auto" should be selected. If tx = "auto" the function
#' assumes that sampling rates < 50kHz corresponds to
#' tx = 10 and > 50kHz to tx = 1.
#' @param seed Integer. Define a custom seed for randomizing data.
#' @usage spectro_calls(files_path, update_progress = NA,
#' db_path, spec_size = NA, window_length = NA,
#' frequency_resolution = NA, overlap = NA,
#' dynamic_range = NA, freq_range = NA, tx = 1, seed = 1002)
#' @return A list with the following components:
#' \itemize{
#'   \item data_x -- an array with the spectrogram matrices
#'   \item data_y -- the labels for each matrix in one-hot-encoded format
#'   \item parameters -- the parameters used to create the matrices
#'   \item labels_df -- the labels with their respective numeric index
#' }
#' @author Bruno Silva
#' @export

spectro_calls <- function(files_path, update_progress = NA,
                          db_path, spec_size = NA, window_length = NA,
                          frequency_resolution = NA, overlap = NA,
                          dynamic_range = NA, freq_range = NA, tx = 1,
                          seed = 1002) {
  
  if(overlap < 0.5 | overlap > 0.75) 
    stop("Overlap must be between 0.5 and 0.75")
  
  time_step_size <- (1 - as.numeric(overlap)) * as.numeric(window_length)
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
      
      morc <- import_audio(
        name,
        low = freq_range[1],
        high = freq_range[2],
        tx = tx)
      
      calls <- peaks2spec(
        morc,
        sound_peaks,
        spec_size,
        window_length,
        frequency_resolution,
        time_step_size,
        dynamic_range,
        freq_range
      )
      
      spec_image <- rbind(spec_image, calls$spec_calls)
      label <- c(label, pull(db_table[
        db_table$recording == audio_files[i],
        "label_class"
      ], 1))
      
      if (is.function(update_progress)) {
        text <- paste0(i, " of ", length(audio_files))
        update_progress(detail = text)
      }
    })
  }
  spec_image <- spec_image[-1, ]
  
  parameters <- data.frame(
    spec_size = spec_size,
    window_length = window_length,
    frequency_resolution = frequency_resolution,
    overlap = overlap,
    dynamic_range = dynamic_range,
    freq_range_low = freq_range[1],
    freq_range_high = freq_range[2],
    img_rows = input_shape[2],
    img_cols = input_shape[1],
    num_classes = length(unique(label))
  )
  return(data_keras(list(spec_image, label, parameters)))
}
