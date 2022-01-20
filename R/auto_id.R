#' @title Automatic classification of sound events in recordings
#' @description Applies automatic classification of sound events on a set of 
#' recordings using a fitted model.
#' @param model_path Path to the fitted model.
#' @param updateProgress Progress bar only to be used inside shiny.
#' @param metadata Parameters used to create train data for fitting the model.
#' @param file_path Path to the folder containing sound recordings.
#' @param out_file Character. Name of the file to output the results.
#' Will be used to name the csv file and the sqlite database.
#' @param out_dir Path to the folder where the output results will be stored.
#' @param save_png Logical. Should a spectrogram of the classified recordings
#' with the identified event(s) and respective classification(s) be saved
#' as png file?
#' @param win_size Window size to split recordings in chunks for classification.
#' One peak per chunk is obtained and classified.
#' @param plot2console Logical. Should a spectrogram of the classified
#' recordings with the identified event(s) and respective classification(s)
#' be plotted in the console while the analysis is running?
#' as png file?
#' @param remove_noise Logical. TRUE indicates that the model was fitted
#' with a non-relevant class which will be deleted from the final output.
#' @param recursive Logical. FALSE indicates that the recordings are in
#' a single folder and TRUE indicates that there are recordings
#' inside subfolders.
#' @param tx Time expanded. Only used in recorders specifically intended for
#' bat recordings. Can take the values "auto" or any numeric value. If the
#' recording is not time expanded tx must be set to 1 (the default). If it's
#' time expanded the numeric value corresponding to the time expansion should
#' be indicated or "auto" should be selected. If tx = "auto" the function
#' assumes that sampling rates < 50kHz corresponds to
#' tx = 10 and > 50kHz to tx = 1.
#' @return Nothing.
#' @details Runs a classification task on the recordings of a specified folder
#' and saves the results of the analysis.
#' @export
#' @author Bruno Silva

auto_id <- function(model_path, updateProgress,
                          metadata,
                          file_path,
                          out_file,
                          out_dir,
                          save_png = TRUE,
                          win_size = 50,
                          plot2console = FALSE,
                          remove_noise = TRUE,
                          recursive = FALSE,
                          tx = 1) {
  spec_size <- metadata$parameters$spec_size
  window_length <- metadata$parameters$window_length
  dynamic_range <- metadata$parameters$dynamic_range
  frequency_resolution <- metadata$parameters$frequency_resolution
  freq_range <- c(
    metadata$parameters$freq_range_low,
    metadata$parameters$freq_range_high
  )

  time_step_size <- (1 - as.numeric(metadata$parameters$overlap)) * as.numeric(metadata$parameters$window_length)
  class_labels <- as.character(metadata$classes$name)

  file_name <- list.files(file_path, recursive = recursive, pattern = "wav|WAV")
  if (recursive == TRUE) file_name <- paste0("/", file_name)

  size <- length(file_name)

  model <- keras::load_model_hdf5(model_path)

  for (i in seq(file_name)) {
    try({
      morc <- import_audio(
        path = paste0(file_path, file_name[i]),
        low = freq_range[1], high = freq_range[2],
        tx = tx
      )

      if (recursive == TRUE) morc$file_name_full_path <- file_name[i]
      sound_peaks <- peaks(morc, win_size = win_size)

      calls <- peaks2spec(morc, sound_peaks,
        spec_size = spec_size,
        window_length = window_length,
        frequency_resolution = frequency_resolution,
        time_step_size = time_step_size,
        dynamic_range = dynamic_range,
        freq_range = freq_range
      )

      out <- classify_calls(calls, sound_peaks, model = model)

      out_tidy <- tidy_output(out,
        class_labels = class_labels,
        remove_noise = remove_noise, min_dist = win_size
      )

      png_file <- ifelse(save_png == T,
        paste0(
          out_dir,
          morc$file_name, ".png"
        ),
        NA
      )

      out_peaks <- save_output(
        output = out_tidy,
        recording = morc,
        out_file = paste0(out_dir, out_file),
        png_file = png_file,
        plot2console = plot2console,
        recursive = recursive
      )
    })

    if (is.function(updateProgress)) {
      text <- paste0(i, " of ", length(file_name))
      updateProgress(detail = text)
    }

    print(paste0(i, " of ", size))
  }
}
