#' Output analysis results
#' @title Save output
#' @description Writes to csv and sqlite database the results of the
#' classification.
#' @param output Object of class "tidy_output"
#' @param recording Object of class "recording"
#' @param out_file Character. Name of databse file to write analysis results. If
#' NA, no analysis results file is created
#' @param png_file Character. Name of file for plotting spectrogram with call
#' labels. If NA, no spectrogram file is created
#' @param plot2console If TRUE plots the spectrogram with the peaks identified
#' to the console. Please be advised that if TRUE the analysis takes
#' considerably more time.
#' @param recursive Logical. Is the analysis beeing processed in recursive mode?
#' @usage save_output_shiny(output, recording, out_file = NA, png_file = NA,
#' plot2console = F, recursive = FALSE)
#' @return Nothing
#' @author Bruno Silva
#' @keywords internal
#' @noRd

save_output_shiny <- function(output, recording, out_file = NA, png_file = NA,
                              plot2console = F, recursive = FALSE) {
  if (!file.exists(paste0(out_file, ".sqlite3"))) {
    create_db(out_file, "", type = "id")
  }


  if (!file.exists(paste0(out_file, ".csv"))) {
    utils::write.table(data.frame(
      recording = character(),
      label_class = character(),
      probability = numeric(),
      fmaxe = numeric(),
      n_calls = numeric()
    ),
    col.names = T,
    file = paste0(out_file, ".csv"), sep = ",", dec = "."
    )
  }

  fs <- recording$fs
  sound_samples <- recording$sound_samples
  tx <- recording$tx
  if (recursive == TRUE) {
    file_name <- recording$file_name_full_path
  } else {
    file_name <- recording$file_name
  }
  if (!is.na(png_file)) {
    grDevices::png(filename = png_file)
    spec2 <- Spectrogram(
      Audio = as.numeric(sound_samples),
      norm = 150,
      col = batsound,
      SamplingFrequency = fs * tx,
      WindowLength = 3,
      FrequencyResolution = 2,
      TimeStepSize = 0.25,
      DynamicRange = 120,
      WindowType = "hanning",
      plot = T,
      PlotFast = T,
      ylim = c(0, 80)
    )
    if (length(output[, 1]) > 0) {
      graphics::abline(
        v = ms2samples(output$peaks,
          fs = fs, tx = tx, inv = TRUE
        ),
        col = grDevices::rgb(0, 0, 0)
      )
      graphics::text(
        x = ms2samples(output$peaks, fs = fs, tx = tx, inv = TRUE),
        y = 5, labels = output$spe, col = "red", srt = 45, cex = 0.8
      )
    }
    grDevices::dev.off()
  }

  if (!is.na(out_file)) {
    if (length(output[, 1]) > 0) {
      aux_csv <- cbind(
        file_name, get_mode(output$spe), round(mean(output$prob), 2),
        round(mean(output$fmaxe) / 1000, 2), length(output$peaks)
      )

      aux_db <- data.frame(
        "recording" = file_name,
        "label_position" = output$peaks,
        "label_class" = get_mode(output$spe),
        "probability" = round(output$prob, 2),
        "fmaxe" = round(output$fmaxe, 2)
      )

      add_record(
        path = paste0(out_file, ".sqlite3"),
        df = aux_db
      )
    } else {
      aux_csv <- cbind(file_name, "Noise", NA, NA, NA)
    }

    utils::write.table(aux_csv,
      file = paste0(out_file, ".csv"),
      append = T, col.names = F, row.names = F, sep = ",", dec = "."
    )
  }

  if (plot2console == T) {
    spec2 <- Spectrogram(
      Audio = as.numeric(sound_samples),
      norm = 150,
      col = grDevices::gray.colors(255,
        start = 0.1,
        end = 0.8, gamma = 0.1
      ),
      SamplingFrequency = fs * tx,
      WindowLength = 3,
      FrequencyResolution = 3,
      TimeStepSize = 0.25,
      DynamicRange = 85,
      WindowType = "hanning",
      plot = T,
      PlotFast = T
    )
    if (length(output[, 1]) > 0) {
      graphics::abline(v = ms2samples(output$peaks,
        fs = fs,
        tx = tx,  inv = TRUE
      ), col = "blue")
      graphics::text(
        x = ms2samples(output$peaks, fs = fs, tx = tx, inv = TRUE),
        y = 5, labels = output$spe, col = "red", srt = 45, cex = 0.8
      )
    }
  }
  return(output$peaks)
}
