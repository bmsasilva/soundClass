#' Imports a recording and creates an object of class "bat_recording".
#' @title Imports a recording
#' @description  Import a recording using the function tuneR::readWave()
#' @param path Full path to the recording
#' @param butt If TRUE filters the recording with a 10th order butterworth
#' filter
#' @param low Minimum frequency in kHz for the butterworth filter
#' @param high Maximum frequency in kHz for the butterworth filter
#' @usage import_audio(path, butt = TRUE, low, high)
#' @return an object of class "bat_recorder". This object is a list
#' with the following components:
#' \itemize{
#'   \item sound_samples -- sound samples of the recording
#'   \item file_name -- name of the recording
#'   \item file_time -- time of modification of the file (indicated for
#'Pettersson Elektronic detectors, for other manufactureus creation time should
#'be preferable but it's not implemented yet)
#'   \item fs -- sample frequency
#'   \item tx -- expanded time factor (normally 10)
#' }
#' @author Bruno Silva
#' @export
  import_audio <- function(path, butt = TRUE, low, high) {

    if(!is.numeric(low)) stop("Parameter low must be numeric", call. = FALSE)
    if(!is.numeric(high)) stop("Parameter high must be numeric", call. = FALSE)
    if(!is.logical(butt)) stop("Parameter butt must be TRUE or FALSE", call. = FALSE)
    if(!is.character(path)) stop("Parameter path must be character", call. = FALSE)

    # Import recording
    dados <- tuneR::readWave(path)

    # Get file atributes
    file_name <- strsplit(path, "/|[\\]")[[1]]
    file_name <- file_name[length(file_name)]
    file_name <- strsplit(as.character(file_name), "\\.")[[1]][1]
    file_time <- file.info(path)$mtime
    fs <- dados@samp.rate
    tx <- ifelse(fs<50000, 10, 1)

    # If recording is stereo, convert to mono (i.e. keep channel with higher amplitude)
    # If is mono keep left channel
    if (dados@stereo)  {
      if (sum(abs(dados@left)) >= sum(abs(dados@right))) {
        som <- dados@left
      } else {
        som <- dados@right
      }
    } else {
      som <- dados@left
    }

    # Apply butterworth filter
    if (butt) {
        limit_low <- low*1000 / (fs * tx / 2)
        limit_high <- high*1000 / (fs * tx / 2)
        bt_low <- signal::butter(10, limit_low, type="high")
        bt_high <- signal::butter(10, limit_high, type="low")
        som <- as.integer(signal::filter(bt_low, som))
        som <- as.integer(signal::filter(bt_high, som))

        # Nova passagem do filtro para limpar melhor
        som <- as.integer(signal::filter(bt_low, som))
        som <- as.integer(signal::filter(bt_high, som))
    }
    structure(list(sound_samples = som, file_name = file_name, file_time = file_time,
                   fs = fs, tx = tx), class='bat_recording')
  }
