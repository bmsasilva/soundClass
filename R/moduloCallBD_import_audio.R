#' Imports a recording and creates an object of class "bat_recording".
#' @title Imports a recording
#' @description  Import a recording using the function tuneR::readWave()
#' @param path Full path to the recording
#' @param butt If TRUE filters the recording with a 10th order butterworth
#' filter
#' @param low Minimum frequency in kHz for the butterworth filter
#' @param high Maximum frequency in kHz for the butterworth filter
#' @param tx Time expanded. Only used in some recorders specifically intended for
#' bat recordings. Can take the values "auto" or any numeric value. If the 
#' recording is not time expanded tx must be set to 1. If it's time expanded the 
#' numeric value correponding to the time expansion should be indicated or 
#' "auto" should be selected. If tx = "auto" the function assumes that sampling 
#' rates < 50kHz correponds to tx = 10 an > 50kHz to tx = 1. 
#' @usage import_audio(path, butt = TRUE, low, high, tx)
#' @return an object of class "bat_recorder". This object is a list
#' with the following components:
#' \itemize{
#'   \item sound_samples -- sound samples of the recording
#'   \item file_name -- name of the recording
#'   \item file_time -- time of modification of the file (indicated for
#'Pettersson Elektronic detectors, for other manufactureus creation time should
#'be preferable but it's not implemented yet)
#'   \item fs -- sample frequency
#'   \item tx -- expanded time factor
#' }
#' @author Bruno Silva
#' @export

import_audio <- function(path, butt = TRUE, low, high, tx = "auto") {
  
  if(!is.logical(butt)) stop("Parameter butt must be TRUE or FALSE", call. = FALSE)
  if(!is.character(path)) stop("Parameter path must be character", call. = FALSE)
  
  # Import recording
  dados <- tuneR::readWave(path)
  
  # Get file atributes (CONFIRMAR A FUNCIONALIDADE COM OS PATH DE WINDOWS)
  file_name <- strsplit(path, "/|[\\]")[[1]]
  file_name <- file_name[length(file_name)]
  file_name <- strsplit(as.character(file_name), "\\.")[[1]][1]
  file_time <- file.info(path)$mtime
  fs <- dados@samp.rate
  if(tx == "auto" & fs < 50000){
    tx <- 10
  } else if(tx == "auto" & fs > 50000){
    tx <- 1
  } else {
    tx <- tx
  }
  
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
