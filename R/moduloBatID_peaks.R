#' Detect peaks in sound files
#' @title Detect peaks
#' @description  Detect peaks
#' @param bat_recording Object of class "bat_recording"
#' @param win_size Window size in ms to search peaks. Sound is divided into
#' equal size chunks of win_size duration and one peak is identified in each chunk.
#' @param plot If TRUE plots the sound_samples with the peaks identified
#' @usage peaks(bat_recording, win_size = 40, plot = F)
#' @return Vector with the temporal position of the identified peaks (in samples)
#' @author Bruno Silva
#' @export
peaks <- function(bat_recording, win_size = 40, plot = F){
  if(!is.btr(bat_recording)){
  stop("Input object must be of class bat_recording. Use
      import_audio() as constructor." , call. =  FALSE)
  }
  fs <- bat_recording$fs
  sound_samples <- bat_recording$sound_samples
  tx <- bat_recording$tx
    len_sound_ms <- length(sound_samples) / (fs * tx / 1000)
    band <- len_sound_ms / win_size

    oscilo_smooth <- sound_samples %>%
      abs %>%
      zoo::rollmean(100) %>%
      stats::approx(n=length(sound_samples)/200)

    pks <- oscilo_smooth %>%
      unlist %>%
      matrix(ncol=2) %>%
      seewave::localpeaks(plot = plot, band = band)

      pks <- floor(pks[,1])
    pks <-pks[-1]
    pks <- pks[-length(pks)]



  return(pks)
}
