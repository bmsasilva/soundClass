#' Detect peaks in non-relevant recordings
#' @title Detect energy peaks in non-relevant recordings
#' @description Detects the temporal position of the number of peaks (events with most energy)
#' defined in the parameter nmax
#' @param bat_recording Object of class "bat_recording"
#' @param nmax Maximum number of peaks to detect in the recording
#' @param plot If TRUE plots the sound_samples with the peak(s) identified
#' @usage add_noise(bat_recording, nmax = 1, plot = F)
#' @return Vector with the temporal position of the identified peak(s) (in samples)
#' @author Bruno Silva
#' @export
find_noise <- function(bat_recording, nmax = 1, plot = F){

  if(!is.btr(bat_recording)){
  stop("Input object must be of class bat_recording. Use
      import_audio() as constructor." , call. =  FALSE)
  }
  
  fs <- bat_recording$fs
  sound_samples <- bat_recording$sound_samples
  tx <- bat_recording$tx

    oscilo_smooth <- sound_samples %>%
      abs %>%
      zoo::rollmean(100) %>%
      stats::approx(n=length(sound_samples)/200)

    pks <- oscilo_smooth %>%
      unlist %>%
      matrix(ncol=2) %>%
      seewave::fpeaks(plot = plot, nmax = nmax)

     pks <- floor(pks[,1])




  return(pks)
}
