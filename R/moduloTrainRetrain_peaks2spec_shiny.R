#' Convert sound to spectrogram
#' @title Sound to spectrogram
#' @description Convert sound to spectrogram
#' @param recording Object of class "rc"
#' @param sound_peaks Peaks detected in recording samples
#' @param frequency_bin If TRUE filter by frequency mean subtraction
#' @param time_bin If TRUE filter by time mean subtraction
#' @param parameters Set of parameters for the analysis.
#' @usage peaks2spec(recording, sound_peaks, parameters = "v1")
#' @return an object of class "calls". This object is a list
#' with the following components:
#' \itemize{
#'   \item spec_calls -- matrix with one spectrogram per row
#'   \item img_cols -- number of columns in each spectrogram
#'   \item img_rows -- number of rows in each spectrogram
#' }
#' @author Bruno Silva
#' @internal
 
# Deveria ser interna esta? como e puxada pela spectro_calls pode não ser necessario exporta-la
peaks2spec_shiny <- function(recording, sound_peaks, spec_size = NA, window_length = NA, 
                       frequency_resolution = NA, time_step_size = NA, dynamic_range = NA,
                       freq_range = NA){
  
  if(!is_rc(recording)){
    stop("Input object must be of class recording. Use
      import_audio() as constructor." , call. =  FALSE)
  }
  if(!is.vector(sound_peaks)) stop("Parameter sound_peaks must be numeric", call. = FALSE)

  fs <- recording$fs
  sound_samples <- recording$sound_samples
  tx <- recording$tx
  input_shape <- c(spec_size / time_step_size, 
                   (freq_range[2]-freq_range[1])*frequency_resolution)
  
  # if(parameters == "v1"){
  #   # all species # smaller image
  #   size <- 20 # ms
  #   window_length <- 1 # em milisegundos
  #   frequency_resolution <- 1 # valor normal e 2
  #   time_step_size <- 0.25
  #   dynamic_range <- 90
  #   freq_range <- c(10, 125) #hertz
  #   input_shape <- c(size / time_step_size, 115)
  # } else if(parameters == "v2"){
  #   # without rhinolophus # smaller image
  #   size <- 20 # ms
  #   window_length <- 1 # em milisegundos
  #   frequency_resolution <- 1 # valor normal e 2
  #   time_step_size <- 0.25
  #   dynamic_range <- 90
  #   freq_range <- c(10, 80) #khertz
  #   input_shape <- c(size / time_step_size, 70)
  # } else if(parameters == "v3"){
  #   # all species  # better resolution
  #   size <- 20 # ms
  #   window_length <- 1 # em milisegundos
  #   frequency_resolution <- 2 # valor normal e 2
  #   time_step_size <- 0.1
  #   dynamic_range <- 90
  #   freq_range <- c(10, 125) #hertz
  #   input_shape <- c(size / time_step_size, 230)
  # } else if(parameters == "v4"){
  #   # without rhinolophus # better resolution
  #   size <- 20 # ms
  #   window_length <- 1 # em milisegundos
  #   frequency_resolution <- 2 # valor normal e 2
  #   time_step_size <- 0.1
  #   dynamic_range <- 90
  #   freq_range <- c(10, 80) #hertz
  #   input_shape <- c(size / time_step_size, 140)
  # } else {
  #   stop("No valid parameters selected", call. = FALSE)
  # }
  
  # Dividir o ficheiro em janelas de tamanho size (definido na versao escolhida - 20ms neste caso)
  # em volta dos sound_peaks de fmaxe
  windows <- matrix(NA, nrow = length(sound_peaks),
                    ncol = ms2samples(spec_size, fs, tx))
  half_window <- ms2samples(spec_size / 2, fs, tx)
  
  # Necessario mudar o primeiro ou o ultimo peak caso
  # fiquem a menos de 20ms do inicio ou do fim da gravacao
  if(sound_peaks[1] < ms2samples(spec_size / 2, fs, tx)) {
    sound_peaks[1] <- ms2samples(spec_size / 2, fs, tx) + 100
  }
  
  if(sound_peaks[length(sound_peaks)] > length(sound_samples) - ms2samples(spec_size / 2, fs, tx)) {
    sound_peaks[length(sound_peaks)] <- length(sound_samples) - ms2samples(spec_size / 2, fs, tx) - 100 
  }
  
  for (i in 1:length(sound_peaks)) {
    windows[i,] <- sound_samples[(sound_peaks[i] - half_window):
                                   (sound_peaks[i] + half_window - 1)]
  }
  
  n_calls <- length(windows[,1])
  spec_df <- matrix(NA, nrow=n_calls, ncol=input_shape[1]*input_shape[2])
  fmaxe_df <- c()
  for(k in 1:n_calls){
    sound_samples_xs <- windows[k,]
    
    spec2 <- Spectrogram(Audio = as.numeric(sound_samples_xs),
                         norm = 150,
                         SamplingFrequency = fs * tx,
                         WindowLength = window_length,
                         FrequencyResolution = frequency_resolution, # valor normal e 2
                         TimeStepSize = time_step_size  ,
                         DynamicRange = dynamic_range,
                         WindowType = "hanning",
                         plot = F)
    
    ## Cut to desired frequency range
    filt <- c(which(as.numeric(colnames(spec2)) < freq_range[1] * 1000), which(as.numeric(colnames(spec2)) > freq_range[2] * 1000))
    spec2_filt <- spec2[,-filt] # eliminar range de frequencia nao necessario
    spec2_filt <- spec2_filt + dynamic_range ; dim(spec2_filt)
    
    ## get fmaxe
    fmaxe <- dimnames(spec2)[[2]][which(spec2 == max(spec2), arr.ind = TRUE)[2]]
    
    ## Force to desired spectrogram size # only because of fs=44100, with fs=300000 this step is not needed
    while(dim(spec2_filt)[1] > input_shape[1]) spec2_filt <- spec2_filt[-1,]
    while(dim(spec2_filt)[2] > input_shape[2]) spec2_filt <- spec2_filt[,-c(length(spec2_filt[1,]))]
    
    
    ## Limpar o espectrograma por frequency bin - usado para criar a base de dados
    ncol <- length(colnames(spec2_filt)) # frequencia
    nrow <- length(rownames(spec2_filt)) # tempo
    
    spec <- denoise(spec2_filt)
    spec[is.na(spec)] <- 0
    spec_df[k,] <- as.numeric(r(spec)) #necessario o r() para as imagens entrarem bem no tensorflow
    fmaxe_df[k] <- as.numeric(fmaxe)
  }
  
  structure(list(spec_calls = spec_df, img_cols = input_shape[1],
                 img_rows = input_shape[2], fmaxe = fmaxe_df), class='calls')
}
