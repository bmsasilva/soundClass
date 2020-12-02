#' @title Label calls on a recording
#' @description Label calls on a recording by clicking on a spectrogram.
#' @param sound_samples The sound samples of the recording
#' @param file_name Name of the recording
#' @param fs Sampling frequency of the recording
#' @param tx Expansion factor of the recording
#' @param db Full path to the database
#' @param win_size A two-element vector, c(widht, height), with the dimension in
#' inches of the spectrogram window
#' @param plot Logical indicating if the spectrogram is. Should only be set to
#' FALSE when using the function inside other function that already plots a spectrogram
#' @return  A dataframe with the file name, the positions of the calls and the
#' respective labels. The dataframe has as many rows as there are calls
#' @details Writes the call positions and labels to a sqlite3 database
#' @export
#' @author Bruno Silva

call_label <- function(sound_samples, file_name, fs, tx, db, plot = TRUE,
win_size = c(13, 5)) {
  options(locatorBell=FALSE)
  ## Plota o espectrograma para o utilizador selecionar os pulsos
  grDevices::x11(width = win_size[1], height = win_size[2])

if(plot){
  Spectrogram(Audio = as.numeric(sound_samples),
              norm = 150,
              SamplingFrequency = fs * tx,
              WindowLength = 3, # em milisegundos
              #FrequencyResolution = .Object@freqResolution,
              TimeStepSize = 2,
              nTimeSteps = NULL,
              Preemphasis = TRUE,
              DynamicRange = 90,
              Omit0Frequency = FALSE,
              WindowType = "hanning",
              WindowParameter = NULL,
              plot = T,
              PlotFast = TRUE,
              add = FALSE,
              col = 'grayscale',
              # xlim = xlim,
              # ylim = ylim,
              # main = .Object@recName,
              xlab = "Time (ms)",
              ylab = "Frequency (kHz)")
}



  pos <- locator(n = 512)
  pos <- floor(pos$x * (fs * tx) / 1000)
  np <- length(pos)/2

  while(is.wholenumber(np)==FALSE) { # Se forem seleccionados pontos em n?mero impar repete a escolha
    ReturnVal <- tkmessageBox(title="Erro", message="E necessario escolher os pulsos novamente",icon="info",type="ok")
    pos <- locator(n = 512)
    pos <- floor(pos$x * (fs * tx) / 1000)
    np <- length(pos)/2
  }

  # Caixa de input para indicar o tipo de pulso e a especie
  vars <- varEntryDialog(vars=c('N', 'FB', 'SC', 'spe'),
                          labels=c('Navigation', 'Feeding buzz','Social Call', "Species"),
                          fun=c(as.integer, as.integer, as.integer, as.character))#, win = win_pos)

  # Se for pressionado o botao "cancel" na caixa de input e necessario
  # criar os campos FB e SC com NA (se for pressionado o botao "submit" com os campos em branco
  # os valores de FB e SC ficam logo como "NA")
  if(length(vars)==0) vars <- list(N = "NA", FB = "NA", SC = "NA", spe = "NA")





  ## Obtém o local de fmaxe de cada pulso seleccionado anteriormente
  j<-1
  maxpos<-NULL
  for (i in 1:np) {
    maxpos[i]<-which.max(abs(sound_samples[pos[j]:pos[j+1]])) + pos[j]
    j<-j+2
  }


output <- data.frame("recording" = file_name,
                     "calls_samples" = maxpos,
                     # "calls_ms" = round(maxpos * 1000 / fs * tx, 1),
                     "N" = vars$N,
                     "FB" = vars$FB,
                     "SC" = vars$SC,
                     "spe" = vars$spe)

add_record(path = db, df = output, table = "rec_labels")

  return(output)
}

