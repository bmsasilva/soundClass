# misclassification gen01
#create_db("~/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen1/",
#          "torunos_misclassifications_day1to15")

# misclassification gen02
create_db("~/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen2/",
          "torunos_misclassifications_day1to21")


##### MENU OPTIONS #####
# misclassification gen01
#db <<- "~/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen1/torunos_misclassifications_day1to15.sqlite3"

# misclassification gen02
db <<- "~/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen2/torunos_misclassifications_day1to21.sqlite3"

cutoff_high <<- 120 # Frequency in kHz (integer)
cutoff_low <<- 10 #Frequency in kHz (integer)
butt <<- TRUE #Apply Butter filter (TRUE or FALSE)
bth <<- 8 #Butter filter order (2, 4, 6, 8, 10)
savecalls <<- 'Yes' #Save analysed calls to workfolder as .png files (Yes or No)
outtable <<- 'Classic' #Variables in result table (Full or Classic)
callsauto <<- 'auto' #Call selection (auto or manual)
#tx = 1, #Time-expanded (integer, value 1 if not time-expanded)
ffts <<- 1024 #FFT size for spectrogram display for call selection
janela <<- 'Hanning'#Window type for spectrogram display for call selection (Hanning or Hamming)
overlap <<- 20 #Overlap for spectrogram display for call selection (1 to 90)
thresh <<- 90 #Threshold for spectrogram display for call selection
sensibility <<- 0.50 #Threshold for bat call selection (R2 from lm fit) (0 to 1)
pulsmax <<- 20 #Number of maximum peaks detected by fpeaks()
coor_y <<- c(10, 150)
coor_zoom_y <<- coor_y
control <<- 1
zoom_control <<- 0
win_pos <<- "+900+400" #posicao das janelas tcltk de erro
x11_width <<- 12 #largura da janela em polegadas
x11_height <<- 6 #altura da janela em polegadas
x11_xpos <<- 290 #posicao inicial do canto superior esquerdo, em pixels

##### MENU FUNCTIONS#####

espectrograma <- function() {

  # Define o ylim por defeito. Se se fizer zoom origina novo ylim
  coor_zoom_y <<- coor_y

  # Desliga o sound_samps quando se seleccionam os pulsos
  options(locatorBell=FALSE)

  # Aplica funcao grav para ler o ficheiro wav e criar variaveis com alguns parametros do objecto resultante da funcao
  morc <- import_audio(path = fileName[index_rec], butt, low = cutoff_low, high = cutoff_high)

  ficheiro <<- morc$file_name
  infoTime <<- morc$file_time
  sound_samps <<- morc$sound_filt
  fs <<- morc$fs
  tx <<- morc$tx

  # Fecha o grafico aberto e cria uma nova janela
  dev.off(2)
  X11(width=x11_width,height=x11_height, xpos=x11_xpos)

  # Plota o espectrograma
  Spectrogram(Audio = as.numeric(sound_samps),
              norm = 150,
              SamplingFrequency = fs * tx,
              WindowLength = 5, # em milisegundos
              FrequencyResolution = 2, # valor normal e 2
              TimeStepSize = 1,
              nTimeSteps = NULL,
              Preemphasis = TRUE,
              DynamicRange = thresh,
              Omit0Frequency = FALSE,
              WindowType = "hanning",
              WindowParameter = NULL,
              plot = TRUE,
              PlotFast = TRUE,
              add = FALSE,
              col = 'grayscale',
              xlim = NULL,
              ylim = c(min(coor_zoom_y),max(coor_zoom_y)),
              main = ficheiro,
              xlab = "Time (ms)",
              ylab = "Frequency (kHz)")

}
setDir <- function() {
  index_rec <<- 1
  indice_pulso <<- 0
  dirname<-tclvalue(tkchooseDirectory())
  setwd(dirname)
  filePath <<- getwd()

  # Verifica se as pastas existem e cria se nao existirem
  if (!file.exists("Analisadas")) dir.create(file.path(filePath, "Analisadas"))
  if (!file.exists("Nao Analisadas")) dir.create(file.path(filePath, "Nao Analisadas"))

  # # Verifica se o ficheiro output existe e cria um com os nomes nas colunas se nao existir
  # if (!file.exists("output_semiauto.csv")){
  #   output_columns <- t(c("ficheiro", "data", "FB", "SC", "fini", "ffin", "fmaxe", "dur", "bw", "Rfer", "Rhip", "RmRe", "Myotis",
  #                         "Ppip", "Ppyg", "Pkuh", "NN", "EE", "Bbar", "Plecotus", "Msch", "Tten", "ID70", "Prob70", "ID80", "Prob80", "ID90", "Prob90"))
  #   write.table(output_columns, file="output_semiauto.csv", row.names = FALSE, col.names = FALSE, sep=",")
  # }

  # Obtem a listagem dos ficheiros com extensao "wav ou "WAV"
  fileName1 <- list.files(filePath, recursive = FALSE, pattern=c("wav"))
  fileName2 <- list.files(filePath, recursive = FALSE, pattern=c("WAV"))
  fileName <<- c(fileName1,fileName2)

  espectrograma()

}
zoom_BS <- function(){
  zoom_coordinates <- locator(2)
  if(length(zoom_coordinates$x)==2){
    zoom_coordinates$x <- floor(zoom_coordinates$x / 1000 * fs * tx) # converter de ms para samples
    zoom_coordinates$y <- floor(zoom_coordinates$y)
    sound_samps <<- sound_samps[min(zoom_coordinates$x):max(zoom_coordinates$x)]
    coor_zoom_y <<- zoom_coordinates$y

    Spectrogram(Audio = as.numeric(sound_samps),
                SamplingFrequency = fs * tx,
                WindowLength = 5, # em milisegundos
                FrequencyResolution = 3, # Quando aplica zoom aplica melhor definicao de frequencia
                TimeStepSize = 1,
                nTimeSteps = NULL,
                Preemphasis = TRUE,
                DynamicRange = 70,
                Omit0Frequency = FALSE,
                WindowType = "hanning",
                WindowParameter = NULL,
                plot = TRUE,
                PlotFast = TRUE,
                add = FALSE,
                col = 'grayscale',
                xlim = NULL,
                ylim = c(min(coor_zoom_y),max(coor_zoom_y)),
                main = ficheiro,
                xlab = "Time (ms)",
                ylab = "Frequency (Hz)")

    zoom_control <<- 1
  }
}

thresh_BS <- function(){

  bbt <- varEntryDialog(vars=c('thresh'),
                        labels=c('threshold (default:70)'))

  # Se utilizador introduzir novo valor thresh
  if(bbt$thresh != ""){
    dev.off(0)

    Spectrogram(Audio = as.numeric(sound_samps),
                SamplingFrequency = fs * tx,
                WindowLength = 5, # em milisegundos
                FrequencyResolution = 2, # Quando aplica zoom aplica melhor definicao de frequencia
                TimeStepSize = 1,
                nTimeSteps = NULL,
                Preemphasis = TRUE,
                DynamicRange = as.numeric(bbt$thresh),
                Omit0Frequency = FALSE,
                WindowType = "hanning",
                WindowParameter = NULL,
                plot = TRUE,
                PlotFast = TRUE,
                add = FALSE,
                col = 'grayscale',
                xlim = NULL,
                ylim = c(min(coor_zoom_y),max(coor_zoom_y)),
                main = ficheiro,
                xlab = "Time (ms)",
                ylab = "Frequency (Hz)")

    # Se utilizador introduzir novo valor filtro butter
  }
}

butterFilts_BS <- function(){# ajustar com a nova funcao butter_filter()

  bbt <- varEntryDialog(vars=c('low','high'),
                        labels=c('limite superior','limite inferior'))

  # Se utilizador introduzir novo valor low e high
  if(bbt$low != "" && bbt$high != ""){
    dev.off(0)
    butt_filter_low (as.numeric(bbt$low)) #Aplica o filtro butter a variavel sound_samps do ambinente geral
    butt_filter_high (as.numeric(bbt$high)) #Aplica o filtro butter a variavel sound_samps do ambinente geral
    Spectrogram(Audio = as.numeric(sound_samps),
                SamplingFrequency = fs * tx,
                WindowLength = 5, # em milisegundos
                FrequencyResolution = 2, # Quando aplica zoom aplica melhor definicao de frequencia
                TimeStepSize = 1,
                nTimeSteps = NULL,
                Preemphasis = TRUE,
                DynamicRange = 70,
                Omit0Frequency = FALSE,
                WindowType = "hanning",
                WindowParameter = NULL,
                plot = TRUE,
                PlotFast = TRUE,
                add = FALSE,
                col = 'grayscale',
                xlim = NULL,
                ylim = c(min(coor_zoom_y),max(coor_zoom_y)),
                main = ficheiro,
                xlab = "Time (ms)",
                ylab = "Frequency (Hz)")

    # Se utilizador introduzir novo valor filtro low
  }else if(bbt$low != "") {
    dev.off(0)
    butt_filter_low (as.numeric(bbt$low))

    Spectrogram(Audio = as.numeric(sound_samps),
                SamplingFrequency = fs * tx,
                WindowLength = 5, # em milisegundos
                FrequencyResolution = 2, # Quando aplica zoom aplica melhor definicao de frequencia
                TimeStepSize = 1,
                nTimeSteps = NULL,
                Preemphasis = TRUE,
                DynamicRange = 70,
                Omit0Frequency = FALSE,
                WindowType = "hanning",
                WindowParameter = NULL,
                plot = TRUE,
                PlotFast = TRUE,
                add = FALSE,
                col = 'grayscale',
                xlim = NULL,
                ylim = c(min(coor_zoom_y),max(coor_zoom_y)),
                main = ficheiro,
                xlab = "Time (ms)",
                ylab = "Frequency (Hz)")


    # Se utilizador introduzir novo valor high
    }else if(bbt$high != "") {
    dev.off(0)
    butt_filter_high (as.numeric(bbt$high))
    Spectrogram(Audio = as.numeric(sound_samps),
                SamplingFrequency = fs * tx,
                WindowLength = 5, # em milisegundos
                FrequencyResolution = 2, # Quando aplica zoom aplica melhor definicao de frequencia
                TimeStepSize = 1,
                nTimeSteps = NULL,
                Preemphasis = TRUE,
                DynamicRange = 70,
                Omit0Frequency = FALSE,
                WindowType = "hanning",
                WindowParameter = NULL,
                plot = TRUE,
                PlotFast = TRUE,
                add = FALSE,
                col = 'grayscale',
                xlim = NULL,
                ylim = c(min(coor_zoom_y),max(coor_zoom_y)),
                main = ficheiro,
                xlab = "Time (ms)",
                ylab = "Frequency (Hz)")

  }
}


call_label_gui <- function(){

    pos <- locator(n = 512)
    pos <- floor(pos$x * (fs * tx) / 1000)
    np <- length(pos)/2

    while(is.wholenumber(np)==FALSE) { # Se forem seleccionados pontos em n?mero impar repete a escolha
      ReturnVal <- tkmessageBox(title="Erro", message="E necessario escolher os pulsos novamente",icon="info",type="ok")
      pos <- locator(n = 512)
      pos <- floor(pos$x * (fs * tx) / 1000)
      np <- length(pos)/2
    }







    ## Obtém o local de fmaxe de cada pulso seleccionado anteriormente
    j<-1
    maxpos<-NULL
    for (i in 1:np) {
      maxpos[i]<-which.max(abs(sound_samps[pos[j]:pos[j+1]])) + pos[j]
      j<-j+2
    }








   abline(v = ms2samples(maxpos, fs = fs, tx = tx, inv=TRUE), col ='darkgray')



   confirmation <- tkmessageBox(message = "Add calls to database?",
                                icon = "question", type = "yesno", default = "yes")

   if (as.character(confirmation) == "yes"){
     # Caixa de input para indicar o tipo de pulso e a especie
     vars <- varEntryDialog(vars=c('N', 'FB', 'SC', 'spe'),
                            labels=c('Navigation', 'Feeding buzz','Social Call', "Species"),
                            fun=c(as.integer, as.integer, as.integer, as.character))#, win = win_pos)

     # Se for pressionado o botao "cancel" na caixa de input e necessario
     # criar os campos FB e SC com NA (se for pressionado o botao "submit" com os campos em branco
     # os valores de FB e SC ficam logo como "NA")
     if(length(vars)==0) vars <- list(N = "NA", FB = "NA", SC = "NA", spe = "NA")

     output <- data.frame("recording" = ficheiro,
                          "calls_samples" = maxpos,
                          # "calls_ms" = round(maxpos * 1000 / fs * tx, 1),
                          "N" = vars$N,
                          "FB" = vars$FB,
                          "SC" = vars$SC,
                          "spe" = vars$spe)
   add_record(path = db, df = output, table = "rec_labels")
   }

}#final da funcao


proxima_rec <- function(){


  file.rename(paste(filePath,fileName[index_rec],sep="/"),
              paste(filePath,"Analisadas",fileName[index_rec],sep="/"))

  index_rec <<- index_rec + 1
  indice_pulso <<- 0
  control <<- 1
  espectrograma()
}

eliminar_pulso <- function(){

  confirmation <- tkmessageBox(message = "Delete all calls from this recording?",
                               icon = "question", type = "yesno", default = "yes")
  if (as.character(confirmation) == "yes"){
    my_db <- dplyr::src_sqlite(db, create = FALSE)
    dbSendQuery(my_db$con, paste0("DELETE FROM rec_labels WHERE recording = ", "'", ficheiro, "'"))
    espectrograma()
  }
}

##### AUXILIAR FUNCTIONS #####

butt_filter_high <- function(limit){
blimit <- limit*1000 / (fs * tx / 2)
bt <- butter(12, blimit, type="high") #Highpass Butter
sound_samps <<- as.integer(filter(bt, sound_samps))
}

butt_filter_low <- function(limit){
  blimit <- limit*1000 / (fs * tx / 2)
  bt <- butter(12, blimit, type="low") #Highpass Butter
  sound_samps <<- as.integer(filter(bt, sound_samps))
}


##### MENU OPTIONS LOAD #####
require(tcltk)
if(exists("menuOPTIONS")) tkdestroy(menuOPTIONS)
menuOPTIONS <- tktoplevel()
tkwm.title(menuOPTIONS,"View Options")
button.id0<- tkbutton(menuOPTIONS,width=15, height=2, text="Escolher Dir",
                      command=setDir)

button.id1<- tkbutton(menuOPTIONS,width=15, height=2,text="Espectrograma",
                      command=espectrograma)


button.op0<- tkbutton(menuOPTIONS,width=15, height=2, text="Butterworth",
                      command=butterFilts_BS)

button.op1<- tkbutton(menuOPTIONS,width=15, height=2, text="Threshold",
                      command=thresh_BS)

button.op2<- tkbutton(menuOPTIONS,width=15, height=2, text="Zoom",
                      command=zoom_BS)

# button.op3<- tkbutton(menuOPTIONS,width=15, height=2, text="Limpar",
#                       command=cut_BS)

button.an0<- tkbutton(menuOPTIONS,width=15, height=2, text="Analisar",

                      command=call_label_gui)




button.an1<- tkbutton(menuOPTIONS,width=15, height=2, text="Eliminar Pulso",
                      command=eliminar_pulso)

button.an2<- tkbutton(menuOPTIONS,width=15, height=1, text="",
                      command=function(){})

button.an3<- tkbutton(menuOPTIONS,width=15, height=2, text="Proxima Rec",
                      command=proxima_rec)

tkgrid(button.id0)
tkgrid(button.id1)
tkgrid(button.op0)
tkgrid(button.op1)
tkgrid(button.op2)
# tkgrid(button.op3)
tkgrid(button.an0)
tkgrid(button.an1)
tkgrid(button.an2)
tkgrid(button.an3)
tkfocus(menuOPTIONS)
