require(magrittr)
require(tuneR)

### montar disco externo caso nessessário - correr na linha de comandos
#sudo fdisk -l # saber o nome do disco
#sudo mount /dev/sdb2 /media/4Thdd # montar o disco sdb2 na pasta 4Thdd
# sudo chown -R bruno:bruno '/media/4Thdd' # dar ownership
# sudo chmod -R 777 /media/4Thdd/ # dar permissoes de escrita

# Unzip files -------------------------------------------------------------
# setwd("/media/4Thdd/Biobox/")
# files <- list.files(".", pattern = ".zip")
# for (i in seq(files)) system(paste("7z x", files[i]))

# Normalize recordings June ----------------------------------------------------
setwd("~/biobox/Agosto/Zapiain C/")
# Opcoes do Script 
task <- "norm" ## "filter" ou "norm"
spectrogram <- FALSE

wavs <- 
  "~/biobox/Agosto/Zapiain C/" %T>%
  setwd() %>%
  list.files(pattern = "wav$", ignore.case = TRUE)

output <- 
  "c://Users//silva//Documents//biobox//Agosto/Zapiain_C_norm//"

if(task == 'norm'){
  for (i in seq(wavs)){
    try({
      fs <- readWave(wavs[i])@samp.rate
      if(fs==44100){
        system(paste0('sox ', wavs[i], ' -D -r 44100 ', paste0(output, substr(wavs[i], 0, nchar(wavs[i]) - 4), "_norm.wav remix 1 norm -3")))
      }else{
        system(paste0('sox ', wavs[i], ' -D -r 300000 ', paste0(output, substr(wavs[i], 0, nchar(wavs[i]) - 4), "_norm.wav remix 1 norm -3")))
      }
      if (i%%10 == 0) print(i)                               
    })#try
  }
}

