# Convert recordings to mono (keep left channel), normalize (to -3 dB) and resample (to 300kHz) 

require(magrittr)
require(tuneR)

##### Opcoes do Script #####
task <- "norm" ## "filter" ou "norm"
spectrogram <- FALSE
############################

wavs <- 
  "/home/bruno/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/" %T>%
  setwd() %>%
  list.files(pattern = "wav$", ignore.case = TRUE)
  
output <- 
  "/home/bruno/Projectos/PHD/ms01/ms01_recs_caseStudy1_torunos/norm" %>%
  paste0('/')

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



if(task == 'filter'){
for (i in seq(wavs)){
  fs <- readWave(wavs[i])@samp.rate
  if(fs==44100){
    system(paste0('sox ', wavs[i], ' ', paste0(output, substr(wavs[i], 0, nchar(wavs[i]) - 4), "_filt.wav sinc 2500-12500"))) # SINC FAZ UM STOPBAND FORA DO INTERVALO DEFINIDO
  }else{
    system(paste0('sox ', wavs[i], ' ', paste0(output, substr(wavs[i], 0, nchar(wavs[i]) - 4), "_filt.wav sinc 30000-125000")))
  }
  if (i%%10 == 0) print(i)                               
}
}

##### Plot Spectro #####
if(spectrogram == TRUE){ 
  for (i in seq(wavs)){
    system(paste0('sox ', wavs[i], ' -n spectrogram -m -o ', paste0(output, substr(wavs[i], 0, nchar(wavs[i]) - 4), ".png")))
  }
}