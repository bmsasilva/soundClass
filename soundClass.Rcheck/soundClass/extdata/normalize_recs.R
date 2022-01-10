# Convert recordings to mono (keep left (remix 1) or right channel(remix 2) ), normalize (to -3 dB)
require(magrittr)
require(tuneR)

wavs <- 
  "/home/bruno/test_esporao/" %T>%
  setwd() %>%
  list.files(pattern = "wav$", ignore.case = TRUE)

output <- 
  "/home/bruno/test_esporao/norm" %>%
  paste0('/')


  for (i in seq(wavs)){
    try({
      aux <- readWave(wavs[i])
      fs <- readWave(wavs[i])@samp.rate
      
      if(aux@stereo == FALSE){
        system(paste0('sox ', wavs[i], ' -D -r ', fs, ' ', paste0(output, substr(wavs[i], 0, nchar(wavs[i]) - 4), "_norm.wav norm -3")))
      } else {
        if(sum(abs(aux@left)) > sum(abs(aux@right))) channel <- "remix 1"
        if(sum(abs(aux@right)) > sum(abs(aux@left))) channel <- "remix 2"
        
        system(paste0('sox ', wavs[i], ' -D -r ', fs, ' ', paste0(output, substr(wavs[i], 0, nchar(wavs[i]) - 4), "_norm.wav ", channel," norm -3")))
        
      }
      

     

      if (i%%10 == 0) print(i)                               
    })#try
  }

