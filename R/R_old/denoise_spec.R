
denoise_spec <- function(spec, frequency_bin = T, time_bin = F){

  ## Limpar o espectrograma por frequency bin - usado para criar a base de dados
  ## Vou experimentar limpar tambem por time bin
  ncol <- length(colnames(spec)) # frequencia
nrow <- length(rownames(spec)) # tempo

mean_frequency_bin <- apply(spec, 2, mean) # mean by frequency bin
mean_time_bin <- apply(spec, 1, mean) # mean by time bin

if (frequency_bin == T && time_bin == T){
  denoise_frequency_bin <- matrix(mean_frequency_bin, nrow, ncol, byrow = T) #matrix(mediana,nrow,ncol) ##Matriz com nrow e ncol com todas as linhas iguais
  denoise_time_bin <- matrix(mean_time_bin, nrow, ncol, byrow = F)
  spec <- spec -  denoise_frequency_bin
  spec <- spec -  denoise_time_bin
} else if (frequency_bin == T){
  denoise_frequency_bin <- matrix(mean_frequency_bin, nrow, ncol, byrow = T) #matrix(mediana,nrow,ncol) ##Matriz com nrow e ncol com todas as linhas iguais
  spec <- spec -  denoise_frequency_bin
} else {
  denoise_time_bin <- matrix(mean_time_bin, nrow, ncol, byrow = F)
  spec <- spec -  denoise_time_bin
}

spec <- spec / max(spec)
spec[spec<0] <- 0
return(spec)
}

