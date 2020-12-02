require(tcltk)
library(EBImage)
library(signal)
r <- function(x) apply(x, 1, rev) # funcao para rodar uma matriz.

## OPCOES
thresh=90
bat <- 1
# ESCOLHER 1 se for morcego e 0 se for ruido
# if(bat==1) fileName <- list.files('./database/Batcall_recs/Processed_20ms', pattern="wav" , full.names=T)# tk_choose.files()  # escolher gravacoes
# if(bat==0) fileName <- list.files('./database/Noisecall_recs/Processed_20ms', pattern="wav" , full.names=T)# tk_choose.files()  # escolher gravacoes
if(bat==1) fileName <- list.files('/home/ubc/DISCO2/ProjectosPessoais/A2020/Recordings/Bats/Recordings/0.pulsosIndividuais_processed_20ms_norm', pattern="wav" , full.names=T)# tk_choose.files()  # escolher gravacoes
if(bat==0) fileName <- list.files('/home/ubc/DISCO2/ProjectosPessoais/A2020/Recordings/Noise/0.processed_20ms_norm', pattern="wav" , full.names=T)# tk_choose.files()  # escolher gravacoes

n_recs <- length(fileName)

rs_df <- matrix(NA, nrow=n_recs, ncol=9200)
for(i in seq(fileName)){
morc <- import_audio(fileName[i], low=10, high=125)
fs <- morc$fs
file <- morc$ficheiro
som <- morc$som
ifelse (fs < 50000, tx <- 10, tx <- 1)
#while(length(som) > fs*tx*20/1000) som <- som[-c(length(som))] # O script de cortar os pulsos e o ruido deixa 1 sample a mais

spec2 <- Spectrogram(Audio = as.numeric(som),
                     norm = 150,
                     SamplingFrequency = fs * tx,
                     WindowLength = 1, # em milisegundos
                     FrequencyResolution = 1, # valor normal e 2
                     TimeStepSize = 0.25,   ## corresponde a 75% sobreposicao
                     DynamicRange = thresh,
                     WindowType = "hanning",
                     plot = F)

## Cut to desired frequency range
filt <- c(which(as.numeric(colnames(spec2)) < 10000), which(as.numeric(colnames(spec2)) >= 125000))
#if(fs==441000) filt <- c(which(as.numeric(colnames(spec2)) < 10000), which(as.numeric(colnames(spec2)) >= 125000))
#if(fs==300000) filt <- c(which(as.numeric(colnames(spec2)) < 10000), which(as.numeric(colnames(spec2)) >= 125000))
spec2_filt <- spec2[,-filt] # eliminar range de frequencia nao necessario
spec2_filt<- spec2_filt + thresh
while(dim(spec2_filt)[1] > 80) spec2_filt <- spec2_filt[-1,]
while(dim(spec2_filt)[2] > 115) spec2_filt <- spec2_filt[,-1]
if(dim(spec2_filt)[1]==80 && dim(spec2_filt)[2]==115){

ncol <- length(colnames(spec2_filt))
nrow <- length(rownames(spec2_filt))
##Calcular mediana por coluna do ln(S)e subtrair a S## LIMPA O ESPREOTRAGRAMA NA VERTICAL
mediana<-matrix(0,1,ncol)
for (j in 1:ncol) ##Calcular a m?diana de cada linha da matrix
{
  mediana[,j] <-  mean(spec2_filt[,j])## Matriz com 1 coluna e cada linha ? a m?dia da linha respectiva de Smag
}
mediana <- matrix(mediana,nrow,ncol, byrow = T) #matrix(mediana,nrow,ncol) ##Matriz com nrow e ncol com todas as linhas iguais
spec2_filt<- spec2_filt - mediana
spec2_filt <- spec2_filt / max(spec2_filt)
spec2_filt[spec2_filt<0] <- 0
#spec2_filt <- as.numeric(spec2_filt)


rs_df[i,] <- as.numeric(r(spec2_filt))
#
# if(i==1){
#   rs_df <- as.numeric(r(spec2_filt))
# }else{
#   rs_df <- rbind(rs_df, as.numeric(r(spec2_filt)))
# }
#train_x <- array(train_x, dim = c(2, img_rows, img_cols, 1))
print(paste0(i,' of ', length(fileName)))

}#final do if dim
}# final do for

path <- '/home/ubc/DADOS/BRUNO/A2020/1.Geraldina/database/'

if(bat == 1) {
  dados_bats <- rs_df #array(rs_df, dim = c(n_recs, img_rows, img_cols, 1))
  labels_bats <- rep(bat, n_recs)
  rec_bats <- fileName
  save(rec_bats,dados_bats, labels_bats, file=paste0(path,"bats_win1ms_step0.25ms_res1khz_threh90.RData"))
} else {
  dados_noise <- rs_df #array(rs_df, dim = c(n_recs, img_rows, img_cols, 1))
  labels_noise <- rep(bat, n_recs)
  rec_noise<- fileName
  save(rec_noise,dados_noise, labels_noise, file=paste0(path,"noise_win1ms_step0.25ms_res1khz_threh90.RData"))
}


######################################################################
## OPCOES
thresh=90
bat <- 0
# ESCOLHER 1 se for morcego e 0 se for ruido
# if(bat==1) fileName <- list.files('./database/Batcall_recs/Processed_20ms', pattern="wav" , full.names=T)# tk_choose.files()  # escolher gravacoes
# if(bat==0) fileName <- list.files('./database/Noisecall_recs/Processed_20ms', pattern="wav" , full.names=T)# tk_choose.files()  # escolher gravacoes
if(bat==1) fileName <- list.files('/home/ubc/DISCO2/ProjectosPessoais/A2020/Recordings/Bats/Recordings/0.pulsosIndividuais_processed_20ms_norm', pattern="wav" , full.names=T)# tk_choose.files()  # escolher gravacoes
if(bat==0) fileName <- list.files('/home/ubc/DISCO2/ProjectosPessoais/A2020/Recordings/Noise/0.processed_20ms_norm', pattern="wav" , full.names=T)# tk_choose.files()  # escolher gravacoes

n_recs <- length(fileName)

rs_df <- matrix(NA, nrow=n_recs, ncol=9200)
for(i in seq(fileName)){
  morc <- import_audio(fileName[i], low=10, high=125)
  fs <- morc$fs
  file <- morc$ficheiro
  som <- morc$som
  ifelse (fs < 50000, tx <- 10, tx <- 1)
  #while(length(som) > fs*tx*20/1000) som <- som[-c(length(som))] # O script de cortar os pulsos e o ruido deixa 1 sample a mais

  spec2 <- Spectrogram(Audio = as.numeric(som),
                       norm = 150,
                       SamplingFrequency = fs * tx,
                       WindowLength = 1, # em milisegundos
                       FrequencyResolution = 1, # valor normal e 2
                       TimeStepSize = 0.25,   ## corresponde a 75% sobreposicao
                       DynamicRange = thresh,
                       WindowType = "hanning",
                       plot = F)

  ## Cut to desired frequency range
  filt <- c(which(as.numeric(colnames(spec2)) < 10000), which(as.numeric(colnames(spec2)) >= 125000))
  #if(fs==441000) filt <- c(which(as.numeric(colnames(spec2)) < 10000), which(as.numeric(colnames(spec2)) >= 125000))
  #if(fs==300000) filt <- c(which(as.numeric(colnames(spec2)) < 10000), which(as.numeric(colnames(spec2)) >= 125000))
  spec2_filt <- spec2[,-filt] # eliminar range de frequencia nao necessario
  spec2_filt<- spec2_filt + thresh
  while(dim(spec2_filt)[1] > 80) spec2_filt <- spec2_filt[-1,]
  while(dim(spec2_filt)[2] > 115) spec2_filt <- spec2_filt[,-1]
  if(dim(spec2_filt)[1]==80 && dim(spec2_filt)[2]==115){

    ncol <- length(colnames(spec2_filt))
    nrow <- length(rownames(spec2_filt))
    ##Calcular mediana por coluna do ln(S)e subtrair a S## LIMPA O ESPREOTRAGRAMA NA VERTICAL
    mediana<-matrix(0,1,ncol)
    for (j in 1:ncol) ##Calcular a m?diana de cada linha da matrix
    {
      mediana[,j] <-  mean(spec2_filt[,j])## Matriz com 1 coluna e cada linha ? a m?dia da linha respectiva de Smag
    }
    mediana <- matrix(mediana,nrow,ncol, byrow = T) #matrix(mediana,nrow,ncol) ##Matriz com nrow e ncol com todas as linhas iguais
    spec2_filt<- spec2_filt - mediana
    spec2_filt <- spec2_filt / max(spec2_filt)
    spec2_filt[spec2_filt<0] <- 0
    #spec2_filt <- as.numeric(spec2_filt)


    rs_df[i,] <- as.numeric(r(spec2_filt))
    #
    # if(i==1){
    #   rs_df <- as.numeric(r(spec2_filt))
    # }else{
    #   rs_df <- rbind(rs_df, as.numeric(r(spec2_filt)))
    # }
    #train_x <- array(train_x, dim = c(2, img_rows, img_cols, 1))
    print(paste0(i,' of ', length(fileName)))

  }#final do if dim
}# final do for

path <- '/home/ubc/DADOS/BRUNO/A2020/1.Geraldina/database/'

if(bat == 1) {
  dados_bats <- rs_df #array(rs_df, dim = c(n_recs, img_rows, img_cols, 1))
  labels_bats <- rep(bat, n_recs)
  rec_bats <- fileName
  save(rec_bats,dados_bats, labels_bats, file=paste0(path,"bats_win1ms_step0.25ms_res1khz_thresh90.RData"))
} else {
  dados_noise <- rs_df #array(rs_df, dim = c(n_recs, img_rows, img_cols, 1))
  labels_noise <- rep(bat, n_recs)
  rec_noise<- fileName
  save(rec_noise,dados_noise, labels_noise, file=paste0(path,"noise_win1ms_step0.25ms_res1khz_thresh90.RData"))
}


