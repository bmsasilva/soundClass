# Usa as gravacoes dos pulsos individuais
# Cria um novo ficheiro de som de 20ms a partir do ponto central
# dos pulsos individuais

#path <- '/media/DADOS/BRUNO/A2020/1.batDecoder_triagem/Batcall_recs/Bbar'
#path <- '/media/DADOS/BRUNO/A2020/1.batDecoder_triagem/Batcall_recs/Eser'
#path <- '/media/DADOS/BRUNO/A2020/1.batDecoder_triagem/Batcall_recs/Mdau'
#path <- '/media/DADOS/BRUNO/A2020/1.batDecoder_triagem/Batcall_recs/Mmyo'
#path <- '/media/DADOS/BRUNO/A2020/1.batDecoder_triagem/Batcall_recs/Ppip'
#path <- '/media/DADOS/BRUNO/A2020/1.batDecoder_triagem/Batcall_recs/Pkuh'
#path <- '/media/DADOS/BRUNO/A2020/1.batDecoder_triagem/Batcall_recs/Mema'
#path <- '/media/DADOS/BRUNO/A2020/1.batDecoder_triagem/Batcall_recs/Nlei'
#path <- '/home/ubc/DADOS/BRUNO/A2020/prototipo_automatico_old/ModelosTriagem/Recordings/Ruido_Originais/Originais/Ruido_DenisPHD_2015'
path <- '/home/ubc/DISCO2/ProjectosPessoais/A2020/Recordings/Bats/Recordings/0.pulsosIndividuais_norm'

setwd(path)
fileName <- list.files(path, recursive = F, pattern=c('wav'))

## Variaveis de opcoes do script
tintervalo <- 10 # metade do intervalo a analisar

for (i in 1:length(fileName)){
  # Importa som para analisar
  morc <- import_audio(fileName[i], low=10, high=125)
  fs <- morc$fs
  file <- morc$ficheiro
  som <- morc$som
  ifelse (fs < 50000, tx <- 10, tx <- 1)
 
  # Seleciona 20ms de som do centro da gravacao (usando ficheiros previamente preparados por "Split recordings(...).R")
  ## Apenas para as gravacoes de pulsos de morcego individuais (as de 80ms). As recs de ruido foram previamente cortadas a 10ms
  center <- floor(length(morc$som)/2)
  center2 <- floor(length(morc$som)/2)-(fs*tx*2/1000)
  center3 <- floor(length(morc$som)/2)+(fs*tx*2/1000)
    
  som <- morc$som[(center-fs*tx*tintervalo/1000):(center+fs*tx*tintervalo/1000)]
  som2 <- morc$som[(center2-fs*tx*tintervalo/1000):(center2+fs*tx*tintervalo/1000)]
  som3 <- morc$som[(center3-fs*tx*tintervalo/1000):(center3+fs*tx*tintervalo/1000)]
  
  savewav(som,fs*tx,file=paste("../0.pulsosIndividuais_processed_20ms_norm/",file,"_",1,".wav"))
  savewav(som2,fs*tx,file=paste("../0.pulsosIndividuais_processed_20ms_norm/",file,"_",2,".wav"))
  savewav(som3,fs*tx,file=paste("../0.pulsosIndividuais_processed_20ms_norm/",file,"_",3,".wav"))
}

import_audio <- function(fileName, butt=TRUE, low, high) { 
  
  # Get the filename without the file extension
  ficheiro <- unlist(strsplit(fileName, "\\."))[1]
  
  # Import recording
  dados <- tuneR::readWave(fileName)
  
  # Get fs, tx and file date
  fs <- dados@samp.rate
  ifelse(fs<50000,tx <- 10, tx <- 1)
  fileTime <- file.info(ficheiro)$mtime
  
  # If recording is stereo, convert to mono (i.e. keep channel with higher amplitude)
  # If is mono keep left channel
  if (dados@stereo)  { 
    if (sum(abs(dados@left)) >= sum(abs(dados@right))) {
      som <- dados@left
    }  else {        
      som <- dados@right
    }
  }  else { 
    som <- dados@left 
  }
  
  # Apply butterworth filter
  if (butt) { 
    limit_low <- low*1000 / (fs * tx / 2)
    limit_high <- high*1000 / (fs * tx / 2)
    bt_low <- butter(10, limit_low, type="high") 
    bt_high <- butter(10, limit_high, type="low")
    som <- as.integer(filter(bt_low, som))
    som <- as.integer(filter(bt_high, som))
    
    # Nova passagem do filtro para limpar melhor
    som <- as.integer(filter(bt_low, som))
    som <- as.integer(filter(bt_high, som))
  }
  list(som=som, ficheiro=ficheiro, fileTime=fileTime, fs=fs, tx=tx)
} #  final da fun??o

