# Load Variables ----------------------------------------------------------
  low_filt <- 10 #If fqrcut=manual, Butter High-pass Frequency in kHz (integer)
  high_filt <- 110 #If fqrcut=manual, Butter Low-pass Frequency in kHz (integer)
  butt <- TRUE #Apply Butter filter (TRUE or FALSE)
  win_pos <- "+900+400" #posicao das janelas tcltk de erro


# Load packages -----------------------------------------------------------
library(zoo)
library(signal)
library(tuneR)
library(seewave)
library(Rutils)
require(tcltk)
  library(RSQLite)
  library(dplyr)
  library(dbplyr)


# Functions ---------------------------------------------------------------

main<- function() {
  try(dev.off(), silent = TRUE)
  fileName <- tcltk::tk_choose.files()
  for (i in 1:length(fileName)) {
      audio_data <- import_audio(fileName[i], butt = butt, low = low_filt, high = high_filt)

      puls <- call_label(sound_original = audio_data$sound_original,
                        sound_filt = audio_data$sound_filt,
                        file_name = audio_data$file_name,
                        fs = audio_data$fs, tx = audio_data$tx,
                        t_intervalo = 40, win_size = c(13, 5))


      # Print to file call parameters
      write.table(puls,
                  file=paste0(fileName[i],"_calls",".csv"),
                  append=TRUE, row.names=FALSE, col.names=FALSE, sep=",")

       dev.off()

  }
}


spec.bs.colors<-function (n) {
  a<-rev(c("#00001F","#00002F","#00003F","#00004F","#00005F",
           "#00006F","#00007F","#00008F","#00009F","#0000AF",
           "#0000BF","#0000CF","#0000DF","#0000EF","#0000FF",
           "#0F00F0","#1F00E0","#2F00D0","#3F00C0","#4F00B0",
           "#5F00A0","#6F0090","#7F0080","#8F0070","#9F0060",
           "#AF0050","#BF0040","#CF0030","#DF0020","#EF0010",
           "#FF0000","#FF0F00","#FF1F00","#FF2F00","#FF3F00",
           "#FF4F00","#FF5F00","#FF6F00","#FF7F00","#FF8F00",
           "#FF9F00","#FFAF00","#FFBF00","#FFCF00","#FFDF00",
           "#FFEF00","#FFFF00","#FFFF0F","#FFFF1F","#FFFF2F",
           "#FFFF3F","#FFFF4F","#FFFF5F","#FFFF6F","#FFFF7F",
           "#FFFF8F","#FFFF9F","#FFFFAF","#FFFFBF","#FFFFCF"))
  c(palette(a))
}





main()

