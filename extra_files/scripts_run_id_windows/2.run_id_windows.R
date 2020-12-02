library(keras)
library(zoo)
library(seewave)
library(tuneR)
library(signal)

for(file in list.files("~/biobox/Analise/R/", full.names = T)) source(file)


class_labels <- c("Noise", "Bbar","Eisa","Eser","Hsav", "Mbec","Mbly","Mdau","Mema","Mesc",
                  "Mmyo","Mmys","Msch","Nlas","Nlei", "Paur", "Paus", "Pkuh", "Ppip","Ppyg", "Tten")

# estou a usar o modelo retrained 10% handrelease do artigo

csv_file <-"analise_Agosto_Egiluze_R.csv"
out_dir <-"~/biobox/Analise/analise_Agosto_Egiluze_R/"
        
auto_id_biobox(model_path = "~/biobox/modelo_inception02_lr0.01_gen2_10percent_handRelease_05-0.9991.hdf5",
        file_path = "~/biobox/Agosto/Egiluze_R_norm/",
        csv_file = csv_file,
          out_dir = 'analise_Agosto_Egiluze_R/',
        class_labels = class_labels,
        save_png = F,
        save_spec = T,
        version = "v2",
        plot2console = F,
        low = 22,
         high = 120)

# mover recs com morcegos para pasta
path <- "~/biobox/Analise/analise_Agosto_Egiluze_R/"
data <- read.csv(paste0(path, csv_file))

filt <- which(data[,2] != "Noise")
data <- data[filt,]
try({
        for(i in seq(data[,1])) file.rename(paste0(data[i,1],".wav"),
                                            paste0("~/biobox/Agosto/Egiluze_R_norm/bats/", data[i,1],".wav")) 
})

################
# csv_file <-"analise_Agosto_Begiristain_R.csv"
# out_dir <-"~/biobox/Analise/analise_Agosto_Begiristain_R/"
# 
# auto_id_biobox(model_path = "~/biobox/modelo_inception02_lr0.01_gen2_10percent_handRelease_05-0.9991.hdf5",
#                file_path = "~/biobox/Agosto/Begiristain_R_norm/",
#                csv_file = csv_file,
#                out_dir = 'analise_Agosto_Begiristain_R/',
#                class_labels = class_labels,
#                save_png = F,
#                save_spec = T,
#                version = "v2",
#                plot2console = F,
#                low = 22,
#                high = 120)
# 
# # mover recs com morcegos para pasta
# path <- "~/biobox/Analise/analise_Agosto_Begiristain_R/"
# data <- read.csv(paste0(path, csv_file))
# 
# filt <- which(data[,2] != "Noise")
# data <- data[filt,]
# 
# try({
#         for(i in seq(data[,1])) file.rename(paste0(data[i,1],".wav"),
#                                             paste0("~/biobox/Agosto/Begiristain_R_norm/bats/", data[i,1],".wav")) 
# })

################
# csv_file <-"analise_Agosto_Petritegi_R.csv"
# out_dir <-"~/biobox/Analise/analise_Agosto_Petritegi_R/"
# 
# auto_id_biobox(model_path = "~/biobox/modelo_inception02_lr0.01_gen2_10percent_handRelease_05-0.9991.hdf5",
#                file_path = "~/biobox/Agosto/Petritegi_R_norm/",
#                csv_file = csv_file,
#                out_dir = 'analise_Agosto_Petritegi_R/',
#                class_labels = class_labels,
#                save_png = F,
#                save_spec = T,
#                version = "v2",
#                plot2console = F,
#                low = 22,
#                high = 120)
# 
# # mover recs com morcegos para pasta
# path <- "~/biobox/Analise/analise_Agosto_Petritegi_R/"
# data <- read.csv(paste0(path, csv_file))
# 
# filt <- which(data[,2] != "Noise")
# data <- data[filt,]
# try({
#         for(i in seq(data[,1])) file.rename(paste0(data[i,1],".wav"),
#                                             paste0("~/biobox/Agosto/Petritegi_R_norm/bats/", data[i,1],".wav")) 
# })
################
# csv_file <-"analise_Agosto_Zapiain_C.csv"
# out_dir <-"~/biobox/Analise/analise_Agosto_Zapiain_C/"
# 
# auto_id_biobox(model_path = "~/biobox/modelo_inception02_lr0.01_gen2_10percent_handRelease_05-0.9991.hdf5",
#                file_path = "~/biobox/Agosto/Zapiain_C_norm/",
#                csv_file = csv_file,
#                out_dir = 'analise_Agosto_Zapiain_C/',
#                class_labels = class_labels,
#                save_png = F,
#                save_spec = T,
#                version = "v2",
#                plot2console = F,
#                low = 10,
#                high = 120)
# 
# # mover recs com morcegos para pasta
# path <- "~/biobox/Analise/analise_Agosto_Zapiain_C/"
# data <- read.csv(paste0(path, csv_file))
# 
# 
# filt <- which(data[,2] != "Noise")
# data <- data[filt,]
# try({
#         for(i in seq(data[,1])) file.rename(paste0(data[i,1],".wav"),
#                                             paste0("~/biobox/Agosto/Zapiain_C_norm/bats/", data[i,1],".wav")) 
# })
