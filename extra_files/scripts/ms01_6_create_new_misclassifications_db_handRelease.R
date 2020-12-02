# Step 1) open wav and RDATA files with noise as bats misclassified recordings
# Step 2) colate all RDATA in data.frame
# Step 3) add data.frame to existing database created with recLabel on
#         misclassified bat recordings

# NOTA: NECESSARIO TROCAR OS NOME DOS RDATA PARA REMOVER O .wav.

library(dplyr)
library(dbplyr)

###### Model gen01 ######
# # Input parameters --------------------------------------------------------
# 
# file_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen1/noise_as_bats/days001-015/"
# files_noise_as_bats <- list.files(file_path, recursive = FALSE, pattern="wav|WAV")
# rdata_noise_as_bats <- list.files(file_path, recursive = FALSE, pattern="rdata|RDATA")
# n_files <- length(files_noise_as_bats)
# version <- "v2"
# 
# 
# # Version parameters ------------------------------------------------------
# 
# if(version == "v1"){
#   # all species # smaller image
#   size <- 20 # ms
#   window_length <- 1 # em milisegundos
#   frequency_resolution <- 1 # valor normal e 2
#   time_step_size <- 0.25
#   dynamic_range <- 90
#   freq_range <- c(10, 125) #hertz
#   input_shape <- c(size / time_step_size, 115)
# } else if(version == "v2"){
#   # without rhinolophus # smaller image
#   size <- 20 # ms
#   window_length <- 1 # em milisegundos
#   frequency_resolution <- 1 # valor normal e 2
#   time_step_size <- 0.25
#   dynamic_range <- 90
#   freq_range <- c(10, 80) #khertz
#   input_shape <- c(size / time_step_size, 70)
# } else if(version == "v3"){
#   # all species  # better resolution
#   size <- 20 # ms
#   window_length <- 1 # em milisegundos
#   frequency_resolution <- 2 # valor normal e 2
#   time_step_size <- 0.1
#   dynamic_range <- 90
#   freq_range <- c(10, 125) #hertz
#   input_shape <- c(size / time_step_size, 230)
# } else if(version == "v4"){
#   # without rhinolophus # better resolution
#   size <- 20 # ms
#   window_length <- 1 # em milisegundos
#   frequency_resolution <- 2 # valor normal e 2
#   time_step_size <- 0.1
#   dynamic_range <- 90
#   freq_range <- c(10, 80) #hertz
#   input_shape <- c(size / time_step_size, 140)
# } else {
#   stop("No valid version selected", call. = FALSE)
# }
# 
# 
# # Create db of noise calls identified as bats -----------------------------
# df <- data.frame(recording = character(),
#                  calls_samples = numeric(),
#                  N = numeric(),
#                  FB = numeric(),
#                  SC = numeric(),
#                  spe = character())
# for(i in seq(rdata_noise_as_bats)){
#   load(paste0(file_path, rdata_noise_as_bats[i]))
#   df <- rbind(df, data.frame(recording = files_noise_as_bats[i],
#                              calls_samples = out_peaks,
#                              N = NA,
#                              FB = NA,
#                              SC = NA,
#                              spe = "noise"))
# }
# 
# 
# # Add data.frame to bat call misclassifications database
# path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen1/torunos_misclassifications_day1to15.sqlite3"
# add_record(path, df, table = "rec_labels")
# 
# # create matrix with spectrograms from location of calls of misclassifications database
# files_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen1/all_misclassified_recs/"
# db_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen1/torunos_misclassifications_day1to15.sqlite3"
# 
# new_train_matrix <- spectro_calls(files_path, db_path, version)
# save(new_train_matrix, file = "new_train_matrix_retrain_gen1.RDATA")

###### Model gen02 ######

# Input parameters --------------------------------------------------------

file_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen2/days1to21/noise_as_bats/"
files_noise_as_bats <- list.files(file_path, recursive = FALSE, pattern="wav|WAV")
rdata_noise_as_bats <- list.files(file_path, recursive = FALSE, pattern="rdata|RDATA")
n_files <- length(files_noise_as_bats)
version <- "v2"


# Version parameters ------------------------------------------------------

if(version == "v1"){
  # all species # smaller image
  size <- 20 # ms
  window_length <- 1 # em milisegundos
  frequency_resolution <- 1 # valor normal e 2
  time_step_size <- 0.25
  dynamic_range <- 90
  freq_range <- c(10, 125) #hertz
  input_shape <- c(size / time_step_size, 115)
} else if(version == "v2"){
  # without rhinolophus # smaller image
  size <- 20 # ms
  window_length <- 1 # em milisegundos
  frequency_resolution <- 1 # valor normal e 2
  time_step_size <- 0.25
  dynamic_range <- 90
  freq_range <- c(10, 80) #khertz
  input_shape <- c(size / time_step_size, 70)
} else if(version == "v3"){
  # all species  # better resolution
  size <- 20 # ms
  window_length <- 1 # em milisegundos
  frequency_resolution <- 2 # valor normal e 2
  time_step_size <- 0.1
  dynamic_range <- 90
  freq_range <- c(10, 125) #hertz
  input_shape <- c(size / time_step_size, 230)
} else if(version == "v4"){
  # without rhinolophus # better resolution
  size <- 20 # ms
  window_length <- 1 # em milisegundos
  frequency_resolution <- 2 # valor normal e 2
  time_step_size <- 0.1
  dynamic_range <- 90
  freq_range <- c(10, 80) #hertz
  input_shape <- c(size / time_step_size, 140)
} else {
  stop("No valid version selected", call. = FALSE)
}


# Create db of noise calls identified as bats -----------------------------
df <- data.frame(recording = character(),
                 calls_samples = numeric(),
                 N = numeric(),
                 FB = numeric(),
                 SC = numeric(),
                 spe = character())
for(i in seq(rdata_noise_as_bats)){
  load(paste0(file_path, rdata_noise_as_bats[i]))
  df <- rbind(df, data.frame(recording = files_noise_as_bats[i],
                             calls_samples = out_peaks,
                             N = NA,
                             FB = NA,
                             SC = NA,
                             spe = "noise"))
}


# Add data.frame to bat call misclassifications database
path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen2/torunos_misclassifications_day1to21.sqlite3"
add_record(path, df, table = "rec_labels")

# create matrix with spectrograms from location of calls of misclassifications database
files_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen2/all_misclassified_recs/"
db_path <- "/home/bruno/Projectos/phd/0.workfolder/meus_papers/ms01_automatic_classification_cnn/ms01_recs_caseStudy1_torunos/misclassifications_gen2/torunos_misclassifications_day1to21.sqlite3"

new_train_matrix <- spectro_calls(files_path, db_path, version)
save(new_train_matrix, file = "new_train_matrix_retrain_gen2.RDATA")
