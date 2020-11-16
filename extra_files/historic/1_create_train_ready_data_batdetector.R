library(keras)
source("./bin/train_test_split.R")
seed <- 1002

# data prep 1 sample per call ---------------------------------------------------------------
setwd('~/Projectos/bat_detector/batdecoder_model_train/')
load('./database/db_1sample_per_call/bats_noRhinos_version2_1samplePerCall.RData')
load('./database/db_1sample_per_call/noise_noRhinos_version2_1samplePerCall.RData')

# Definir nº de classes
label <- c(rep("bat", length(files_bats)), rep("noise", length(files_noise)))
num_classes <- length(unique(label))

# Set completo
dados_train <- rbind(spec_image_bats, spec_image_noise) #set completo
dados_train <- cbind(dados_train, label)
remove(spec_image_bats) # free ram
remove(spec_image_noise)# free ram
gc() # free ram
n_recs <- length(label)

new_df <- train_test_split(dados_train, 
                           percent = c(1/4, 3/4), seed = seed)

train_x <- dados_train[(1:train_len),]
test_x <- dados_train[(train_len + 1) : n_recs,]
remove(dados_train) # clean RAM
gc() # clean RAM
test_x <- array(test_x, dim = c((n_recs - train_len), img_rows, img_cols, 1)) #assim o array é aceite na rede mas não me parece bem
gc() # clean RAM
train_y<-to_categorical(label[1:train_len], num_classes = num_classes)
gc() # clean RAM
test_y<-to_categorical(label[(train_len + 1) : n_recs], num_classes = num_classes)
test_recs <- recs[(train_len + 1) : n_recs]



################################################################################################################################
class_labels <- c("bat", "noise")


#name_classes <- c("Noise", unique(bat_species))







# Parametros dos spectrogramas da bd (tem de ser igual aos do script que originou os espectrogramas)
size <- 20 # ms
window_length <- 1 # em milisegundos
frequency_resolution <- 1 # valor normal e 2
time_step_size <- 0.25
dynamic_range <- 90
freq_range <- c(10, 80) #khertz
input_shape_db <- c(size / time_step_size, freq_range[2] - freq_range[1])

# Parametros para o treino
total = length(label)
img_rows = input_shape_db[2] # img_rows e img_cols estao trocados aqui pq aplico o rotate no script antes (com rotate as imagens aparecem como devem. sem rotate aparecem de pernas para o ar)
img_cols = input_shape_db[1]
input_shape=c(img_rows, img_cols, 1)


# Save processed data for retraining
save(label,num_classes, input_shape,train_x, train_y, test_x, test_y, files_bats, files_noise, recs, test_recs, name_classes,
     file = "./database/db_1sample_per_call/retrain_ready_noRhinos_1samplePerCall.RData")

train_x <- array(train_x, dim = c(train_len, img_rows, img_cols, 1))
gc() # clean RAM


#checking the dimentions
dim(train_x)
dim(test_x)
dim(train_y)
dim(test_y)

# Save processed data to minimize future time of data prep
save(label,num_classes, input_shape,train_x, train_y, test_x, test_y, files_bats, files_noise, recs, test_recs, name_classes,
     file = "./database/db_1sample_per_call/train_ready_noRhinos_1samplePerCall.RData")


# data prep 3 samples per call ---------------------------------------------------------------
setwd('~/Projectos/bat_detector/batdecoder_model_train/')
load('./database/db_3samples_per_call/bats_withRhinos_version2_3samplesPerCall.RData')
load('./database/db_3samples_per_call/noise_withRhinos_version2_3samplesPerCall.RData')

bat_species <- c()
for (i in seq(rec_bats)){
  aux <-
    strsplit(rec_bats[[i]], "/")[[1]][[11]] %>%
    substr(1,4)
  bat_species[i] <- aux
}

# Juntar algumas especies em grupos 
bat_species[bat_species %in% c("Plec")] <- "Paur"
bat_species[bat_species %in% c("Mnat")] <- "Mesc"

# Eliminar rhinos
del_rhino <- which(bat_species %in% c("Rmeh", "Reur", "Rhip",
                                      "Rfer"))
bat_species <- bat_species[-del_rhino]
labels_bats <- labels_bats[-del_rhino]
rec_bats <- rec_bats[-del_rhino]
dados_bats <- dados_bats[-del_rhino, ]

class_labels <- c("Noise", "Bbar","Eisa","Eser","Hsav", "Mbec","Mbly","Mdau","Mema","Mesc",
                       "Mmyo","Mmys","Msch","Nlas","Nlei","Paur", "Paus", "Pkuh", "Ppip","Ppyg", "Tten")


# Definir nº de classes
num_classes <- length(unique(bat_species)) + 1 # +1 para incluir o ruido como classe
name_classes <- c("Noise", unique(bat_species))

# Set completo
dados_train <- rbind(dados_bats, dados_noise) #set completo
remove(dados_bats) # free ram
remove(dados_noise)
gc() # free ram
labels_bats <- as.numeric(as.factor(bat_species))
label <-c(labels_bats, labels_noise)
recs <- c(rec_bats, rec_noise)
n_recs <- length(recs)

# Parametros dos spectrogramas da bd (tem de ser igual aos do script que originou os espectrogramas)
size <- 20 # ms
window_length <- 1 # em milisegundos
frequency_resolution <- 1 # valor normal e 2
time_step_size <- 0.25
dynamic_range <- 90
freq_range <- c(10, 80) #khertz
input_shape_db <- c(size / time_step_size, freq_range[2] - freq_range[1])

# Randomizar a ordem dos casos.
set.seed(seed)
randomize <- sample(n_recs)
dados_train <- dados_train[randomize,]
label <- label[randomize]
recs <- recs[randomize]

# Parametros para o treino
total = length(label)
img_rows = 70 # img_rows e img_cols estao trocados aqui pq aplico o rotate no script antes (com rotate as imagens aparecem como devem. sem rotate aparecem de pernas para o ar)
img_cols = 80
input_shape=c(img_rows, img_cols, 1)

#TRAINING DATA AND TESTING DATA###
train_len <- floor(n_recs * 0.7) 
train_x <- dados_train[(1:train_len),]
test_x <- dados_train[(train_len + 1) : n_recs,]
remove(dados_train) # clean RAM
gc() # clean RAM
train_x <- array(train_x, dim = c(train_len, img_rows, img_cols, 1))
gc() # clean RAM
train_y<-to_categorical(label[1:train_len], num_classes = num_classes)
gc() # clean RAM
test_x <- array(test_x, dim = c((n_recs - train_len), img_rows, img_cols, 1)) #assim o array é aceite na rede mas não me parece bem
gc() # clean RAM
test_y<-to_categorical(label[(train_len + 1) : n_recs], num_classes = num_classes)
test_recs <- recs[(train_len + 1) : n_recs]

#checking the dimentions
dim(train_x) 
dim(test_x)
dim(train_y) 
dim(test_y)

# Save processed data to minimize future time of data prep
save(label, class_labels, num_classes, input_shape,train_x, train_y, test_x, test_y, rec_bats, rec_noise, recs, test_recs, name_classes,
     file = "./database/db_3samples_per_call/train_ready_noRhinos_3samplesPerCall.RData")
