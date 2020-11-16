library(dplyr)
library(dbplyr)
library(tensorflow)
library(keras)
library(abind)

# Carregar bd de re-train
load("../recLabel_database/ms01/datasets/re_train_ready_noRhinos_MschPpygAdjusted_sampleFromOriginalBD.RDATA")

# Carregar as matrizes da base de dados dos erros
load("../recLabel_database/ms01/datasets/new_train_matrix_retrain_handRelesase_1percent.RDATA") 
# aparecem alguns na não sei porque. passalos a 0
new_train_matrix[[1]] [which(is.na(new_train_matrix[[1]]))] <- 0

new_train_matrix[[2]] <- gsub(pattern = "noise", replacement = "Noise", x = new_train_matrix[[2]])
new_train_matrix[[2]] <- gsub(pattern = "EE", replacement = "Eisa", x = new_train_matrix[[2]])

# Parametros para o treino
total = dim(new_train_matrix[[1]])[1]
img_rows = input_shape[2] # img_rows e img_cols estao trocados aqui pq aplico o rotate no script antes (com rotate as imagens aparecem como devem. sem rotate aparecem de pernas para o ar)
img_cols = input_shape[1]
input_shape=c(img_rows, img_cols, 1)
seed = 1002

# Converter para one hot encoding
aux <- new_train_matrix[[2]]
aux <- gsub(pattern = "Noise", replacement = 0, x = aux)
aux <- gsub(pattern = "Bbar", replacement = 1, x = aux)
aux <- gsub(pattern = "Eisa", replacement = 2, x = aux)
aux <- gsub(pattern = "Eser", replacement = 3, x = aux)
aux <- gsub(pattern = "Hsav", replacement = 4, x = aux)
aux <- gsub(pattern = "Mbec", replacement = 5, x = aux)
aux <- gsub(pattern = "Mbly", replacement = 6, x = aux)
aux <- gsub(pattern = "Mdau", replacement = 7, x = aux)
aux <- gsub(pattern = "Mema", replacement = 8, x = aux)
aux <- gsub(pattern = "Mesc", replacement = 9, x = aux)
aux <- gsub(pattern = "Mmyo", replacement = 10, x = aux)
aux <- gsub(pattern = "Mmys", replacement = 11, x = aux)
aux <- gsub(pattern = "Msch", replacement = 12, x = aux)
aux <- gsub(pattern = "Nlas", replacement = 13, x = aux)
aux <- gsub(pattern = "Nlei", replacement = 14, x = aux)
aux <- gsub(pattern = "Paur", replacement = 15, x = aux)
aux <- gsub(pattern = "Paus", replacement = 16, x = aux)
aux <- gsub(pattern = "Pkuh", replacement = 17, x = aux)
aux <- gsub(pattern = "Ppip", replacement = 18, x = aux)
aux <- gsub(pattern = "Ppyg", replacement = 19, x = aux)
aux <- gsub(pattern = "Tten", replacement = 20, x = aux)

aux_y <- to_categorical(as.numeric(aux), num_classes = num_classes)
aux_x <- new_train_matrix[[1]]

###### Converter para array ########
aux_x <- array(aux_x, dim = c(total, img_cols, img_rows))

##### juntar ao array da bd original
re_train_x <- abind(re_train_x, aux_x, along = 1)
re_train_y <- rbind(re_train_y, aux_y)

# Randomizar a ordem dos casos.
set.seed(seed)
randomize <- sample(dim(re_train_y)[1])
re_train_x <- re_train_x[randomize,,]
re_train_y <- re_train_y[randomize,]

dim(re_train_x) <- c(dim(re_train_x),1)

###### Re-train model gen1 ######
# # load net structure
# model <- load_model_hdf5("../recLabel_database/ms01/gen01_model/modelo_inception02_gen1_v2_16-0.9821.hdf5")
# summary(model)
# 
# model %>%
#   compile(
#     optimizer = optimizer_sgd(lr=0.001, momentum=0.9, nesterov=T), # diminui learning rate 10x 
#     loss = 'categorical_crossentropy',
#     metrics = c('accuracy')
#   )
# 
# history<-model %>% fit(re_train_x, re_train_y, 
#                        batch_size = 64, 
#                        epochs = 1,
#                        callbacks = list(callback_model_checkpoint("../recLabel_database/ms01/gen02_model/modelo_inception02_gen2.hdf5", monitor = "acc")),
#                        shuffle = TRUE,
#                        verbose = 1)
# 
# save(history, file="../recLabel_database/ms01/gen02_model/history_model_gen02.RDATA")

# teste com o mesmo learning rate
model <- load_model_hdf5("~/Projectos/bat_detector/batdecoder_model_train/model_results/inception03_ms01/generation1/modelo_inception02_gen1_v2_16-0.9821.hdf5")
summary(model)

model %>%
  compile(
    optimizer = optimizer_sgd(lr=0.01, momentum=0.9, nesterov=T), # mesmo leraning rate do base model 
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )

history<-model %>% fit(re_train_x, re_train_y, 
                       batch_size = 64, 
                       epochs = 15,
                       callbacks = list(callback_early_stopping(patience = 1, monitor = 'acc'),
                                        callback_model_checkpoint("../recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_1percent_handRelease_{epoch:02d}-{acc:.4f}.hdf5", monitor = "acc")),
                       shuffle = TRUE,
                       verbose = 1)

save(history, file="../recLabel_database/ms01/gen02_model/history_model_lr0.01_gen02_1percent_handRelease.RDATA")

# # teste com o mesmo learning rate (2ª tentativa pq a 1ª correu mt mal)
# model <- load_model_hdf5("~/Projectos/bat_detector/batdecoder_model_train/model_results/inception03_ms01/generation1/modelo_inception02_gen1_v2_16-0.9821.hdf5")
# summary(model)
# 
# model %>%
#   compile(
#     optimizer = optimizer_sgd(lr=0.01, momentum=0.9, nesterov=T), # mesmo leraning rate do base model 
#     loss = 'categorical_crossentropy',
#     metrics = c('accuracy')
#   )
# 
# history<-model %>% fit(re_train_x, re_train_y, 
#                        batch_size = 64, 
#                        epochs = 15,
#                        callbacks = list(callback_early_stopping(patience = 3, monitor = 'acc'),
#                                         callback_model_checkpoint("../recLabel_database/ms01/gen02_model/modelo_inception02_lr0.01_gen2_1percentV2_handRelease_{epoch:02d}-{acc:.4f}.hdf5", monitor = "acc")),
#                        shuffle = TRUE,
#                        verbose = 1)
# 
# save(history, file="../recLabel_database/ms01/gen02_model/history_model_lr0.01_gen02_1percentV2_handRelease.RDATA")
