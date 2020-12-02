library(keras)
seed <- 1002
setwd('~/Projectos/bat_detector/batdecoder_model_train/')
load("./database/ms01/train_ready_noRhinos_MschPpygAdjusted_3samplesPerCall.RDATA")
batch_size <- 64 #define batch_size even after loading space

# Modelo inception02 - (especies_noRhinos + ruido) - 2 batch training   --------
train_y <- train_y[,-22]

# treinei com x1, y1 e depois retreinei com x2, y2
# train_y1 <- train_y[1:30000,] 
# train_x1 <- train_x[1:30000,,] 
# dim(train_x1) <- c(dim(train_x1),1)

train_y2 <- train_y[30001:58534,] 
train_x2 <- train_x[30001:58534,,] 
dim(train_x2) <- c(dim(train_x2),1)

# load net structure
source("./model_structures/model_inception03_funtionalAPI.R")
summary(model)

history<-model %>% fit(train_x1, train_y1, 
                       batch_size = batch_size, 
                       validation_data=list(test_x, test_y),
                       epochs = 50,
                       callbacks = list(callback_early_stopping(patience = 2, monitor = 'val_acc'),
                                        callback_model_checkpoint("./model_results/inception03_ms01/generation1/modelo_inception03_gen1_{epoch:02d}-{val_acc:.4f}.hdf5", monitor = "acc")),
                       shuffle = TRUE,
                       verbose = 1)
save(history, file="./model_results/inception03_ms01/generation1/history_model_inception03.RDATA")

