library(keras)
seed <- 1002

model<-load_model_hdf5('/home/ubc/DADOS/BRUNO/A2020/1.Geraldina/models/model_triagem_313.hdf5')

load(paste0('/media/DADOS/BRUNO/A2020/1.Geraldina/database/bats_win1ms_step0.25ms_res1khz_thresh90_filt.RData'))
load(paste0('/home/ubc/DADOS/BRUNO/A2020/MyPapers/geraldina/RetrainModels_estacaoMitra/day1_errors/processed/noise_errorsDay1.RData'))

## andomizar a ordem dos casos dos morcegos. APENAS QD SE UTILIZA MENOS BATS DO QUE O TOTAL
set.seed(seed)
randomize <- sample(length(labels_bats))
dados_bats <- dados_bats[randomize,]
labels_bats <- labels_bats[randomize]
rec_bats <- rec_bats[randomize]

# Definir tamanhoda amostra de morcegos
size <- length(rec_noise)
dados_bats <-dados_bats[seq(size),]
labels_bats <-labels_bats[seq(size)]
rec_bats <-rec_bats[seq(size)]
  
bat_species <- c()
for (i in seq(rec_bats)){
aux <- 
  strsplit(rec_bats[[i]], "/")[[1]][11] %>%
  substr(1,4)
  bat_species[i] <- aux
}

# # Set completo apenas com numero de bats igual a ruido
# dados_train <- rbind(dados_bats[1:length(dados_noise[,1]),], dados_noise) #set completo
# label <-c(labels_bats[1:length(dados_noise[,1])], labels_noise)
# recs <- c(rec_bats[1:length(dados_noise[,1])], rec_noise)
# n_recs <- length(recs)
# remove(dados_bats) # free ram
# remove(dados_noise) # free ram
# gc() # free ram

# Set completo 
dados_train <- rbind(dados_bats, dados_noise) #set completo
label <-c(labels_bats, labels_noise)
recs <- c(rec_bats, rec_noise)
n_recs <- length(recs)
remove(dados_bats) # free ram
remove(dados_noise) # free ram
gc() # free ram


# Randomizar a ordem dos casos.
set.seed(seed)
randomize <- sample(n_recs)
dados_train <- dados_train[randomize,]
label <- label[randomize]
recs <- recs[randomize]

# Parametros para o treino
total = length(label)
img_rows = 115 # img_rows e img_cols estao trocados aqui pq aplico o rotate no script antes (com rotate as imagens aparecem como devem. sem rotate aparecem de pernas para o ar)
img_cols = 80
input_shape=c(img_rows, img_cols, 1)

#TRAINING DATA AND TESTING DATA###
train_len <- floor(n_recs * 0.7) 
train_x <- dados_train[(1:train_len),]
test_x <- dados_train[(train_len + 1) : n_recs,]
remove(dados_train) # clean RAM
gc() # clean RAM
train_x <- array(train_x, dim = c(train_len, img_rows, img_cols, 1))
train_y<-to_categorical(label[1:train_len], num_classes = 2)
test_x <- array(test_x, dim = c((n_recs - train_len), img_rows, img_cols, 1)) #assim o array é aceite na rede mas não me parece bem
test_y<-to_categorical(label[(train_len + 1) : n_recs], num_classes = 2)
test_recs <- recs[(train_len + 1) : n_recs]

#checking the dimentions
dim(train_x) 
dim(test_x)
dim(train_y) 
dim(test_y)

# #a linear stack of layers
# model<-keras_model_sequential()
# model %>%  
#   layer_conv_2d(filter=32, kernel_size=c(5,5), padding="valid", input_shape=input_shape, strides=c(2,2), kernel_initializer = "lecun_uniform" ) %>%  
#   layer_activation("relu") %>%  
#   layer_batch_normalization() %>%
#   layer_max_pooling_2d(pool_size=c(2,2)) %>% 
#   #layer_dropout(rate = 0.25) %>% 
# 
#   layer_conv_2d(filter=32, kernel_size=c(3,3), kernel_initializer = "lecun_uniform", strides=c(1,1)) %>%  
#   layer_activation("relu") %>% 
#   layer_batch_normalization() %>%
#   layer_max_pooling_2d(pool_size=c(2,2)) %>% 
#   #layer_dropout(rate = 0.25) %>% 
#   
#   layer_flatten() %>% 
#   layer_dropout(0.25)%>%
#   layer_dense(64) %>%  
#   layer_activation("relu") %>%  
#   layer_dropout(0.25) %>%  
#   # layer_dense(2048) %>%  
#   # layer_activation("relu") %>%  
#   # layer_dropout(0.5) %>% 
#   layer_dense(2) %>%  
#   layer_activation("softmax") 
#   # compile(
#   #   optimizer = 'rmsprop',
#   #   loss = 'binary_crossentropy',
#   #   metrics = c('accuracy')
#   # )
# 
# 
# #Model's Optimizer
# #defining the type of optimizer-ADAM-Adaptive Momentum Estimation
# #opt<-optimizer_adam( lr= 0.0001 , decay = 1e-6 )
# #lr-learning rate , decay - learning rate decay over each update
# 
# # model %>%
# #   compile(
# #     optimizer = 'rmsprop',
# #     loss = 'binary_crossentropy',
# #     metrics = c('accuracy')
# #   )


model %>%
compile( 
  optimizer = optimizer_sgd(lr=0.01, momentum=0.9, nesterov=T),
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

#Summary of the Model and its Architecture
summary(model)


# O retarin acontece ate a a val_acc não aumentar 2 epochs de seguida
history<-model %>% fit(train_x, train_y, 
               batch_size=512, 
               validation_data=list(test_x,test_y),
               epochs=4,
               #validation_split=0.3,
               shuffle=TRUE,
               verbose = 1)

loss_and_metrics <- 
  model %>% 
  evaluate(test_x, test_y, batch_size = 512)

classes <- 
  model %>% 
  predict(test_x, test_size = 128)

classes<-cbind(classes,test_y, test_recs)
View(classes)

#####
data_augmentation <- TRUE  
if(!data_augmentation) {  
  model %>% fit( train_x, train_y , 
                 batch_size=256, 
                 validation_data=list(test_x,test_y),
                 epochs=50,
                 #validation_split=0.3,
                 shuffle=TRUE,
   verbose = 1)
} else {  
  #Generating images
  
  gen_images <- image_data_generator(featurewise_center = TRUE,
                                     featurewise_std_normalization = TRUE,
                                     rotation_range = 20,
                                     width_shift_range = 0.30,
                                     height_shift_range = 0.30,
                                     horizontal_flip = TRUE )
  
  #Fit image data generator internal statistics to some sample data
  gen_images %>% fit_image_data_generator(train_x)
  
  #Generates batches of augmented/normalized data from image data and #labels to visually see the input images to the Model
  model %>% fit_generator(
    flow_images_from_data(train_x, train_y,
                          gen_images,
                          batch_size=256,
                          save_to_dir="./images_out/"),
    steps_per_epoch=as.integer(50000/256),
    epochs = 80,
    validation_data = list(test_x, test_y) )
}
#use save_to_dir argument to specify the directory to save the #images generated by the Model and to visually check the Model's #output and ability to classify images.

save_model_hdf5(model, '/home/ubc/DADOS/BRUNO/A2020/1.Geraldina/models/model_triagem_4.3.1_errorsDay1.hdf5', overwrite = TRUE,
                include_optimizer = TRUE)
save(history, file="/home/ubc/DADOS/BRUNO/A2020/1.Geraldina/models/history_mod431.RDATA")

