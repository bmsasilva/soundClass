library(keras)

LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

rdata_list <- LoadToEnvironment("/home/bruno/packages_test_data/recordings/sp_data.RDATA")
rdata_list <- rdata_list[[names(rdata_list)[1]]]

# Parametros para o treino
total <- dim(rdata_list[[1]])[1]
img_rows = rdata_list[[3]]$input_shape_rows # img_rows e img_cols estao trocados aqui pq aplico o rotate no script antes (com rotate as imagens aparecem como devem. sem rotate aparecem de pernas para o ar)
img_cols = rdata_list[[3]]$input_shape_cols

#input_shape=c(img_rows, img_cols, 1)
input_shape=c(img_cols, img_rows, 1)
seed = 1002

num_classes <- length(unique(rdata_list[[2]]))

rdata_list[[2]] <- as.factor(rdata_list[[2]])#converter para factor para facilitar os numeros e aos nomes de classe repectivos
labels_code <- as.integer(rdata_list[[2]]) - 1
labels_name <- as.character(rdata_list[[2]])
labels_df <- data.frame(name = levels(rdata_list[[2]]), code = (1:length(levels(rdata_list[[2]])))-1)

data_y <- to_categorical(as.numeric(labels_code), num_classes = num_classes)

# # Randomizar a ordem dos casos e converter para array.
set.seed(seed)
randomize <- sample(dim(data_y)[1])
data_x <- rdata_list[[1]]
data_x <- data_x[randomize,]
data_y <- data_y[randomize,]
data_x <- array(data_x, dim = c(total, img_cols, img_rows, 1))

# load net structure
source("./inst/model_architectures/model_vgg_sequential.R")
summary(model)

# fit
model %>%
  compile(
    optimizer = optimizer_sgd(lr=0.01, momentum=0.9, nesterov=T), # mesmo leraning rate do base model 
    loss = 'categorical_crossentropy',                                                                                                                     
    metrics = c('accuracy')
  )

history<-model %>% fit(data_x, data_y, 
                       batch_size = 64, 
                       epochs = 20,
                       callbacks = list(callback_early_stopping(patience = 10, monitor = 'accuracy'),
                                        callback_model_checkpoint("./model_{epoch:02d}-{accuracy:.4f}.hdf5", monitor = "accuracy")),
                       shuffle = TRUE,
                       validation_split = 0.2,
                       verbose = 1)

save(history, file="./history_model.RDATA")

###### so para confirmar que as imagens estão a entrar correctamebnte
  gen_images <- image_data_generator()
  
  #Fit image data generator internal statistics to some sample data
  gen_images %>% fit_image_data_generator(data_x)
  
  #Generates batches of augmented/normalized data from image data and #labels to visually see the input images to the Model
  model %>% fit(
    flow_images_from_data(data_x, data_y,
                          gen_images,
                          batch_size=64,
                          save_to_dir="./extra_files/"),
    steps_per_epoch=5,
    epochs = 80 )
