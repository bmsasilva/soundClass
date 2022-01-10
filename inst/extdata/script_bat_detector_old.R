# files paths
# rec_folder <- system.file("extdata/recordings", package="soundClass")
#label_database <- system.file("extdata", 'demo_db_calls.sqlite3', package="soundClass")

rec_folder <- "/home/bruno/ms_data/training_recordings/"
#setwd("/home/bruno/recordings/")
label_database <- "/home/bruno/ms_data/db_bat_calls.sqlite3"


# spectrograms parameters
spec_params <- list(
  spec_size = 20, #ms
  window_length = 1, #ms
  overlap = 0.5, #percentage
  frequency_resolution = 1, #integer, 1 equal 1kHz per bin, 2 equals 0.5kHz per bin, and so on
  dynamic_range = 100, #higher values will result in lower minimum intensities
  freq_range = c(10, 80) #minimum and maximum frequencies used in kHz
)

# Computing training spectrograms from the labels database
train_calls2 <- spectro_calls(
  files_path = rec_folder,
  updateProgress = NULL, # only for shiny app
  db_path = label_database,
  spec_size = spec_params$spec_size,
  window_length = spec_params$window_length,
  frequency_resolution = spec_params$frequency_resolution,
  time_step_size = (1 - spec_params$overlap) * spec_params$window_length, # vou colocar a funcao a aceitar o overlap directo
  dynamic_range = spec_params$dynamic_range,
  freq_range = spec_params$freq_range,
  tx = "auto"
)

# This function outputs a list with three components: 1) the spectrogram matrices, 2) the labels for each matrix 
# and 3) the parameters used to create the matrices. Using the function train_data_process() this object is transformed to the Keras structure: the spectrogram matrices are converted into an array (XX train data) and the labels one-hot-encoded (YY train data). For better performance the observations are also randomized using a custom seed for reproducibility:
  
  train_data2 <- data_keras(train_calls2, seed = 1002)
  
  # Once the data has been prepared, a blank model must be loaded into R and compiled. A pre-defined model is supplied with the package but the user can also define a custom model. If the pre-defined model is used, two variables must be defined in the global environment before loading the model: the input shape (input_shape) with the format “c(number of rows, number of columns, number of channels)” and the number of classes (num_classes). These values can be found in the third component of the list train_calls (note that the number of channels is always 1, as we are working with grayscale images).
  
  # variables for loading net structure
  input_shape <- c(train_calls[[3]]$img_rows, train_calls[[3]]$img_cols, 1)
  num_classes <- train_calls[[3]]$num_classes
  
  # With the training data prepared and the global model variables defined the model can now be loaded with the base function source():
    
    # load model
    model_path <- system.file("extdata/model_architectures", "model_vgg_sequential.R", package="soundClass")
  source(model_path, local=TRUE)
  
  # At this point, with the training data prepared and the blank model loaded, the parameters for compiling and fitting the model must be set. Please note that as we are using classification models, both parameters loss and metrics should always be set as they are in this example. The other parameters can be tuned as desired:
    
    # parameters for compiling and fitting the model
    mod_params <- list(
      batch_size = 16,
      epochs = 100,
      patience = 4,
      train_per = 0.7,
      lr = 0.01,
      mt = 0.9,
      loss =  'categorical_crossentropy',
      metrics = 'accuracy'
    )
  # 
  # The model is now compiled with a stochastic gradient descent optimizer, an iterative algorithm widely used in machine learning (LeCun et al., 2012)⁠. For further information about the parameters in the optimizer please refer to Keras documentation (https://keras.io/api/).
  
  # compile model
  model %>%
    compile(
      optimizer = optimizer_sgd(lr = 0.01,
                                momentum = 0.9,
                                nesterov = T),
      loss = 'categorical_crossentropy',
      metrics = 'accuracy'
    )
  
  # The compiled model is now ready to be fitted. Three calbacks (objects that can perform actions at various stages of training) are defined: 1) early stopping, 2) model checkpoint and 3)  csv logger. These objects permit to, respectively: 1) stop the fitting process if there is no improvement in the validation dataset accuracy for a defined number of epochs, 2) save the partially fitted model to disk after each iteration and keep only the best model after training for further use and 3) save to disk the log of the training process. For further information about the callbacks available and their usage please refer to Keras documentation (https://keras.io/api/). To fit the model the function fit() from package generics is used:
  
  # fit model
  model %>% fit(train_data$data_x,
                train_data$data_y,
                batch_size = mod_params$batch,
                epochs = mod_params$epochs,
                callbacks = list(
                  callback_early_stopping(patience = mod_params$patience, monitor = 'val_accuracy'),
                  callback_model_checkpoint("./fitted_model.hdf5",
                                            monitor = "val_accuracy", save_best_only = T),
                  callback_csv_logger("./fitted_model_log.csv")),
                shuffle = TRUE,
                validation_split = 1 - mod_params$train_per,
                verbose = 1)
  
  train_calls[[2]] <- factor(train_calls[[2]])#converter para factor para facilitar os numeros e aos nomes de classe repectivos
  labels_name <- as.character(train_calls[[2]])
  labels_df <- data.frame(name = levels(train_calls[[2]]), code = (1:length(levels(train_calls[[2]])))-1)
  metadata <- list(parameters =  train_calls[[3]], classes = labels_df)

  
  auto_id_shiny(model_path = '/home/bruno/ms_data/fitted_model.hdf5', # só funciona com a save_output_shiny_DEBUG
                updateProgress = NA,
                metadata = metadata,
                file_path = "/home/bruno/ms_data/validation_recordings/",
                out_file ='id_results',
                out_dir = "/home/bruno/ms_data/output/",
                save_png = TRUE,                   
                win_size = 50,
                remove_noise = TRUE,
                plot2console = FALSE,
                recursive = FALSE,
                tx = "auto")
  
  
  
  
  auto_id_shiny(model_path = './fitted_model.hdf5', # só funciona com a save_output_shiny_DEBUG
                updateProgress = NA,
                metadata = metadata,
                file_path = rec_folder,
                out_file ='id_results',
                out_dir = paste0(rec_folder, "/", "output/"),
                save_png = TRUE,                   
                win_size = 50,
                remove_noise = TRUE,
                plot2console = FALSE,
                recursive = FALSE,
                tx = "auto")
  
  auto_id_shiny(model_path = './fitted_model.hdf5', # só funciona com a save_output_shiny_DEBUG
           updateProgress = NA,
           metadata = metadata,
           file_path = paste0(rec_folder,"/noise/"),
           out_file ='id_results',
           out_dir = paste0(rec_folder, "/", "output/"),
           save_png = TRUE,                   
           win_size = 40,
           remove_noise = TRUE,
           plot2console = FALSE,
           recursive = FALSE,
           tx = 1)
  
  auto_id_shiny(model_path = './fitted_model.hdf5', # só funciona com a save_output_shiny_DEBUG
                updateProgress = NA,
                metadata = metadata,
                file_path = paste0(rec_folder,"/bats/"),
                out_file ='id_results',
                out_dir = paste0(rec_folder, "/", "output/"),
                save_png = TRUE,                   
                win_size = 40,
                remove_noise = TRUE,
                plot2console = FALSE,
                recursive = FALSE,
                tx = 1)
  
  auto_id_shiny(model_path = './fitted_model.hdf5', # só funciona com a save_output_shiny_DEBUG
                updateProgress = NA,
                metadata = metadata,
                file_path = paste0(rec_folder,"/bats/bbar_mesc/"),
                out_file ='id_results',
                out_dir = paste0(rec_folder, "/", "output/"),
                save_png = TRUE,                   
                win_size = 40,
                remove_noise = TRUE,
                plot2console = FALSE,
                recursive = FALSE,
                tx = 1)
  