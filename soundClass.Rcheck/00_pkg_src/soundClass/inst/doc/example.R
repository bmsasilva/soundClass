## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----keras, eval = F, echo = T------------------------------------------------
#  install.packages("keras")
#  library(keras)
#  keras::install_keras()

## ----setup, eval = F, echo = T------------------------------------------------
#  install.packages("soundClass")
#  library(soundClass)

## ----folder, eval = F, echo = T-----------------------------------------------
#  folder_path <- "~/data_example"
#  dir.create(folder_path)

## ----train_calls, eval = F, echo = T------------------------------------------
#  train_calls <- spectro_calls(
#   files_path = "~/data_example/training_recordings",
#   db_path = "~/data_example/db_bat_calls.sqlite3",
#   spec_size = 20,
#   window_length = 1,
#   frequency_resolution = 1,
#   time_step_size = 0.5,
#   dynamic_range = 100,
#   freq_range = c(10, 80),
#   tx = "auto",
#   seed = 1002)

## ----model_load, eval = F, echo = T-------------------------------------------
#  input_shape <- c(train_calls[[3]]$img_rows, train_calls[[3]]$img_cols, 1)
#  num_classes <- train_calls[[3]]$num_classes
#  model_path <- system.file("inst/model_architectures", "model_vgg_sequential.R", package="soundClass")
#  source(model_path, local=TRUE)

## ----model_compile, eval = F, echo = T----------------------------------------
#   model %>%
#    keras::compile(
#     optimizer = optimizer_sgd(
#                  lr = 0.01,
#                  momentum = 0.9,
#                  nesterov = TRUE),
#     loss = 'categorical_crossentropy',
#     metrics = 'accuracy'
#    )

## ----model_fit, eval = F, echo = T--------------------------------------------
#   model %>% keras::fit(train_data$data_x,
#          train_data$data_y,
#          batch_size = 64,
#          epochs = 20,
#          callbacks = list(
#           callback_early_stopping(patience = 2, monitor = 'val_accuracy'),
#           callback_model_checkpoint("./fitted_model.hdf5",
#                        monitor = "val_accuracy", save_best_only = T),
#           callback_csv_logger("./fitted_model_log.csv")),
#          shuffle = TRUE,
#          validation_split = 1 - mod_params$train_per,
#          verbose = 1)

## ----model_deploy, eval = F, echo = T-----------------------------------------
#     auto_id("./fitted_model.hdf5",
#            "./fitted_model_metadata.RDATA",
#            "~/data_example/training_recordings",
#            out_file = "id_results.csv",
#            out_dir = paste0(rec_folder, "/", "output/"),
#            save_png = TRUE,
#            win_size = 40,
#            remove_noise = TRUE,
#            plot2console = FALSE,
#            recursive = FALSE)

