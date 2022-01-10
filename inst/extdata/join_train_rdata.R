# so para usar para mim. nao vai entrar no package

join_train_rdata <- function(train1, train2, save2disk = TRUE) {
  train1 <- load2env(train1)
  train2 <- load2env(train2)
  # if (!identical(train1$train_data[[3]][,c("img_rows", "img_cols")], train2$train_data[[3]])) stop("Train datasets have diferent parameters")
  train_joined <- list(
    rbind(train1$train_data[[1]], train2$train_data[[1]]),
    c(train1$train_data[[2]], train2$train_data[[2]]), 
    train1$train_data[[3]]
  )
  train_joined[[3]]$num_classes <- train1$train_data[[3]]$num_classes + train2$train_data[[3]]$num_classes
  if (save2disk) save(train_joined, file = "train_data_joined.RDATA")
  return(train_joined)
}

######
  # train1 <- '/mnt/5F9DC8AD3B9B9A40/Bruno/recs_database/train_data_ready/train_data_no_rhinos_only_noise_dyn120.RDATA'
  # train2 <- '/mnt/5F9DC8AD3B9B9A40/Bruno/recs_database/train_data_ready/train_data_no_rhinos_species_dyn120.RDATA'
  # 
  # train_data <- join_train_rdata(train1,train2)
