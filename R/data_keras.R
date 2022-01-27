#' Process train data for Keras format
#' @title Process train data for Keras format
#' @description Processes the train data outputted by function 'spectro_calls' to
#' Keras format.
#' @param rdata_list An object created with
#' the 'spec_calls' function.
#' @param seed Integer. Used for the randomization of the observations.
#' @return A list with two components: data_x, an array with the XX data and
#' data_y, the labels one-hot-encoded.
#' @details The spectrogram matrices are converted into an
#' array (XX train data) and the labels one-hot-encoded (YY train data). The
#' observations are also randomized.
#' @keywords internal
#' @noRd
#' @author Bruno Silva

data_keras <- function(rdata_list, seed = 1002) {
  set.seed(seed)
  total <- dim(rdata_list[[1]])[1]
  img_rows <- rdata_list[[3]]$img_rows
  img_cols <- rdata_list[[3]]$img_cols
  num_classes <- length(unique(rdata_list[[2]]))
  rdata_list[[2]] <- factor(rdata_list[[2]])
  labels_code <- as.integer(rdata_list[[2]]) - 1
  labels_df <- data.frame(
    name = levels(rdata_list[[2]]),
    code = (1:length(levels(rdata_list[[2]]))) - 1
  )
  data_x <- rdata_list[[1]]
  data_y <- labels_code
  randomize <- sample(length(data_y))
  data_x <- data_x[randomize, ]
  data_y <- data_y[randomize]
  data_y <- keras::to_categorical(as.numeric(data_y), num_classes = num_classes)
  data_x <- array(data_x, dim = c(total, img_rows, img_cols, 1))
  return(list(data_x = data_x, data_y = data_y, parameters = rdata_list[[3]],
              classes = labels_df))
}
