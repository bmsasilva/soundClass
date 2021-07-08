#' @author Bruno Silva
#' @keywords internal

train_data_process <- function(rdata_list, seed = 1002){

total <- dim(rdata_list[[1]])[1]
img_rows <- rdata_list[[3]]$img_rows
img_cols <- rdata_list[[3]]$img_cols
input_shape <- c(img_rows, img_cols, 1) 
num_classes <- length(unique(rdata_list[[2]])) 

rdata_list[[2]] <- factor(rdata_list[[2]])
labels_code <- as.integer(rdata_list[[2]]) - 1
labels_name <- as.character(rdata_list[[2]])
labels_df <- data.frame(name = levels(rdata_list[[2]]), 
                        code = (1:length(levels(rdata_list[[2]])))-1)

set.seed(seed)
data_x <- rdata_list[[1]]
data_y <- labels_code
randomize <- sample(length(data_y))
data_x <- data_x[randomize,]
data_y <- data_y[randomize]

data_y <- keras::to_categorical(as.numeric(data_y), num_classes = num_classes)
data_x <- array(data_x, dim = c(total, img_rows, img_cols, 1))

return(list(data_x = data_x, data_y = data_y))
}