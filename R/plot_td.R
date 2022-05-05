plot_td <- function(train_data, index){
  for(index in index){
    title <- max.col(matrix(train_data$data_y[index,], nrow=1)) - 1
    title <- train_data$classes[which(train_data$classes$code == title), 1]
    if(title == 0) title <- "Noise"
    image(r_cw(matrix(train_data$data_x[index,,,], nrow = train_data$parameters$img_rows)), main = title)
  }
}


