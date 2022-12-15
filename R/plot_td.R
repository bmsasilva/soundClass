#' @title Plot training spectrograms
#' @description Plot training spectrograms
#' @param train_data Train data object returned by function spectro_calls()
#' @param index Vector indicating th index of the spectrograms to plot.
#' @usage plot_td(train_data, index)
#' @return A plot
#' @author Bruno Silva
#' @importFrom graphics image
#' @export
 plot_td <- function(train_data, index){
   for(index in index){
     title <- max.col(matrix(train_data$data_y[index,], nrow=1)) - 1
     title <- train_data$classes[which(train_data$classes$code == title), 1]
     if(title == 0) title <- "Irrelevant"
     graphics::image(r_cw(matrix(train_data$data_x[index,,,], nrow = train_data$parameters$img_rows)), main = title)
   }
 }
