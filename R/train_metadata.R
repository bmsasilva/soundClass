#' @title Obtain train metadata to run a fitted model
#' @description Obtain train metadata from
#' the output of function \link{spectro_calls}. Needed to run a fitted model 
#' @param train_data Output of function \link{spectro_calls}.
#' @usage train_metadata(train_data)
#' @return A list with the following components:
#' \itemize{
#'   \item parameters -- parameters of the spectrograms
#'   \item classes -- class names and respective codes
#' }
#' @author Bruno Silva
#' @export
train_metadata <- function(train_data){
  metadata <- list(parameters =  train_data$parameters,
                   classes = train_data$classes)
  return(metadata)
}