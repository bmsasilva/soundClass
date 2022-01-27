#' @title Obtain train metadata to run a fitted model
#' @description Obtain train metadata from
#' the output of function \link{spectro_calls}. Needed to run a fitted model 
#' @param train_data Output of function \link{spectro_calls}.
#' @param save2disk If a string is provided, the resulting object 
#' is saved to disk (using the string as file name).
#' @usage train_metadata(train_data, save2disk = NA)
#' @return A list with the following components:
#' \itemize{
#'   \item parameters -- parameters of the spectrograms
#'   \item classes -- class names and respective codes
#' }
#' @author Bruno Silva
#' @keywords export
train_metadata <- function(train_data, save2disk = NA){
  metadata <- list(parameters =  train_data$parameters,
                   classes = train_data$classes)
  if(!is.na(save2disk)) save(metadata, file = paste0(save2disk,".RDATA"))
  return(metadata)
}