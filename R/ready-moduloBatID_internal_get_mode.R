#' Obtain the mode of a vector
#' @title Calculate mode
#' @description  Calculate the mode of a vector
#' @param v vector
#' @usage get_mode(v)
#' @return The mode of the vector
#' @author Bruno Silva
#' @keywords internal
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
