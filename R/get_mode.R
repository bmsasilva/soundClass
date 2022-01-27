#' Obtain the mode of a vector
#' @title Obtain the mode of a vector
#' @description  Obtain the mode of a vector
#' @param v vector
#' @usage get_mode(v)
#' @return The mode of the vector
#' @author Bruno Silva
#' @noRd
#' @keywords internal

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
