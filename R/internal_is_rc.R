#' Check if an object is of class "rc"
#' @title Check if an object is of class "rc"
#' @description  Check if an object is of class "rc"
#' @param x any R object
#' @usage is_rc(x)
#' @return TRUE if object is of class "rc", FALSE otherwiese
#' @author Bruno Silva
#' @noRd
#' @keywords internal

is_rc <- function(x) inherits(x, "rc")
