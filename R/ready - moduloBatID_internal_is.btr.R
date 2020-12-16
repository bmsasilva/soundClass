#' Verify if object is of class "recording"
#' @title Verify if object is of class "recording"
#' @description  Verify if object is of class "recording"
#' @param x Object
#' @usage is.btr(x)
#' @return TRUE or FALSE
#' @author Bruno Silva
#' @keywords internal
is.btr <- function(x) inherits(x, "recording")
