#' Rotate a matrix counter clockwise
#' @title Rotate a matrix
#' @description  Rotate a matrix 90º counter clockwise
#' @param mat a matrix
#' @usage r(mat)
#' @return Rotated matrix
#' @author Bruno Silva
#' @keywords internal

r <- function(mat) apply(mat, 1, rev)
