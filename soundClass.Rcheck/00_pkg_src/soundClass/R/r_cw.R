#' Rotate a matrix clockwise
#' @title Rotate a matrix
#' @description  Rotate a matrix 90º clockwise
#' @param mat a matrix
#' @usage r_cw(mat)
#' @return Rotated matrix
#' @author Bruno Silva
#' @keywords internal
#' @noRd
 
r_cw <- function(mat) t(apply(mat, 2, rev))