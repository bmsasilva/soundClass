#' Check for even number
#' @title Check for even number
#' @description  Check for even number
#' @param x a number
#' @usage is_even(x)
#' @return TRUE if number is even, FALSE if number is odd
#' @author Bruno Silva
#' @keywords internal

is_even <- function(x){
  if((x %% 2) == 0){
    TRUE
  } else {
    FALSE
  }
}
  
