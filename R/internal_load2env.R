#' Load a RDATA file to a new environment
#' @title Load a RDATA file to a new environment
#' @description  Load a RDATA file to a new environment
#' @param rdata_file Character. Path to a .RDATA file
#' @usage load2env(rdata_file, env = new.env())
#' @return A new environment with loaded objects
#' @author Bruno Silva
#' @noRd
#' @keywords internal

load2env <- function(rdata_file, env=new.env()) {
  load(rdata_file, env)
  return(env)
}