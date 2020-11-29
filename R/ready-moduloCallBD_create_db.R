#' @title Create a sqlite3 database
#' @description Create a sqlite3 database with a predefined table in a specific folder,
#' if a database with the specified name doesn't exist already
#' @param path Path to the folder where the database will be created
#' @param name Name of the database to be created
#' @return  Nothing
#' @examples path <- '~/Projectos/R_packages/development/recLabel/inst/recordings/'
#' name <- 'test'
#' create_db(path, db_name, table_name)
#' @details A sqlite3 database with the specified name with a predefined table
#' is created inside the folder
#' @export
#' @author Bruno Silva
#' @import dbplyr

create_db <- function(path, db_name, table_name) {
  db <- paste0(path, db_name, '.sqlite3')
  if(!file.exists(db)) {
    dplyr::src_sqlite(db, create = TRUE)
    my_db <- dplyr::src_sqlite(db, create = FALSE)
    table <- data.frame(recording = character(),
                        label_position = numeric(),
                        label_class = character(),
                        observations = character())
    dplyr::copy_to(my_db, table, table_name, temporary = FALSE)
    DBI::dbDisconnect(my_db$con)
  }
}


