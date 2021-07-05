#' @title Create a sqlite3 database
#' @description Create a sqlite3 database with a predefined table in a specific folder,
#' if a database with the specified name doesn't exist already
#' @param path Path to the folder where the database will be created
#' @param db_name Name of the database to be created
#' @param table_name Name of the table to be created inside the database
#' @param type Type of database to create. Possible options are: "reference" which creates
#' a database to be used for training purposes, and "id" which creates a database 
#' to output the results of the automatic identification 
#' @return  Nothing
#' @examples create_db(".//", db_name = "test", type = "reference")
#' @details A sqlite3 database with the specified name with a predefined table
#' is created inside the folder
#' @export
#' @author Bruno Silva
#' @import dplyr DBI

create_db <- function(path, db_name, table_name = "labels", type = "reference") {
 
   db <- paste0(path, db_name, '.sqlite3')
  
  if(!file.exists(db) & type == "reference") {
    dplyr::src_sqlite(db, create = TRUE)
    my_db <- dplyr::src_sqlite(db, create = FALSE)
    table <- data.frame(recording = character(),
                        label_position = numeric(),
                        label_class = character(),
                        observations = character())
    dplyr::copy_to(my_db, table, table_name, temporary = FALSE)
    DBI::dbDisconnect(my_db$con)
  } 
    if(!file.exists(db) & type == "id") {
      dplyr::src_sqlite(db, create = TRUE)
      my_db <- dplyr::src_sqlite(db, create = FALSE)
      table <- data.frame(recording = character(),
                          label_position = numeric(),
                          label_class = character(),
                          probability = numeric(),
                          fmaxe = numeric())
      dplyr::copy_to(my_db, table, table_name, temporary = FALSE)
      DBI::dbDisconnect(my_db$con)
    }
  }



