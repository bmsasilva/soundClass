#' @title Create a sqlite3 database
#' @description Create a sqlite3 database (if a database with the specified
#' name doesn't exist already) with predefined tables. Two types of
#' databases are possible, one to store recordings annotations and another
#' to store the output of the classification.
#' @param path Character. Path to the folder where the database will be created.
#' @param db_name Character. Name of the database to be created.
#' @param table_name Character. Name of the table to be created in the
#' database. It is mandatory to use the default table name "labels"
#' if the database is intended to be used in conjunction with other
#' functions of this package.
#' @param type Character indicating the type of database to create. Possible
#' options are: "reference" which creates a database to be used to store
#' recordings annotations for training purposes, and "id" which
#' creates a database to output the results of the automatic classification.
#' @usage create_db(path, db_name = NA, table_name = "labels",
#' type = "reference")
#' @examples
#' \dontrun{
#' dir_path <- tempdir()
#' create_db(dir_path,
#' db_name = "test",
#' table_name = "labels",
#' type = "reference")
#' file.remove(file.path(dir_path, "test.sqlite3"))
#' }
#' @return  Nothing
#' @export
#' @author Bruno Silva
#' @import dplyr DBI

create_db <- function(path, db_name = NA, table_name = "labels",
                      type = "reference") {
  if (!is.na(db_name)) {
    db <- paste0(path, "//", db_name, ".sqlite3")
  } else {
    db <- paste0(path, ".sqlite3")
    }

  if (file.exists(db)) message("Database already exists")

  if (!file.exists(db) & type == "reference") {
    dplyr::src_sqlite(db, create = TRUE)
    my_db <- dplyr::src_sqlite(db, create = FALSE)
    table <- data.frame(
      recording = character(),
      label_position = numeric(),
      label_class = character(),
      observations = character()
    )
    dplyr::copy_to(my_db, table, table_name, temporary = FALSE)
    DBI::dbDisconnect(my_db$con)
  }
  if (!file.exists(db) & type == "id") {
    dplyr::src_sqlite(db, create = TRUE)
    my_db <- dplyr::src_sqlite(db, create = FALSE)
    table <- data.frame(
      recording = character(),
      label_position = numeric(),
      label_class = character(),
      probability = numeric(),
      fmaxe = numeric()
    )
    dplyr::copy_to(my_db, table, table_name, temporary = FALSE)
    suppressWarnings(DBI::dbDisconnect(my_db$con))
  }
}
