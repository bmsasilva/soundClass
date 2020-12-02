#' Add a record to a sqlite database
#' @title Add a record to a sqlite database
#' @description  Append a row with temporal labels of a sound recording to a
#' pre-existing table in a sqlite database
#' @param path Full path to the database, including name and extension
#' @param table Table name to append dataframe
#' @usage add_record(path, df, table)
#' @return Nothing
#' @author Bruno Silva
#' @export
#' @import dbplyr

add_record <- function(path, df, table) {
if(!file.exists(path)) stop("Database doesn't exist. Run create_db() first")
my_db <- dbplyr::src_sqlite(path, create = FALSE)
if(!DBI::dbExistsTable(my_db$con, table)) stop("Table was not found in database")
DBI::dbWriteTable(my_db$con, table, df, append = TRUE, row.names = FALSE)
DBI::dbDisconnect(my_db$con)
}
