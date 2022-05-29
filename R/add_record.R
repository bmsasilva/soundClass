#' Add a record to a sqlite database
#' @title Add a record to a sqlite database
#' @description  Append observations with temporal labels (in samples) 
#' of a sound recording to a pre-existing table named "labels" in a sqlite
#' database created with \code{\link{create_db}}.
#' @param path Character. Full path to the database, including name 
#' and extension.
#' @param df Dataframe to append to table "labels" in the database.
#' @usage add_record(path, df)
#' @return Nothing
#' @author Bruno Silva
#' @keywords internal
#' @noRd
#' @import dplyr DBI

add_record <- function(path, df) {
  if(!file.exists(path)) stop("Database doesn't exist. Run create_db() first")
  my_db <- dplyr::src_sqlite(path, create = FALSE)
  if(!DBI::dbExistsTable(my_db$con, "labels")) stop("Table 'labels' was not
                                                     found in the database")
  DBI::dbWriteTable(my_db$con, "labels", df, append = TRUE, row.names = FALSE)
}
