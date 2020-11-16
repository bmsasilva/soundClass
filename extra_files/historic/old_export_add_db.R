
# db <- "~/Projectos/R_packages/development/recLabel/inst/recordings/test-db.sqlite3"

add_db <- function(db, table, data) {
  # table = "table_name"
  # db = "path_to_db"

# if db exists, do nothing, else create db
if(!file.exists(db)) dplyr::src_sqlite(db, create = TRUE)

# connect to db
my_db <- dplyr::src_sqlite(db, create = FALSE)

# check if table exists, else create table
if(!dbExistsTable(my_db$con, table)) copy_to(my_db, data, table, temporary = FALSE)

# append rows to db
db_insert_into(con = my_db$con, table = table, values = data)

# close connection
dbDisconnect(my_db$con)

}

# # Create query
# local_db <- tbl(my_db, "my_table")
# show_query(local_db)
#
# # Pull data from remote db to local tible
# tt <- collect(local_db)
