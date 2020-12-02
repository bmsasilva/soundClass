## databse managemente function ##

db <-
my_db <- dplyr::src_sqlite(db, create = FALSE) # open connection
src_tbls(my_db) #list tables

# # Create query
local_path <- tbl(my_db, "rec_labels")
show_query(local_path)
#
# # Pull df from remote path to local tible
# tt <- collect(local_path)
