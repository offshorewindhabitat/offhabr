librarian::shelf(
  DBI, devtools, fs, glue, here)
load_all()

path_db    <- system.file("offhab.duckdb", package="offhabr")
dir_export <- here("inst/_export_duckdb")

file_delete(path_db)

con <- oh_con(
  path_db   = path_db,
  read_only = F)

sql <- glue("IMPORT DATABASE '{dir_export}'")
dbExecute(con, sql)

dbDisconnect(con, shutdown=T)
