librarian::shelf(
  DBI, devtools, glue, here)
load_all()

con <- oh_con()

# make edits
# con <- oh_con(read_only = F)
# dbRemoveTable(con, "iris")
# dbDisconnect(con, shutdown=T)

dir_export <- here("inst/_export_duckdb")
sql <- glue("EXPORT DATABASE '{dir_export}'")
dbExecute(con, sql)

dbDisconnect(con, shutdown=T)
