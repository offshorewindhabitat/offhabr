librarian::shelf(
  devtools, glue, here)
load_all()

con <- oh_con()

dir_export <- here("inst/_export_duckdb")
sql <- glue("EXPORT DATABASE '{dir_export}'")
dbExecute(con, sql)
