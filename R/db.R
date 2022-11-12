#' Connect to OffHab Postgres database
#'
#' @return a `DBI::dbConnect()` object
#' @import DBI
#' @export
#' @concept db
#'
#' @examples
#' \dontrun{
#' con <- oh_pg_con()
#' DBI::dbListTables(con)
#' }
oh_pg_con <- function(){
  # brew services start postgresql
  # brew services stop postgresql

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "offhab",
    host     = "localhost",
    port     = 5432)
}

#' Connect to AquaMaps Postgres database
#'
#' @return a `DBI::dbConnect()` object
#' @import DBI
#' @export
#' @concept db
#'
#' @examples
#' \dontrun{
#' con <- am_pg_con()
#' DBI::dbListTables(con)
#' }
am_pg_con <- function(){
  # brew services start postgresql
  # brew services stop postgresql

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "aquamaps",
    host     = "localhost",
    port     = 5432)
}

#' Create index in database
#'
#' @param con database connection from `DBI::dbConnect()`, eg from `am_pg_con()`
#' @param tbl table name as character
#' @param fld field name as character
#' @param geom logical (default: FALSE) whether geometry field, so create GIST() index
#'
#' @return nothing
#' @import glue DBI
#' @export
#' @concept db
#'
#' @examples
#' \dontrun{
#' con <- am_pg_con()
#' create_index(con, "am_cell_blocks", "geom", geom=T)
#' create_index(con, "am_cell_blocks", "zcb_id")
#' }
create_index <- function(con, tbl, fld, geom=F){
  sfx <- ifelse(
    geom,
    glue::glue(" USING GIST ({fld})"),
    glue::glue("({fld})"))
  sql <- glue::glue("CREATE INDEX IF NOT EXISTS {tbl}_{fld}_idx ON {tbl}{sfx}")
  DBI::dbSendQuery(con, sql)
}

