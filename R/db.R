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
oh_pg_con <- function(autocommit=T){
  # brew services start postgresql
  # brew services stop postgresql

  opts <- ifelse(autocommit, "", "-c synchronous_commit=off")

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "offhab",
    host     = "localhost",
    port     = 5432,
    options  = opts)
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

#' Connect to IUCN RedList Postgres database
#'
#' @return a `DBI::dbConnect()` object
#' @import DBI
#' @export
#' @concept db
#'
#' @examples
#' \dontrun{
#' con <- rl_pg_con()
#' DBI::dbListTables(con)
#' }
rl_pg_con <- function(){
  # brew services start postgresql
  # brew services stop postgresql

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = "iucn",
    host     = "localhost",
    port     = 5432)
}

#' Create index in database
#'
#' @param con database connection from `DBI::dbConnect()`, eg from `am_pg_con()`
#' @param tbl table name as character
#' @param flds character vector of fields in table used for index
#' @param geom logical (default: FALSE) whether geometry field, so create GIST() index
#' @param unique logical (default: FALSE) whether to impose a unique constraint, to prevent duplicates; default: FALSE
#' @param overwrite logical (default: FALSE) whether to overwrite existing index
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
create_index <- function(con, tbl, flds, geom=F, unique=F, overwrite=F){
  # tbl = "taxa"; flds = c("tbl_orig", "aphia_id"); unique = T; geom=F
  stopifnot(!(geom == T & length(flds) != 1))
  sfx <- ifelse(
    geom,
    glue::glue(" USING GIST ({flds})"),
    glue::glue("({paste(flds, collapse=', ')})"))
  idx <- glue("{tbl}_{paste(flds, collapse='_')}_idx")

  if (overwrite)
    dbSendQuery(con, glue("DROP INDEX IF EXISTS {idx}"))

  sql <- glue::glue(
    "CREATE {ifelse(unique, 'UNIQUE','')} INDEX IF NOT EXISTS {idx} ON {tbl}{sfx}")
  DBI::dbSendQuery(con, sql)
}

