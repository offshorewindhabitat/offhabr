#' Connect to OffHab database
#'
#' Using local filesystem database `RSQLite::SQLite()`
#'
#' @param path_sqlite path to SQLite database file
#' @param read_only defaults to FALSE, i.e. write
#'
#' @return a `DBI::dbConnect()` object
#' @import DBI
#' @importFrom RSQLite SQLite
#' @importFrom glue glue
#' @export
#' @concept db
#'
#' @examples
#' \dontrun{
#' con <- oh_con()
#' DBI::dbListTables(con)
#' }
oh_con <- function(
    path_sqlite = system.file("offhab.sqlite", package="offhabr")){

  options("sqlite.enable_rstudio_connection_pane"=TRUE)

  DBI::dbConnect(
    RSQLite::SQLite(),
    path_sqlite)

  # https://rstudio.github.io/connections/
  # remotes::install_github("edgararuiz/connections")
  # connection_open() - Opens the database connection. Use instead of dbConnect(), but use the exact same arguments. It also automatically starts the Connections pane.
  # connection_close() - Closes the database connection.
  # connections::connection_open(RSQLite::SQLite(), path_sqlite)

  # https://support.posit.co/hc/en-us/articles/115010915687-Using-RStudio-Connections-in-the-RStudio-IDE
  # devtools::install_github("r-dbi/odbc")

  # Load RStudio IDE Connection pane with:
  # devtools::load_all("~/Github/ecoquants/offhabr")
  # con <- offhabr::oh_con()

#   "/Users/bbest/Github/ecoquants/offhabr/inst/offhab.sqlite"
#   devtools::load_all("~/Github/ecoquants/offhabr")
#   path_sqlite = system.file("offhab.sqlite", package = "offhabr")
#
#   con <- DBI::dbConnect(RSQLite::SQLite(), path_sqlite)
#
# con <- DBI::dbConnect(
#   RSQLite::SQLite(),
#   "/Users/bbest/Github/ecoquants/offhabr/inst/offhab.sqlite")
}

#' Connect to OffHab Postgres database
#'
#' @return a `DBI::dbConnect()` object
#' @import DBI
#' @importFrom RPostgres Postgres
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
  # brew services start postgresql@14
  # brew services stop postgresql@14

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
  # brew services start postgresql@14
  # brew services stop postgresql@14

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
#' @param show logical (default: FALSE) whether to show SQL statement
#' @param exec logical (default: TRUE) whether to execute SQL statement
#'
#' @return nothing
#' @import DBI
#' @importFrom glue glue
#' @export
#' @concept db
#'
#' @examples
#' \dontrun{
#' con <- am_pg_con()
#' create_index(con, "am_cell_blocks", "geom", geom=T)
#' create_index(con, "am_cell_blocks", "zcb_id")
#' }
create_index <- function(con, tbl, flds, geom=F, unique=F, overwrite=F, show=F, exec=T){
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
  if (show)
    message(sql)
  if (exec)
    DBI::dbSendQuery(con, sql)
}

