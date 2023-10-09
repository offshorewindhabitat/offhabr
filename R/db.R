#' Connect to OffHab database
#'
#' Using local filesystem database `RSQLite::SQLite()`
#'
#' @param path_db path to database file, defaults to duckdb in this R package
#' @param read_only only applicable to duckdb (not SQLite); defaults to TRUE; vs set to FALSE if writing to database
#'
#' @return a `DBI::dbConnect()` object
#' @import DBI
#' @importFrom glue glue
#' @importFrom fs path_ext
#' @importFrom duckdb duckdb
#' @export
#' @concept db
#'
#' @examples
#' \dontrun{
#' con <- oh_con()
#' DBI::dbListTables(con)
#' }
oh_con <- function(
    path_db = system.file("offhab.duckdb", package="offhabr"),
    read_only = T){

  # path_db = system.file("offhab.sqlite", package="offhabr")
  # options("sqlite.enable_rstudio_connection_pane"=TRUE)

  if (!fs::path_ext(path_db) %in% c("sqlite", "duckdb"))
    stop("Sorry, only path_db extensions with .sqlite or .duckdb are supported")

  switch(
    fs::path_ext(path_db),

    sqlite = DBI::dbConnect(
      RSQLite::SQLite(),
      path_db),

    duckdb = DBI::dbConnect(
      duckdb::duckdb(
        dbdir = path_db,
        read_only = read_only)) )
}

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
  # brew services start postgresql@14
  # brew services stop postgresql@14

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
  idx <- ifelse(
    unique,
    glue("{tbl}_unique_idx"),
    glue("{tbl}_{paste(flds, collapse='_')}_idx"))

  if (overwrite)
    dbSendQuery(con, glue("DROP INDEX IF EXISTS {idx}"))

  sql <- glue::glue(
    "CREATE {ifelse(unique, 'UNIQUE','')} INDEX IF NOT EXISTS {idx} ON {tbl}{sfx}")
  if (show)
    message(sql)
  if (exec)
    DBI::dbSendQuery(con, sql)
}

