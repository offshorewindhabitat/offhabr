#' Connect to OffHab Postgres Database
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

#' Connect to AquaMaps Postgres Database
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

