#' Get AquaMaps cell grid
#'
#' Get raster that only contains the `hcaf_id`.
#'
#' Cannot simply load this as an available dataset into the R package since
#' `terra::rast()` only loads a pointer to raster file, not its contents.
#'
#' @return A `terra` raster layer with 1 layer:
#' \describe{
#'   \item{hcaf_id}{half degree cell identifier (integer) from `am_cells_grd`}
#' }
#' @export
#' @concept aquamaps
#'
#' @examples
#' am_cells_grd <- get_am_cells_grd()
get_am_cells_grd <- function(){
  tif <- system.file(package = "offhabr", "am_cells_grd.tif")
  stopifnot(file.exists(tif))
  terra::rast(system.file(package = "offhabr", "am_cells_grd.tif"))
}

