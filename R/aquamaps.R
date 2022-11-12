#' AquaMaps cell raster contained by BOEM blocks
#'
#' Get raster with AquaMaps `hcaf_id` for cells contained by the BOEM blocks `boem_blocks`.
#'
#' Cannot simply load this as an available dataset into the R package since
#' `terra::rast()` only loads a pointer to raster file, not its contents.
#'
#' @return A `terra` raster layer with 1 layer:
#' \describe{
#'   \item{hcaf_id}{half degree cell identifier (integer) from `am_cell_blocks`}
#' }
#' @export
#' @concept aquamaps
#'
#' @examples
#' am_cell_blocks_grd <- get_am_cell_blocks_grd()
get_am_cell_blocks_grd <- function(){
  tif <- system.file(package = "offhabr", "am_cell_blocks_grd.tif")
  stopifnot(file.exists(tif))
  terra::rast(tif)
}

#' AquaMaps cell raster contained by OffHab zones
#'
#' Get raster with AquaMaps `hcaf_id` for cells contained by OffHab zones `oh_zones`.
#'
#' Cannot simply load this as an available dataset into the R package since
#' `terra::rast()` only loads a pointer to raster file, not its contents.
#'
#' @return A `terra` raster layer with 1 layer:
#' \describe{
#'   \item{hcaf_id}{half degree cell identifier (integer) from `am_cell_zones`}
#' }
#' @export
#' @concept aquamaps
#'
#' @examples
#' am_cell_zones_grd <- get_am_cell_zones_grd()
get_am_cell_zones_grd <- function(){
  tif <- system.file(package = "offhabr", "am_cell_zones_grd.tif")
  stopifnot(file.exists(tif))
  terra::rast(tif)
}


