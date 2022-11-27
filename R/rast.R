
#' OffHab reference raster
#'
#' Get OffHab reference raster of cells by `cell_id`, `zone_id` or all `NA` and
#' optionally trim to `zone_id`.
#'
#' The OffHab reference raster is based on the GEBCO global bathymetry clipped to the
#' United States in the web Mercator projection (EPSG:3857) for readily using
#' with interactive maps (e.g. `leaflet::leaflet()`).
#'
#' @param type the type of raster, being one of: `"NA"` (default all `NA` values)
#'   `"cell_id"` with unique cell indices to set values of reference raster, or
#'   `"zone_id"` corresponding with the `zone_id` of `oh_zones`
#' @param zone_id the `zone_id` (integer) to trim the output raster, being one of the
#'   zones found in the `zone_id` field of `oh_zones` or `"ALL"` zones (the default)
#'
#' @return return a reference raster (from `terra::rast()`)
#' @export
#' @concept rast
#' @importFrom terra mask ncell rast trim values
#'
#' @examples
#'
#' # reference raster with all NA values
#' r_na <- oh_rast()
#' r_na
#'
#' # zone_id for all zones
#' r_zid <- oh_rast("zone_id")
#' r_zid
#' terra::plot(r_zid)
#'
#' # cell_id for all zones
#' r_cid <- oh_rast("cell_id")
#' r_cid
#' terra::plot(r_cid)
#'
#' # cell_id for Straits of Florida (zone_id: 4; zone_key: fls)
#' r_cid_fls <- oh_rast("cell_id", 4)
#' r_cid_fls
#' terra::plot(r_cid_fls)
oh_rast <- function(type = c("NA", "cell_id", "zone_id"), zone_id = "ALL"){
  stopifnot(type %in% c("NA", "cell_id", "zone_id"))
  stopifnot(zone_id == "ALL" | is.numeric(zone_id))
  type = type[1]

  tif <- system.file("oh_zones.tif", package = "offhabr")
  r_z <- terra::rast(tif)
  # terra::plot(r_z)
  r <- switch(
    type,
    cell_id = terra::setValues(r_z, 1:(terra::ncell(r_z))) %>%
      mask(r_z),
    `NA`    = terra::setValues(r_z, NA),
    zone_id = r_z)
  names(r) <- type

  if (is.numeric(zone_id)){
    r <- r %>%
      terra::mask(r_z == zone_id, maskvalues=c(NA,0)) %>%
      terra::trim()
  }
  r
}

