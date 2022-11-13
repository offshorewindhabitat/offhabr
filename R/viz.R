#' Map polygons
#'
#' Make an interactive choropleth map, optionally with a color scheme that
#' diverges around a midpoint value.
#'
#' @param ply a `sf` polygon data frame
#' @param fld_val an unquoted field name (see `tidy-select`) for the column in
#'   `ply` containing the value to map
#' @param fld_id an unquoted field name (see `tidy-select`) for the column in
#'   `ply` containing the identifier to include in the popup
#' @param str_val the string name for the field value in the legend and popup
#' @param str_id the string name for the identifier in the popup
#' @param div_mid the divergent midpoint of the field value `fld_val` for
#'   setting as the midpoint of the color palette `col_pal`; default is `NULL`
#' @param col_pal color palette for the polygon fill color; default is
#'   "Spectral"
#' @param fill_opacity fill opacity; default is 0.6
#' @param brdr_color border color; default is "gray"
#' @param brdr_opacity border opacity; default is 0.9
#' @param brdr_weight border weight; default is 1
#' @param base_map the basemap (see  `leaflet$providers`); default is
#'   "CartoDB.Positron"
#' @param base_opacity the opacity of the basemap; default is 0.5
#'
#' @return a `leaflet::leaflet()` map
#' @import dplyr glue leaflet scales
#' @export
#' @concept viz
#'
#' @examples
#' # map zones by area
#' oh_map_ply(
#'   ply     = oh_zones_s1k,
#'   fld_val = area_km2,
#'   fld_id  = zone_name,
#'   str_val = "area (km^2)",
#'   str_id  = "Zone")
#'
#' # map zones by area with divergent color scheme around mean
#' oh_map_ply(
#'   ply     = oh_zones_s1k,
#'   fld_val = area_km2,
#'   fld_id  = zone_name,
#'   str_val = "area (km^2)",
#'   str_id  = "Zone",
#'   div_mid = mean(oh_zones$area_km2))
oh_map_ply <- function(
    ply, fld_val, fld_id, str_val, str_id,
    div_mid = NULL,
    col_pal = "Spectral", fill_opacity = 0.6,
    brdr_color = "gray", brdr_opacity = 0.9, brdr_weight = 1,
    base_map = "CartoDB.Positron", base_opacity = 0.5){

  vals <- dplyr::pull(ply, {{fld_val}})
  ids  <- dplyr::pull(ply, {{fld_id}})

  m <- div_mid
  if (is.null(m)){
    pal <- scales::col_numeric(col_pal, domain = vals, reverse = T)
  } else {
    # check midpoint is between min and max of vals
    stopifnot(all(
      m > min(vals),
      m < max(vals)))

    # get max delta above and below midpoint
    d <- max(abs(c(m - min(vals), max(vals) - m)))
    # set range
    r <- c(min(vals), m - d, m + d, max(vals))

    pal <- scales::col_numeric(col_pal, domain = r, reverse = T)
  }

  popups <- glue::glue("{str_id}: <b>{ids}</b><br>{str_val}: {vals}")

  leaflet::leaflet() %>%
    leaflet::addProviderTiles(
      leaflet::providers[[base_map]],
      options = leaflet::providerTileOptions(
        opacity = base_opacity)) %>%
    leaflet::addPolygons(
      data = ply,
      fillColor = pal(vals), fillOpacity = fill_opacity,
      color = brdr_color, opacity = brdr_opacity, weight = brdr_weight,
      popup = popups) %>%
    leaflet::addLegend(
      pal = pal,
      values = range(vals),
      title = str_val)
}

