#' Setup interactive map with OffHab defaults
#'
#' Make an interactive map with basemap
#'
#' @param base_map the basemap (see  `leaflet$providers`); default is
#'   "CartoDB.Positron"
#' @param base_opacity the opacity of the basemap; default is 0.5
#'
#' @return a `leaflet::leaflet()` map
#' @import leaflet
#' @export
#' @concept viz
#'
#' @examples
#' # basic map with defaults
#' oh_map()
#'
#' # map zones by area with divergent color scheme around mean
#' oh_map(
#'   ply     = oh_zones_s1k,
#'   fld_val = area_km2,
#'   fld_id  = zone_name,
#'   str_val = "area (km^2)",
#'   str_id  = "Zone",
#'   div_mid = mean(oh_zones$area_km2))
oh_map <- function(
    base_map = "CartoDB.Positron", base_opacity = 0.5){

  leaflet::leaflet() %>%
    leaflet::addProviderTiles(
      leaflet::providers[[base_map]],
      options = leaflet::providerTileOptions(
        opacity = base_opacity)) %>%
    leaflet.extras::addFullscreenControl()
}

#' Add polygons to map
#'
#' Add polygons to an interactive choropleth map, optionally with a color scheme that
#' diverges around a midpoint value.
#'
#' @param map a `leaflet` map, eg from `oh_map()`
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
#' @param add_legend logical of whether to add legend; default is TRUE
#' @param ...  additional arguments passed onto `leaflet::addPolygons()`
#'
#' @return a `leaflet::leaflet()` map
#' @import dplyr leaflet
#' @importFrom scales col_numeric
#' @importFrom glue glue
#' @export
#' @concept viz
#'
#' @examples
#' # map zones by area
#' oh_map() %>%
#' oh_add_ply(
#'   ply     = oh_zones_s1k,
#'   fld_val = area_km2,
#'   fld_id  = zone_name,
#'   str_val = "area (km^2)",
#'   str_id  = "Zone")
#'
#' # map zones by area with divergent color scheme around mean
#' oh_map() %>%
#' oh_add_ply(
#'   ply     = oh_zones_s1k,
#'   fld_val = area_km2,
#'   fld_id  = zone_name,
#'   str_val = "area (km^2)",
#'   str_id  = "Zone",
#'   div_mid = mean(oh_zones$area_km2))
oh_add_ply <- function(
    map, ply, fld_val, fld_id, str_val, str_id,
    div_mid = NULL,
    col_pal = "Spectral", fill_opacity = 0.6,
    brdr_color = "gray", brdr_opacity = 0.9, brdr_weight = 1,
    add_legend = T,
    ...){

  vals <- dplyr::pull(ply, {{fld_val}})
  ids  <- dplyr::pull(ply, {{fld_id}})

  m <- div_mid
  if (is.null(m)){
    pal <- scales::col_numeric(col_pal, domain = vals, reverse = T)
    v <- vals
  } else {
    v <- vals
    # if midpoint is between min and max of vals
    if (m < min(vals) | m > max(vals)){
      message("Midpoint is outside range of values. Adding to range.")
      v <- c(m, v)
    }

    # get max delta above and below midpoint
    d <- max(abs(c(m - min(v), max(v) - m)))
    # set range
    r <- c(min(v), m - d, m + d, max(v))

    pal <- scales::col_numeric(col_pal, domain = r, reverse = T)
  }

  popups <- glue::glue("{str_id}: <b>{ids}</b><br>{str_val}: <b>{vals}</b>")
  labels <- popups |> lapply(htmltools::HTML)

  map <- map %>%
    leaflet::addPolygons(
      data      = ply,
      fillColor = pal(vals), fillOpacity = fill_opacity,
      color     = brdr_color,    opacity = brdr_opacity, weight = brdr_weight,
      popup     = popups,
      label     = labels,
      ...)
  if (add_legend)
    map <- map %>%
    leaflet::addLegend(
      pal    = pal,
      values = range(v),
      title  = str_val)
  map
}

#' Get Colors
#'
#' typically used with `leaflet::addPolygons()`
#'
#' @param x character vector of names
#' @param v_colors vector of named characters with colors; defaults to `oh_colors`
#'
#' @return
#' @export
#' @concept viz
#'
#' @examples
get_colors <- function(x, v_colors = oh_colors){
  v_colors[x] |>
    unname()
}

