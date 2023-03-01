#' Setup interactive map with OffHab defaults
#'
#' Make an interactive map with basemap
#'
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
oh_map <- function(base_opacity = 0.5){

  leaflet::leaflet() %>%
    # add base: blue bathymetry and light brown/green topography
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base",
        opacity = base_opacity)) |>
    # add reference: placename labels and borders
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference",
        opacity = base_opacity)) |>
    leaflet.extras::addFullscreenControl()
}

#' Map Cloud-Optimized GeoTIFF
#'
#' Make an interactive map with basemap
#'
#' @param cog_file filename of Cloud-Optimized GeoTIFF (COG)
#' @param cog_dir directory of path to COG; defaults to "https://storage.googleapis.com/offhab_lyrs"
#' @param cog_range range of values to colorramp in COG; default: `c(1, 100)`
#' @param cog_method method for interpolation between zoom levels; choose "nearest" for categorical; default: "average" (for continuous)
#' @param cog_palette color ramp that applies to the `colormap_name` of
#'   [https://api.cogeo.xyz/cog/tiles](https://api.cogeo.xyz/docs#/Cloud%20Optimized%20GeoTIFF/tile_cog_tiles__TileMatrixSetId___z___x___y___scale_x__format__get);
#'   default: "spectral_r"
#' @param lgnd_palette color ramp that applies to both `leaflet::colorNumeric()`
#'   default: "Spectral"
#' @param lgnd_palette_r boolean whether `lgnd_palette` is reversed in
#'   `leaflet::colorNumeric()` color ramp
#'   default: True
#' @param bb bounding box to feed `leaflet::fitBounds()` defaults to US48:
#'   `c(-129.4, 23.2, -64.7, 48.9)`
#' @param title title for legend; default: `"% Habitat"`
#' @param base_opacity the opacity of the basemap; default is `0.5`
#'
#' @return a `leaflet::leaflet()` map
#' @import leaflet
#' @export
#' @concept viz
#'
#' @examples
#' # map by aphia_id
#' oh_map_cog("aphia_100599_web.tif")
oh_map_cog <- function(
  cog_file,
  cog_dir        = "https://storage.googleapis.com/offhab_lyrs",
  cog_range      = c(1, 100),
  cog_method     = "average",
  cog_palette    = "spectral_r",
  lgnd_palette   = "Spectral",
  lgnd_palette_r = TRUE,
  bb             = c(-129.4, 23.2, -64.7, 48.9),
  title          = "% Habitat",
  base_opacity   = 0.5){

  stopifnot(all(is.numeric(cog_range), length(cog_range)==2, cog_range[2] >= cog_range[1]))

  cog_url   <- glue("{cog_dir}/{cog_file}")
  tile_opts <- glue(
    "resampling_method={cog_method}&rescale={paste(cog_range, collapse=',')}&return_mask=true&colormap_name={cog_palette}")
  tile_url  <- glue(
    "https://api.cogeo.xyz/cog/tiles/WebMercatorQuad/{{z}}/{{x}}/{{y}}@2x?url={cog_url}&{tile_opts}")

  leaflet::leaflet() |>
    # add base: blue bathymetry and light brown/green topography
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base",
        opacity = base_opacity)) |>
    # add reference: placename labels and borders
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference",
        opacity = base_opacity)) |>
    addTiles(
      urlTemplate=tile_url) |>
    fitBounds(bb[1], bb[2], bb[3], bb[4]) |>
    leaflet.extras::addFullscreenControl() |>
  addLegend(
    pal    = colorNumeric(lgnd_palette, cog_range[1]:cog_range[2], reverse = lgnd_palette_r),
    values = c(cog_range[1], cog_range[2]),
    title  = title)
}

oh_map_cog_lyr <- function(
    lyr_key,
    lyr_title = "% Habitat",
    con   = con,
    ...){

  d_rng <- tbl(con, "lyrs") |>
    filter(lyr_key == !!lyr_key) |>
    select(val_min, val_max) |>
    collect()

  oh_map_cog(
    cog_file  = glue("{lyr_key}.tif"),
    cog_range = c(d_rng$val_min, d_rng$val_max),
    title     = lyr_title, ...)
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


