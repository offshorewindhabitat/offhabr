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
#' @param cog_opacity the opacity of the COG tile layer; default is `0.9`
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
  cog_opacity    = 0.9,
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
      urlTemplate=tile_url,
      options = tileOptions(
        opacity = cog_opacity)) |>
    fitBounds(bb[1], bb[2], bb[3], bb[4]) |>
    leaflet.extras::addFullscreenControl() |>
  addLegend(
    pal    = colorNumeric(lgnd_palette, cog_range[1]:cog_range[2], reverse = lgnd_palette_r),
    values = c(cog_range[1], cog_range[2]),
    title  = title)
}

#' Map Cloud-Optimize TIFF OffHab layer
#'
#' Use offhabr `lyr_key` to pull range from database and render map with `oh_map_cog()`
#'
#' @param lyr_key layer key (in form of `{dataset_key}_{aphia_id}`)
#' @param lyr_title layer title
#' @param con database connection
#' @param ... parameters passed onto `oh_map_cog()`
#'
#' @return `leaflet::leaflet()` object
#' @concept viz
#' @import leaflet
oh_map_cog_lyr <- function(
    lyr_key,
    lyr_title  = "% Habitat",
    con        = con,
    show_zones = T,
    ...){

  d_rng <- tbl(con, "lyrs") |>
    filter(lyr_key == !!lyr_key) |>
    select(val_min, val_max) |>
    collect()

  m <- oh_map_cog(
    cog_file  = glue("{lyr_key}.tif"),
    cog_range = c(d_rng$val_min, d_rng$val_max),
    title     = lyr_title, ...)

  if (show_zones){
    f_zns <- oh_zones_s1k |>
      filter(zone_version == 1)

    m <- m |>
      addPolygons(
        data        = f_zns,
        color       = "black",
        weight      = 1,
        opacity     = 1,
        fillOpacity = 0,
        label       = ~zone_name)
  }
 m

}

#' Map Cloud-Optimize TIFF OffHab Species
#'
#' Use offhabr `aphia_id` to pull range from database and render map with `oh_map_cog()`
#'
#' @param aphia_id unique taxonomic identifier from [MarineSpecies.org](https://www.marinespecies.org/about.php#what_is_aphia)
#' @param lyr_title layer title
#' @param con database connection
#' @param ... other parameters to `oh_map_cog()`
#'
#' @return htmlwidget map as `leaflet::leaflet()`
#' @export
#' @concept viz
oh_map_cog_sp <- function(
    aphia_id,
    lyr_title = "% Habitat",
    con   = con,
    ...){

  # aphia_id = 282884

  d_lyr <- tbl(con, "lyrs") |>
    filter(
      aphia_id == !!aphia_id,
      is_ds_prime == TRUE) |>
    select(lyr_key, val_min, val_max, bbox) |>
    collect()
  b <- d_lyr$bbox[[1]]

  oh_map_cog(
    cog_file  = glue("{d_lyr$lyr_key}.tif"),
    cog_range = c(d_lyr$val_min, d_lyr$val_max),
    title     = lyr_title, ...) |>
    fitBounds(b[1], b[2], b[3], b[4])
}

#' Map score as deviation from average in zone with blocks
#'
#' @param zone_key one of `zone_key` from `offhabr::oh_zones`
#' @param stk_web_tif path to `stack_web.tif` containing layers "oh_zones_v1_web" and "score_v1_web"
#' @param zonal_blocks_csv path to zonal average values of blocks
#'
#' @return htmlwidget map as `leaflet::leaflet()` with `attr("zone_name")`
#' @import dplyr htmltools leaflet readr
#' @importFrom terra rast mask trim global
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @export
#' @concept viz
oh_map_zone_score_dev <- function(
    zone_key,
    stk_web_tif      = "~/My Drive/projects/offhab/data/derived/stack_web.tif",
    zonal_blocks_csv = "~/Github/ecoquants/offhab-scripts/data/zonal_blocks.csv"){
  # map zone's deviation from score avg for raster and blocks

  # raster stack with zones and scores
  stk_web <- terra::rast(stk_web_tif)

  # attribute scores for blocks
  d_b <- readr::read_csv(zonal_blocks_csv, show_col_types=F)

  # polygons of block
  ply_blocks <- offhabr::oh_blocks |>
    dplyr::filter(zone_version == 1) |>
    dplyr::left_join(
      d_b, by = c("block_id" = "block_id_v1"))

  # get raster of score for zone, applying mask
  zone_id <- offhabr::oh_zones_s1k |>
    dplyr::filter(zone_key == !!zone_key) |>
    dplyr::pull(zone_id)
  r_z <- stk_web[["oh_zones_v1_web"]] == zone_id
  r_score_z <- stk_web[["score_v1_web"]] |>
    terra::mask(r_z, maskvalues=c(NA,0)) |>
    terra::trim()

  # normalize scores: [x_i - mean(x)] / sd(x) [see terra::scale()]
  score_z_avg <- terra::global(r_score_z, "mean", na.rm=T) |> as.numeric()
  score_z_sd  <- terra::global(r_score_z, "sd", na.rm=T) |> as.numeric()
  r_score_zn  <- (r_score_z - score_z_avg) / score_z_sd

  # get polygons of blocks for zone, calculate same normalized score
  ply_blocks_z <- ply_blocks |>
    dplyr::filter(
      zone_key == !!zone_key) |>
    dplyr::mutate(
      score_v1_web_zn = (score_v1_web - score_z_avg) / score_z_sd)

  # setup color ramp
  score_zn_vals <- values(r_score_zn, na.rm=T)
  score_zn_rng  <- range(score_zn_vals)
  pal <- leaflet::colorNumeric(
    "Spectral", score_zn_rng, na.color = "transparent", reverse = T)

  # setup popups and labels
  popups <- with(
    ply_blocks_z, glue::glue(
      "BOEM Block: <b>{protraction_number} {block_number}</b><br>
     BOAM Plan: <b>{plan_additional_information}</b><br>
     area_km<sup>2</sup>: <code>{round(area_km2, 2)}</code><br>
     score: <b><code>{round(score_v1_web,1)}</code></b><br>
     sd from zone avg score: <b><code>{round(score_v1_web_zn, 2)}</code></b>"))
  labels <- popups |> lapply(htmltools::HTML)


  m <- leaflet::leaflet() |>
    # add base: blue bathymetry and light brown/green topography
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = leaflet::providerTileOptions(
        variant = "Ocean/World_Ocean_Base",
        opacity = 0.5)) |>
    # add reference: placename labels and borders
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = leaflet::providerTileOptions(
        variant = "Ocean/World_Ocean_Reference",
        opacity = 0.5)) |>
    leaflet::addRasterImage(
      r_score_zn,
      colors = pal, opacity = 0.7) |>
    leaflet::addLegend(
      pal = pal, values = score_zn_vals,
      title = glue(
        "Deviation from<br>
       Score Avg in Zone")) |>
    leaflet::addPolygons(
      data        = ply_blocks_z,
      fillColor   = ~pal(score_v1_web_zn),
      fillOpacity = 0.9,
      color       = 'black',
      opacity     = 0.9,
      weight      = 0.5,
      popup       = popups,
      label       = labels) |>
    leaflet.extras::addFullscreenControl()

  # set zone_name for populating header as attribute of output map
  zone_name <- oh_zones_s1k |>
    dplyr::filter(
      zone_version == 1,
      zone_key == !!zone_key) |>
    dplyr::pull(zone_name)
  attr(m, "zone_name") <- zone_name

  m
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
#' @return hexadecimal colors
#' @export
#' @concept viz
get_colors <- function(x, v_colors = oh_colors){
  v_colors[x] |>
    unname()
}


