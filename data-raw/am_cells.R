librarian::shelf(
  aquamapsdata, dplyr, DBI, glue,
  janitor, mapview, sf, terra, tibble, units)
devtools::load_all()

# am_cells: run before aquamaps_db.R ----
con_sl <- aquamapsdata::default_db("sqlite") # /Users/bbest/Library/Application Support/aquamaps/am.db

pts_cells <- tbl(con_sl, "hcaf_r") %>%
  select(hcaf_id = ID, CsquareCode, CenterLong, CenterLat) %>%
  collect() %>%
  janitor::clean_names() %>%
  st_as_sf(
    coords = c("center_long", "center_lat"),
    remove = F, crs = 4326)

r_na <- rast(
  xmin=-180, xmax=180, ymin=-90, ymax=90,
  resolution = c(0.5, 0.5))

r_cells <- rasterize(pts_cells, r_na, field = "hcaf_id")

am_cells_ply <- as.polygons(r_cells) %>%
  st_as_sf() %>%
  rename(
    hcaf_id = "lyr.1",
    geom    = "geometry") %>%
  left_join(
    pts_cells %>%
      st_drop_geometry() %>%
      select(hcaf_id, csquare_code, center_long, center_lat),
    by = "hcaf_id")

# am_cell_zones ----
am_cell_zones <- st_intersection(
  am_cells_ply, # original: 177,869 features and 2 fields (hcaf_id, geom)
  oh_zones %>%
    select(
      zone_key, geom)) %>%
  rowid_to_column("zc_id") %>%
  mutate(
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  select(
    zc_id, zone_key,
    hcaf_id, csquare_code, center_long, center_lat,
    area_km2, geom)
# unique by: 0.5 degree hcaf_id + zone_key
# reduced: 1,159 features and 3 fields (with some repeats of hcaf_id b/c zone_key)
# mapView(am_cell_zones, zcol = "zone_key")
usethis::use_data(am_cell_zones, overwrite = TRUE)

# write to pg db
con <- oh_pg_con()
st_write(
  am_cell_zones, con, "am_cell_zones",
  layer_options = c(
    # https://gdal.org/drivers/vector/pg.html#layer-creation-options
    "OVERWRITE=yes", "LAUNDER=true"))
create_index(con, "am_cell_zones", "geom", geom=T)
create_index(con, "am_cell_zones", "zc_id")
create_index(con, "am_cell_zones", "zone_key")
create_index(con, "am_cell_zones", "hcaf_id")
create_index(con, "am_cell_zones", "csquare_code")

# am_cells_grd.tif ----
am_cell_zones_grd <- setValues(
  r_cells,
  ifelse(
    values(r_cells) %in% unique(am_cell_zones$hcaf_id),
    values(r_cells),
    NA)) %>%
  trim()
names(am_cell_zones_grd) = "hcaf_id"
am_cell_zones_grd %>%
  writeRaster("inst/am_cell_zones_grd.tif", overwrite=T)

# check readable
stopifnot(file.exists(system.file(package = "offhabr", "am_cell_zones_grd.tif")))
am_cell_zones_grd <- rast(system.file(package = "offhabr", "am_cell_zones_grd.tif"))
# usethis::use_data(am_cell_zones_grd, overwrite = TRUE)
#   doesn't work b/c terra::rast() are only pointers to raster on filesystem


# am_cell_blocks: run after aquamaps_db.R ----
con <- oh_pg_con()

am_cell_blocks <-  st_read(
  con,
  query =
    "WITH
       b AS (
         SELECT block_key, block_type, zone_key, geom FROM boem_blocks), -- WHERE zone_key = 'cgm'
       zc AS (
         SELECT zc_id, hcaf_id, csquare_code, geom FROM am_cell_zones)   -- WHERE zone_key = 'cgm'
     SELECT
       block_key, block_type, zone_key,
       zc_id, hcaf_id, csquare_code,
       CASE
         WHEN ST_CoveredBy(b.geom, zc.geom)
         THEN b.geom
         ELSE
          ST_Multi(
            ST_Intersection(b.geom, zc.geom))
         END AS geom
     FROM b
       INNER JOIN zc
       ON ST_INTERSECTS(b.geom, zc.geom)
          AND Not ST_Touches(b.geom, zc.geom)") %>%
  rowid_to_column("zcb_id") %>%
  mutate(
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  relocate(geom, .after = last_col())
# mapView(am_cell_blocks, zcol = "zcb_id")

# unique by: block + cell + zone
# 5645 features and 8 fields
usethis::use_data(am_cell_blocks, overwrite = TRUE)

# write to pg db
st_write(
  am_cell_blocks, con, "am_cell_blocks",
  layer_options = c(
    # https://gdal.org/drivers/vector/pg.html#layer-creation-options
    "OVERWRITE=yes", "LAUNDER=true"))

create_index(con, "am_cell_blocks", "geom", geom=T)
create_index(con, "am_cell_blocks", "zcb_id")
create_index(con, "am_cell_blocks", "block_key")
create_index(con, "am_cell_blocks", "csquare_code")
create_index(con, "am_cell_blocks", "hcaf_id")
create_index(con, "am_cell_blocks", "zone_key")

am_cell_blocks_grd <- setValues(
  r_cells,
  ifelse(
    values(r_cells) %in% unique(am_cell_blocks$hcaf_id),
    values(r_cells),
    NA)) %>%
  trim()
names(am_cell_blocks_grd) = "hcaf_id"
am_cell_blocks_grd %>%
  writeRaster("inst/am_cell_blocks_grd.tif", overwrite=T)

# check readable
stopifnot(file.exists(system.file(package = "offhabr", "am_cell_blocks_grd.tif")))
am_cell_blocks_grd <- rast(system.file(package = "offhabr", "am_cell_blocks_grd.tif"))
mapView(am_cell_blocks_grd)
# usethis::use_data(am_cells_grd, overwrite = TRUE)
#   doesn't work b/c terra::rast() are only pointers to raster on filesystem
