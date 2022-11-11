# run after aquampas_db.R

librarian::shelf(
  aquamapsdata, dplyr, DBI, janitor, mapview, sf, terra, tibble, units)
devtools::load_all()

con_sl <- aquamapsdata::default_db("sqlite") # /Users/bbest/Library/Application Support/aquamaps/am.db

pts_cells <- tbl(con_sl, "hcaf_r") %>%
  select(ID, CsquareCode, CenterLong, CenterLat) %>%
  collect() %>%
  janitor::clean_names() %>%
  st_as_sf(
    coords = c("center_long", "center_lat"),
    remove = F, crs = 4326)

r_na <- rast(
  xmin=-180, xmax=180, ymin=-90, ymax=90,
  resolution = c(0.5, 0.5))

r_cells <- rasterize(pts_cells, r_na, field = "id")

ply_cells <- as.polygons(r_cells) %>%
  st_as_sf() %>%
  rename(
    id   = "lyr.1",
    geom = "geometry") %>%
  left_join(
    pts_cells %>%
      st_drop_geometry() %>%
      select(id, csquare_code, center_long, center_lat),
    by = "id")

am_cells_ply <- st_intersection(
  ply_cells, # original: 177,869 features and 2 fields (ID, geom)
  oh_zones %>%
    select(
      zone_key, geom)) %>%
  rowid_to_column("cell_id") %>%
  mutate(
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  select(
    cell_id, zone_key,
    id, csquare_code, center_long, center_lat, area_km2, geom)
# unique by: 0.5 degree ID + zone_key
# reduced: 1,159 features and 3 fields (with some repeats of ID b/c zone_key)
# mapView(ply_cells_oh, zcol = "zone_key")

usethis::use_data(am_cells_ply, overwrite = TRUE)

# write to pg db
con <- oh_pg_con()
st_write(
  am_cells_ply, con, "am_cells_ply",
  layer_options = c(
    # https://gdal.org/drivers/vector/pg.html#layer-creation-options
    "OVERWRITE=yes", "LAUNDER=true"))
dbSendQuery(
  con,
  "CREATE INDEX IF NOT EXISTS am_cells_ply_geom_idx ON am_cells_ply USING GIST (geom);")

am_cells_grd <- setValues(
  r_cells,
  ifelse(
    values(r_cells) %in% am_cells_ply$id,
    values(r_cells),
    NA)) %>%
  trim()
names(am_cells_grd) = "id"
am_cells_grd %>%
  writeRaster("inst/am_cells_grd.tif", overwrite=T)

# check readable
stopifnot(file.exists(system.file(package = "offhabr", "am_cells_grd.tif")))
am_cells_grd <- rast(system.file(package = "offhabr", "am_cells_grd.tif"))
# usethis::use_data(am_cells_grd, overwrite = TRUE)
#   doesn't work b/c terra::rast() are only pointers to raster on filesystem
