librarian::shelf(
  dplyr, DBI, glue, janitor, mapview, sf, tibble, terra, units)
devtools::load_all()

# am_cells.geom & .area_km2 from OLD am_cell_zones ----
con <- oh_pg_con()

am_cell_zones <- st_read(con, "am_cell_zones")
am_cells_geom <- am_cell_zones %>%
  group_by(hcaf_id) %>%
  summarize(
    .groups = "drop") %>%
  st_make_valid() %>%
  mutate(
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units())

am_cells <- dbReadTable(con, "am_cells") %>%
  tibble() %>%
  left_join(
    am_cells_geom,
    by = "hcaf_id") %>%
  relocate(
    area_km2, geom, .after = last_col())

st_write(
  am_cells, con, "am_cells", delete_layer = T)
create_index(con, "am_cells", "geom", geom=T)
create_index(con, "am_cells", "hcaf_id", unique=T)

create_index(con, "am_spp", "species_id", unique=T)
create_index(con, "am_spp", "aphia_id")

# *_ply x cell_id ----
oh_cells_ply <- dbGetQuery(
  con,
  "WITH
  c  AS (
    SELECT
      cell_id, geom
    FROM oh_cells),
  d AS (
    SELECT
      hcaf_id, geom
    FROM am_cells)
  SELECT
    'am_cells' AS tbl,
    d.hcaf_id AS ply_id,
    c.cell_id
  FROM c JOIN
    d ON ST_Covers(d.geom, c.geom)") %>%
  tibble()
dbWriteTable(con, "oh_cells_ply", oh_cells_ply, append=T)

# test raster ----
librarian::shelf(
  terra)

con <- oh_pg_con()

c <- tbl(con, "am_spp") %>%
  filter(aphia_id == 159181) %>% # Fis-50623 _Linophryne arborifera_ (illuminated netdevil anglerfish)
  # NOTE: can have more than one species_id per aphia_id, so takes avg probability
  select(aphia_id, species_id) %>%
  left_join(
    tbl(con, "am_spp_cells") %>%
      select(species_id, probability, hcaf_id),
    by = "species_id") %>%
  left_join(
    tbl(con, "oh_cells_ply") %>%
      filter(tbl == "am_cells") %>%
      select(ply_id, cell_id),
    by = setNames("ply_id", "hcaf_id")) %>%
  left_join(
    tbl(con, "oh_cells") %>%
      select(cell_id, block_id, zone_id, elev),
    by = "cell_id") %>%
  left_join(
    tbl(con, "oh_zones") %>%
      select(zone_id, zone_key, zone_name, region),
    by = "zone_id") %>%
  left_join(
    tbl(con, "boem_blocks") %>%
      select(block_id, block_key, block_type),
    by = "block_id") %>%
  group_by(
    cell_id,
    block_id, block_key,
    zone_id, zone_key, zone_name,
    elev,
    aphia_id) %>%
  summarize(
    probability = mean(probability, na.rm = T),
    .groups = "drop") %>%
  collect()

# * for all cells ----
r <- rast(system.file("oh_na.tif", package = "offhabr"))
r[c$cell_id] <- c[["probability"]]
r <- terra::trim(r)
names(r) <- "probability"
# mapView(r, maxpixels=ncell(r))

# * for zone South Atlantic (soa) ----

# cells
c_z <- c %>%
  filter(zone_key == "soa")

# raster
r_z <- rast(system.file("oh_na.tif", package = "offhabr"))
r_z[c_z$cell_id] <- c_z[["probability"]]
r_z <- terra::trim(r_z)
names(r_z) <- "probability"
# mapView(r_z, maxpixels=ncell(r_z))
