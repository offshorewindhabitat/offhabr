# SouthWest Fisheries Science Center Density Surface Models of Marine Mammals (Becker et al, 2020)
#
# [Predictive Models of Cetacean Densities in the California Current Ecosystem, 2020b | InPort](https://www.fisheries.noaa.gov/inport/item/64349)
#
#  The entire study area was divided into 0.1° cells (ranging from approximately 10km x 11km in the south to 7km x 11km in the north), and density, area, and abundance calculated for each.
#
#  Density value in animals/km^2; Source: From model
#
#    aphia_id lyr                                      species_2                      n
#       <int> <chr>                                    <chr>                      <int>
#  1   137087 Minke_whale_summer_fall                  Balaenoptera acutorostrata 12257
#  2   137090 Blue_whale_summer_fall                   Balaenoptera musculus      12257
#  3   137091 Fin_whale_summer_fall                    Balaenoptera physalus      12257
#  4   137092 Humpback_whale_summer_fall               Megaptera novaeangliae     12257
#  5   137094 Short_beaked_common_dolphin_summer_fall  Delphinus delphis          12257
#  6   137098 Rissos_dolphin_summer_fall               Grampus griseus            12257
#  7   137107 Striped_dolphin_summer_fall              Stenella coeruleoalba      12257
#  8   137111 Bottlenose_dolphin_summer_fall           Tursiops truncatus         12257
#  9   242608 Bairds_beaked_whale_summer_fall          Berardius bairdii          12257
# 10   254975 Northern_right_whale_dolphin_summer_fall Lissodelphis borealis      12257
# 11   254978 Long_beaked_common_dolphin_summer_fall   Delphinus capensis         12257
# 12   254987 Dalls_porpoise_summer_fall               Phocoenoides dalli         12257
# 13  1571909 Pacific_white_sided_dolphin_summer_fall  Lagenorhynchus obliquidens 12257

# packages
librarian::shelf(
  devtools, dplyr, janitor, mapview, purrr, sf, tibble, tidyr,
  units, worrms)
load_all()

# read layers ----
# geodatabase
gdb <- "/Users/bbest/My Drive/projects/offhab/data/noaa.maps.arcgis.com/swfsc_cce_becker_et_al_2020b.gdb"

lyrs <- st_layers(gdb) %>% .$name
sw_density <- tibble(
  lyr = lyrs) %>%
  mutate(
    sf = map(lyr, ~ read_sf(gdb, layer = .))) %>%
  unnest(sf) %>%
  rename(
    geom = Shape) %>%
  st_as_sf() %>%
  # oh_fix_ply() ----
  # TODO: oh_fix_ply(): clean_names(), + area_km2 | geom, -(shape_length, shape_area)
  clean_names() %>%
  mutate(
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  select(-shape_length, -shape_area) %>%
  relocate(geom, .after = last_col())

# prepend aphia_id ----
#   from WoRMS matching and update tables taxa, taxa_wm
sw_density <- wm_add_aphia_id(sw_density, species_2)

# prepend rowid ----
sw_density <- sw_density %>%
  rowid_to_column("row_id")

con_oh <- oh_pg_con()
tbl(con_oh, "oh_cells_ply") %>%
  group_by(tbl) %>%
  summarize(n = n()) %>%
  collect()

# group_by(geom) -> *_ply BY (ply_id) ----
sw_density <- sw_density %>%
  group_by(geom) %>%
  mutate(
    ply_id = group_indices())
sw_density_ply <- sw_density %>%
  summarize(
    ply_id = first(ply_id),
    .groups = "drop") %>%
  relocate(ply_id)
sw_density <- sw_density2 %>%
  ungroup() %>%
  st_drop_geometry()
# length(unique(sw_density$lyr)) #      13
# nrow(sw_density)               # 159,341
# nrow(sw_density_ply)           #  12,257
#                      159,341/12,257 = 13
# mapView(sw_density_ply)

# write to database ----
con <- oh_pg_con()
dbWriteTable(con, "sw_density", sw_density, overwrite=T)
create_index(con, "sw_density", "row_id", unique = T)
create_index(con, "sw_density", "aphia_id")
create_index(con, "sw_density", "ply_id")
st_write(sw_density_ply, con, "sw_density_ply", delete_layer=T)
create_index(con, "sw_density_ply", "ply_id", unique = T)
create_index(con, "sw_density_ply", "geom", geom = T)

# *_ply x cell_id ----
sw_density_ply <- st_read(con, "sw_density")
oh_cells_ply <- dbGetQuery(
  con,
  "WITH
  c  AS (
    SELECT
      cell_id, geom
    FROM oh_cells),
  d AS (
    SELECT
      ply_id, geom
    FROM sw_density_ply)
  SELECT
    'sw_density_ply' AS tbl,
    d.ply_id,
    c.cell_id
  FROM c JOIN
    d ON ST_Covers(d.geom, c.geom)") %>%
  tibble()
dbWriteTable(con, "oh_cells_ply", oh_cells_ply, overwrite=T)
create_index(con, "oh_cells_ply", c("tbl", "ply_id" ,"cell_id"), unique = T)

# append aphia_id after fuzzy match in wm_add_aphia_id() ----
x <- tbl(con, "taxa") %>%
  collect()
st_write(x, con, "taxa_0")

table(is.na(x$aphia_id))
DBI::dbSendQuery(con, "DELETE FROM taxa where aphia_id IS NULL")
# 178
con <- oh_pg_con()
x <- tbl(con, "taxa_0") %>%
  filter(
    tbl == "am_spp",
    is.na(aphia_id)) %>%
  select(-aphia_id) %>%
  collect()
# View(x)

y <- offhabr:::wm_add_aphia_id(x, taxa)
dbWriteTable(con, "taxa", y, append=T)

taxas <- z$taxa
am_spp <- tbl(con, "am_spp") %>%
  filter(
    taxa %in% taxas,
    is.na(aphia_id)) %>%
  select(-aphia_id) %>%
  collect() %>%
  left_join(
    y %>%
      filter(!is.na(aphia_id))%>%
      select(-tbl, -fld),
    by = "taxa") %>%
  relocate(aphia_id)

taxas_sql <- paste(am_spp$taxa, collapse="','")
DBI::dbSendQuery(
  con,
  glue("DELETE FROM am_spp WHERE taxa IN ('{taxas_sql}')"))
dbWriteTable(con, "am_spp", am_spp, append=T)

# test raster ----
librarian::shelf(
  terra)

con <- oh_pg_con()

c <- tbl(con, "sw_density") %>%
  filter(aphia_id == 137087) %>% # Minke whale
  select(aphia_id, density, ply_id) %>%
  left_join(
    tbl(con, "oh_cells_ply") %>%
      filter(tbl == "sw_density_ply") %>%
      select(cell_id, ply_id),
    by = "ply_id") %>%
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
  select(
    cell_id,
    block_id, block_key,
    zone_id, zone_key, zone_name,
    elev,
    aphia_id, density) %>%
  collect()

# * for all cells ----
r <- rast(system.file("oh_na.tif", package = "offhabr"))
r[c$cell_id] <- c$density
r <- terra::trim(r)
names(r) <- "density"
mapView(r, maxpixels=ncell(r))

# * for zone Southern California (soc) ----

# cells
c_z <- c %>%
  filter(zone_key == "soc")

# raster
r_z <- rast(system.file("oh_na.tif", package = "offhabr"))
r_z[c_z$cell_id] <- c_z$density
r_z <- terra::trim(r_z)
names(r_z) <- "density"

# blocks
b_z <- boem_blocks %>%
  filter(zone_key == "soc")

c_b <- c_z %>%
  group_by(block_id) %>%
  summarize(
    n_cells = n(),
    density = mean(density),
    .groups = "drop")
table(c_b$n_cells) %>% hist()

# get area per cell across all Zones
c_Z <- tbl(con, "oh_cells") %>%
  select(cell_id, zone_id) %>%
  collect()
r_Z <- rast(system.file("oh_na.tif", package = "offhabr"))
r_Z[c_Z$cell_id] <- c_Z$zone_id
r_Z <- terra::trim(r_Z)
names(r_Z) <- "zone_id"
r_Z_a <- cellSize(r_Z, unit="km")
v_Z_a <- values(r_Z_a, na.rm=T)
range(v_Z_a) # 0.1022107 0.1927433

# mapView(r_Z, maxpixels=ncell(r_Z))
# get area per Block
b_a <- boem_blocks %>%
  pull(area_km2)
range(b_a) # 0.6680945 23.1188952

# map
mapView(r_z, maxpixels=ncell(r_z)) +
  mapView(b_z)
