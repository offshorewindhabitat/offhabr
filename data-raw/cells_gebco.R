# bathymetry, high res

# run before:
# - crm_bathy.R
# - gebco_bathy.R

# tifs <- list.files(dir_tif, "tif$", full.names=T)
# stk <- stack(tifs)

# packages ----
librarian::shelf(
  devtools, dplyr, fasterize, glue, here, leaflet, mapview, ncdf4, purrr,
  rlang, sf, terra, tibble, tidyr)
load_all()

# paths ----
dir_g      <- "/Users/bbest/My Drive/projects/offhab/data/gebco.net"
# rasters in geographic projection
g_tif  <- glue("{dir_g}/gebco_gcs.tif")
gc_tif <- glue("{dir_g}/gebco_cell_gcs.tif")
# rasters in mercator projection
e_tif  <- glue("{dir_g}/oh_elev.tif")
c_tif  <- glue("{dir_g}/oh_cell.tif")
na_tif <- glue("{dir_g}/oh_na.tif")
cg_tif <- glue("{dir_g}/oh_cell-gcs.tif")
b_tif  <- glue("{dir_g}/oh_block.tif")
z_v1_tif  <- glue("{dir_g}/oh_zone_v1.tif")
z_v2_tif  <- glue("{dir_g}/oh_zone_v2.tif")
oh_zones_v1_tif <- here("inst/oh_zones_v1.tif")
oh_zones_v2_tif <- here("inst/oh_zones_v2.tif")

# generate rasters in mercator projection ----

# get gebco elevation
r_e <- rast(g_tif)

# make cell identifier for geographic projection
#  NOTE: must be datatype="INT4U", otherwise weird offset duplication
r_c <- setValues(r_e, NA)
cell_ids <- cells(r_e)
r_c[cell_ids] <- cell_ids
names(r_c) <- "cell_id_gcs"
writeRaster(r_c, gc_tif, overwrite=T, datatype="INT4U")

# zone and block rasters
r_z_v1 <- oh_zones %>%
  filter(zone_version == 1) %>%
  rasterize(r_e, field = "zone_id")
names(r_z_v1) <- "zone_id_v1"
r_z_v2 <- oh_zones %>%
  filter(zone_version == 2) %>%
  rasterize(r_e, field = "zone_id")
names(r_z_v2) <- "zone_id_v2"
r_b <- rasterize(oh_blocks, r_e, field = "block_id")

# project to mercator
r_bilinear <- r_e %>%
  project(
    leaflet:::epsg3857,
    method = "bilinear")
r_near <- rast(list(r_c, r_b, r_z_v1, r_z_v2)) %>%
  terra::project(
    leaflet:::epsg3857,
    method = "near")

# write NA and cell_id (cell identifier) rasters in web Mercator projection
r_na <- setValues(r_bilinear, NA)
cell_ids <- cells(r_bilinear)
r_c <- r_na
r_c[cell_ids] <- cell_ids
names(r_c) <- "cell_id"
writeRaster(
  r_na, na_tif, overwrite=T,
  datatype="INT1U")
writeRaster(
  r_c, c_tif, overwrite=T,
  datatype="INT4U")

# write all NA make cell identifier (cell_id) for Mercator projection
r_c <- setValues(r_bilinear, NA)
cell_ids <- cells(r_bilinear)
r_c[cell_ids] <- cell_ids
names(r_c) <- "cell_id"
writeRaster(
  r_c, c_tif, overwrite=T,
  datatype="INT4U")

# write other rasters to most efficient data type
r_cbze <- rast(list(r_near, r_bilinear))
r_cbze
# class       : SpatRaster
# dimensions  : 7183, 14678, 5  (nrow, ncol, nlyr)
# resolution  : 481.3177, 481.3177  (x, y)
# extent      : -14378304, -7313523, 2733139, 6190444  (xmin, xmax, ymin, ymax)
# coord. ref. : WGS 84 / Pseudo-Mercator
# sources     : spat_NnLCuSQdChU2UBo_10223.tif  (4 layers)
#               spat_gWFqRxGX99ns1Jj_10223.tif
# names       : cell_id_gcs, block_id, zone_id_v1, zone_id_v2,        elev
# min values  :        1038,        1,          1,         12, -6366.20312
# max values  :    90163648,     5754,         11,         22,    37.35491

r_cbze %>%
  subset("cell_id_gcs") %>%
  writeRaster(
    cg_tif, overwrite = T,
    datatype = "INT4U")
r_cbze %>%
  subset("block_id") %>%
  writeRaster(
    b_tif, overwrite = T,
    datatype = "INT2U")
r_cbze %>%
  subset("zone_id_v1") %>%
  writeRaster(
    z_v1_tif, overwrite = T,
    datatype = "INT1U")
file.copy(z_v1_tif, oh_zones_v1_tif, overwrite = T)
r_cbze %>%
  subset("zone_id_v2") %>%
  writeRaster(
    z_v2_tif, overwrite = T,
    datatype = "INT1U")
file.copy(z_v2_tif, oh_zones_v2_tif, overwrite = T)

r_cbze %>%
  subset("elev") %>%
  writeRaster(
    e_tif, overwrite = T,
    datatype = "FLT4S")

# convert raster to points in geographic coordinate system and inject into db ----
r_cbze <- rast(c(c_tif, b_tif, z_tif, e_tif))

cids_r <- terra::values(r_cbze$cell_id, na.rm = T)
p_cbze <- r_cbze %>%
  as.points(values=T, na.rm=T, na.all=T) # 14,916,905 # 14,918,018
f_cbze <- st_as_sf(p_cbze) %>%
  st_transform(4326) %>%
  filter(
    !is.na(cell_id))
st_geometry(f_cbze) <- "geom" # 14,918,018 features and 4 fields
stopifnot(
  sum(duplicated(f_cbze$cell_id)) == 0)
# nrow(f_cbze) # 14,916,905

# write to db ----
con <- oh_pg_con()

# get ranges of values to use smallest data type
# d <- f_cbze %>%
#   st_drop_geometry() %>%
#   tibble()
# summary(d)
st_write(
  f_cbze,
  con, "cells", delete_layer = T,
  field.types = c(
    cell_id  = "INTEGER",
    block_id = "SMALLINT",
    zone_id  = "SMALLINT",
    elev     = "REAL",
    geom     = "GEOMETRY")) # 3.8 min
# https://www.postgresql.org/docs/current/datatype-numeric.html
# SMALLINT: +/-                    32,768
# INTEGER:  +/-             2,147,483,648
# BIGINT:   +/- 9,223,372,036,854,775,808
# REAL:             4 bytes variable-precision, inexact 6 decimal digits precision
# DOUBLE PRECISION: 8 bytes variable-precision, inexact 15 decimal digits precision
create_index(con, "cells", "cell_id", unique = T)
create_index(con, "cells", "block_id")
create_index(con, "cells", "zone_id")
create_index(con, "cells", "geom", geom = T)

# test zones ----
r_z_v1z <- oh_rast(type = "zone_id", zone_version=1)
# terra::plet(r_z_v1z, tiles="Esri.NatGeoWorldMap")
r_z_v2z <- oh_rast(type = "zone_id", zone_version=2)
# terra::plet(r_z_v2z, tiles="Esri.NatGeoWorldMap")

# test db ----
# r <- rast(na_tif)
con <- oh_pg_con()
r <- oh_rast()
zone_ids <- oh_zones %>%
  filter(
    zone_key %in% c("cec-wc")) %>%
  pull(zone_id)
d <- tbl(con, "cells") %>%
  filter(zone_id %in% zone_ids) %>%
  select(cell_id, elev) %>%
  collect()
r[d$cell_id] <- d$elev
r <- terra::trim(r)
mapview(r, maxpixels=ncell(r))
# plot(r)
