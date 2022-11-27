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
z_tif  <- glue("{dir_g}/oh_zone.tif")

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
r_z <- rasterize(oh_zones, r_e, field = "zone_id")
r_b <- rasterize(boem_blocks, r_e, field = "block_id")

# project to mercator
r_bilinear <- r_e %>%
  project(
    leaflet:::epsg3857,
    method = "bilinear")
r_near <- rast(list(r_c, r_b, r_z)) %>%
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
# names       :  cell_id_gcs, block_id, zone_id,        elev
# min values  :     1039,        1,       1, -6366.20312
# max values  : 90163648,     5596,      11,    37.35491
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
  subset("zone_id") %>%
  writeRaster(
    z_tif, overwrite = T,
    datatype = "INT1U")
r_cbze %>%
  subset("elev") %>%
  writeRaster(
    e_tif, overwrite = T,
    datatype = "FLT4S")

# convert raster to points in geographic coordinate system and inject into db ----
r_cbze <- rast(c(c_tif, b_tif, z_tif, e_tif))

p_cbze <- r_cbze %>%
  as.points(values=T, na.rm=T, na.all=T) # 14,916,905
f_cbze <- st_as_sf(p_cbze) %>%
  st_transform(4326)  %>%
  mutate(geom = geometry)
st_geometry(f_cbze) <- "geom"
f_cbze <- f_cbze %>% select(-geometry)
# sum(duplicated(f_cbze$cell_id)) # 0

# write to db ----
con <- oh_pg_con()
st_write(f_cbze, con, "oh_cells", delete_layer = T)
create_index(con, "oh_cells", "cell_id", unique = T)
create_index(con, "oh_cells", "block_id")
create_index(con, "oh_cells", "zone_id")
create_index(con, "oh_cells", "geom", geom = T)

# test ----
r <- rast(na_tif)
zone_ids <- oh_zones %>%
  filter(
    zone_key %in% c("fls", "soa")) %>%
  pull(zone_id)
d <- tbl(con, "oh_cells") %>%
  filter(zone_id %in% zone_ids) %>%
  select(cell_id, elev) %>%
  collect()
r[d$cell_id] <- d$elev
r <- terra::trim(r) # 105,432,074 -> 3,967,920
# mapview(r, maxpixels=ncell(r))
# plot(r)


# OLD: gebco by zone ----
# rs_g_z <- list()
# for (i in 1:nrow(oh_zones)){
#   z <- oh_zones %>%
#     slice(i)
#   message(with(z, glue("zone {i} of {nrow(oh_zones)}: {zone_name} ({zone_key}) ~ {Sys.time()}")))
#
#   tif <- glue("{dir_gebco}/gebco_zone-{z$zone_key}.tif") %>% as.character()
#   if (file.exists(tif)){
#     message("  {basename(tif)} exists, skipping")
#     next()
#   }
#   rs_g_z[i] <- mask(r_g, z) %>%
#     terra::trim() %>%
#     project(
#       leaflet:::epsg3857, "bilinear") %>%
#     terra::trim()
#
#   writeRaster(rs_g_z[[i]], tif)
# }

# OLD: crm_* ----
# dir_crm_tif <- "~/My Drive/projects/offhab/data/ngdc.noaa.gov - bathymetry (CRM)"
# crm_tif     <- glue("{dir_crm_tif}/crm_zones.tif")
# crm_tifs    <- list.files(dir_crm_tif, "tif$", full.names=T)
#
# rs_crm <- map(crm_tifs, rast)
# r_crm <- mosaic(sprc(rs_crm))
# mapview(rs_crm[[1]], layer.name = "layer")
# writeRaster(r_crm, crm_tif)
# rs_crm[[1]]
# crm_r
