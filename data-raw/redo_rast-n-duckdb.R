# packages, con ----
librarian::shelf(
  devtools, here, terra)
load_all()

con_pg <- oh_pg_con()

# oh_zones: gdal_translate to smaller ----
z_tif  <- system.file("oh_zones.tif", package = "offhabr")
r_z <- terra::rast(z_tif)
plot(r_z)

# gdalinfo oh_zones.tif
# Driver: GTiff/GeoTIFF
# Files: oh_zones.tif
# Size is 14678, 7183
# ...
# Metadata:
#   AREA_OR_POINT=Area
# Image Structure Metadata:
#   COMPRESSION=LZW
#   INTERLEAVE=BAND
# ...
# Band 1 Block=14678x1 Type=Byte, ColorInterp=Gray
#   Description = zone_id
#   Min=1.000 Max=22.000
# ...

# ?terra::writeRaster
#   datatype = # "INT1U", "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S"

# https://gdal.org/drivers/raster/gtiff.html
# Internal nodata masks
#   FILETYPE_MASK bit value is set on the TIFFTAG_SUBFILETYPE
#   GDAL_TIFF_INTERNAL_MASK configuration option is set to YES
# Sparse files
#   SPARSE_OK creation option to YES
# Creation Options
#   NUM_THREADS=number_of_threads/ALL_CPUS
#   BIGTIFF=YES

setwd(here("inst"))

# gdal_translate \
# -a_nodata 255 \
# -co COMPRESS=DEFLATE \
# -co ZLEVEL=9 \
# -co PREDICTOR=2 \
# -co TILED=YES \
# -co SPARSE_OK=TRUE \
# -ot Byte \
# oh_zones.tif \
# oh_zones_7.tif
r7 <- terra::rast(here("inst/oh_zones_7.tif")) # WHOAH! 2.2MB to 153.4 KB -- BEST!
plot(r7)

# gdal_translate \
# -a_nodata 255 \
# -co COMPRESS=LZW \
# -co PREDICTOR=2 \
# -co TILED=YES \
# -co SPARSE_OK=TRUE \
# -ot Byte \
# oh_zones.tif \
# oh_zones_8.tif
# r8 <- terra::rast(here("inst/oh_zones_8.tif")) # WHOAH! 2.2MB to 349 KB
# plot(r8)

# raster enable and upgrade postgis ----

# before: brew upgrade postgis
# SELECT postgis_full_version() ;
# POSTGIS="3.3.1 3786b21" [EXTENSION] PGSQL="140"
# GEOS="3.11.0-CAPI-1.17.0" PROJ="9.1.0" LIBXML="2.10.2" LIBJSON="0.16" LIBPROTOBUF="1.4.1"
# WAGYU="0.5.0 (Internal)"

# after: brew upgrade postgis
# SELECT postgis_extensions_upgrade();

# SELECT postgis_full_version();
# POSTGIS="3.3.2 4975da8" [EXTENSION] PGSQL="140"
# GEOS="3.11.1-CAPI-1.17.1" PROJ="9.1.1" LIBXML="2.10.3" LIBJSON="0.16" LIBPROTOBUF="1.4.1"
# WAGYU="0.5.0 (Internal)"

# enable
# ALTER SYSTEM postgis.enable_outdb_rasters = true;
# ALTER SYSTEM SET postgis.gdal_enabled_drivers TO 'ENABLE_ALL';
# SELECT pg_reload_conf();
# invalidate/reconnect to db

# CREATE EXTENSION postgis_raster;
# ALTER DATABASE offhab SET postgis.enable_outdb_rasters = true;

# raster2pgsql \
# -d \
# -s 3857 \
# -C \
# -r \
# -t 256x256 \
# -I \
# -R \
# -F \
# '/Users/bbest/Github/ecoquants/offhabr/inst/oh_zones_8.tif' \
# 'oh_zones_8' \
# | psql offhab

# rpostgis attempts ----

librarian::shelf(rpostgis)
pgPostGIS(con)
pgListGeom(con, geog = TRUE)
pgListRast(con)

r8 <- pgGetRast(con, "oh_zones_8")
# [pgGetRast.R](https://github.com/mablab/rpostgis/blob/74a6ac71bcc21b7b5edf0ed6b59dcbfd4d7bfc59/R/pgGetRast.R#L128-L135)

vals <- dbGetQuery(con, "SELECT ST_DumpValues(ST_Union(rast), ) AS vals FROM oh_zones_8")
range(vals)
rout <- raster::raster(nrows = info$rows, ncols = info$cols,
                       xmn = info$xmn, xmx = info$xmx, ymn = info$ymn, ymx = info$ymx,
                       crs = sp::CRS(p4s), val = vals)

# select
# ST_AsTIFF(ST_Union(rast), 'LZW') as tiff
# FROM oh_zones_8 oz

# test oh_zones ----
# r <- rast(na_tif)
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


# db -> rast() stack -> *.tif ----
librarian::shelf(
  devtools, here, mapview, terra)
load_all()

con <- oh_pg_con()

tbl(con, "cells_ds_ply") %>%
  group_by(tbl) %>%
  summarize(n_cells = n()) %>%
  collect()
#   tbl                     n_cells
#   <chr>                   <int64>
# 1 am_cells             14,910,891
# 2 gm_model_hexagons     3,535,323
# 3 rl_range          1,387,006,236
# 4 sw_density_ply        5,759,071

tbl(con, "cells_ds_ply") %>%
  distinct(tbl, ply_id) %>%
  group_by(tbl) %>%
  summarize(n_mdls = n()) %>%
  # show_query()
  collect()
#   tbl                 n_mdls
#   <chr>              <int64>
# 1 am_cells             1,108
# 2 gm_model_hexagons   17,039 # No way! only a few models
# 3 rl_range               706 # WHAT!? the least models, but most cells?
# 4 sw_density_ply       8,652 # same! no way

tbl(con, "cells_ds_ply") # %>% colnames() %>% paste(collapse=", ") %>% cat()
# tbl, ply_id, cell_id, ds_key
tbl(con, "ds_rl") # %>% colnames() %>% paste(collapse=", ") %>% cat()
# aphia_id, taxa, id_no, binomial, presence, origin, seasonal, compiler, yrcompiled, citation, subspecies, subpop, source, island, tax_comm, dist_comm, generalisd, legend, kingdom, phylum, class, order, family, genus, category, marine, terrestial, freshwater, area_km2, row_id

tbl(con, "cells_ds_rast") %>%
  distinct(tbl, ds_key, mdl_id) %>%
  group_by(tbl, ds_key) %>%
  summarize(n_mdls = n()) %>%
  # show_query()
  collect()
#   tbl        ds_key  n_mdls
#   <chr>      <chr>  <int64>
# 1 du_density NA          50
# 2 nc_density NA          93
# 3 oa_models  NA         759
# 4 sm_model   NA           1
# 5 ve_model   NA           1
# 6 vg_model   NA           1

# ?terra::writeRaster
#   datatype = # "INT1U", "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S"

# https://gdal.org/programs/gdal_translate.html
# -ot {Byte/Int8/Int16/UInt16/UInt32/Int32/UInt64/Int64/Float32/Float64/
#      CInt16/CInt32/CFloat32/CFloat64}

# https://search.r-project.org/CRAN/refmans/raster/html/dataType.html
# Datatype definition	minimum possible value	maximum possible value
# LOG1S	FALSE(0)	TRUE(1)
# INT1S	-127	    127
# INT1U	0	        255
# INT2S	-32,767	  32,767
# INT2U	0	        65,534
# INT4S	-2,147,483,647	2,147,483,647
# INT4U	0	        4,294,967,296
# FLT4S	-3.4e+38	3.4e+38
# FLT8S	-1.7e+308	1.7e+308

# * rl_* (0.5 or 50) black abalone ----
tbl(con, "ds_rl") %>%
  filter(aphia_id == 405012) %>%
  select(aphia_id, taxa, id_no, row_id)
#   aphia_id taxa                 id_no row_id
#      <int> <chr>                <dbl>  <int>
# 1   405012 Haliotis cracherodii 41880      1
# black abalone along CA coast: https://www.iucnredlist.org/species/41880/78775277
cell_ids <- tbl(con, "cells_ds_ply") %>%
  filter(
    tbl    == "rl_range",
    ply_id == 1) %>%
  pull(cell_id)
# sum(duplicated(cell_ids)) # 0
r <- oh_rast()
r[cell_ids] <- 0.5 # TODO: 0.5 REAL vs 50 SMALLINT
# r <- terra::trim(r)
lyr <- "rl_405012"
names(r) <-  lyr
# mapview::mapView(r, maxpixels=ncell(r))
plot(r)
tif <- glue("{lyr}_0.tif")
writeRaster(r, tif, overwrite=T)
# gdal_translate -co COMPRESS=LZW -co PREDICTOR=2 -co TILED=YES \
#   -co SPARSE_OK=TRUE -ot Float32 rl_405012_0.tif rl_405012_0b.tif
# 5.9 MB -> 62.3 KB

r[cell_ids] <- 50
writeRaster(
  r, datatype = "INT1U",
  glue("{lyr}_1.tif"), overwrite=T)
# gdal_translate -a_nodata 255 -co COMPRESS=LZW -co PREDICTOR=2 -co TILED=YES \
#   -co SPARSE_OK=TRUE -ot Byte rl_405012_1.tif rl_405012_1b.tif
# 1.5 MB -> 44 KB
# gdal_translate -a_nodata 255 \
#   -co COMPRESS=DEFLATE -co ZLEVEL=9 \
#   -co PREDICTOR=2 -co TILED=YES -co SPARSE_OK=TRUE -ot Byte \
#   rl_405012_1.tif rl_405012_1c.tif
# 1.5 MB -> 29 KB
rast(glue("{lyr}_0.tif")) %>% trim() %>% mapView()
rast(glue("{lyr}_0b.tif")) %>% trim() %>% mapView()
rast(glue("{lyr}_1.tif")) %>% trim() %>% mapView()
rast(glue("{lyr}_1b.tif")) %>% trim() %>% mapView()

# * vg_model (0 to 1) black abalone ----
d_cv <- tbl(con, "cells_ds_rast") %>%
  filter(
    tbl    == "vg_model",
    mdl_id == 1) %>%
  select(cell_id, val) %>%
  collect()
range(d_cv$val) # 217.1408 6174.7437
# TODO: add fld datasets.val_max: 6174.7437
# TODO: add fld datasets.xformOrigto100: val / 6174.7437 * 100
# TODO: add fld datasets.xform100toOrig: val / 100 * 6174.7437
d_cv <- d_cv %>%
  mutate(
    val_100  = round(val / 6174.7437 * 100),
    val_orig = val_100 / 100 * 6174.7437)
range(d_cv$val_100) # 4 100
# sum(duplicated(cell_ids)) # 0
r <- oh_rast()
r[d_cv$cell_id] <- d_cv$val
# r <- terra::trim(r)
lyr <- "vg"
names(r) <-  lyr
# mapview::mapView(r, maxpixels=ncell(r))
plot(r)
tif <- glue("{lyr}_0.tif")
writeRaster(r, tif, overwrite=T)
# gdal_translate -co COMPRESS=LZW -co PREDICTOR=2 -co TILED=YES \
#   -co SPARSE_OK=TRUE -ot Float32 vg_0.tif vg_0b.tif
# 69.6 MB -> 22.4 MB

r[d_cv$cell_id] <- d_cv$val_100
writeRaster(
  r, datatype = "INT1U",
  glue("{lyr}_1.tif"), overwrite=T)
# gdal_translate -a_nodata 255 -co COMPRESS=LZW -co PREDICTOR=2 -co TILED=YES \
#   -co SPARSE_OK=TRUE -ot Byte vg_1.tif vg_1b.tif
# 4 MB -> 720 KB
# gdal_translate -a_nodata 255 \
#   -co COMPRESS=DEFLATE -co ZLEVEL=9 \
#   -co PREDICTOR=2 -co TILED=YES -co SPARSE_OK=TRUE -ot Byte vg_1.tif vg_1c.tif
# 4 MB -> 450 KB

rast(glue("{lyr}_0.tif")) %>% trim() %>% mapView()
rast(glue("{lyr}_0b.tif")) %>% trim() %>% mapView()
rast(glue("{lyr}_1.tif")) %>% trim() %>% mapView()
rast(glue("{lyr}_1b.tif")) %>% trim() %>% mapView()

# try directory of rasters ----
tifs <- c("oh_zones_7.tif", "rl_405012_1c.tif", "vg_1c.tif")
r <- rast(tifs)
plot(r[["vg"]])

writeRaster(
  r, datatype = "INT1U",
  "stack3_1.tif", overwrite=T)
# gdal_translate -a_nodata 255 \
#   -co COMPRESS=DEFLATE -co ZLEVEL=9 \
#   -co PREDICTOR=2 -co TILED=YES -co SPARSE_OK=TRUE -ot Byte \
#   stack3_1.tif stack3_1c.tif
# 12 MB -> 598 KB
r <- rast("stack3_1c.tif")
plot(r[["vg"]])
plot(r[["zone_id"]])
plot(r[["rl_405012"]])

# duckdb() ----

# https://duckdb.org/docs/api/r

# connect to default duckdb
devtools::load_all()
con <- oh_con(read_only=F)

# write a table to it
dbWriteTable(con, "iris", iris)
# list tables
dbListTables(con)
# remove table
dbRemoveTable(con, "iris")

# disconnect and shutdown
dbDisconnect(con, shutdown=T)

# zones per tif ----
librarian::shelf(
  devtools, fs, glue, leaflet, multidplyr, stringr, terra, tidyr)
load_all("~/Github/ecoquants/offhabr")

dir_tif <- "/Users/bbest/My Drive/projects/offhab/data/_oh_rast0to100"

rx_tif <- "([a-z]{2})_([0-9]+)\\.tif$"
d <- tibble(
  path_tif = list.files(dir_tif, rx_tif, full.names = T, recursive = T)) %>%
  mutate(
    tif       = basename(path_tif),
    ds_key    = str_replace(tif, rx_tif, "\\1"),
    aphia_id  = str_replace(tif, rx_tif, "\\2"))
# nrow(d)         # 11,245
# table(d$ds_key)
# #   am   oa   rl
# # 9645  641  959
system.file("oh_zones.tif", package = "offhabr")
# r_z <- offhabr::oh_rast("zone_id")

tif_to_zones_db <- function(tif){
  # tif = d$path_tif[1]
  dir_csv <- "/Users/bbest/My Drive/projects/offhab/data/_oh_rast0to100/csv"
  csv <- glue::glue("{dir_csv}/zones_{fs::path_ext_remove(basename(tif))}.csv")

  r   <- terra::rast(tif)             # terra::plet(r  , tiles="Esri.NatGeoWorldMap")
  # r_z <- offhabr::oh_rast("zone_id")  # terra::plet(r_z, tiles="Esri.NatGeoWorldMap")
  r_z_tif <- "/Users/bbest/Github/ecoquants/offhabr/inst/oh_zones.tif"
  r_z <- terra::rast(r_z_tif)

  d_z <- terra::zonal(r, r_z, fun="mean", na.rm=T) %>%
    tidyr::pivot_longer(!zone_id, names_to = "lyr_key", values_to = "mean") %>%
    dplyr::filter(!is.na(mean)) %>%
    dplyr::select(zone_id, lyr_key, mean) %>%
    dplyr::mutate(
      dtime_exec = Sys.time())

  # con <- offhabr::oh_con()
  readr::write_csv(d_z, csv)

  T
}

cl <- new_cluster(parallel::detectCores() - 2)
cluster_library(
  cl, c(
    "DBI", "dplyr", "duckdb", "fs", "glue", "purrr", "readr", "terra", "tidyr"))
cluster_assign(
  cl,
  tif_to_zones_db = tif_to_zones_db)

d %>%
  # slice(1:3) %>%
  partition(cl) %>%
  mutate(
    status = map_lgl(path_tif, tif_to_zones_db))

# * TODO: mv data from CSVs of zones per tif into duckdb ----
librarian::shelf(
  # devtools, fs, glue, leaflet, multidplyr, sf, stringr, terra, tidyr)
  DBI, devtools, purrr, readr)
load_all("~/Github/ecoquants/offhabr")
options(readr.show_col_types = F)

dir_csv <- "/Users/bbest/My Drive/projects/offhab/data/_oh_rast0to100/csv"

d_z <- tibble(
  csv = list.files(dir_csv, "zones.+csv$", full.names = T, recursive = T)) %>%
  # slice(1:10) %>%
  mutate(
    data = imap(csv, function(csv, i){
      if (i %% 100 == 0)
        message(glue("{i} ~ {Sys.time()}"))
      read_csv(csv)
      }),
    nrow = map_int(data, nrow)) %>%
  filter(nrow > 0) %>%
  unnest(data) %>%
  select(-csv, -nrow)
# table(d_z$lyr_key)

# load_all()
con <- oh_con()
if ("lyr_zones" %in% dbListTables(con)){
  # dbRemoveTable(con, "lyr_zones")
  dbWriteTable(con, "lyr_zones", d_z)
  # https://duckdb.org/docs/sql/data_types/overview.html
  dbSendStatement(
    con,
    "ALTER TABLE lyr_zones ALTER zone_id TYPE USMALLINT;") # INT2U
  dbSendStatement(
    con,
    "CREATE UNIQUE INDEX idx_lyr_zones ON lyr_zones (zone_id, lyr_key);")
  dbGetQuery(
    con,
    "SELECT table_name, column_name, data_type FROM information_schema.columns")
} else{
  dbWriteTable(con, "lyr_zones", d_z, append = T)
}

dir_lyrs <- "/Users/bbest/My Drive/projects/offhab/data/_oh_rast0to100"
d_lyrs <- tbl(con, "lyr_zones") %>%
  group_by(lyr_key) %>%
  summarize(n_zones = n()) %>%
  collect() %>%
  tidyr::separate(lyr_key, c("ds_key", "aphia_id"), "_", remove=F) %>%
  mutate(
    tif = glue("{dir_lyrs}/{ds_key}/{lyr_key}.tif")) %>%
  select(-n_zones)
# dbRemoveTable(con, "lyrs")
dbWriteTable(con, "lyrs", d_lyrs)
dbGetQuery(
  con,
  "SELECT table_name, column_name, data_type FROM information_schema.columns
  WHERE table_name = 'lyrs'")
range(as.integer(d_lyrs$aphia_id)) # 101 1,610,477 # INT4U # UINTEGER
dbSendStatement(
  con,
  "ALTER TABLE lyrs ALTER aphia_id TYPE UINTEGER;") # INT4U
dbSendStatement(
  con,
  "CREATE UNIQUE INDEX idx_lyrs ON lyrs (lyr_key);")
dbSendStatement(
  con,
  "CREATE INDEX idx_lyrs_ds_key ON lyrs (ds_key);")
dbSendStatement(
  con,
  "CREATE INDEX idx_lyrs_aphia_id ON lyrs (aphia_id);")

# dbDisconnect(con)
# duckdb::duckdb_shutdown(duckdb::duckdb())
dbDisconnect(con, shutdown=T)


# taxa ----
devtools::load_all()
con_dk <- oh_con(read_only = F) # dbDisconnect(con_dk, shutdown=T)
con_pg <- oh_pg_con()


d_taxa_wm <- tbl(con_pg, "taxa_wm") %>%
  collect()
d_taxa_wm
dbWriteTable(con_dk, "taxa_wm", d_taxa_wm)
dbListTables(con_dk)


# duckdb() -> sqlite ----

# Whoah! on server https://rstudio.offshorewindhabitat.info:
# > con <- oh_con()
#
# Using path_duckdb: /usr/local/lib/R/site-library/offhabr/offhab.duckdb (read_only = TRUE)
# Error in h(simpleError(msg, call)) :
#   error in evaluating the argument 'drv' in selecting a method for function 'dbConnect': rapi_startup: Failed to open database: IO Error: Trying to read a database file with version number 39, but we can only read version 41.
# The database file was created with DuckDB version v0.6.0 or v0.6.1.
#
# The storage of DuckDB is not yet stable; newer versions of DuckDB cannot read old database files and vice versa.
# The storage will be stabilized when version 1.0 releases.
#
# For now, we recommend that you load the database file in a supported version of DuckDB, and use the EXPORT DATABASE command followed by IMPORT DATABASE on the current version of DuckDB.
#
# See the storage page for more information: https://duckdb.org/internals/storage
librarian::shelf(
  DBI, dplyr, duckdb, here, RSQLite)
devtools::load_all()

path_sqlite <- file.path(system.file(package="offhabr"), "offhab.sqlite")
path_duckdb <- file.path(system.file(package="offhabr"), "offhab.duckdb")

con_dk <- dbConnect(duckdb(dbdir = path_duckdb, read_only = T))
con_sl <- dbConnect(RSQLite::SQLite(), path_sqlite)
# dbDisconnect(con_dk, shutdown=T)
# dbDisconnect(con_sl)

dbListTables(con_dk)
# [1] "iris"           "lyr_zone_stats" "taxa_wm"

d_lyr_zone_stats <- tbl(con_dk, "lyr_zone_stats") |>
  collect()
dbWriteTable(con_sl, "lyr_zone_stats", d_lyr_zone_stats)
# tbl(con_sl, "lyr_zone_stats")

d_taxa_wm <- tbl(con_dk, "taxa_wm") |>
  collect()
dbWriteTable(con_sl, "taxa_wm", d_taxa_wm)
tbl(con_sl, "taxa_wm")

dbDisconnect(con_sl)

library(connections)
library(RSQLite)
con <- connection_open(RSQLite::SQLite(), path_sqlite)
tbl(con, "taxa_wm")

