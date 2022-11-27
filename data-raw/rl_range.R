# IUCN RedList Range Maps
#
# source: https://www.iucnredlist.org/resources/spatial-data-download
# metadata: {dir_g}/RLSpatial_metadata_v6.2.pdf
#
# directories in {dir_g}:
# ABALONES
# CONUS
# LOBSTERS
# MAMMALS_MARINE_ONLY
# MARINEFISH
# REEF_FORMING_CORALS
# TURTLES

# [1] "shp"        "id_no"      "binomial"   "presence"   "origin"
# [6] "seasonal"   "compiler"   "yrcompiled" "citation"   "subspecies"
# [11] "subpop"     "source"     "island"     "tax_comm"   "dist_comm"
# [16] "generalisd" "legend"     "kingdom"    "phylum"     "class"
# [21] "order_"     "family"     "genus"      "category"   "marine"
# [26] "terrestial" "freshwater" "SHAPE_Leng" "SHAPE_Area" "geometry"

# packages
librarian::shelf(
  devtools, dplyr, fs, glue, janitor, mapview, purrr, sf, stringr, tibble, tidyr,
  units, worrms)
load_all()

# read shps, separate invalid geoms, push to db, get worms aphia_id ----
# TODO: shift worms aphia_id to after clip global iucn.rl_range to offhab.rl_range

# paths
dir_g <- "/Users/bbest/My Drive/projects/offhab/data/iucnredlist.org"

# database connections
con_rl <- rl_pg_con()
con_oh <- oh_pg_con()

# * make database connections spatial ----
dbSendQuery(con_rl, "CREATE EXTENSION IF NOT EXISTS postgis;")

# * read layers and write out invalid vs valid geopackages ----
d_shps <- tibble(
  shp = list.files(dir_g, "shp$", recursive = T, full.names = T)) %>%
  mutate(
    size = fs::file_size(shp)) %>%
  arrange(size)
for (i in 1:nrow(d_shps)){ # i = 1
  d_shp <- d_shps %>% slice(i)
  shp <- d$shp[i]
  basename(shp)

  message(with(d_shp, glue(
    "{i} of {nrow(d_shps)}: {basename(shp)} ({size}) ~ {Sys.time()}")))

  f <- read_sf(shp)
  is_valids <- st_is_valid(f)

  b <- basename(shp) %>% path_ext_remove()
  pfx <- glue("{dir_g}/{b}")
  f %>%
    filter(is_valids) %>%
    st_write(glue("{pfx}_valids.gpkg"), append=F)
  f %>%
    filter(!is_valids) %>%
    st_write(glue("{pfx}_invalids.gpkg"), append=F)
}
rm(f, f_valid, f_invalid)
message(glue("FINISH ~ {Sys.time()}"))

# * consolidate invalids ----
d_invalids <- tibble(
  gpkg = list.files(dir_g, "_invalids.gpkg$", recursive = F, full.names = T)) %>%
  mutate(
    size = fs::file_size(gpkg),
    sf = map(gpkg, st_read)) %>%
  unnest(sf) %>%
  st_as_sf()
d_invalids %>%
  st_write(glue("{dir_g}/_invalids_ALL.gpkg"), append=F)
# 108 features with 30 fields and geometry type Multi Polygon
rm(d_invalids)

# * TODO: make invalids valid ----
# d_invalids <- st_read(glue("{dir_g}/_invalids_ALL.gpkg")) %>%
#     st_make_valid()

# * read valid geopackages ----
d_gpkgs <- tibble(
  gpkg = list.files(dir_g, "_valids.gpkg$", recursive = F, full.names = T)) %>%
  mutate(
    size = file_size(gpkg)) %>%
  arrange(size)

# * iterate over geopackages ----
for (i in 1:nrow(d_gpkgs)){ # i = 1
  gpkg <- d_gpkgs$gpkg[i]
  message(glue("{i} of {nrow(d_gpkgs)}: {basename(gpkg)} ~ {Sys.time()}"))

  rl_range <- read_sf(gpkg) %>%
    # TODO: oh_fix_ply() ----
    clean_names() %>%
    mutate(
      area_km2  = st_area(geom) %>%
        set_units(km^2) %>%
        drop_units(),
      taxa = ifelse(
        !is.na(subspecies),
        subspecies,
        binomial)) %>%
    select(-shape_leng, -shape_area) %>%
    relocate(geom, .after = last_col()) %>%
    relocate(taxa)

  message(glue("  wm_add_aphia_id() ~ {Sys.time()}"))
  rl_range <- wm_add_aphia_id(rl_range, taxa)

  message(glue("  write to db ~ {Sys.time()}"))
  dbSendQuery(
    con_rl, glue(
      "DELETE FROM rl_range WHERE id_no IN ({paste(rl_range$id_no, collapse=',')})"))
  if (i == 1){
    st_write(rl_range, con_rl, "rl_range", ovewrite=T)
    # create_index(con_rl, "rl_range", "id_no", unique = T)
    create_index(con_rl, "rl_range", "id_no")
    create_index(con_rl, "rl_range", "geom", geom = T)
    create_index(con_rl, "rl_range", "aphia_id")
  } else {
    st_write(rl_range, con_rl, "rl_range", append=T)
  }
}

# * enable and refresh cross-database querying for iucn to offhab dbs ----
dbSendQuery(con_oh, "CREATE EXTENSION IF NOT EXISTS postgres_fdw")
dbSendQuery(
  con_oh,
  "CREATE SERVER iucn_server FOREIGN DATA WRAPPER postgres_fdw
   OPTIONS (host 'localhost', port '5432', dbname 'iucn')")
dbSendQuery(
  con_oh,
  "CREATE USER MAPPING FOR bbest SERVER iucn_server OPTIONS (user 'bbest')")
dbSendQuery(con_oh, "CREATE SCHEMA iucn")

# refresh cross-database schema
dbSendQuery(con_oh, "DROP SCHEMA IF EXISTS iucn CASCADE")
dbSendQuery(con_oh, "CREATE SCHEMA iucn")
dbSendQuery(con_oh, "IMPORT FOREIGN SCHEMA public from SERVER iucn_server into iucn")

# fixes to iucn.rl_ranges ----

# * add `row_id` pkey fld ----
dbSendQuery(con_rl, "ALTER TABLE rl_range ADD COLUMN row_id SERIAL PRIMARY KEY")

# * fix "subpopulations" in `taxa` fld ----
rs <- tbl(con_rl, "rl_range") %>%
  select(-geom) %>%
  collect()

rs_taxa_subpops <- rs %>%
  filter(str_detect(taxa, "subpop")) %>%
  select(row_id, taxa) %>%
  mutate(
    taxa_words = map(taxa, ~str_split(., " ")[[1]]),
    taxa       = map_chr(taxa_words, ~paste(.[1:2], collapse = " ")),
    subpop     = map_chr(taxa_words, ~{
      paste(.[3:(length(.)-1)], collapse = " ")
    })) %>%
  select(-taxa_words)

# check before update
# row_ids <- rs_taxa_subpops$row_id
# rl_range_1 <- tbl(con_rl, "rl_range") %>%
#   filter(row_id %in% row_ids) %>%
#   select(row_id, taxa, subpop) %>%
#   collect()
# rl_range_1

qs <- rs_taxa_subpops %>%
  glue_data(
    "UPDATE rl_range SET
      taxa   = '{taxa}',
      subpop = '{subpop}'
    WHERE row_id = {row_id};")
walk(qs, ~dbSendQuery(con_rl, .))

# check after update
# rl_range_2 <- tbl(con_rl, "rl_range") %>%
#   filter(row_id %in% row_ids) %>%
#   select(row_id, taxa, subpop) %>%
#   collect()
# rl_range_2

# iucn.rl_range -> offhab.rl_range for OH zone cells  ----

# refresh cross-database schema
dbSendQuery(con_oh, "DROP SCHEMA IF EXISTS iucn CASCADE")
dbSendQuery(con_oh, "CREATE SCHEMA iucn")
dbSendQuery(con_oh, "IMPORT FOREIGN SCHEMA public from SERVER iucn_server into iucn")

# load_all()
con_oh_autooff <- oh_pg_con(autocommit=F)
dbSendQuery(con_oh, "DELETE FROM oh_cells_ply WHERE tbl = 'rl_range'") # ply_id 1:3: 2,049,119
dbSendQuery(con_oh, "DROP INDEX oh_cells_ply_tbl_ply_id_cell_id_idx")

sa <- st_read(con_oh, query = "SELECT ST_Union(geom) AS geom FROM oh_zones")
st_bbox(sa)

row_ids <- dbGetQuery(
  con_oh,
  "WITH
    z AS (
      SELECT ST_Union(geom) AS geom
      FROM oh_zones),
    r AS (
      SELECT row_id, geom
      FROM iucn.rl_range
      WHERE
        presence IN (1, 2, 3, 4))
    SELECT row_id
    FROM r
    JOIN z ON ST_Intersects(r.geom, z.geom)") %>%
  pull(row_id)

dts <- numeric(0)
t0 <- Sys.time()
for (i in 1:length(row_ids)){ # i = 1
  row_id <- row_ids[i] # row_ids[2] row_ids[88]

  # dbSendQuery(
  #   con_oh,
  #   glue(
  #     "DELETE FROM oh_cells_ply WHERE tbl = 'rl_range' AND ply_id = {row_id}")) # ply_id 1:3: 2,049,119

  # NOTE: ST_Subdivide() is critical for pt in poly 1000x performance improvement!
  #   https://postgis.net/docs/ST_Subdivide.html
  dt <- system.time({
    dbSendQuery(
      con_oh,
      glue(
      "WITH
      c  AS (
        SELECT
          cell_id, geom
        FROM oh_cells),
      d AS (
        SELECT
          row_id, ST_Subdivide(geom, 512) AS geom
        FROM iucn.rl_range WHERE
        row_id = {row_id})
      INSERT INTO oh_cells_ply (tbl, ply_id, cell_id)
      SELECT
        'rl_range' AS tbl,
        d.row_id AS ply_id,
        c.cell_id
      FROM c JOIN
        d ON ST_Covers(d.geom, c.geom)")) # d ON ST_Intersects(d.geom, c.geom)"))  #
  })

  # report progress
  dt_i <- dt[["elapsed"]]
  dts <- c(dts, dt_i)
  dt_avg <- mean(dts)
  eta <- t0 + as.difftime(
    (length(row_ids) - i) * dt_avg, units = "secs")
  message(glue("{i} of {length(row_ids)}, row_id={row_id}: finished in {round(dt_i, 1)} sec; ETA all: {eta}"))
  #  61 of 706, row_id= 494: finished in 22.8 sec; ETA all: 2022-11-22 00:06:07
  # 706 of 706, row_id=6354: finished in  5.9 sec; ETA all: 2022-11-21 20:00:54
}


# delete duplicates
dbSendQuery(con_oh, "ALTER TABLE oh_cells_ply ADD COLUMN row_id SERIAL PRIMARY KEY")

x <- dbGetQuery(con_oh, "SELECT * FROM oh_cells_ply LIMIT 100")
x <- tribble(
  ~tbl      , ~ply_id, ~cell_id,
  "rl_range",       1,        1,
  "rl_range",       2,        1,
  "rl_range",       1,        2,
  "rl_range",       1,        1)
dbWriteTable(con_oh, "oh_cells_ply_tmp", x)
dbSendQuery(con_oh, "ALTER TABLE oh_cells_ply_tmp ADD COLUMN row_id SERIAL PRIMARY KEY")
dbGetQuery(
  con_oh,
  "SELECT row_id,
      ROW_NUMBER() OVER (
        PARTITION BY tbl, ply_id, cell_id
        ORDER BY  tbl, ply_id, cell_id )
        AS row_num
    FROM oh_cells_ply_tmp")

dbSendQuery(
  con_oh,
  "DELETE FROM oh_cells_ply
  WHERE row_id IN
  (SELECT row_id
    FROM
    (SELECT row_id,
      ROW_NUMBER() OVER (
        PARTITION BY tbl, ply_id, cell_id
        ORDER BY  tbl, ply_id, cell_id )
        AS row_num
    FROM oh_cells_ply) t
    WHERE t.row_num > 1 )")
tbl(con_oh, "oh_cells_ply_tmp") %>%
  collect()

# recreate index
create_index(con_oh, "oh_cells_ply", c("tbl", "ply_id" ,"cell_id"), unique = T)

# DETAIL:  Key (tbl, ply_id, cell_id)=(rl_range, 392, 80988346) is duplicated.

# + tbl `rl_range`: iucn.rl_range except geom and only for those in US
flds_sql <- tbl(con_rl, "rl_range") %>%
  colnames() %>%
  setdiff("geom") %>%
  paste(collapse = '", "')
dbSendQuery(con_oh, glue(
  "CREATE TABLE rl_range AS
  SELECT \"{flds_sql}\" FROM iucn.rl_range
  WHERE row_id IN (
    SELECT ply_id
    FROM oh_cells_ply
    WHERE tbl = 'rl_range'
    GROUP BY ply_id)"))

# get respective counts from all original to US filtered by presence
dbGetQuery(con_rl, "SELECT COUNT(*) AS n FROM rl_range") # 6,364
dbGetQuery(con_oh, "SELECT COUNT(*) AS n FROM rl_range") #   706

# check unique species; duplicates are sub-populations
dbGetQuery(con_oh, "SELECT COUNT(DISTINCT taxa    ) AS n_taxa     FROM rl_range") # 692
dbGetQuery(con_oh, "SELECT COUNT(DISTINCT aphia_id) AS n_aphia_id FROM rl_range") # 682


# table(rs$presence)
#    1    2    3    4    5    6
# 6263    6   27   20   13   35
# Mapping_Standards_Version_1.19_2021.pdf
# 1: Extant
# 2: Probably Extant (deprecated)
# 3: Possibly Extant
# 4: Possibly Extinct
# 5: Extinct
# 6: Presence Uncertain
# TODO: filter(presence %in% c(1, 2, 3, 4))




# # subset to offhab database ----
#
#
# # *_ply x cell_id ----
# sw_density_ply <- st_read(con, "sw_density")
# oh_cells_ply <- dbGetQuery(
#   con,
#   "WITH
#   c  AS (
#     SELECT
#       cell_id, geom
#     FROM oh_cells),
#   d AS (
#     SELECT
#       ply_id, geom
#     FROM sw_density_ply)
#   SELECT
#     'sw_density_ply' AS tbl,
#     d.ply_id,
#     c.cell_id
#   FROM c JOIN
#     d ON ST_Covers(d.geom, c.geom)") %>%
#   tibble()
# dbWriteTable(con, "oh_cells_ply", oh_cells_ply, overwrite=T)
# create_index(con, "oh_cells_ply", c("tbl", "ply_id" ,"cell_id"), unique = T)
#
# # test raster ----
# librarian::shelf(
#   terra)
#
# con <- oh_pg_con()
#
# c <- tbl(con, "sw_density") %>%
#   filter(aphia_id == 137087) %>% # Minke whale
#   select(aphia_id, density, ply_id) %>%
#   left_join(
#     tbl(con, "oh_cells_ply") %>%
#       filter(tbl == "sw_density_ply") %>%
#       select(cell_id, ply_id),
#     by = "ply_id") %>%
#   left_join(
#     tbl(con, "oh_cells") %>%
#       select(cell_id, block_id, zone_id, elev),
#     by = "cell_id") %>%
#   left_join(
#     tbl(con, "oh_zones") %>%
#       select(zone_id, zone_key, zone_name, region),
#     by = "zone_id") %>%
#   left_join(
#     tbl(con, "boem_blocks") %>%
#       select(block_id, block_key, block_type),
#     by = "block_id") %>%
#   select(
#     cell_id,
#     block_id, block_key,
#     zone_id, zone_key, zone_name,
#     elev,
#     aphia_id, density) %>%
#   collect()
#
# # * for all cells ----
# r <- rast(system.file("oh_na.tif", package = "offhabr"))
# r[c$cell_id] <- c$density
# r <- terra::trim(r)
# names(r) <- "density"
# mapView(r, maxpixels=ncell(r))
#
# # * for zone Southern California (soc) ----
#
# # cells
# c_z <- c %>%
#   filter(zone_key == "soc")
#
# # raster
# r_z <- rast(system.file("oh_na.tif", package = "offhabr"))
# r_z[c_z$cell_id] <- c_z$density
# r_z <- terra::trim(r_z)
# names(r_z) <- "density"
#
# # blocks
# b_z <- boem_blocks %>%
#   filter(zone_key == "soc")
#
# c_b <- c_z %>%
#   group_by(block_id) %>%
#   summarize(
#     n_cells = n(),
#     density = mean(density),
#     .groups = "drop")
# table(c_b$n_cells) %>% hist()
#
# # get area per cell across all Zones
# c_Z <- tbl(con, "oh_cells") %>%
#   select(cell_id, zone_id) %>%
#   collect()
# r_Z <- rast(system.file("oh_na.tif", package = "offhabr"))
# r_Z[c_Z$cell_id] <- c_Z$zone_id
# r_Z <- terra::trim(r_Z)
# names(r_Z) <- "zone_id"
# r_Z_a <- cellSize(r_Z, unit="km")
# v_Z_a <- values(r_Z_a, na.rm=T)
# range(v_Z_a) # 0.1022107 0.1927433
#
# # mapView(r_Z, maxpixels=ncell(r_Z))
# # get area per Block
# b_a <- boem_blocks %>%
#   pull(area_km2)
# range(b_a) # 0.6680945 23.1188952
#
# # map
# mapView(r_z, maxpixels=ncell(r_z)) +
#   mapView(b_z)
