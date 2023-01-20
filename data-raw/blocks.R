# code to prepare BOEM Wind Energy Areas (`oh_blocks`) based on:
# - [Renewable Energy GIS Data | Bureau of Ocean Energy Management](https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data)
#   - BOEMWindLeases_6_30_2022: 3282
#   - BOEMWindPlanningAreaOutlines_7_26_2022: 5563

librarian::shelf(
  dplyr, geojsonsf, glue, mapview, mregions, rgdal,
  rnaturalearth, sf, stringr, units)
devtools::load_all()
select = dplyr::select
mapviewOptions(fgb = F)

dir_data <- "/Users/bbest/My Drive/projects/offhab/data"
gdb <- glue("{dir_data}/boem.gov/BOEMWindLayers_4Download.gdb")

lyrs <- rgdal::ogrListLayers(gdb)
# lyrs
# [1] "BOEM_MHKLeasesandPlanningAreas"
# [2] "BOEMWindLeases_6_30_2022"
# [3] "BOEMWindPlanningAreas_07_26_22"
# [4] "BOEMWindPlanningAreaOutlines_7_26_2022"
# [5] "BOEMWindLeaseOutlines_6_30_2022"
# mapview(read_sf(gdb, lyrs[1]), layer.name = lyrs[1]) +
#   mapview(read_sf(gdb, lyrs[2]), layer.name = lyrs[2]) +
#   mapview(read_sf(gdb, lyrs[3]), layer.name = lyrs[3]) +
#   mapview(read_sf(gdb, lyrs[4]), layer.name = lyrs[4]) +
#   mapview(read_sf(gdb, lyrs[5]), layer.name = lyrs[5])

lse <- read_sf(gdb, lyrs[2])
# table(lse$LEASE_TYPE)
# Commercial           Research Right-of-Way Grant
#       3282                  7                  1
lse <- lse %>%
  select(-LEASE_NUMBER_COMPANY) %>%
  filter(LEASE_TYPE == "Commercial")
# mapview(boem_lse)

pln <- read_sf(gdb, lyrs[3])

intersect(names(pln), names(lse))
# [1] "PROTRACTION_NUMBER" "BLOCK_NUMBER"       "BLOCK_LABEL"        "SUB_BLOCK"
# [5] "Shape_Length"       "Shape_Area"         "Shape"

flds_pln_not_lse <- setdiff(names(pln), names(lse))
# [1] "ADDITIONAL_INFORMATION" "CATEGORY1"              "CATEGORY2"
# [4] "URL1"                   "URL2"

flds_lse_not_pln <- setdiff(names(lse), names(pln))
# [1] "LEASE_NUMBER"     "LEASE_TYPE"       "RESOURCE"         "COMPANY"
# [5] "LEASE_DATE"       "LEASE_TERM"       "PROJECT_EASEMENT" "STATE"
# [9] "PROTRACTION_NAME" "LEASE_DOCUMENT1"  "LEASE_DOCUMENT2"

pln <- pln %>%
  rename_with(
    .fn   = ~ paste0("plan_", .x),
    .cols = all_of(flds_pln_not_lse))

lse <- lse %>%
  rename_with(
    .fn   = ~ paste0("lease_", .x),
    .cols = all_of(flds_lse_not_pln))

oh_blocks <- bind_rows(
  lse %>%
    mutate(
      block_type = "lease"),
  pln %>%
    mutate(
      block_type = "plan")) %>%
  select(-Shape_Length, -Shape_Area) %>%
  rename(
    geom = Shape) %>%
  filter(
    plan_CATEGORY1 != "Gulf of Mexico Call Area") %>%
  mutate(
    BLOCK_NUMBER = str_trim(BLOCK_NUMBER))

# set block_key_pz (pre-zones) ----
names(oh_blocks) <- oh_blocks %>%
  colnames() %>%
  janitor::make_clean_names()
oh_blocks <- oh_blocks %>%
  mutate(
    block_key_pz = glue(
      "{str_trim(protraction_number)}_{str_trim(block_number)}{str_trim(sub_block)}"))
stopifnot(sum(duplicated(oh_blocks$block_key_pz)) == 0)

# filter outside zones ----
# oh_blocks_0 <- oh_blocks
oh_blocks <- oh_blocks %>%
  filter(
    st_intersects(
      oh_blocks, st_union(oh_zones), sparse = F)[,1])

# get non-overlapping ----

str_trim(oh_blocks$sub_block) %>% table()
#       a   A   b   B   c   C   d   D   e   E   f   F   g   G
# 798  32 259  36 204  34 214  32 215  35 256  35 198  36 203
#   h   H   i   I   j   J   k   K   l   L   m   M   n   N   o
#  35 197  34 235  36 176  36 190  31 179  31 271  33 209  31
#   O   p   P
# 220  30 225
# NOTE: upper (in BLOCK_LABEL) and lowercase (not in BLOCK_LABEL)

# * check overlaps: OK ----
o <- st_intersection(oh_blocks)

o <- o %>%
  st_make_valid()
# o_1 <- o
table(st_is_valid(o))
# FALSE    TRUE
#    10  30,658
o <- o |>
  filter(
    st_is_valid(o)) |>
  mutate(
    area_km2  = st_area(geom)  |>
      set_units(km^2) |>
      drop_units(),
    block_key_pz_o1 = map2_chr(
      origins, n.overlaps,
      \(origins, n.overlaps){
        if (n.overlaps != 1)
          return(NA)
        oh_blocks |>
          slice(origins) |>
          pull(block_key_pz)
      }),
    block_key_pz_ogt1 = map2_chr(
      origins, n.overlaps,
      \(origins, n.overlaps){
        if (n.overlaps == 1)
          return(NA)
        oh_blocks |>
          slice(origins) |>
          pull(block_key_pz) |>
          paste(collapse="; ")
      }) ) |>
  arrange(desc(area_km2))
# o_2 <- o
nrow(oh_blocks)  #  5,116
nrow(o)          # 30,658
# remove self-intersections
# o <- o_2
table(is.na(o$block_key_pz_o1))
#  FALSE    TRUE
#  5,106  25,552
o <- o %>%
  filter(
    block_key_pz != block_key_pz_o1 | is.na(block_key_pz_o1))
nrow(o) # 25,552

i <- 1
o$origins[[i]]
o$n.overlaps[i]
o$block_key_pz[i]    # NI09-03_6252KK
oh_blocks %>%
  slice(o$origins[[i]]) %>%
  pull(block_key_pz) # NI09-03_6252KK; NI10-03_6252j
bk_o <- setdiff(slice(oh_blocks, o$origins[[i]]) %>% pull(block_key_pz), o$block_key_pz[i])
# mapView(o[1,]) + mapView(oh_blocks %>% filter(block_key_pz==bk_o))
# just a sliver so ok
which.max(o$area_km2) # 1

# get zone_version for two sets of blocks ----
pts_oh_blocks <- oh_blocks %>%
  mutate(
    geom = st_centroid(geom))

# * version 1: full regions, not restricting by OceanAdapt bottom trawl areas ----
pts_oh_blocks_v1 <- pts_oh_blocks %>%
  st_join(
    oh_zones %>%
      filter(zone_version == 1) %>%
      select(zone_version, zone_id, zone_key, geom))

oh_blocks_v1 <- oh_blocks %>%
  left_join(
    pts_oh_blocks_v1 %>%
      st_drop_geometry() %>%
      select(
        block_key_pz,
        zone_version, zone_id, zone_key),
    by = "block_key_pz") %>%
  mutate(
    block_key = glue("v{zone_version}_{zone_key}_{str_trim(protraction_number)}_{str_trim(block_number)}{str_trim(sub_block)}"),
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  arrange(block_key) %>%
  tibble::rowid_to_column("block_id")

# * version 2: regions restricted to OceanAdapt bottom trawl areas ----
pts_oh_blocks_v2 <- pts_oh_blocks %>%
  st_join(
    oh_zones %>%
      filter(zone_version == 2) %>%
      select(zone_version, zone_id, zone_key, geom))

oh_blocks_v2 <- oh_blocks %>%
  left_join(
    pts_oh_blocks_v2 %>%
      st_drop_geometry() %>%
      select(
        block_key_pz,
        zone_version, zone_id, zone_key),
    by = "block_key_pz") %>%
  mutate(
    block_key = glue("v{zone_version}_{zone_key}_{str_trim(protraction_number)}_{str_trim(block_number)}{str_trim(sub_block)}"),
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  arrange(block_key) %>%
  tibble::rowid_to_column("block_id") %>%
  mutate(
    # NOTE: make block_id unique by adding max of version 1
    block_id = block_id + max(oh_blocks_v1$block_id))

# combine versions 1 & 2 ----

# oh_blocks_1 <- oh_blocks
oh_blocks <- rbind(
  oh_blocks_v1,
  oh_blocks_v2) %>%
  select(-block_key_pz) %>%
  relocate(block_id, block_key, block_type, zone_version, zone_id, zone_key) %>%
  relocate(
    all_of(colnames(oh_blocks) %>% str_subset("^lease_")),
    .after = last_col()) %>%
  relocate(
    all_of(colnames(oh_blocks) %>% str_subset("^plan_")),
    .after = last_col()) %>%
  relocate(
    area_km2, geom,
    .after = last_col())
# colnames(oh_blocks)
# oh_blocks_1 %>%
#   # janitor::clean_names() %>%
#   colnames()

stopifnot(sum(duplicated(oh_blocks$block_key)) == 0)
stopifnot(sum(duplicated(oh_blocks$block_id)) == 0)

# mapview(oh_blocks)
usethis::use_data(oh_blocks, overwrite = TRUE)

# write to pg db
con <- oh_pg_con()
st_write(
  oh_blocks, con, "blocks", delete_layer=T)
create_index(con, "blocks", "geom", geom=T)
create_index(con, "blocks", "block_id", unique=T)
create_index(con, "blocks", "block_key", unique=T)
dbSendQuery(con, "ALTER TABLE blocks ALTER COLUMN block_id TYPE SMALLINT")

# tbl(con, "blocks") %>%
#   group_by(zone_key) %>%
#   summarize(n = n()) %>%
#   collect()
#     zone_key       n
#     <chr>    <int64>
#   1 NA           447
# blocks_nozone <- st_read(
#   con, query = "SELECT * FROM blocks WHERE zone_key IS NULL")
# mapView(blocks_nozone)
# Ah, Hawaii

# usethis::use_data(oh_blocks, overwrite = T)
