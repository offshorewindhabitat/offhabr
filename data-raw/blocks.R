# code to prepare BOEM Wind Energy Areas (`oh_blocks`) based on:
# - [Renewable Energy GIS Data | Bureau of Ocean Energy Management](https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data)

librarian::shelf(
  dplyr, geojsonsf, glue, janitor, mapview, mregions, rgdal,
  rnaturalearth, sf, stringr, units)
devtools::load_all()
select = dplyr::select
mapviewOptions(fgb = F)

dir_data <- "/Users/bbest/My Drive/projects/offhab/data"
gdb <- glue("{dir_data}/raw/boem.gov/BOEMWindLayers_4Download.gdb")

(d_lyrs <- st_layers(gdb, do_count = T))
# Driver: OpenFileGDB
# Available layers:
#   layer_name geometry_type features fields crs_name
# 1             BOEM_MHKLeasesandPlanningAreas Multi Polygon       17     21   WGS 84
# 2 BOEM_Wind_Planning_Area_Outlines_8_30_2023 Multi Polygon       23      8   WGS 84
# 3                 BOEM_Wind_Leases_9_27_2023 Multi Polygon     3880     18   WGS 84
# 4              Wind_Lease_Outlines_9_27_2023 Multi Polygon       41     15   WGS 84
# 5        BOEM_Wind_Planning_Areas_10_06_2023 Multi Polygon     8685     11   WGS 84
lyrs <- d_lyrs$name

# mapview(read_sf(gdb, lyrs[1]), layer.name = lyrs[1]) +
#   mapview(read_sf(gdb, lyrs[2]), layer.name = lyrs[2]) +
#   mapview(read_sf(gdb, lyrs[3]), layer.name = lyrs[3]) +
#   mapview(read_sf(gdb, lyrs[4]), layer.name = lyrs[4]) +
#   mapview(read_sf(gdb, lyrs[5]), layer.name = lyrs[5])

lse <- read_sf(gdb, lyrs[3])  # "BOEM_Wind_Leases_9_27_2023"
# table(lse$LEASE_TYPE)
# Commercial           Easement           Research Right-of-Way Grant
#      3,867                  6                  6                  1
lse <- lse %>%
  select(-LEASE_NUMBER_COMPANY) %>%
  filter(LEASE_TYPE == "Commercial")
# mapView(lse)

pln <- read_sf(gdb, lyrs[5])  # "BOEM_Wind_Planning_Areas_10_06_2023"
# mapView(pln)

intersect(names(pln), names(lse))
# [1] "PROTRACTION_NUMBER" "BLOCK_NUMBER"       "BLOCK_LABEL"        "SUB_BLOCK"
# [5] "Shape_Length"       "Shape_Area"         "Shape"

flds_pln_not_lse <- setdiff(names(pln), names(lse))
# [1] "ADDITIONAL_INFORMATION" "URL1"                   "URL2"
# [4] "PRIMARY_WPA_CATEGORY"   "SECONDARY_WPA_CATEGORY"

flds_lse_not_pln <- setdiff(names(lse), names(pln))
# [1] "STATE"            "LEASE_TYPE"       "RESOURCE"         "COMPANY"
# [5] "LEASE_TERM"       "LEASE_DATE"       "LEASE_NUMBER"     "PROJECT_EASEMENT"
# [9] "PROTRACTION_NAME" "LEASE_DOCUMENT1"  "LEASE_DOCUMENT2"

# lse_0 <- lse; pln_0 <- pln
# lse <- lse_0; pln <- pln_0

lse <- lse |>
  rename_with(
    .fn   = ~ paste0("lease_", str_replace(.x, "LEASE_", "")),
    .cols = all_of(flds_lse_not_pln)) |>
  clean_names() |>
  mutate(
    block_key = glue(
      "{str_trim(protraction_number)}_{str_trim(block_number)}{str_trim(sub_block)}")) |>
  relocate(block_key)
stopifnot(sum(duplicated(lse$block_key)) == 0)

pln <- pln |>
  rename_with(
    .fn   = ~ paste0("plan_", .x),
    .cols = all_of(flds_pln_not_lse)) |>
  clean_names() |>
  mutate(
    block_key = glue(
      "{str_trim(protraction_number)}_{str_trim(block_number)}{str_trim(sub_block)}")) |>
  relocate(block_key) |>
  arrange(plan_additional_information, block_key)

pln_block_key_dupes <- pln$block_key[duplicated(pln$block_key)]
pln |>
  filter(block_key %in% pln_block_key_dupes) |>
  st_drop_geometry() |>
  select(plan_additional_information) |>
  table()
# plan_additional_information
# Gulf of Maine Request for Competitive Interest (RFCI)
# 1
# Marine Research Array Requested Lease
# 1
# Oregon Call Area - Brookings
# 79
# Oregon Call Area - Coos Bay
# 24
# Oregon Draft WEA A - Coos Bay
# 24
# Oregon Draft WEA B - Brookings
# 79
pln <- pln |>
  filter(!duplicated(pln$block_key))
pln |>
  st_drop_geometry() |>
  filter(block_key %in% pln_block_key_dupes) |>
  select(plan_additional_information) |>
  table()
# plan_additional_information
# Gulf of Maine Request for Competitive Interest (RFCI)
# 1
# Oregon Call Area - Brookings
# 79
# Oregon Call Area - Coos Bay
# 24

stopifnot(sum(duplicated(lse$block_key)) == 0)
stopifnot(sum(duplicated(pln$block_key)) == 0)
stopifnot(intersect(lse$block_key, pln$block_key) == 0)

# spatial row bind, with st_geometry column
blks_geom <- rbind(
  lse |>
    select(block_key),
  pln |>
    select(block_key)) |>
  rename(geom = Shape)

# non-spatial row bind, with differing columns
blks_data <- bind_rows(
  lse |>
    st_drop_geometry() |>
    mutate(
      block_type = "lease"),
  pln |>
    st_drop_geometry() |>
    mutate(
      block_type = "plan")) |>
  select(-shape_length, -shape_area) |>
  mutate(
    protraction_number = str_trim(protraction_number),
    block_number       = str_trim(block_number),
    sub_block          = str_trim(sub_block))
stopifnot(nrow(blks_data) == (nrow(lse) + nrow(pln)))

oh_blocks <- blks_geom |>
  inner_join(
    blks_data,
    by = "block_key")
stopifnot(nrow(oh_blocks) == (nrow(lse) + nrow(pln)))
# table(oh_blocks$block_type)
# lease   plan
# 3,867  8,581
# mapView(oh_blocks)

# set block_key_pz (pre-zones) ----
# names(oh_blocks) <- oh_blocks %>%
#   colnames() %>%
#   janitor::make_clean_names()
# oh_blocks <- oh_blocks %>%
#   mutate(
#     block_key_pz = glue(
#       "{str_trim(protraction_number)}_{str_trim(block_number)}{str_trim(sub_block)}"))
# stopifnot(sum(duplicated(oh_blocks$block_key_pz)) == 0)

# filter outside zones ----
# oh_blocks_0 <- oh_blocks
# mapView(oh_blocks)
# mapView(oh_zones |> filter(zone_version == 1))
oh_blocks <- oh_blocks |>
  filter(
    st_intersects(
      oh_blocks, st_union(filter(oh_zones, zone_version==1)), sparse = F)[,1])

# get non-overlapping ----
# b_a <- oh_blocks |>
#   filter(sub_block == "a")
#
# mapView(b_a)
# st_drop_geometry(b_a)|>
#   View()

str_trim(oh_blocks$sub_block) %>% table()
#    .    A   AA   AB   AC   AD    B   BA   BB   BC   BD
# 2453  545    5    5    5    5  519    1    1    1    1
#    C   CA   CB   CC   CD    D   DA   DB   DC   DD    E
#  537    2    2    2    2  541    3    3    3    3  562
#   EA   EB   EC   ED    F   FA   FB   FC   FD    G   GA
#    3    3    3    3  500    5    5    5    5  524    4
#   GB   GC   GD    H   HA   HB   HC   HD    I   IA   IB
#    4    4    4  534    3    3    3    3  531    1    1
#   IC   ID    J   JA   JB   JC   JD    K   KA   KB   KC
#    1    1  499    2    2    2    2  528    2    2    2
#   KD    L   LA   LB   LC   LD    M   MA   MB   MC   MD
#    2  515    1    1    1    1  567    2    2    2    2
#    N   NA   NB   NC   ND    O   OA   OB   OC   OD    P
#  524    2    2    2    2  565    1    1    1    1  545
#   PA   PB   PC   PD
#    3    3    3    3
# NOTE: now BLOCK_LABEL is all upper case :)

# * check overlaps: OK ----
o <- st_intersection(oh_blocks)

o <- o |>
  st_make_valid()
# o_1 <- o
table(st_is_valid(o))
# FALSE    TRUE
#    29  77,325
# o1 <- o # o <- o1
o <- o |>
  filter(
    st_is_valid(o)) |>
  mutate(
    area_km2  = st_area(geom)  |>
      set_units(km^2) |>
      drop_units(),
    block_key_o1 = map2_chr(
      origins, n.overlaps,
      \(origins, n.overlaps){
        if (n.overlaps != 1)
          return(NA)
        oh_blocks |>
          slice(origins) |>
          pull(block_key)
      }),
    block_key_ogt1 = map2_chr(
      origins, n.overlaps,
      \(origins, n.overlaps){
        if (n.overlaps == 1)
          return(NA)
        oh_blocks |>
          slice(origins) |>
          pull(block_key) |>
          paste(collapse="; ")
      }) ) |>
  arrange(desc(area_km2))
# o_2 <- o
nrow(o)          # 51,003
# remove self-intersections
# o <- o_2
table(is.na(o$block_key_o1))
#   FALSE    TRUE
#  11,578  65,747
o <- o %>%
  filter(
    block_key != block_key_o1 | is.na(block_key_o1))
nrow(o) # 65,747

i <- 1
o$origins[[i]]    # 10158 11194
o$n.overlaps[i]   # 2
o$block_key[i]    # NK10-04_7073NA
oh_blocks |>
  slice(o$origins[[i]]) |>
  pull(block_key) # NK10-04_7073NA; NK10-04_7073MM
bk_o <- setdiff(slice(oh_blocks, o$origins[[i]]) %>% pull(block_key), o$block_key[i])
# names(o)
# mapView(o[1,]) + mapView(oh_blocks %>% filter(block_key==bk_o))
# just a sliver so ok
which.max(o$area_km2) # 1
o$area_km2[1] # 1.438229

# get zone_version for two sets of blocks ----
pts_oh_blocks <- oh_blocks |>
  select(block_key) |>
  mutate(
    geom = st_centroid(geom))
# mapView(pts_oh_blocks)

# * version 1: full regions, not restricting by OceanAdapt bottom trawl areas ----
pts_oh_blocks_v1 <- pts_oh_blocks |>
  st_join(
    oh_zones |>
      filter(zone_version == 1) |>
      select(zone_version, zone_id, zone_key, geom))

oh_blocks_v1 <- oh_blocks |>
  left_join(
    pts_oh_blocks_v1 |>
      st_drop_geometry() |>
      select(
        block_key,
        zone_version, zone_id, zone_key),
    by = "block_key") |>
  # mapview::mapView(oh_blocks_v1)     # 12,001 × 26
  mutate(
    block_key = glue("v{zone_version}_{zone_key}_{block_key}"),
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  arrange(block_key) %>%
  tibble::rowid_to_column("block_id")

# mapview::mapView(oh_blocks)        # 12,001 x 23
# mapview::mapView(pts_oh_blocks_v1) # 12,001 × 5
# mapview::mapView(oh_blocks_v1)     # 12,001 × 28
# table(oh_blocks_v1$zone_key, useNA = "ifany")

# oh_blocks_v1 |>
#   filter(zone_key == "cec") |>
#   mapview::mapView()

# * version 2: regions restricted to OceanAdapt bottom trawl areas ----
# pts_oh_blocks_v2 <- pts_oh_blocks %>%
#   st_join(
#     oh_zones %>%
#       filter(zone_version == 2) %>%
#       select(zone_version, zone_id, zone_key, geom))
#
# oh_blocks_v2 <- oh_blocks %>%
#   left_join(
#     pts_oh_blocks_v2 %>%
#       st_drop_geometry() %>%
#       select(
#         block_key_pz,
#         zone_version, zone_id, zone_key),
#     by = "block_key_pz") %>%
#   mutate(
#     block_key = glue("v{zone_version}_{zone_key}_{str_trim(protraction_number)}_{str_trim(block_number)}{str_trim(sub_block)}"),
#     area_km2  = st_area(geom) %>%
#       set_units(km^2) %>%
#       drop_units()) %>%
#   arrange(block_key) %>%
#   tibble::rowid_to_column("block_id") %>%
#   mutate(
#     # NOTE: make block_id unique by adding max of version 1
#     block_id = block_id + max(oh_blocks_v1$block_id))

# combine versions 1 & 2 ----

# oh_blocks_1 <- oh_blocks
# oh_blocks <- rbind(
#   oh_blocks_v1,
#   oh_blocks_v2) %>%
#   select(-block_key_pz) %>%
#   relocate(block_id, block_key, block_type, zone_version, zone_id, zone_key) %>%
#   relocate(
#     all_of(colnames(oh_blocks) %>% str_subset("^lease_")),
#     .after = last_col()) %>%
#   relocate(
#     all_of(colnames(oh_blocks) %>% str_subset("^plan_")),
#     .after = last_col()) %>%
#   relocate(
#     area_km2, geom,
#     .after = last_col())
# colnames(oh_blocks)
# oh_blocks_1 %>%
#   # janitor::clean_names() %>%
#   colnames()

oh_blocks <- oh_blocks_v1

stopifnot(sum(duplicated(oh_blocks$block_key)) == 0)
stopifnot(sum(duplicated(oh_blocks$block_id)) == 0)

# mapView(oh_blocks)

# TODO: get latest download +
#  Maine [Offshore Wind Planning Areas](https://www.arcgis.com/apps/mapviewer/index.html?layers=ad4e83ed78d24319b641ebbaf1f7298e)

oh_blocks <- oh_blocks |>
  relocate(block_type, .after = block_key) |>
  relocate(zone_version, zone_id, zone_key, .after = block_type) |>
  relocate(protraction_number, block_number, block_label, sub_block, .after = zone_key) |>
  relocate(geom, .after = last_col())
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
