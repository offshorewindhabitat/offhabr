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
    BLOCK_NUMBER = str_trim(BLOCK_NUMBER)) %>%
  st_join(
    oh_zones %>%
      select(zone_key, geom)) %>%
  mutate(
    block_key = glue("{zone_key}_{PROTRACTION_NUMBER}_{BLOCK_NUMBER}{SUB_BLOCK}"),
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  arrange(block_key) %>%
  tibble::rowid_to_column("block_id")
# mapview(oh_blocks, zcol = "zone_key")

oh_blocks <- oh_blocks %>%
  relocate(block_id, block_key, block_type, zone_key) %>%
  relocate(
    all_of(colnames(oh_blocks) %>% str_subset("^lease_")),
    .after = last_col()) %>%
  relocate(
    all_of(colnames(oh_blocks) %>% str_subset("^plan_")),
    .after = last_col()) %>%
  relocate(
    area_km2, geom,
    .after = last_col()) %>%
  janitor::clean_names()
# colnames(oh_blocks)

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

usethis::use_data(oh_blocks, overwrite = T)
